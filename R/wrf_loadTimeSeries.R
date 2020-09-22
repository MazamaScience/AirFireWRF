#' @export
#'
#' @title Load time series WRF model run data
#' 
#' @param modelName Model identifier.
#' @param modelRun Model initialization timestamp as "YYYYMMDDHH".
#' @param modelRunHours Vector of hours forecasted from init time, i.e. 7:24.
#' @param baseUrl Base URL for WRF output.
#' @param varName WRF variable to load.
#' @param res Resolution of raster in degrees.
#' @param xlim A vector of coordinate longitude bounds.
#' @param ylim A vector of coordinate latitude bounds.
#' 
#' @description Loads time series WRF model run data as a \pkg{raster} package 
#' \emph{RasterBrick}. If the specified data does not exist on the user's 
#' machine, then it will try to be downloaded from the AirFire database.
#' 
#' On 2020-09-17, available model identifiers include the following:
#' \itemize{
#'   \item{PNW-1.33km}
#'   \item{PNW-4km}
#' }
#' 
#' @return A \pkg{raster} package \emph{RasterBrick} object.
#'
#' @examples
#' \donttest{
#' # library(WRFmet)
#' # setWRFDataDir("~/Data/WRF")
#' 
#' # Takes a ridiculously long time to download 4 ~1GB files
#' #tempTimeSeries <- wrf_loadTimeSeries(
#' # modelName = "PNW-1.33km",
#' # modelRun = "2020091712",
#' # modelRunHours = 7:10,
#' # varName = "TSK",
#' # res = 0.05,
#' #  xlim = c(-125, -117),
#' # ylim = c(45, 49)
#' # )
#'
#' # rasterVis::levelplot(tempTimeSeries)
#' }

wrf_loadTimeSeries <- function(
  modelName = NULL,
  modelRun = NULL,
  modelRunHours = NULL,
  baseUrl = "http://m2.airfire.org",
  varName = NULL,
  res = NULL,
  xlim = NULL,
  ylim = NULL
) {
  
  # Download requested model run hours
  filePaths <- sapply(modelRunHours, function(modelRunHour) {
    
    filePath <- wrf_download(
      modelName = modelName,
      modelRun = modelRun,
      modelRunHour = modelRunHour,
      baseUrl = baseUrl
    )
    
    return(filePath)
    
  })
  
  # Load model run hours as RasterLayers for the specified variable
  rasterLayers <- sapply(filePaths, function(filePath) {
    
    nc <- ncdf4::nc_open(filePath)
    
    lon <- ncdf4::ncvar_get(nc, varid = "XLONG")
    lat <- ncdf4::ncvar_get(nc, varid = "XLAT")
    
    coords <- data.frame(
      x = as.vector(lon),
      y = as.vector(lat)
    )
    
    ext <- raster::extent(coords)
    
    rColCount <- (ext@xmax - ext@xmin) / res
    rRowCount <- (ext@ymax - ext@ymin) / res
    
    varValues <- ncdf4::ncvar_get(nc, varid = varName)
    
    referenceRaster <- raster::raster(nrows = rRowCount, ncols = rColCount)
    raster::extent(referenceRaster) <- raster::extent(coords)
    
    rasterLayer <- raster::rasterize(
      x = coords,
      y = referenceRaster,
      field = as.vector(varValues),
      fun = mean
    )
    
    xDomain <- c(raster::xmin(rasterLayer), raster::xmax(rasterLayer))
    yDomain <- c(raster::ymin(rasterLayer), raster::ymax(rasterLayer))
    
    # Calculate xlim
    if ( is.null(xlim) ) {
      xlim <- xDomain
    } else {
      xlim <- c(
        max(xlim[1], xDomain[1]),
        min(xlim[2], xDomain[2])
      )
    }
    
    # Calculate ylim
    if ( is.null(ylim) ) {
      ylim <- yDomain
    } else {
      ylim <- c(
        max(ylim[1], yDomain[1]),
        min(ylim[2], yDomain[2])
      )
    }
    
    cells <- raster::cellFromXY(rasterLayer, cbind(xlim, ylim))
    ext <- raster::extentFromCells(rasterLayer, cells)
    
    # Crop to xlim, ylim
    rasterLayer <- raster::crop(rasterLayer, ext)
    rasterLayer@title <- basename(filePath)
    
    return(rasterLayer)
    
  })
  
  # Consolidate hour RasterLayers into a RasterBrick
  rasterBrick <- raster::brick(rasterLayers)
  names(rasterBrick) <- paste0("hour_", modelRunHours)
  
  # TODO: Set raster brick title
  
  return(rasterBrick)
  
}
