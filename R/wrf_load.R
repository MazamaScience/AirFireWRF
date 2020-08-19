#' @export
#'
#' @title Load rasterized data for a WRF model
#'
#' Some useful variables:
#' \itemize{
#'   \item{XLAT - Latitude (degrees North)}
#'   \item{XLAT_U - Latitude (degrees North)}   
#'   \item{XLAT_V - Latitude (degrees North)}
#'   \item{XLONG - Longitude (degrees East)}
#'   \item{XLONG_U - Longitude (degrees East)}
#'   \item{XLONG_V - Longitude (degrees East)}
#'   \item{U - Wind X component (m/s)}
#'   \item{V - Wind Y component (m/s)}
#'   \item{W - Wind Z component (m/s)}
#'   \item{U10 - Wind X component at 10 meters (m/s)}
#'   \item{V10 - Wind Y component at 10 meters (m/s)}
#'   \item{ZNU - eta values on half (mass) levels}
#'   \item{ZNW - eta values on full (w) levels}
#'   \item{LU_INDEX - Land use category}
#'   \item{Q2 - QV at 2 M (kg)}
#'   \item{T - Perturbation potential temperature theta-t0 (K)}
#'   \item{T2 - Temperature at 2 meters (K)}
#'   \item{TH2 - Potential temperature at 2 meters (K)}
#'   \item{HGT - Terrain height (m)}
#'   \item{RAINNC - Accumulated total grid scale precipitation (mm)}
#'   \item{CFRACT - Total cloud fraction}
#'   \item{PBLH - Planetary boundary layer height (m)}
#' }
#'
#' @param modelName Model identifier.
#' @param modelRun Model initialization datestamp as "YYYYMMDDHH".
#' @param modelMode Hour forecasted from initial time as "HH", i.e. '07'.
#' @param localPath Absolute path to a NetCDF file not found in `modelDataDir`.
#' @param vars WRF variable(s) to load.
#' @param res Resolution of raster in degrees.
#' @param xlim A vector of coordinate longitude bounds.
#' @param ylim A vector of coordinate latitude bounds.
#' @param verbose Logical to display messages.
#'
#' @return A \pkg{raster} package \emph{RasterBrick} object.
#'
#' @examples
#' \donttest{
#' library(WRFmet)
#' 
#' raster <- wrf_load(
#'   localPath = '~/Data/WRF/wrfout_d3-2020071512-f07-0000.nc',
#'   vars = c('HGT', 'TSK'),
#'   res = 0.1,
#'   xlim = c(-125, -116),
#'   ylim = c(45, 50)
#' )
#' 
#' print(raster)
#' }

wrf_load <- function(
  modelName = NULL,
  modelRun = NULL,
  modelMode = NULL,
  localPath = NULL,
  vars = NULL,
  res = NULL,
  xlim = NULL,
  ylim = NULL,
  verbose = TRUE
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  if ( is.null(localPath) ) {
    
    MazamaCoreUtils::stopIfNull(modelName)
    MazamaCoreUtils::stopIfNull(modelRun)
    MazamaCoreUtils::stopIfNull(modelMode)
    
  } else {
    
    clean <- FALSE
    if ( !file.exists(localPath) )
      stop(sprintf("File not found at localPath = '%s'", localPath))
    
  }
  
  if ( !is.logical(verbose) ) verbose <- TRUE
  
  # ----- Access WRF File ------------------------------------------------------
  
  if ( is.null(localPath) ) { # No localPath
    
    fileName <- paste0('wrfout_d3-', modelRun, '-f', modelMode, '-0000.nc')
    filePath <- file.path(getModelDataDir(), fileName)
    
    if ( !file.exists(filePath) ) {
      # TODO: Download model run file from database
      # filePath <- downloadedFilePath
    }
    
  } else { # User specified localPath
    
    filePath <- localPath
    
  }
  
  nc <- ncdf4::nc_open(filePath)
  
  # ----- Define raster grid ---------------------------------------------------
  
  # Get reading coordinates
  lon <- ncdf4::ncvar_get(nc, varid = "XLONG")
  lat <- ncdf4::ncvar_get(nc, varid = "XLAT")
  
  coords <- data.frame(
    x = as.vector(lon),
    y = as.vector(lat)
  )

  if (is.null(res)) {
    # Approximate a grid that covers all the model run's sample points with a 
    # single grid cell measuring DX x DY m^2:
    #
    # *****T*****
    # *****|*****
    # L----+----R
    # *****|*****
    # *****B*****
    
    # Rows and columns of WRF sample points
    pColCount <- ncol(lon)
    pRowCount <- nrow(lon)
    pHalfColCount <- floor(pColCount / 2)
    pHalfRowCount <- floor(pRowCount / 2)
    
    # Center points of the WRF scan's four edges
    left   <- c(lon[            1, pHalfColCount], lat[            1, pHalfColCount])
    right  <- c(lon[    pRowCount, pHalfColCount], lat[    pRowCount, pHalfColCount])
    top    <- c(lon[pHalfRowCount,             1], lat[pHalfRowCount,             1])
    bottom <- c(lon[pHalfRowCount,     pColCount], lat[pHalfRowCount,     pColCount])
    
    # Scan's midle lon and lat lengths
    midLonLength <- geosphere::distGeo(left, right)
    midLatLength <- geosphere::distGeo(top, bottom)
    
    # Grid delta dimensions
    ncGlobals <- ncdf4::ncatt_get(nc, 0)
    dX <- ncGlobals$DX
    dY <- ncGlobals$DY
    
    # Raster cell rows and columns
    rColCount <- floor(midLonLength / dX)
    rRowCount <- floor(midLatLength / dY)
  } else {
    ext <- raster::extent(coords)
    
    # Approximated rows and columns of a Raster grid
    rColCount <- (ext@xmax - ext@xmin) / res
    rRowCount <- (ext@ymax - ext@ymin) / res
  }
  
  # ----- Read in variable data ------------------------------------------------
  
  # TODO: Make sure all the specified vars share the same grid dimensions
  
  rasterLayers <- sapply(vars, function(var) {
    # Read in the variable values
    varValues <- ncdf4::ncvar_get(nc, varid = var)
    
    # TODO: Let users provide a reference raster as an argument, that way they
    # could, for instance, rasterize WRF points onto a BlueSky model grid
    
    # Define an empty raster layer to use as a reference grid
    referenceRaster <- raster::raster(nrows = rRowCount, ncols = rColCount)
    raster::extent(referenceRaster) <- raster::extent(coords)
    #raster::crs(rbase) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 +units=m"
    
    # Rasterize data points onto the reference raster
    rasterLayer <- raster::rasterize(
      x = coords,
      y = referenceRaster,
      field = as.vector(varValues),
      fun = mean
    )
    
    return(rasterLayer)
  })
  
  # Combine the separate variable layers into a RasterStack
  rasterStack <- raster::stack(rasterLayers)
  
  # Solidify the RasterStack into a more efficient/fast RasterBrick
  rasterBrick <- raster::brick(rasterStack)
  
  # ----- Crop RasterBrick -----------------------------------------------------
  
  if ( is.null(xlim) && is.null(ylim) ) {
    
    # Full grid
    return(rasterBrick)
    
  } else {
    
    xDomain <- c(raster::xmin(rasterBrick), raster::xmax(rasterBrick))
    yDomain <- c(raster::ymin(rasterBrick), raster::ymax(rasterBrick))
    
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
    
    cells <- raster::cellFromXY(rasterBrick, cbind(xlim, ylim))
    ext <- raster::extentFromCells(rasterBrick, cells)
    
    # Crop to xlim, ylim
    return(raster::crop(rasterBrick, ext))
    
  }
  
}
