#' @export
#'
#' @title Load rasterized data for a WRF model
#'
#' @param nc WRF NetCDF file.
#' @param vars WRF variable(s) to load.
#' @param xlim A vector of coordinate longitude bounds.
#' @param ylim A vector of coordinate latitude bounds.
#' @param verbose Logical to display messages.
#'
#' @return A \pkg{raster} package \emph{RasterBrick} object.
#'
#' @examples
#' \donttest{
#' library(WRFmet)
#' nc <- ncdf4::nc_open("~/Data/WRF/wrfout_d3-2020071512-f07-0000.nc")
#' raster <- wrf_createRaster(nc, c("HGT", "TSK"))
#' print(raster)
#' }

wrf_createRaster <- function(
  nc = NULL,
  vars = NULL,
  xlim = NULL,
  ylim = NULL,
  verbose = TRUE
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  if (typeof(vars))
    stop(sprintf("vars must be a character vector of variable names"))
  
  if ( !is.logical(verbose) ) verbose <- TRUE
  
  # ----- Define raster grid ---------------------------------------------------
  
  # Get reading coordinates
  lon <- ncdf4::ncvar_get(nc, varid = "XLONG")
  lat <- ncdf4::ncvar_get(nc, varid = "XLAT")
  
  coords <- data.frame(
    x = as.vector(lon),
    y = as.vector(lat)
  )
  
   # Define grid dimensions
  colCount <- dim(lon)[1]
  rowCount <- dim(lon)[2]
  
  # ----- Read in variable data ------------------------------------------------
  
  rasterLayers <- sapply(vars, function(var) {
    # Read in the variable values
    varValues <- ncdf4::ncvar_get(nc, varid = var)
    
    # Define an empty raster layer to use as a reference grid
    referenceRaster <- raster::raster(nrows = rowCount, ncols = colCount)
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
