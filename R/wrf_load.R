#' @export
#'
#' @title Load WRF model run data
#' 
#' @param modelName Model identifier.
#' @param modelRun Model initialization timestamp as "YYYYMMDDHH".
#' @param modelRunHour Hour forecasted from initial time, i.e. 7.
#' @param baseUrl Base URL for WRF output.
#' @param localPath Absolute path to a NetCDF file not found in `WRFDataDir`.
#' @param vars WRF variable(s) to load. If \code{NULL}, the following subset
#' specified by AirFire will be loaded: XLONG, XLAT, XLONG_U, XLAT_U, XLONG_V, 
#' XLAT_V, U, V, U10, V10, ZNU, ZNW, LU_INDEX, Q2, T, T2, TH2, HGT, RAINNC, 
#' CFRACT, and PBLH.
#' @param res Resolution of raster in degrees.
#' @param xlim A vector of coordinate longitude bounds.
#' @param ylim A vector of coordinate latitude bounds.
#' @param verbose Logical to display messages.
#' 
#' @description Loads WRF model run data as a \pkg{raster} package 
#' \emph{RasterBrick}. If the specified data does not exist on the user's 
#' machine, then it will try to be downloaded from the AirFire database.
#' 
#' On 2020-09-03, available model identifiers include the following:
#' \itemize{
#'   \item{PNW-1.33km}
#'   \item{PNW-4km}
#' }
#' 
#' Some useful variables provided by WRF:
#' \itemize{
#'   \item{XLONG - Longitude (degrees East)}
#'   \item{XLAT - Latitude (degrees North)}
#'   \item{XLONG_U - Longitude (degrees East)}
#'   \item{XLAT_U - Latitude (degrees North)}
#'   \item{XLONG_V - Longitude (degrees East)}
#'   \item{XLAT_V - Latitude (degrees North)}
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
#' @return A \pkg{raster} package \emph{RasterBrick} object.
#'
#' @examples
#' \donttest{
#' library(WRFmet)
#' setWRFDataDir("~/Data/WRF")
#' 
#' raster <- wrf_load(
#'   modelName = "PNW-4km",
#'   modelRun = "2020082612",
#'   modelRunHour = 9,
#'   vars = c("HGT", "TSK", "U10", "V10"),
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
  modelRunHour = NULL,
  baseUrl = "http://m2.airfire.org",
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
    MazamaCoreUtils::stopIfNull(modelRunHour)
    MazamaCoreUtils::stopIfNull(baseUrl)
    
  } else {
    
    clean <- FALSE
    if ( !file.exists(localPath) )
      stop(sprintf("File not found at localPath = '%s'", localPath))
    
  }
  
  if ( is.null(vars) ) {
    vars <- c(
      "XLONG", "XLAT", "XLONG_U", "XLAT_U", "XLONG_V", "XLAT_V", "U", "V", 
      "U10", "V10", "ZNU", "ZNW", "LU_INDEX", "Q2", "T", "T2", "TH2", "HGT", 
      "RAINNC", "CFRACT", "PBLH"
    )
  }
  
  if ( length(vars) < 1 ) {
    stop(sprintf("Must specify at least one WRF variable"))
  }
  
  if ( !is.logical(verbose) ) verbose <- TRUE
  
  # ----- Download and convert -------------------------------------------------
  
  if ( is.null(localPath) ) { # No localPath
    
    paddedModelRunHour <- stringr::str_pad(modelRunHour, 2, pad = "0")
    fileName <- paste0(modelName, "_", modelRun, "_", paddedModelRunHour, ".nc")
    filePath <- file.path(getWRFDataDir(), fileName)
    
    if ( !file.exists(filePath) ) {
      filePath <- wrf_download(
        modelName = modelName,
        modelRun = modelRun,
        modelRunHour = modelRunHour,
        baseUrl = baseUrl,
        verbose = verbose
      )
    }
    
  } else { # User specified localPath
    
    filePath <- localPath
    
  }
  
  # ----- Define raster grid ---------------------------------------------------
  
  # Define NetCDF file handle
  nc <- ncdf4::nc_open(filePath)
  
  # Get reading coordinates
  lon <- ncdf4::ncvar_get(nc, varid = "XLONG")
  lat <- ncdf4::ncvar_get(nc, varid = "XLAT")
  
  coords <- data.frame(
    x = as.vector(lon),
    y = as.vector(lat)
  )

  if (is.null(res)) {
    # Define a grid to rasterize the model's sample points onto. A single grid 
    # cell should measure approximately DX x DY meters (provided by the model):
    #
    # * * * * * T * * * * *
    #  ** **  * | * **  **
    #   L-------+-------R
    #    *** ** | ** ***
    #     *** **B** ***
    
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
    
    # The swath's middle longitude and latitude lengths
    midLonLength <- geosphere::distGeo(left, right)
    midLatLength <- geosphere::distGeo(top, bottom)
    
    # Target grid cell dimensions
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
  
  # Combine the separate variable layers into a RasterBrick
  rasterBrick <- raster::brick(rasterLayers)
  
  # ----- Crop RasterBrick -----------------------------------------------------
  
  if ( is.null(xlim) && is.null(ylim) ) {
    
    # Full grid
    result <- rasterBrick
    
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
    result <- raster::crop(rasterBrick, ext)
    
  }
  
  # ----- Set final raster title -----------------------------------------------
  
  # Generate title from file name
  # Ex. "PNW-4km_2020090812_07.nc" -> "PNW-4km 2020-09-08 12pm, Hour 7"
  fileName <- basename(filePath)
  
  modelNameStr <- stringr::str_extract(fileName, "^.*(?=_\\d{10})")
  modelRunStr <- stringr::str_extract(fileName, "\\d{10}(?=_)")
  modelRunHourStr <- stringr::str_extract(fileName, "\\d{2}(?=\\.nc$)")
  
  datetime <- lubridate::ymd_h(modelRunStr)
  timeStr <- strftime(datetime, format = "%Y-%m-%d %I %p", tz = "UTC")
  
  title <- paste0(modelNameStr, " ", timeStr, ", forecast hour ", modelRunHourStr)
  
  result@title <- title
  
  return(result)
  
}
