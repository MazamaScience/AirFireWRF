

wrf_loadList <- function(
  modelName = NULL,
  modelRun = NULL,
  modelRunHours = NULL,
  baseUrl = "http://m2.airfire.org",
  varNames = NULL,
  res = NULL,
  xlim = NULL,
  ylim = NULL
) {
  
  # ----- Download files -------------------------------------------------------
  
  filePaths <- sapply(modelRunHours, function(modelRunHour) {
    
    # Download model run file
    filePath <- wrf_download(
      modelName = modelName,
      modelRun = modelRun,
      modelRunHour = modelRunHour,
      baseUrl = baseUrl
    )
    
    return(filePath)
    
  })
  
  # ----- Build list -----------------------------------------------------------
  
  # Make a list of "model run hour" RasterBricks
  hourBrickList <- sapply(
    X = filePaths,
    varNames = varNames,
    res = res,
    xlim = xlim,
    ylim = ylim,
    FUN = .loadHourBrick
  )
  
  names(hourBrickList) <- paste0(
    modelName, "_",
    modelRun, "_",
    stringr::str_pad(modelRunHours, 2, pad = "0")
  )
  
  return(hourBrickList)
  
}

.loadHourBrick <- function(
  filePath = NULL,
  varNames = NULL,
  res = NULL,
  xlim = NULL,
  ylim = NULL
) {
  
  # ----- Define reference grid ------------------------------------------------
  
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
  
  refGrid <- raster::raster(nrows = rRowCount, ncols = rColCount)
  raster::extent(refGrid) <- raster::extent(coords)
  
  # ----- Build WRF variable list ----------------------------------------------
  
  varLayerList <- sapply(
    X = varNames,
    nc = nc,
    coords = coords,
    refGrid = refGrid,
    xlim = xlim,
    ylim = ylim,
    FUN = .loadVarLayer
  )
  
  hourBrick <- raster::brick(varLayerList)
  names(hourBrick) <- varNames
  
  return(hourBrick)
  
}

.loadVarLayer <- function(
  nc = NULL,
  varName = NULL,
  coords = NULL,
  refGrid = NULL,
  xlim = NULL,
  ylim = NULL
) {
  
  varValues <- ncdf4::ncvar_get(nc, varid = varName)
  
  varLayer <- raster::rasterize(
    x = coords,
    y = refGrid,
    field = as.vector(varValues),
    fun = mean
  )
  
  # ----- Crop layer -----------------------------------------------------------
  
  xDomain <- c(raster::xmin(varLayer), raster::xmax(varLayer))
  yDomain <- c(raster::ymin(varLayer), raster::ymax(varLayer))
  
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
  
  cells <- raster::cellFromXY(varLayer, cbind(xlim, ylim))
  extent <- raster::extentFromCells(varLayer, cells)
  varLayer <- raster::crop(varLayer, extent)
  
  return(varLayer)
  
}

if (FALSE) {
  
  library(AirFireWRF)
  setWRFDataDir("~/Data/WRF")
  
  # Test loading multiple variables from multiple hours of a model run
  multVarMultHour <- wrf_loadList(
    modelName = "PNW-4km",
    modelRun = "2020101012",
    modelRunHours = 7:10,
    baseUrl = "http://m2.airfire.org",
    varNames = c("HGT", "TSK", "U10", "V10"),
    res = 0.1,
    xlim = c(-125, -116),
    ylim = c(45.5, 50)
  )
  
  # Test loading a single variable from multiple hours of a model run
  singVarMultHour <- wrf_loadList(
    modelName = "PNW-4km",
    modelRun = "2020101012",
    modelRunHours = 7:10,
    baseUrl = "http://m2.airfire.org",
    varNames = c("TSK"),
    res = 0.1,
    xlim = c(-125, -116),
    ylim = c(45.5, 50)
  )
  
  # Test loading multiple variables from a single hour of a model run
  multVarSingHour <- wrf_loadList(
    modelName = "PNW-4km",
    modelRun = "2020101012",
    modelRunHours = 7,
    baseUrl = "http://m2.airfire.org",
    varNames = c("HGT", "TSK", "U10", "V10"),
    res = 0.1,
    xlim = c(-125, -116),
    ylim = c(45.5, 50)
  )
  
  # Test loading a single variable from a single hour of a model run
  singVarSingHour <- wrf_loadList(
    modelName = "PNW-4km",
    modelRun = "2020101012",
    modelRunHours = 7,
    baseUrl = "http://m2.airfire.org",
    varNames = c("TSK"),
    res = 0.1,
    xlim = c(-125, -116),
    ylim = c(45.5, 50)
  )
  
}
