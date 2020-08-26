wrf_download <- function(
  modelName = "PNW-4km",
  modelRun = NULL,
  forecastHour = NULL,
  baseUrl = "http://m2.airfire.org/PNW/4km/WRF",
  verbose = TRUE
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(modelName)
  MazamaCoreUtils::stopIfNull(modelRun)
  MazamaCoreUtils::stopIfNull(baseUrl)
  
  # Just in case
  if ( length(modelName) > 1 || length(modelRun) > 1 ) {
    warning(paste0(
      "'modelName' or 'modelRun' has multiple values -- ",
      "first value being used."
    ))
    modelName <- modelName[1]
    modelRun <- as.character(modelRun[1])
  }
  
  # Verify YYYYmmddHH
  if ( !stringr::str_detect(modelRun, "[0-9]{10}") ) {
    stop("'modelRun' parameter must have 10 digits")
  }
  
  # Defaults
  if ( !is.logical(verbose) ) verbose <- TRUE
  
  # ----- Create URL, name and path---------------------------------------------
  
  # Create directory URL
  dataDirUrl <- paste0(
    baseUrl, "/",
    modelRun, "/"
  )
  
  fileName <- paste0("wrfout_d3.", modelRun, ".f", 
                     stringr::str_pad(forecastHour, 2, pad = "0"), ".0000")
  filePath <- file.path(getWRFDataDir(), fileName)
  
  # ----- Download data --------------------------------------------------------
  
  if ( file.exists(filePath) ) {
    
    if ( verbose )
      message(paste0("WRF model run exists at: ", filePath))
    
  } else {
    
    fileUrl <- paste0(dataDirUrl, fileName)
    tryCatch(
      expr = {
        utils::download.file(url = fileUrl, destfile = filePath, quiet = !verbose)
      },
      error = function(e) {
        stop(paste0("Error downloading: ", modelName))
      }
    )
    
  }
  
  return(filePath)
  
}

if (FALSE) {
  library(WRFmet)
  
  setWRFDataDir('~/Data/WRF')
  
  latestRun <- wrf_listLatestModelRun()
  wrf_download(modelRun = latestRun, forecastHour = 7)
}