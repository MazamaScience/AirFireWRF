#' @export
#'
#' @title Download WRf model data from AirFire
#' 
#' @param modelName Model identifier.
#' @param modelRun Model initialization timestamp as "YYYYMMDDHH".
#' @param modelRunHour Hour forecasted from initial time, i.e. 7.
#' @param baseUrl Base URL for WRF output.
#' @param verbose If \code{FALSE}, suppress status messages (if any), and the
#' progress bar.
#' 
#' @description Downloads a copy of the specified WRF model run to the package 
#' data directory. The file data can then be loaded with \code{wrf_load()}.
#'
#' On 2020-08-27, available model identifiers include the following:
#' \itemize{
#'   \item{PNW-4km}
#' }
#'
#' @examples
#' \donttest{
#' library(AirFireWRF)
#' setWRFDataDir('~/Data/WRF')
#' 
#' modelRun <- wrf_latestModelRun("PNW-4km")
#' wrf_download("PNW-4km", "2020082500", 11)
#' }

wrf_download <- function(
  modelName = NULL,
  modelRun = NULL,
  modelRunHour = NULL,
  baseUrl = "http://m2.airfire.org", # http://128.95.182.72
  verbose = TRUE
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(modelName)
  MazamaCoreUtils::stopIfNull(modelRun)
  MazamaCoreUtils::stopIfNull(modelRunHour)
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
  
  paddedModelRunHour <- stringr::str_pad(modelRunHour, 2, pad = "0")
  
  if (modelName == "PNW-1.33km") {
    
    modelNameWRFDir <- "PNW/1.33km/WRF"
    remoteFileName <- paste0("wrfout_d4.", modelRun, ".f", paddedModelRunHour, ".0000")
    
  } else if (modelName == "PNW-4km") {
    
    modelNameWRFDir <- "PNW/4km/WRF"
    remoteFileName <- paste0("wrfout_d3.", modelRun, ".f", paddedModelRunHour, ".0000")
    
  } else {
    stop("No model named '", modelName, "'")
  }
  
  # Create directory URL
  dataDirUrl <- paste0(
    baseUrl, "/",
    modelNameWRFDir, "/",
    modelRun, "/"
  )
  
  localFileName <- paste0(modelName, "_", modelRun, "_", paddedModelRunHour, ".nc")
  
  filePath <- file.path(getWRFDataDir(), localFileName)
  
  # ----- Download data --------------------------------------------------------
  
  if ( file.exists(filePath) ) {
    
    if ( verbose )
      message(paste0("WRF model run exists at: ", filePath))
    
  } else {
    
    fileUrl <- paste0(dataDirUrl, remoteFileName)
    
    # Make sure the WRF output directory exists
    tryCatch(
      expr = {
        modelRunFiles <- MazamaCoreUtils::html_getLinkNames(dataDirUrl)
      },
      error = function(e) {
        stop(paste0("Error accessing WRF output directory '", dataDirUrl, "': ", e))
      }
    )
    
    # Make sure the model run file exists in the WRF output directory
    if ( !(remoteFileName %in% modelRunFiles) ) {
      stop(paste0(remoteFileName, " does not exist in: ", dataDirUrl))
    }
    
    # Download model run file
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
