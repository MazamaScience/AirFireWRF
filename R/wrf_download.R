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
#' On 2020-08-26, available model identifiers include the following:
#' \itemize{
#'   \item{PNW-1.33km}
#'   \item{PNW-4km}
#' }
#'
#' @examples
#' \donttest{
#' library(WRFmet)
#' setWRFDataDir('~/Data/WRF')
#' 
#' modelRun <- wrf_latestModelRun("PNW-4km")
#' wrf_download("PNW-4km", modelRun, 8)
#' }

wrf_download <- function(
  modelName = "PNW-4km",
  modelRun = NULL,
  modelRunHour = NULL,
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
                     stringr::str_pad(modelRunHour, 2, pad = "0"), ".0000")
  filePath <- file.path(getWRFDataDir(), paste0(fileName, ".nc"))
  
  # ----- Download data --------------------------------------------------------
  
  if ( file.exists(filePath) ) {
    
    if ( verbose )
      message(paste0("WRF model run exists at: ", filePath))
    
  } else {
    
    fileUrl <- paste0(dataDirUrl, fileName)
    modelRunFiles <- MazamaCoreUtils::html_getLinkNames(dataDirUrl)
    
    # Make sure the file exists in the database
    if ( !(fileName %in% modelRunFiles) ) {
      stop(paste0(fileName, " does not exist in: ", dataDirUrl))
    }
    
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
