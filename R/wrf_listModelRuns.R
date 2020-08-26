#' @export
#'
#' @title Find latest WRF model run from AirFire
#' @description Scans the directory of WRF model output and returns the most 
#' recent \code{modelRun} string(s). If \code{count > 1}, the most recent
#' \code{count} will be returned in low-hi order.
#' 
#' On 2020-08-26, available model identifiers include the following:
#' \itemize{
#'   \item{PNW-1.33km}
#'   \item{PNW-4km}
#' }
#'
#' Users wanting the latest data will typically call this function first,
#' followed by wrf_load().
#'
#' @param modelName Model identifier.
#' @param count Number of most recent \code{modelRun} strings to return. Set to
#' \code{Inf} to get \emph{all} of the available model runs.
#' @param baseUrl Base URL for WRF output.
#'
#' @return Timestamp of the latest model run.
#'
#' @examples
#' \donttest{
#' library(WRFmet)
#' wrf_latestModelRun("PNW-4km")
#' }

wrf_latestModelRun <- function(
  modelName = "PNW-4km",
  count = 1,
  baseUrl = "http://m2.airfire.org/PNW/4km/WRF/"
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(modelName)
  MazamaCoreUtils::stopIfNull(baseUrl)
  
  # Just in case
  if ( length(modelName) > 1 ) {
    warning(paste0(
      "'modelName' has multiple values -- ",
      "first value being used."
    ))
    modelName <- modelName[1]
  }
  
  # ----- Download modelRun datestamps -----------------------------------------
  
  # Create directory URL
  dataDirUrl <- paste0(
    baseUrl, "/"
    #modelName
  )
  
  # Gather runs from specified model
  tryCatch(
    expr = {
      links <- MazamaCoreUtils::html_getLinkNames(dataDirUrl)
    },
    error = function(e) {
      stop(paste0("Error reading: ", dataDirUrl))
    }
  )
  
  # Format model run timestamps
  modelRun <-
    links %>%
    stringr::str_replace("/", "") %>%
    stringr::str_subset("^[0-9]{10}$") %>%
    utils::tail(count) %>%
    unique() %>%
    sort()
  
  return(modelRun)
  
}
