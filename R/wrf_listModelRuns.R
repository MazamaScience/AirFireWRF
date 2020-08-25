#' @export
#'
#' @title List WRF model runs available from AirFire
#' @description Finds all of the WRF model runs available from AirFire.
#'
#' @param modelName Model identifier.
#' @param baseUrl Base URL for WRF output.
#'
#' @return A list of WRF model run strings.
#'
#' @examples
#' \donttest{
#' library(WRFmet)
#' wrf_listModelRuns()
#' }

wrf_listModelRuns <- function(
  modelName = 'PNW-4km',
  baseUrl = 'http://m2.airfire.org/PNW/4km/WRF/'
) {
  
  linkNames <- MazamaCoreUtils::html_getLinkNames(baseUrl)
  modelRuns <- sub('/', '', linkNames)
  
  return(modelRuns)
  
}

#' @export
#'
#' @title List latest WRF model run available from AirFire
#' @description Finds the latest WRF model runs available from AirFire and lists
#' the most recent \code{count} model run strings.
#'
#' @param modelName Model identifier.
#' @param baseUrl Base URL for WRF output.
#' @param count Number of most recent model run strings to return.
#'
#' @return The latest model run string.
#'
#' @examples
#' \donttest{
#' library(WRFmet)
#' wrf_listLatestModelRun(count = 2)
#' }

wrf_listLatestModelRun <- function(
  modelName = 'PNW-4km',
  baseUrl = 'http://m2.airfire.org/PNW/4km/WRF/',
  count = 1
) {
  
  modelRuns <- sort(wrf_listModelRuns(baseUrl = baseUrl))
  
  return(modelRuns[1:count])
  
}
