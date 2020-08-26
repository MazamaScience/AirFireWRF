#' @export
#' 
#' @title List downloaded WRF model run files
#'
#' @param WRFDataDir Directory in which model run files are located.
#' @param pattern A regex pattern to use for filtering model files.
#' @param full Logical. Show the full path of the model (used for local loading).
#' @param ... Additional arguments to be passed to \code{list.files()}.
#'
#' @return A list of downloaded model run files

#' @examples
#' \donttest{
#' library(WRFmet)
#' setWRFDataDir("~/Data/WRF")
#'
#' modelRun <- wrf_latestModelRun("PNW-4km")
#' filePath <- wrf_download("PNW-4km", modelRun, 7)
#' wrf_downloaded()
#' }

wrf_downloaded <- function(
  WRFDataDir = getWRFDataDir(),
  pattern = ".nc",
  full = FALSE,
  ...
) {
  list.files(
    path = WRFDataDir,
    full.names = full,
    no.. = TRUE,
    pattern = pattern,
    ...
  )
}