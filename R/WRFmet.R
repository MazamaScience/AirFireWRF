#' @docType package
#' @name WRFmet
#' @title Utilities for working with WRF model output
#' @description A suite of utility functions providing functionality commonly
#' needed for working with WRF model output in R.
NULL

# ----- Example datasets ------------------------------------------------------

#' PNW-4km example dataset
#'
#' @docType data
#' @keywords datasets
#' @name PNW_Sample
#' @title PNW Example Dataset
#' @format A RasterBrick with four layers
#' @description
#' This dataset contains forecasted readings for temperature (TSK), elevation 
#' (HGT), and wind velocity (U10 & V10) across the Pacific Northwest on July 15,
#' 2020. Each variable is stored in its own \emph{RasterLayer} in an overlying
#' \emph{RasterBrick} object. This data is used in some examples in the package
#' documentation.
NULL

# ----- Internal Package State -------------------------------------------------

WRFmetEnv <- new.env(parent = emptyenv())
WRFmetEnv$dataDir <- NULL

# ----- Data Directory Configuration -------------------------------------------

#' @docType data
#' @keywords environment
#' @name WRFDataDir
#' @title Directory for Modeling Data
#' @format Absolute path string.
#' @description This package maintains an internal directory location which
#' users can set using \code{setWRFDataDir()}. All package functions use this
#' directory whenever datasets are created or loaded.
#'
#' The default setting when the package is loaded is \code{getwd()}.
#' @seealso \link{getWRFDataDir}
#' @seealso \link{setWRFDataDir}
NULL

#' @export
#' @title Get Package Data Directory
#' @description Returns the package data directory where model data is located.
#' @return Absolute path string.
#' @seealso \link{WRFDataDir}
#' @seealso \link{setWRFDataDir}

getWRFDataDir <- function() {
  if ( is.null(WRFmetEnv$dataDir) ) {
    stop(paste0(
      "No data directory found. ",
      "Please set a data directory with setWRFDataDir('~/Data/WRF')"
    ), call. = FALSE)
  } else {
    return(WRFmetEnv$dataDir)
  }
}

#' @export
#' @title Set Package Data Directory
#' @param dataDir directory where model datasets are located
#' @description Sets the package data directory where model data is located.
#' If the directory does not exist, it will be created.
#' @return Silently returns previous value of data directory.
#' @seealso \link{WRFDataDir}
#' @seealso \link{getWRFDataDir}

setWRFDataDir <- function(dataDir) {
  old <- WRFmetEnv$dataDir
  dataDir <- path.expand(dataDir)
  tryCatch({
    if (!file.exists(dataDir)) dir.create(dataDir)
    WRFmetEnv$dataDir <- dataDir
  }, warning = function(warn) {
    warning("Invalid path name.")
  }, error   = function(err) {
    stop(paste0("Error in setWRFDataDir(", dataDir, ")."))
  })
  return(invisible(old))
}

#' @keywords internal
#' @title Remove package data directory
#' @description Resets the package data directory to NULL. Used for internal
#' testing.
#' @return Silently returns previous value of data directory.
#' @seealso WRFDataDir
#' @seealso getWRFDataDir
#' @seealso setWRFDataDir
removeWRFDataDir <- function() {
  old <- WRFmetEnv$dataDir
  WRFmetEnv$dataDir <- NULL
}
