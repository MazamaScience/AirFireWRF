#' @name example_PNW
#' @docType data
#' @keywords datasets
#' @title Example dataset over the Pacific Northwest
#' @format A RasterBrick containing 4 RasterLayers.
#' @description The \code{example_PNW} dataset contains forecasted readings for 
#' temperature (\code{TSK}), elevation (\code{HGT}), and wind velocity 
#' (\code{U10} & \code{V10}) across the Pacific Northwest 7 hours after 12pm on
#' July 15, 2020. This data is used in several examples in the package 
#' documentation.
#' 
#' This dataset was generated on 2020-09-29 by running:
#'
#' \preformatted{
#' library(AirFireWRF)
#'
#' example_PNW <- wrf_load(
#'   localPath = "~/Data/WRF/wrfout_d3-2020071512-f07-0000.nc",
#'   varNames = c('HGT', 'TSK', 'U10', 'V10'),
#'   res = 0.06,
#'   xlim = c(-125, -111),
#'   ylim = c(42, 49)
#' )
#' 
#' # Have to manually set the title since the filename format is outdated
#' example_PNW@title <- "PNW-4km 2020-07-15 12 PM, forecast hour 7"
#' 
#' save(example_PNW, file = "data/example_PNW.rda", compress = "xz")
#' }
NULL
