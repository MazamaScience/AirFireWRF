#' @export
#'
#' @title Plot a vectorfield of wind velocity
#'
#' @param nc WRF NetCDF file.
#' @param res Resolution of raster in degrees.
#' @param xlim A vector of coordinate longitude bounds.
#' @param ylim A vector of coordinate latitude bounds.
#' @param bg A name of a WRF variable to use as a background raster or FALSE for
#' no background.
#' @param arrowCount The number of arrows to plot.
#' @param arrowScale Numeric multiplier for arrow body length.
#' @param arrowColor Color of arrows.
#' @param headSize Size of arrowhead.
#'
#' @return A \emph{Trellis} object.
#'
#' @examples
#' \donttest{
#' library(WRFmet)
#' nc <- ncdf4::nc_open("~/Data/WRF/wrfout_d3-2020071512-f07-0000.nc")
#' w <- wrf_windField(
#'   nc = nc,
#'   res = 0.08,
#'   xlim = c(-126, -115),
#'   ylim = c(45, 51),
#'   bg = "HGT",
#'   arrowScale = 0.05,
#'   arrowColor = 'white',
#'   headSize = 0.05
#' )
#' }

wrf_windField <- function(
  nc = NULL,
  res = NULL,
  xlim = NULL,
  ylim = NULL,
  bg = "HGT",
  arrowCount = 1000,
  arrowScale = 1,
  arrowColor = 'white',
  headSize = 0.05
) {
  
  bgValid <- FALSE
  
  if (is.character(bg)) {
    if (bg %in% names(nc$var)) {
      bgValid <- TRUE
      bgRaster <- wrf_createRaster(
        nc = nc,
        vars = c(bg),
        res = res,
        xlim = xlim,
        ylim = ylim
      )
    }
  }
  
  windRaster <- wrf_createRaster(
    nc = nc,
    vars = c("U10", "V10"),
    res = res,
    xlim = xlim,
    ylim = ylim
  )
  
  vectorField <- rasterVis::vectorplot(
    object = windRaster,
    isField = 'dXY',
    region = if (bgValid) bgRaster[[bg]] else FALSE,
    narrows = arrowCount,
    length = headSize,
    aspX = arrowScale,
    aspY = arrowScale,
    col.arrows = arrowColor
  )
  
  return(vectorField)
  
}
