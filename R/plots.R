#' @export
#' @title Creates an empty plot
#'
#' @param title Title of the plot.
#' @param xlab x-axis label.
#' @param ylab y-axis label.
#' @param color Label for color value in legend.
#' @param flab Label for fill value in legend.
#' @param xlim A vector of coordinate longitude bounds.
#' @param ylim A vector of coordinate latitude bounds.
#' @param ratio Plot dimensions ratio (x/y).
#'
#' @return A GGPlot object.

plot_base <- function(
  title = NULL,
  xlab = "Longitude",
  ylab = "Latitude",
  color = NULL,
  flab = NULL,
  xlim = NULL,
  ylim = NULL,
  ratio = NULL
) {
  
  if ( is.null(xlab) ) xlab <- "Longitude"
  if ( is.null(ylab) ) ylab <- "Latitude"
  
  if ( is.null(ratio) ) {
    
    coordSystem <- ggplot2::coord_cartesian(
      xlim = xlim,
      ylim = ylim
    )
    
  } else {
    
    coordSystem <- ggplot2::coord_fixed(
      ratio = ratio,
      xlim = xlim,
      ylim = ylim
    )
  }
  
  gg <-
    ggplot2::ggplot() +
    ggplot2::labs(
      title = title,
      x = xlab,
      y = ylab,
      color  = color,
      fill = flab
    ) +
    coordSystem
  
  return(gg)
  
}

#' @export
#' @title Creates a plot for a single raster layer
#'
#' @param raster A RasterLayer.
#' @param fillLow Color for lowest raster value.
#' @param fillHigh Color for highest raster value.
#' @param fillNa Color for na raster values.
#' @param title Title of the plot.
#' @param xlab x-axis label.
#' @param ylab y-axis label.
#' @param flab Label for fill value in legend.
#' @param xlim A vector of coordinate longitude bounds.
#' @param ylim A vector of coordinate latitude bounds.
#' @param ratio Plot dimensions ratio (x/y).
#'
#' @return A GGPlot object.
#' 
#' @examples
#' \donttest{
#' library(WRFmet)
#' 
#' plot_raster(
#'   raster = example$HGT,
#'   ratio = 1.4
#' )
#' }

plot_raster <- function(
  raster = NULL,
  fillLow = "#132c43",
  fillHigh = "#55b2f8",
  fillNa = "transparent",
  title = NULL,
  xlab = NULL,
  ylab = NULL,
  flab = NULL,
  xlim = NULL,
  ylim = NULL,
  ratio = NULL
) {
  
  # MazamaCoreUtils::stopIfNull(raster)
  # 
  # if ( !("RasterLayer" %in% class(raster)) ) {
  #   stop("The given raster is not a RasterLayer")
  # }
  # 
  # if ( is.null(title) ) title <- names(raster)[1]
  # if ( is.null(flab) ) flab <- title
  # 
  # plot <-
  #   plot_base(
  #     title = title,
  #     xlab = xlab,
  #     ylab = ylab,
  #     flab = flab,
  #     xlim = xlim,
  #     ylim = ylim,
  #     ratio = ratio
  #   ) +
  #   layer_raster(
  #     raster = raster,
  #     alpha = 1
  #   ) +
  #   ggplot2::scale_fill_gradient(
  #     low = fillLow,
  #     high = fillHigh,
  #     na.value = fillNa
  #   )
  # 
  # return(plot)

}

# # A plot function that will draw a raster, spPolyDF, spPointDF, and a vector field in that order
#
# plot_standard <- function(
#   raster = NULL,
#   fillLow = "#132c43",
#   fillHigh = "#55b2f8",
#   fillNa = "transparent",
#   title = NULL,
#   xlab = NULL,
#   ylab = NULL,
#   flab = NULL,
#   xlim = NULL,
#   ylim = NULL,
#   ratio = 1
# ) {
#
#   plot <-
#     plot_base(
#       title = title,
#       xlab = xlab,
#       ylab = ylab,
#       flab = flab,
#       xlim = xlim,
#       ylim = ylim,
#       ratio = ratio
#     ) +
#     ggplot2::scale_fill_gradient(
#       low = fillLow,
#       high = fillHigh,
#       na.value = fillNa
#     ) +
#     layer_raster(
#       raster = raster
#     )
#
#   return(plot)
#
# }
