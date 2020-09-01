#' @export
#' @title Creates a plot for a single raster layer
#'
#' @param raster A RasterLayer.
#'
#' @return A ggplot object.

plot_raster <- function(
  raster = NULL
) {
  
  plot <-
    ggplot2::ggplot() +
    layer_raster(raster)

  return(plot)
  
}