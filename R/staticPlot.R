#' @export
#' @title Create a Raster layer for plotting
#'
#' @param raster A RasterBrick.
#' @param alpha Transparency of layer.
#'
#' @return A geom_tile ggproto object.

layer_raster <- function(
  raster = NULL,
  alpha = 1
) {
  # TODO: Look into potentially using ggspatial::layer_spatial
  # https://paleolimbot.github.io/ggspatial/
  coords <- raster::xyFromCell(raster, seq_len(raster::ncell(raster)))
  readings <- raster::stack(as.data.frame(raster::getValues(raster)))
  names(readings) <- c('value', 'variable')
  
  df <- cbind(coords, readings)
  
  res <- ggplot2::geom_tile(
    data = df,
    ggplot2::aes(
      x = .data$x,
      y = .data$y,
      fill = .data$value
    )
  ) 
  
  return(res)
}

#' @export
#' @title Create a state polygons layer for plotting
#'
#' @param xlim A vector of coordinate longitude bounds.
#' @param ylim A vector of coordinate latitude bounds.
#'
#' @return A geom_polygon ggproto object.

layer_states <- function(
  xlim = NULL,
  ylim = NULL
) {
  states <- ggplot2::map_data(
    'state',
    xlim = xlim,
    ylim = ylim
  )
  
  res <- ggplot2::geom_polygon(
    data = states,
    ggplot2::aes(
      y = .data$lat,
      x = .data$long,
      group = .data$group
    ),
    fill = 'NA',
    color = 'black'
  )
  
  return(res)
}

#' @export
#' @title Create a vectorfield layer for plotting
#'
#' @param uvRaster A RasterBrick with 2 RasterLayers: U and V vector components.
#' @param xlim A vector of coordinate longitude bounds.
#' @param ylim A vector of coordinate latitude bounds.
#' @param arrowCount Number of arrows to draw.
#' @param arrowScale Arrow length scale factor.
#' @param arrowColor Arrow color.
#' @param headSize Arrow head size.
#' @param alpha Transparency of layer.
#'
#' @return An annotation_custom ggproto object.

layer_vectorField <- function(
  uvRaster = NULL,
  xlim = NULL,
  ylim = NULL,
  arrowCount = 1000,
  arrowScale = 0.05,
  arrowColor = 'white',
  headSize = 0.05,
  alpha = 1
) {
  # Removes ALL spacing around lattice plots
  # From: https://stat.ethz.ch/pipermail/r-help/2007-January/123556.html
  noPaddingTheme <- list(
    layout.heights = list(
      top.padding = 0,
      main.key.padding = 0,
      key.axis.padding = 0,
      axis.xlab.padding = 0,
      xlab.key.padding = 0,
      key.sub.padding = 0,
      bottom.padding = 0
    ),
    layout.widths = list(
      left.padding = 0,
      key.ylab.padding = 0,
      ylab.axis.padding = 0,
      axis.key.padding = 0,
      right.padding = 0
    ),
    axis.line = list(col = 'transparent')
  )
  
  vectorField <- rasterVis::vectorplot(
    object = uvRaster,
    isField = 'dXY',
    region = FALSE,
    narrows = arrowCount,
    length = headSize,
    aspX = arrowScale,
    aspY = arrowScale,
    col.arrows = arrowColor,
    colorkey = FALSE,
    alpha = alpha,
    xlab = NULL,
    ylab = NULL,
    scales = list(draw = FALSE),
    par.settings = noPaddingTheme
  )
  
  vectorField$aspect.fill <- TRUE
  vectorFieldGrob <- ggplotify::as.grob(vectorField)
  extent <- uvRaster@extent
  
  res <- ggplot2::annotation_custom(
      grob = vectorFieldGrob,
      xmin = extent@xmin,
      xmax = extent@xmax,
      ymin = extent@ymin,
      ymax = extent@ymax
  )
  
  return(res)
}
