#' @title Creates an empty plot
#'
#' @keywords internal
#'
#' @param title Title of plot.
#' @param xlab Label for x-axis.
#' @param ylab Label for y-axis.
#' @param clab Label for legend color scale.
#' @param flab Label for legend fill scale.
#' @param xlim A vector of coordinate longitude bounds.
#' @param ylim A vector of coordinate latitude bounds.
#' @param ratio Aspect ratio of plot.
#'
#' @return A ggplot object.

plot_base <- function(
  title = NULL,
  xlab = "Longitude",
  ylab = "Latitude",
  clab = NULL,
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
  
  plot <-
    ggplot2::ggplot() +
    ggplot2::labs(
      title = title,
      x = xlab,
      y = ylab,
      color  = clab,
      fill = flab
    ) +
    coordSystem
  
  return(plot)
  
}

#' @export
#' @title Creates a plot for a single raster layer
#'
#' @param ... Arguments passed on to ggplot2::continuous_scale()
#' @param raster A RasterLayer.
#' @param colors A vector of colours to use for n-colour gradient.
#' @param values A vector of positions (between 0 and 1) for each color in the 
#' colors vector.
#' @param fillNa Color for na raster values.
#' @param title Title of plot.
#' @param xlab Label for x-axis.
#' @param ylab Label for y-axis.
#' @param flab Label for legend fill scale.
#' @param xlim A vector of coordinate longitude bounds.
#' @param ylim A vector of coordinate latitude bounds.
#' @param ratio Aspect ratio of plot.
#'
#' @return A ggplot object.
#' 
#' @examples
#' \donttest{
#' library(WRFmet)
#' library(raster)
#' 
#' plot_raster(
#'   raster = example_PNW$HGT,
#'   title = "PNW Elevation",
#'   flab = "Meters",
#'   ratio = 1.4
#' )
#' }

plot_raster <- function(
  ...,
  raster = NULL,
  colors = grDevices::terrain.colors(10),
  values = NULL,
  fillNa = "transparent",
  title = NULL,
  xlab = NULL,
  ylab = NULL,
  flab = NULL,
  xlim = NULL,
  ylim = NULL,
  ratio = NULL
) {
  
  plot <-
    plot_base(
      title = title,
      xlab = xlab,
      ylab = ylab,
      flab = flab,
      xlim = xlim,
      ylim = ylim,
      ratio = ratio
    ) +
    layer_raster(
      raster = raster
    ) +
    ggplot2::scale_fill_gradientn(
      ...,
      colors = colors,
      values = values,
      na.value = fillNa
    )
      
  return(plot)
  
}

#' @export
#' @title Creates a comprehensive plot
#'
#' @param ... Arguments passed on to ggplot2::continuous_scale()
#' @param bgRaster A RasterLayer for the background.
#' @param polys A SpatialPolygonsDataFrame.
#' @param states Logical for including state polygons or not. 
#' @param uRaster A RasterLayer for longitudinal vector components.
#' @param vRaster A RasterLayer for latitudinal vector components.
#' @param bgRasterColors Vector of colours to use for the bgRaster's n-colour 
#' gradient.
#' @param bgRasterValues A vector of positions (between 0 and 1) for each color in the 
#' colors vector.
#' @param bgRasterNaColor Color for na ngRaster values.
#' @param polyColor Color for spatial polygon outlines.
#' @param polyFill Color for spatial polygon interiors.
#' @param stateColor Color for state polygon outlines.
#' @param stateFill Color for state polygon interiors.
#' @param arrowCount Number of arrows to draw.
#' @param arrowScale Arrow length scale factor.
#' @param arrowColor Arrow color.
#' @param arrowHead Arrow head size.
#' @param arrowAlpha Transparency of vector field layer.
#' @param title Title of plot.
#' @param xlab Label for x-axis.
#' @param ylab Label for y-axis.
#' @param flab Label for legend fill scale.
#' @param xlim A vector of coordinate longitude bounds.
#' @param ylim A vector of coordinate latitude bounds.
#' @param ratio Aspect ratio of plot.
#'
#' @return A ggplot object.
#' 
#' @examples
#' \donttest{
#' library(WRFmet)
#' library(raster)
#' 
#' plot_standard(
#'   bgRaster = example_PNW$HGT,
#'   uRaster = example_PNW$U10,
#'   vRaster = example_PNW$V10,
#'   states = TRUE,
#'   stateColor = "red",
#'   stateFill = "transparent",
#'   arrowColor = "black",
#'   title = "PNW-4km 2020-07-15 12pm - Hour 7",
#'   flab = "Elev (m)",
#'   xlim = c(-125, -111),
#'   ylim = c(42, 49),
#'   ratio = 1.4
#' )
#' }

plot_standard <- function(
  ...,
  bgRaster = NULL,
  states = FALSE,
  polys = NULL,
  uRaster = NULL,
  vRaster = NULL,
  bgRasterColors = grDevices::terrain.colors(10),
  bgRasterValues = NULL,
  bgRasterNaColor = "transparent",
  stateColor = "black",
  stateFill = "white",
  polyColor = "black",
  polyFill = "white",
  arrowCount = 1000,
  arrowScale = 0.05,
  arrowColor = "white",
  arrowHead = 0.05,
  arrowAlpha = 1,
  title = NULL,
  xlab = NULL,
  ylab = NULL,
  flab = NULL,
  xlim = NULL,
  ylim = NULL,
  ratio = NULL
) {
  
  # Create the background raster layer
  if ( !(is.null(bgRaster)) ) {
    rasterLayer <- layer_raster(
      raster = bgRaster
    )
  } else {
    rasterLayer <- NULL
  }
  
  if ( !is.null(polys) ) {
    polysLayer <- layer_spPolys(
      spdf = polys,
      color = polyColor,
      fill = polyFill
    )
  } else {
    polysLayer <- NULL
  }
  
  if ( states ) {
    statesLayer <- layer_states(
      color = stateColor,
      fill = stateFill,
      xlim = xlim,
      ylim = ylim
    )
  } else {
    statesLayer <- NULL
  }
  
  # Create the vector field layer
  if ( !(is.null(uRaster)) && !(is.null(vRaster)) ) {
    vectorFieldLayer <- layer_vectorField(
      uRaster = uRaster,
      vRaster = vRaster,
      arrowCount = arrowCount,
      arrowScale = arrowScale,
      arrowHead = arrowHead,
      arrowColor = arrowColor,
      alpha = arrowAlpha,
      xlim = xlim,
      ylim = ylim
    )

    # Have to manually set the plot scale limits
    if (is.null(bgRaster) && is.null(xlim) && is.null(ylim) ) {
      extent <- raster::extent(uRaster)
      xlim = c(extent@xmin, extent@xmax)
      ylim = c(extent@ymin, extent@ymax)
    }
  } else {
    vectorFieldLayer <- NULL
  }

  plot <-
    plot_base(
      title = title,
      xlab = xlab,
      ylab = ylab,
      flab = flab,
      xlim = xlim,
      ylim = ylim,
      ratio = ratio
    ) +
    rasterLayer +
    polysLayer +
    statesLayer +
    vectorFieldLayer +
    ggplot2::scale_fill_gradientn(
      ...,
      colors = bgRasterColors,
      values = bgRasterValues,
      na.value = bgRasterNaColor
    )

  return(plot)
  
}



