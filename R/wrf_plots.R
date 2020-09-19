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

wrf_basePlot <- function(
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
#' @param raster A RasterBrick or RasterLayer.
#' @param varName The name of a raster variable.
#' @param colors A vector of colours to use for n-colour gradient.
#' @param values A vector of positions (between 0 and 1) for each color in the 
#' colors vector.
#' @param naColor Color for na raster values.
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
#' wrf_rasterPlot(
#'   raster = example_PNW,
#'   varName = "HGT",
#'   flab = "Elev (m)",
#'   ratio = 1.4
#' )
#' }

wrf_rasterPlot <- function(
  raster = NULL,
  varName = NULL,
  colors = grDevices::terrain.colors(10),
  values = NULL,
  naColor = "transparent",
  title = NULL,
  xlab = NULL,
  ylab = NULL,
  flab = NULL,
  xlim = NULL,
  ylim = NULL,
  ratio = NULL
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  if ( is.null(title) && !is.null(raster) ) {
    title <- raster@title
  }
  
  # ----- Create layers --------------------------------------------------------
  
  plot <-
    wrf_basePlot(
      title = title,
      xlab = xlab,
      ylab = ylab,
      flab = flab,
      xlim = xlim,
      ylim = ylim,
      ratio = ratio
    ) +
    layer_raster(
      raster = raster,
      varName = varName
    ) +
    ggplot2::scale_fill_gradientn(
      colors = colors,
      values = values,
      na.value = naColor
    )
      
  return(plot)
  
}

#' @export
#' @title Creates a comprehensive plot
#'
#' @param raster A RasterBrick with layers for WRF variables.
#' @param polys A SpatialPolygonsDataFrame.
#' @param states Logical for including state polygons or not.
#' @param bgName The name of the background raster layer.
#' @param ctrName The name of the contour raster layer.
#' @param uName The name of the u component raster layer.
#' @param vName The name of the v component raster layer.
#' @param bgRasterColors Vector of colors to use for the background raster's 
#' n-color gradient.
#' @param bgRasterValues A vector of positions (between 0 and 1) for each color 
#' in the bgRasterColors vector.
#' @param bgRasterNaColor Color for na background raster values.
#' @param polyWidth Line thickness of spatial polygon outlines.
#' @param polyColor Color for spatial polygon outlines.
#' @param polyFill Color for spatial polygon interiors.
#' @param stateWidth Line thickness of state polygon outlines.
#' @param stateColor Color for state polygon outlines.
#' @param stateFill Color for state polygon interiors.
#' @param arrowCount Number of vector field arrows to draw.
#' @param arrowScale Scale factor of vector field arrow body length.
#' @param arrowWidth Line thickness of vector field arrows.
#' @param arrowColor Color of vector field arrows.
#' @param arrowHead Size of vector field arrowheads.
#' @param arrowAlpha Transparency of vector field arrows.
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
#' wrf_standardPlot(
#'   raster = example_PNW,
#'   bgName = "HGT",
#'   uName = "U10",
#'   vName = "V10",
#'   states = TRUE,
#'   arrowAlpha = 0.75,
#'   title = "PNW Elevation & Wind Velocity",
#'   flab = "Elev (m)",
#'   xlim = c(-125, -111),
#'   ylim = c(42, 49),
#'   ratio = 1.4
#' )
#' }

wrf_standardPlot <- function(
  raster = NULL,
  states = FALSE,
  polys = NULL,
  bgName = NULL,
  ctrName = NULL,
  uName = NULL,
  vName = NULL,
  bgRasterColors = grDevices::terrain.colors(10),
  bgRasterValues = NULL,
  bgRasterNaColor = "transparent",
  stateWidth = 0.5,
  stateColor = "red",
  stateFill = "transparent",
  polyWidth = 0.5,
  polyColor = "red",
  polyFill = "transparent",
  ctrBreaks = NULL,
  ctrWidth = 0.25,
  ctrColor = "black",
  arrowCount = 1000,
  arrowScale = 0.05,
  arrowWidth = 0.8,
  arrowColor = "black",
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
  
  # ----- Validate parameters --------------------------------------------------
  
  if ( is.null(title) && !is.null(raster) ) {
    title <- raster@title
  }
  
  # ----- Create layers --------------------------------------------------------
  
  # Create the background raster layer
  if ( is.null(bgName) ) {
    rasterLayer <- NULL
  } else {
    rasterLayer <- layer_raster(
      raster = raster[[bgName]]
    )
  }
  
  # Create the spatial polygons layer
  if ( is.null(polys) ) {
    polysLayer <- NULL
  } else {
    polysLayer <- layer_spPolys(
      polygons = polys,
      color = polyColor,
      fill = polyFill,
      lineWidth = polyWidth
    )
  }
  
  # Create the states polygons layer
  if ( !states ) {
    statesLayer <- NULL
  } else {
    statesLayer <- layer_states(
      color = stateColor,
      fill = stateFill,
      lineWidth = stateWidth,
      xlim = xlim,
      ylim = ylim
    )
  }
  
  # Create the contour layer
  if ( is.null(ctrName) ) {
    contourLayer <- NULL
  } else {
    contourLayer <- layer_contours(
      raster = raster,
      varName = ctrName,
      breaks = ctrBreaks,
      color = ctrColor,
      lineWidth = ctrWidth
    )
  }
  
  # Create the vector field layer
  if ( is.null(uName) || is.null(vName) ) {
    vectorFieldLayer <- NULL
  } else {
    vectorFieldLayer <- layer_vectorField(
      raster = raster,
      uName = uName,
      vName = vName,
      arrowCount = arrowCount,
      arrowScale = arrowScale,
      arrowHead = arrowHead,
      arrowWidth = arrowWidth,
      arrowColor = arrowColor,
      alpha = arrowAlpha,
      xlim = xlim,
      ylim = ylim
    )
    
    # Manually set the plot scale limits when there is no background raster 
    # layer
    if ( is.null(xlim) && is.null(ylim) ) {
      extent <- raster::extent(raster)
      xlim = c(extent@xmin, extent@xmax)
      ylim = c(extent@ymin, extent@ymax)
    }
  }
  
  # ----- Build the plot -------------------------------------------------------

  plot <-
    wrf_basePlot(
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
    contourLayer + 
    vectorFieldLayer +
    ggplot2::scale_fill_gradientn(
      colors = bgRasterColors,
      values = bgRasterValues,
      na.value = bgRasterNaColor
    )

  return(plot)
  
}
