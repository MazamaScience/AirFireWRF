#' @export
#' @importFrom rlang .data
#' 
#' @title Create a Raster layer for plotting
#'
#' @param raster A RasterBrick or RasterLayer.
#' @param varName The name of a raster variable.
#' @param breaks A vector of raster values to use as palette breaks.
#' @param alpha Transparency of layer.
#'
#' @return A geom_tile ggproto object.

layer_raster <- function(
  raster = NULL,
  varName = NULL,
  breaks = NULL,
  alpha = 1
) {

  # ----- Validate parameters --------------------------------------------------
  
  if ( is.null(raster) ) {
    stop("Must provide a raster")
  }
  
  if ( "RasterBrick" %in% class(raster) ) {
    if ( raster::nlayers(raster) > 1 ) {
      
      if ( is.null(varName) ) {
        stop("Must provide a variable name when raster has multiple layers")
      }
      
      rasterLayer <- raster[[varName]]
    } else {
      rasterLayer <- raster
    }
  } else if ( "RasterLayer" %in% class(raster) ) {
    rasterLayer <- raster
  } else {
    stop("Must provide either a RasterBrick or RasterLayer")
  }
  
  # ----- Create layer ---------------------------------------------------------
  
  coords <- raster::xyFromCell(rasterLayer, seq_len(raster::ncell(rasterLayer)))
  readings <- raster::stack(as.data.frame(raster::getValues(rasterLayer)))
  names(readings) <- c("value", "variable")
  
  df <- cbind(coords, readings)

  layer <- ggplot2::geom_tile(
    data = df,
    ggplot2::aes(
      x = .data$x,
      y = .data$y,
      fill = if ( is.null(breaks) ) {
        .data$value
        } else {
          cut(.data$value, breaks, include.lowest = TRUE)
        }
    ),
    alpha = alpha
  )

  return(layer)
  
}

#' @export
#' @title Create a contour lines layer for plotting
#'
#' @param raster A RasterBrick or RasterLayer
#' @param varName The name of a raster variable.
#' @param breaks A numerical vector of contour line levels.
#' @param lineWidth Width of contour lines.
#' @param color Color of contour lines.
#' @param alpha Transparency of layer.
#' @param ... Additional parameters for ggplot2::geom_contour()
#'
#' @return A geom_contour ggproto object.

layer_contours <- function(
  raster = NULL,
  varName = NULL,
  breaks = NULL,
  lineWidth = 0.5,
  color = "black",
  alpha = 1,
  ...
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  if ( is.null(raster) ) {
    stop("Must provide a raster")
  }
  
  if ( "RasterBrick" %in% class(raster) ) {
    if ( raster::nlayers(raster) > 1 ) {
      
      if ( is.null(varName) ) {
        stop("Must provide a variable name when raster has multiple layers")
      }
      
      rasterLayer <- raster[[varName]]
    } else {
      rasterLayer <- raster
    }
  } else if ( "RasterLayer" %in% class(raster) ) {
    rasterLayer <- raster
  } else {
    stop("Must provide either a RasterBrick or RasterLayer")
  }
  
  # ----- Create layer ---------------------------------------------------------
  
  coords <- raster::xyFromCell(rasterLayer, seq_len(raster::ncell(rasterLayer)))
  readings <- raster::stack(as.data.frame(raster::getValues(rasterLayer)))
  names(readings) <- c("value", "variable")
  
  df <- cbind(coords, readings)
  
  layer <- ggplot2::geom_contour(
    data = df,
    ggplot2::aes(
      x = .data$x,
      y = .data$y,
      z = .data$value
    ),
    breaks = breaks,
    size = lineWidth,
    color = color,
    alpha = alpha,
    ...
  )
  
  return(layer)
  
}


#' @export
#' @title Create a spatial polygons layer for plotting
#'
#' @param polygons A SpatialPolygonsDataFrame.
#' @param lineWidth Line width of polygon outlines.
#' @param color Outline color.
#' @param fill Fill color.
#'
#' @return A geom_polygon ggproto object.

layer_spPolys <- function(
  polygons = NULL,
  lineWidth = 0.5,
  color = "black",
  fill = "transparent"
) {
  
  polygons@data$id <- rownames(polygons@data)
  points <- ggplot2::fortify(polygons, region = 'id')
  df <- merge(points, polygons@data, by = 'id')
  
  layer <- ggplot2::geom_polygon(
    data = df,
    ggplot2::aes(
      x = .data$long,
      y = .data$lat,
      group = .data$group
    ),
    size = lineWidth,
    color = color,
    fill = fill
  )
  
  return(layer)
  
}


#' @export
#' @title Create a state polygons layer for plotting
#'
#' @param lineWidth Line width of state borders.
#' @param color Line color of state borders.
#' @param fill Fill color of state polygons.
#' @param xlim A vector of coordinate longitude bounds.
#' @param ylim A vector of coordinate latitude bounds.
#'
#' @return A geom_polygon ggproto object.

layer_states <- function(
  lineWidth = 0.5,
  color = "black",
  fill = "transparent",
  xlim = NULL,
  ylim = NULL
) {
  
  states <- ggplot2::map_data(
    "state",
    xlim = xlim,
    ylim = ylim
  )
  
  layer <- ggplot2::geom_polygon(
    data = states,
    ggplot2::aes(
      y = .data$lat,
      x = .data$long,
      group = .data$group
    ),
    size = lineWidth,
    fill = fill,
    color = color
  )
  
  return(layer)
  
}


#' @export
#' @title Create a points layer for plotting
#'
#' @param points A SpatialPointsDataFrame of coordinates and data.
#' @param size Point size.
#'
#' @return A geom_point ggproto object.

layer_points <- function(
  points = NULL,
  size = 1
) {
  
  layer <- ggplot2::geom_point(
    data = points,
    ggplot2::aes(
      x = .data$x,
      y = .data$y,
      color = .data$value
    ),
    size = size
  )
  
  return(layer)
  
}


#' @export
#' @title Create a vector field layer for plotting
#'
#' @param raster A RasterBrick with layers for u/v vector components.
#' @param uName The name of the u component layer.
#' @param vName The name of the v component layer.
#' @param arrowCount Number of arrows to draw.
#' @param arrowScale Arrow length scale factor.
#' @param arrowWidth Line width of arrows.
#' @param arrowHead Arrow head size.
#' @param arrowColor Arrow color.
#' @param alpha Transparency of layer.
#' @param xlim A vector of coordinate longitude bounds.
#' @param ylim A vector of coordinate latitude bounds.
#'
#' @return An annotation_custom ggproto object.

layer_vectorField <- function(
  raster = NULL,
  uName = NULL,
  vName = NULL,
  arrowCount = 1000,
  arrowScale = 0.05,
  arrowHead = 0.05,
  arrowWidth = 1.2,
  arrowColor = "dodgerblue",
  alpha = 1,
  xlim = NULL,
  ylim = NULL
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
    axis.line = list(col = "transparent")
  )
  
  if ( is.null(raster) ) {
    stop("Must provide a raster")
  }
  
  if ( is.null(uName) || is.null(vName) ) {
    stop("Must provide uName and vName")
  }
  
  uvRaster <- raster::brick(raster[[uName]], raster[[vName]])
  
  if ( !is.null(xlim) && !is.null(ylim) ) {
    extent <- raster::extent(c(xlim[1], xlim[2], ylim[1], ylim[2]))
    uvRaster <- raster::crop(uvRaster, extent)
  }
  
  vectorField <- rasterVis::vectorplot(
    object = uvRaster,
    isField = "dXY",
    region = FALSE,
    narrows = arrowCount,
    lwd.arrows = arrowWidth,
    length = arrowHead,
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
  extent <- raster::extent(uvRaster)
  
  layer <- ggplot2::annotation_custom(
      grob = vectorFieldGrob,
      xmin = extent@xmin,
      xmax = extent@xmax,
      ymin = extent@ymin,
      ymax = extent@ymax
  )
  
  return(layer)
  
}


# ===== DEBUGGING ==============================================================

if (FALSE) {
  
  library(WRFmet)
  
  extent <- raster::extent(WRFmet::example_PNW)
  xlim <- c(round(extent@xmin), round(extent@xmax))
  ylim <- c(round(extent@ymin), round(extent@ymax))
  
  points <- data.frame(
    x = runif(10, min = -125, max = -111),
    y = runif(10, min = 42, max = 49),
    value = runif(10, min = 0, max = 100)
  )
  
  MazamaSpatialUtils::setSpatialDataDir('~/Data/Spatial')
  MazamaSpatialUtils::loadSpatialData('USCensusStates')
  
  waPoly <- USCensusStates[USCensusStates@data$stateCode == 'WA',]
  
  ggplot2::ggplot() +
    ggplot2::scale_fill_gradientn(
      colors = grDevices::terrain.colors(10),
      na.value = "transparent"
    ) +
    ggplot2::scale_color_gradient(
      low = 'white',
      high = 'black',
      na.value = 'transparent'
    ) +
    layer_raster(
      raster = WRFmet::example_PNW$HGT
    ) +
    layer_spPolys(
      polygons = waPoly,
      fill = 'transparent',
      color = 'red'
    ) +
    layer_points(
      points = points,
      size = 3
    ) +
    layer_contours(
      raster = WRFmet::example_PNW$HGT,
      breaks = c(100, 1500, 2500)
    ) +
    layer_vectorField(
      raster = WRFmet::example_PNW,
      uName = "U10",
      vName = "V10",
      arrowColor = 'black',
      alpha = 0.9
    ) +
    ggplot2::coord_fixed(
      ratio = 1.4,
      xlim = xlim,
      ylim = ylim
    ) +
    ggplot2::labs(
      title = 'Elevation & Wind Map',
      x = 'Longitude',
      y = 'Latitude',
      fill = 'Elev (m)',
      color  = 'PM2.5'
    )

}
