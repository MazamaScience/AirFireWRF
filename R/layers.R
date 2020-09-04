#' @export
#' @importFrom rlang .data
#' 
#' @title Create a Raster layer for plotting
#'
#' @param raster A RasterLayer or a RasterBrick with one layer.
#' @param alpha Transparency of layer.
#'
#' @return A geom_tile ggproto object.

layer_raster <- function(
  raster = NULL,
  alpha = 1
) {

  if ( is.null(raster) ) {
    stop("Raster must not be NULL")
  }
  
  if ( !("RasterLayer" %in% class(raster)) &&
       !(("RasterBrick" %in% class(raster)) && raster::nlayers(raster) == 1) ) {
    stop("Raster must be a RasterLayer or a RasterBrick with one layer")
  }
  
  # TODO: Look into potentially using ggspatial::layer_spatial
  # https://paleolimbot.github.io/ggspatial/
  coords <- raster::xyFromCell(raster, seq_len(raster::ncell(raster)))
  readings <- raster::stack(as.data.frame(raster::getValues(raster)))
  names(readings) <- c("value", "variable")
  
  df <- cbind(coords, readings)

  layer <- ggplot2::geom_tile(
    data = df,
    ggplot2::aes(
      x = .data$x,
      y = .data$y,
      fill = .data$value
    )
  )

  return(layer)
  
}


#' @export
#' @title Create a spatial polygons layer for plotting
#'
#' @param spdf A SpatialPolygonsDataFrame.
#' @param color Outline color.
#' @param fill Fill color.
#'
#' @return A geom_polygon ggproto object.

layer_spPolys <- function(
  spdf = NULL,
  color = 'black',
  fill = 'white'
) {
  
  spdf@data$id <- rownames(spdf@data)
  points <- ggplot2::fortify(spdf, region = 'id')
  df <- merge(points, spdf@data, by = 'id')
  
  layer <- ggplot2::geom_polygon(
    data = df,
    ggplot2::aes(
      x = .data$long,
      y = .data$lat,
      group = .data$group
    ),
    color = color,
    fill = fill
  )
  
  return(layer)
  
}

#' @export
#' @title Create a state polygons layer for plotting
#'
#' @param xlim A vector of coordinate longitude bounds.
#' @param ylim A vector of coordinate latitude bounds.
#' @param color Line color.
#'
#' @return A geom_polygon ggproto object.

layer_states <- function(
  xlim = NULL,
  ylim = NULL,
  color = 'black'
) {
  
  states <- ggplot2::map_data(
    'state',
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
    fill = 'NA',
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
#' @param uRaster A RasterLayer for longitudinal vector components.
#' @param vRaster A RasterLayer for latitudinal vector components.
#' @param xlim A vector of coordinate longitude bounds.
#' @param ylim A vector of coordinate latitude bounds.
#' @param arrowCount Number of arrows to draw.
#' @param arrowScale Arrow length scale factor.
#' @param arrowColor Arrow color.
#' @param arrowHead Arrow head size.
#' @param alpha Transparency of layer.
#'
#' @return An annotation_custom ggproto object.

layer_vectorField <- function(
  uRaster = NULL,
  vRaster = NULL,
  xlim = NULL,
  ylim = NULL,
  arrowCount = 1000,
  arrowScale = 0.05,
  arrowColor = 'white',
  arrowHead = 0.05,
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
  
  if ( is.null(uRaster) || is.null(vRaster) ) {
    stop("Must provide a uRaster and a vRaster")
  }
  
  uvRaster <- raster::brick(uRaster, vRaster)
  
  # TODO: Actually use the xlim and ylim params to crop the uvRaster before
  # calling vectorPlot. This should properly resample the raster so the output
  # isn't just a zoomed in version of a larger vector field.
  
  vectorField <- rasterVis::vectorplot(
    object = uvRaster,
    isField = "dXY",
    region = FALSE,
    narrows = arrowCount,
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
    x = c(-120, -118, -112),
    y = c(43, 47, 48),
    value = c(20, 110, 68)
  )
  
  MazamaSpatialUtils::setSpatialDataDir('~/Data/Spatial')
  MazamaSpatialUtils::loadSpatialData('USCensusStates')
  
  waPoly <- USCensusStates[USCensusStates@data$stateCode == 'WA',]
  
  wrfMap <- 
    ggplot2::ggplot() +
    ggplot2::scale_fill_gradient(
      low = 'black',
      high = 'gray70',
      na.value = 'transparent'
    ) +
    ggplot2::scale_color_gradient(
      low = 'green',
      high = 'red',
      na.value = 'transparent'
    ) +
    layer_raster(
      raster = WRFmet::example_PNW$HGT
    ) +
    layer_spPolys(
      spdf = waPoly,
      fill = 'transparent',
      color = 'red'
    ) +
    layer_points(
      points = points,
      size = 3
    ) +
    layer_vectorField(
      uRaster = WRFmet::example_PNW$U10,
      vRaster = WRFmet::example_PNW$V10,
      arrowColor = 'yellow',
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
  
  print(wrfMap)
  
}
