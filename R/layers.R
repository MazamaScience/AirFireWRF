#' @export
#' @importFrom rlang .data
#' 
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
  
  res <- ggplot2::geom_polygon(
    data = states,
    ggplot2::aes(
      y = .data$lat,
      x = .data$long,
      group = .data$group
    ),
    fill = 'NA',
    color = color
  )
  
  return(res)
}

#' @export
#' @title Create a points layer for plotting
#'
#' @param points A SpatialPointsDataFrame of coordinates and data.
#'
#' @return A geom_point ggproto object.

layer_points <- function(
  points = NULL
) {
  
  res <- ggplot2::geom_point(
    data = points,
    ggplot2::aes(
      x = .data$x,
      y = .data$y,
      color = .data$value
    )
  )
  
  return(res)
}

#' @export
#' @title Create a vector field layer for plotting
#'
#' @param uvRaster A RasterBrick with 2 RasterLayers: U and V vector components.
#' @param uLayer A RasterLayer of U vector components.
#' @param vLayer A RasterLayer of V vector components.
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
  uLayer = NULL,
  vLayer = NULL,
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
  
  if ( is.null(uvRaster) ) {
    
    if ( is.null(uLayer) || is.null(vLayer) )
      stop(sprintf("Must provide either a uvRaster or a uLayer & vLayer"))
    
    uvRaster <- raster::brick(uLayer, vLayer)
  }
  
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

if (FALSE) {
  library(WRFmet)
  
  nc <- ncdf4::nc_open('~/Data/WRF/wrfout_d3-2020071512-f07-0000.nc')
  
  xlim <- c(-133, -106)
  ylim <- c(40, 51)
  rasterRes <- 0.06
  
  elevRaster <- wrf_createRaster(
    nc = nc,
    vars = 'HGT',
    xlim = xlim,
    ylim = ylim,
    res = rasterRes
  )
  
  windRaster <- wrf_createRaster(
    nc = nc,
    vars = c('U10', 'V10'),
    xlim = xlim,
    ylim = ylim,
    res = rasterRes
  )
  
  points <- data.frame(
    x = c(-120, -110, -112),
    y = c(42, 45, 48),
    value = c(20, 11000, 68)
  )
  
  wrfMap <- 
    ggplot2::ggplot() +
    ggplot2::scale_fill_gradient(
      low = 'black',
      high = 'white',
      na.value = 'transparent'
    ) +
    ggplot2::scale_color_gradient(
      low = 'green',
      high = 'red',
      na.value = 'transparent'
    ) +
    layer_raster(
      raster = elevRaster
    ) +
    layer_states(
      xlim = xlim,
      ylim = ylim
    ) +
    layer_points(
      points = points
    ) +
    layer_vectorField(
      uvRaster = windRaster,
      alpha = 0.75
    ) +
    ggplot2::coord_cartesian(
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
  
  wrfMap
}
