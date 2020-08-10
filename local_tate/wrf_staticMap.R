layer_raster <- function(
  raster = NULL,
  alpha = 1
) {
  x <- raster::sampleRegular(raster, 50000, asRaster = TRUE)
  
  coords <- raster::xyFromCell(x, seq_len(ncell(x)))
  readings <- stack(as.data.frame(getValues(x)))
  names(readings) <- c('value', 'variable')
  
  df <- cbind(coords, readings)
  
  res <- ggplot2::geom_raster(
    data = df,
    ggplot2::aes(
      x = .data$x,
      y = .data$y,
      fill = .data$value
    ),
    alpha = 0.3
  ) 
  
  return(res)
}

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

# Debug
if (TRUE) {
  library(WRFmet)
  library(magrittr)
  
  nc <- ncdf4::nc_open('~/Data/WRF/wrfout_d3-2020071512-f07-0000.nc')
  
  xlim <- c(-132, -105)
  ylim <- c(39, 51)
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
  
  map <- 
    ggplot2::ggplot() +
    ggplot2::scale_fill_continuous(na.value = 'transparent') +
    layer_raster(
      raster = elevRaster
    ) +
    layer_states(
      xlim = xlim,
      ylim = ylim
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
      fill = 'Elev (m)'
    )
  
  map
}
