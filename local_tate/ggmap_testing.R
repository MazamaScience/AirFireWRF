
library(WRFmet)

nc <- ncdf4::nc_open('~/Data/WRF/wrfout_d3-2020071512-f07-0000.nc')

xlim <- c(-130, -105)
ylim <- c(40, 50)
rasterRes <- 0.06

elevRaster <- wrf_createRaster(
  nc = nc,
  varNames = 'HGT',
  xlim = xlim,
  ylim = ylim,
  res = rasterRes
)

windRaster <- wrf_createRaster(
  nc = nc,
  varNames = c('U10', 'V10'),
  xlim = xlim,
  ylim = ylim,
  res = rasterRes
)


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
  
  return(vectorFieldGrob)
}




map <- ggmap::get_stamenmap(
  bbox = c(left = xlim[1], bottom = ylim[1], right = xlim[2], top = ylim[2]),
  zoom = 6
)

grob <- layer_vectorField(
  uvRaster = windRaster,
  alpha = 0.75
)

ggmap::ggmap(map) +
  ggmap::inset(
    grob = grob,
    xmin = xlim[1],
    xmax = xlim[2],
    ymin = ylim[1],
    ymax = ylim[2]
  )

