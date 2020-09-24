raster_map <- function(
  raster,
  index = 1,
  palette = "Greys",
  breaks = NULL,
  direction = 1,
  title = NULL,
  timezone = "UTC",
  col_state = "black",
  col_county = "gray80",
  verbose = TRUE
) {
  
  layer <- raster[[index]]
  
  layerList <- lapply(
    X = raster,
    FUN = function(r) {
      if ( index > dim(r)[3] ) {
        stop('Index out of range.')
      }
      return(r[[index]])
    }
  )
  
  layerNames <- names(layerList)
  gg_list <- list()
  for ( i in seq_along(layerList) ) {
    title <- layerNames[i]
    layer <- layerList[[i]]
    
    # Get layer coordinate limits
    limits <- raster::extent(layer)
    xlim <- c(limits@xmin, limits@xmax)
    ylim <- c(limits@ymin, limits@ymax)
    
    #timeString <- raster_createTimeStrings(layer)
    
    gg <-
      ggplot2::ggplot() +
      layer_raster(
        raster = layer,
        breaks = breaks
      ) +
      layer_counties(
        xlim = xlim,
        ylim = ylim,
        color = col_county
      ) +
      layer_states(
        xlim = xlim,
        ylim = ylim,
        color = col_state
      ) +
      ggplot2::labs(
        title = title,
        subtitle = "timeString",
        x = 'Longitude',
        y = 'Latitude',
        fill = layer@title
      ) +
      ggplot2::scale_fill_brewer(
        na.value = NA,
        palette = palette,
        direction = direction
      ) +
      ggplot2::coord_fixed(ratio = 1.3, xlim = xlim, ylim = ylim) +
      ggplot2::theme_classic() +
      # Customizations to the classic theme
      ggplot2::theme(
        panel.border = ggplot2::element_rect(fill = "NA", colour = "black")
      )
      
    gg_list[[title]] <- gg
  }
  
  return(cowplot::plot_grid(plotlist = gg_list))
  
}

if (FALSE) {
  
  #layerList <- list("HGT" = example_PNW$HGT, "TSK" = example_PNW$TSK)
  
  filePaths <- c("~/Data/WRF/PNW-4km_2020092412_07.nc",
                 "~/Data/WRF/PNW-4km_2020092412_08.nc",
                 "~/Data/WRF/PNW-4km_2020092412_09.nc",
                 "~/Data/WRF/PNW-4km_2020092412_10.nc")
  
  layerList <- lapply(
    X = filePaths,
    FUN = function(filePath) {
      
      raster <- wrf_load(
        localPath = filePath,
        varNames = "TSK",
        res = 0.1,
        xlim = c(-125, -117),
        ylim = c(45, 49)
      )
      
      return(raster)
    }
  )
  names(layerList) <- basename(tools::file_path_sans_ext(filePaths))
  
  raster_map(
    raster = layerList,
    breaks = c(270, 280, 290, 300, 310, Inf),
    palette = "Reds",
    col_county = "gray50"
  )
}

# Useful utils:
#   * is.Raster() or is.RasterBrick() or is.RasterLayer()
#   * raster_createTimeStrings()
#
# - It would be very nice to somehow contain a RasterLayer's units within it.

# If we are planning on a standalone AirFirePlots package, then I would want to 
# put all the layer_~() functions in there.