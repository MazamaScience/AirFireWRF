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
    
    # Get state and counties data for plotting
    states <- ggplot2::map_data('state', xlim = xlim, ylim = ylim)
    counties <- ggplot2::map_data('county', xlim = xlim, ylim = ylim)
    
    if (TRUE) {
      gg <-
        ggplot2::ggplot() +
        layer_raster(
          raster = layer,
          breaks = breaks
        ) +
        ggplot2::geom_path(
          data = counties,
          ggplot2::aes(x = .data$long, y = .data$lat, group = .data$group),
          fill = 'NA',
          color = col_county
        ) +
        ggplot2::geom_polygon(
          data = states,
          ggplot2::aes(y = .data$lat, x = .data$long, group = .data$group),
          fill = 'NA',
          color = col_state
        ) +
        ggplot2::labs(
          title = title,
          subtitle = "timeString",
          x = 'Longitude',
          y = 'Latitude',
          fill = title
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
    } else {
      gg <-
        rasterVis::gplot(layer) +
        ggplot2::geom_raster(ggplot2::aes(fill = cut(.data$value, breaks))) +
        ggplot2::geom_path(
          data = counties,
          ggplot2::aes(x = .data$long, y = .data$lat, group = .data$group),
          fill = 'NA',
          color = col_county
        ) +
        ggplot2::geom_polygon(
          data = states,
          ggplot2::aes(y = .data$lat, x = .data$long, group = .data$group),
          fill = 'NA',
          color = col_state
        ) +
        ggplot2::scale_fill_brewer(
          na.value = NA,
          palette = palette,
          direction = direction
        ) +
        ggplot2::labs(
          title = title,
          subtitle = "timeString",
          x = 'Longitude',
          y = 'Latitude',
          fill = 'PM2.5'
        ) +
        ggplot2::coord_fixed(ratio = 1.3, xlim = xlim, ylim = ylim) +
        ggplot2::theme_classic() +
        # Customizations to the classic theme
        ggplot2::theme(
          panel.border = ggplot2::element_rect(fill = "NA", colour = "black")
        )
    }
      
    gg_list[[title]] <- gg
  }
  
  return(cowplot::plot_grid(plotlist = gg_list))
  
}

if (FALSE) {
  layerList <- list("HGT" = example_PNW$HGT, "TSK" = example_PNW$TSK)
  raster_map(layerList, breaks = c(0, 50, 100, 150, 200, 250, 300, 350, Inf))
}

# Useful utils:
#   * is.Raster() or is.RasterBrick() or is.RasterLayer()
#   * raster_createTimeStrings()
#
# - It would be very nice to somehow contain a RasterLayer's units within it.