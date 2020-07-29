#' @export
#'
#' @title Load data for a single WRF model
#'
#' @param localPath Absolute path to a NetCDF file not found in `modelDataDir`.
#' @param xlim A vector of coordinate longitude bounds.
#' @param ylim A vector of coordinate latitude bounds.
#' @param verbose Logical to display messages.
#'
#' @return A \pkg{raster} package \emph{RasterBrick} object.
#'
#' @examples
#' \donttest{
#' library(WRFmet)
#' library(ggplot2)
#' library(maps)
#' 
#' localPath <- "~/Data/WRF/wrfout_d3-2020071512-f07-0000.nc"
#' raster <- wrf_load(localPath)
#'
#' states <- map_data("state")
#' 
#' rasterVis::gplot(raster) +
#'   geom_raster(aes(fill = .data$value)) +
#'   geom_polygon(data = states, aes(x = long, y = lat, group = group), color = "black", fill = NA) +
#'   scale_fill_continuous(na.value = "red")
#' }

wrf_load <- function(
  localPath = NULL,
  xlim = NULL,
  ylim = NULL,
  verbose = TRUE
) {
  
  raw_nc <- ncdf4::nc_open(localPath)
  
  lon <- ncdf4::ncvar_get(raw_nc, varid = "XLONG")
  lat <- ncdf4::ncvar_get(raw_nc, varid = "XLAT")
  elev <- ncdf4::ncvar_get(raw_nc, varid = "HGT")
  
  coords <- data.frame(
    x = as.vector(lon),
    y = as.vector(lat)
  )
  
  # Define grid
  cols <- dim(elev)[1] * 0.8
  rows <- dim(elev)[2] * 0.8
  #coordsExtent <- extent(coords)
  #ratio <- (coordsExtent@ymax - coordsExtent@ymin) / (coordsExtent@xmax - coordsExtent@xmin)
  #rows <- round(cols * ratio)
  
  # Create raster
  rbase <- raster::raster(nrows = rows, ncols = cols)
  raster::extent(rbase) <- raster::extent(coords)
  #raster::crs(rbase) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 +units=m"
  
  raster <- raster::rasterize(coords, rbase, field = as.vector(elev), fun = mean)
  
  return(raster)
}
