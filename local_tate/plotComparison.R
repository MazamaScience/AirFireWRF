localPath <- "~/Data/WRF/wrfout_d3-2020071512-f07-0000.nc"
raster <- wrf_load(localPath)

# Test plots
library(ggplot2)
library(maps)
states <- map_data("state")

raw_nc <- ncdf4::nc_open(localPath)

lon <- ncdf4::ncvar_get(raw_nc, varid = "XLONG")
lat <- ncdf4::ncvar_get(raw_nc, varid = "XLAT")
elev <- ncdf4::ncvar_get(raw_nc, varid = "HGT")

coords <- data.frame(
  x = as.vector(lon),
  y = as.vector(lat),
  elev = as.vector(elev)
)

# The raw spatial point readings fanned across the PNW
ggplot() + 
  geom_point(data = coords, aes(x = x, y = y, color = elev), size = 0.01, shape = 15) +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = "black", fill = NA) +
  coord_cartesian(xlim = c(-130, -106), ylim = c(39, 51))

# Converting the NetCDF straight to a raster (squishes fan into a rectangle)
rawRaster <- raster::brick(localPath, varname = c("HGT"), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
rasterVis::gplot(rawRaster) +
  ggplot2::geom_raster(ggplot2::aes(fill = .data$value)) +
  geom_polygon(data=states, aes(x=long, y=lat, group=group), color="black", fill=NA) +
  scale_fill_continuous(na.value = "transparent")

# The rasterized spatial points
rasterVis::gplot(raster) +
  ggplot2::geom_raster(ggplot2::aes(fill = .data$value)) +
  geom_polygon(data=states, aes(x=long, y=lat, group=group), color="black", fill=NA) +
  scale_fill_continuous(na.value = "red") +
  coord_cartesian(xlim = c(-130, -106), ylim = c(39, 51))