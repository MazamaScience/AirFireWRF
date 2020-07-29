# https://www.climatescience.org.au/sites/default/files/physics-3.9-new-pt1.pdf
# https://www.climatescience.org.au/sites/default/files/werner_nesting.pdf
# https://en.wikipedia.org/wiki/Arakawa_grids
# WRF uses Arakawa Grid type 'C'
#
# u: West-East vector component 
# v: South-North vector component
# x: Non-vector met variable
#
# +--v--+--v--+--v--+
# |     |     |     |
# u  x  u  x  u  x  u
# |     |     |     |
# +--v--+--v--+--v--+
# |     |     |     |
# u  x  u  x  u  x  u
# |     |     |     |
# +--v--+--v--+--v--+

# ----- Open NetCDF file -------------------------------------------------------

# Create old and new file paths
filePath <- "~/Data/WRF/wrfout_d3-2020071512-f07-0000.nc"
rawFilePath <- filePath
v2FilePath <- stringr::str_replace(rawFilePath, "\\.nc$", "_v2.nc")

# Open nc file
raw_nc <- ncdf4::nc_open(filePath)

# ----- Create latitude and longitude axes -------------------------------------

row <- raw_nc$dim$west_east$vals
col <- raw_nc$dim$south_north$vals

# Useful information is found in the global attributes
global_attributes <- ncdf4::ncatt_get(raw_nc, varid = 0) # varid=0 means 'global'
lons <- ncdf4::ncvar_get(raw_nc, varid = "XLONG")
lats <- ncdf4::ncvar_get(raw_nc, varid = "XLAT")

XCENT <- global_attributes[["CEN_LON"]] # x center
YCENT <- global_attributes[["CEN_LAT"]] # y center
XORIG <- lons[1, 1]
YORIG <- lats[1, 1]

# Now we have enough information about the domain to figure out W, E, S, N
w <- XORIG
e <- XORIG + 2 * abs(XCENT - XORIG)
s <- YORIG
n <- YORIG + 2 * (YCENT - YORIG)

# Knowing the grid dimensions and the true edges, we can define legitimate
# lat/lon dimensions
lat <- seq(s, n, length.out = length(row))
lon <- seq(w, e, length.out = length(col))

# ----- Create time axis -------------------------------------------------------

# ----- Create new ncdf4 object ------------------------------------------------

# Get skin temp values
tsk <- ncdf4::ncvar_get(raw_nc, "HGT")

tsk <- apply(apply(tsk, 2, rev), 2, rev)

latDim <- ncdf4::ncdim_def("lat", "Degrees North", lat)
lonDim <- ncdf4::ncdim_def("lon", "Degrees East", lon)

tskVar <- ncdf4::ncvar_def(
  name = "TSK",
  units = "K",
  dim = list(lonDim, latDim),
  missval = -1e30
)

# Create a new netcdf file
nc <- ncdf4::nc_create(v2FilePath, tskVar)

# Put data into the newly defined variable
ncdf4::ncvar_put(nc, tskVar, tsk)

# Close the file
ncdf4::nc_close(nc)
######################
test <- raster::brick(v2FilePath)
sp::plot(test)
