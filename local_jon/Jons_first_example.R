# Jon's first work with WRFmet
#

library(raster)
library(WRFmet)
setWRFDataDir("~/Data/WRF")

# NOTE:  My desktop machine sometimes doesn't recognized m2.airfire.org
# NOTE:  So, from Robert: m2.airfire.org = 128.95.182.72

baseUrl <- "http://128.95.182.72"

modelName <- "PNW-4km"
modelRunHour <- 7

# Find the latest modelRun
modelRun <- wrf_latestModelRun("PNW-4km", baseUrl = baseUrl)
print(modelRun)

# Note that model runs are typicaly available starting with hour 7.

# Get the first hour of forecast output
# wrf_download("PNW-4km", modelRun, modelRunHour, baseUrl = baseUrl)
# localPath <- wrf_downloaded()[1]

# Basic test of a single raster layer
raster <- wrf_load(
  modelName = modelName,
  modelRun = modelRun,
  modelRunHour = modelRunHour,
  vars = "HGT",
  res = 0.1,
  xlim = c(-125, -116),
  ylim = c(45, 50)
)

# raster package plot()
raster::plot(raster, col = terrain.colors(255))
maps::map('state', add = TRUE)

# Now work with the full rasterBrick

rasterBrick <- wrf_load(
  modelName = modelName,
  modelRun = modelRun,
  modelRunHour = modelRunHour,
  res = 0.1,
  xlim = c(-125, -116),
  ylim = c(45, 50)
)

raster::labels(rasterBrick)

gg <- wrf_standardPlot(
  raster = rasterBrick,
  bgName = "Q2",
  uName = "U10",
  vName = "V10",
  #ctrName = "T",
  states = TRUE,
  title = sprintf("%s %s -- Hour %d", modelName, modelRun, modelRunHour),
  flab = "Q2",
  ratio = 1.4
)

print(gg)



# How about a special subset
rasterBrick <- wrf_load(
  modelName = modelName,
  modelRun = modelRun,
  modelRunHour = modelRunHour,
  vars = c("U10", "V10", "T", "HGT"),
  res = 0.1,
  xlim = c(-125, -116),
  ylim = c(45, 50)
)


gg <- wrf_standardPlot(
  raster = rasterBrick,
  bgName = "HGT",
  uName = "U10",
  vName = "V10",
  ctrName = "T",
  states = TRUE,
  title = sprintf("%s %s -- Hour %d", modelName, modelRun, modelRunHour),
  flab = "Elev (m)",
  ratio = 1.4
)

print(gg)
