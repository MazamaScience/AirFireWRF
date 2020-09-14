# Create a map with:
#  * Bluesky PNW-4km output over Oregon
#  * WRF PNW-4km vector field for the same timestep
#  * State boundaries
#  ? county or HUC8 boundaries
#  ? monitoring dots
#  ? RAWS data
#

modelName <- "PNW-4km"
modelRun <- 2020091400
modelRunHour <- 12

# ----- Spatial Data -----------------------------------------------------------

library(MazamaSpatialUtils)
MazamaSpatialUtils::setSpatialDataDir("~/Data/Spatial")
MazamaSpatialUtils::loadSpatialData("USCensusStates_05")

OR <- subset(USCensusStates_05, stateCode == "OR")

plot(OR)

bbox(OR)

xlim <- c(-125, -116)
ylim <- c(41, 47)

# ----- Bluesky Data -----------------------------------------------------------

library(raster)
library(AirFireModeling)
AirFireModeling::setModelDataDir("~/Data/BlueSky")

# Load BlueSky model run
bsList <- AirFireModeling::raster_load(
  modelName = modelName,
  modelRun = modelRun,
  xlim = xlim,
  ylim = ylim
)

bsBrick <- bsList[[1]]

bsHour12 <- bsBrick[[modelRunHour]]

AirFireModeling::raster_ggmap(bsHour12)

# ----- WRF Data ---------------------------------------------------------------

library(ggplot2)
library(WRFmet)
WRFmet::setWRFDataDir("~/Data/WRF")

# Basic test of a single raster layer
wrfUV <- wrf_load(
  modelName = modelName,
  modelRun = modelRun,
  modelRunHour = modelRunHour,
  vars = c("U10","V10"),
  res = 0.1,
  xlim = xlim,
  ylim = ylim
)

# ----- Plot #1 ----------------------------------------------------------------

gg <- AirFireModeling::raster_ggmap(bsHour12)

gg +
  layer_vectorField(
    uRaster = wrfUV$U10,
    vRaster = wrfUV$V10,
    arrowColor = "blue"
  ) + 
  ggtitle(sprintf("Smoke and Winds for run %d, hour %d", modelRun, modelRunHour))

# ----- Plot #2 ----------------------------------------------------------------

plot_standard(
  bgRaster = bsHour12,
  uRaster = wrfUV$U10,
  vRaster = wrfUV$V10,
  title = sprintf("%s %s -- Hour %d", modelName, modelRun, modelRunHour),
  flab = "PM2.5"
) +
  ###layer_spPolys(OR, fill = 'transparent')
  layer_states(fill = 'transparent', xlim = xlim, ylim = ylim)

# ----- Plot #3 ----------------------------------------------------------------

gg <- 
  # Define an empty plotting space
  ggplot2::ggplot() +
  # Draw the temperature raster
  layer_raster(
    raster = bsHour12
  ) +
  # Crop the plot area
  ggplot2::coord_fixed(
    ratio = 1.4,
    xlim = xlim,
    ylim = ylim
  ) +
  # Set raster color palette
  ###ggplot2::scale_fill_gradientn(
  ggplot2::scale_color_stepsn(
    colors = c('white','gray90','gray95','gray90','gray80','gray60','gray40','gray20'),
    values = c(12,35,55,150,250,350,500),
    na.value = 'transparent'
  ) +
  # Set the plot labels
  ggplot2::labs(
    title = 'WRF Temperature',
    x = 'Longitude',
    y = 'Latitude',
    fill = 'PM2.5'
  )

gg

# ===== Portland Area ==========================================================

xlim <- c(-123, -121)
ylim <- c(44, 46)

bsHour12 <- AirFireModeling::raster_load(
  modelName = modelName,
  modelRun = modelRun,
  xlim = xlim,
  ylim = ylim
)[[1]][[modelRunHour]]

# Basic test of a single raster layer
wrfUV <- wrf_load(
  modelName = modelName,
  modelRun = modelRun,
  modelRunHour = modelRunHour,
  vars = c("U10","V10"),
  res = 0.1,
  xlim = xlim,
  ylim = ylim
)

# ----- Plot #4 ----------------------------------------------------------------

AirFireModeling::raster_ggmap(bsHour12) +
  layer_vectorField(
    uRaster = wrfUV$U10,
    vRaster = wrfUV$V10,
    arrowColor = "blue"
  ) + 
  ggtitle(sprintf("Smoke and Winds for run %d, hour %d", modelRun, modelRunHour))


