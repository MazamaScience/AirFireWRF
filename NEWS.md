# WRFmet 0.1.12

* Tweak to color and line width in `layer_vectorField()`.

# WRFmet 0.1.11

* `wrf_standardPlot()` can now plot a contour layer.
* Added line thickness parameters in `wrf_standardPlot()` for the spatial and 
state polygon layers.
* Updated `wrf_standardPlot()` layer color defaults:
  * Spatial polygons: `color` -> "red", `fill` -> "transparent".
  * State polygons: `color` -> "red", `fill` -> "transparent".
  * Vector field: `arrowColor` -> "black"
* Removed the `...` parameter from both `wrf_raster()` and `wrf_standardPlot()`
since it was more confusing than helpful (only held additional arguments to
`ggplot2::scale_fill_gradientn()`).

# WRFmet 0.1.10

* First pass at `wrf_loadTimeSeries()`.

# WRFmet 0.1.9

* Added new plot layer function: `layer_contours()`.
* Changed `layer_spPolys()`'s `spdf` parameter to `polygons`.
* Added `arrowWidth` as a parameter to `layer_vectorField()`.
* Added `lineWidth` as a parameter to `layer_states()`.
* Added `lineWidth` as a parameter to `layer_spPolys()`.

# WRFmet 0.1.8

* example_PNW raster now has a proper title.

# WRFmet 0.1.7

* Changed layer_vectorField() to take in a single raster object `raster` and the
names of its u and v layers: `uName` and `vName`.
* Changed layer_raster() to take in an optional `varName` parameter in case the
given raster is a multilayer *RasterBrick*.
* Changed the names of `plot_base()`, `plot_raster()`, and `plot_standard()` to
`wrf_basePlot()`, `wrf_rasterPlot()`, and `wrf_standardPlot()` respectively.
* `wrf_rasterPlot()` and `wrf_standardPlot()` plot titles default to their
raster title.
* Updated all vignettes and functions, and examples to properly use these new 
parameters and function names.

# WRFmet 0.1.6

* `wrf_load()` now sets the title of the loaded *RasterBrick* to the format:
"[model domain] yyyy-mm-dd hh[am/pm], Hour [forecast hour]"

# WRFmet 0.1.5

* Changed default polygon fill from 'white' to 'transparent' for `layer_states()`
and `layer_spPolys()`.

# WRFmet 0.1.4

* Added ability to download and load PNW-1.33km WRF files.
* `plot_standard()` now supports drawing both U.S. state polygons and generic 
_SpatialPolygonDataFrame_s.
* `wrf_load()` now has a variable preset for AirFire.
* Removed parameters `fillLow` and `fillHigh` from `plot_standard()` and 
`plot_raster()` to instead support parameters for 
`ggplot2::scale_fill_gradientn()`.
* `layer_vectorField()` 
  * No longer supports the `uvRaster` parameter.
  * Changed parameters `uLayer` and `vLayer` to `uRaster` and `vRaster`.
  * Properly resamples cropped rasters.
* Hid `plot_base()` from users.

# WRFmet 0.1.3

* Added "Saving Data Subsets" article.
* Renamed the `sample_PNW` dataset to `example_PNW`.

# WRFmet 0.1.2

* First pass at standard plotting functions `plot_base()` and `plot_raster()`.
* Added `layer_~()` functions to build up ggplot plots: `layer_raster()`,
`layer_spPolys()`, `layer_states()`, `layer_points()`, `layer_vectorField()`.
* Added `wrf_download()` and `wrf_downloaded()`.
* Added `wrf_latestModelRun()` to list available model runs.

# WRFmet 0.1.1

* Added "Static Layered Plots" article.

# WRFmet 0.1.0

* Initial Release
