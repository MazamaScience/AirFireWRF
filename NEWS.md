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
