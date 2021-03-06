% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wrf_load.R
\name{wrf_load}
\alias{wrf_load}
\title{Load WRF model run data}
\usage{
wrf_load(
  modelName = NULL,
  modelRun = NULL,
  modelRunHour = NULL,
  baseUrl = "http://m2.airfire.org",
  localPath = NULL,
  varNames = NULL,
  res = NULL,
  xlim = NULL,
  ylim = NULL,
  verbose = TRUE
)
}
\arguments{
\item{modelName}{Model identifier.}

\item{modelRun}{Model initialization timestamp as "YYYYMMDDHH".}

\item{modelRunHour}{Hour forecasted from initial time, i.e. 7.}

\item{baseUrl}{Base URL for WRF output.}

\item{localPath}{Absolute path to a NetCDF file not found in `WRFDataDir`.}

\item{varNames}{WRF variable(s) to load. If \code{NULL}, the following subset
specified by AirFire will be loaded: XLONG, XLAT, XLONG_U, XLAT_U, XLONG_V, 
XLAT_V, U, V, U10, V10, ZNU, ZNW, LU_INDEX, Q2, T, T2, TH2, HGT, RAINNC, 
CFRACT, and PBLH.}

\item{res}{Resolution of raster in degrees.}

\item{xlim}{A vector of coordinate longitude bounds.}

\item{ylim}{A vector of coordinate latitude bounds.}

\item{verbose}{Logical to display messages.}
}
\value{
A \pkg{raster} package \emph{RasterBrick} object.
}
\description{
Loads WRF model run data as a \pkg{raster} package 
\emph{RasterBrick}. If the specified data does not exist on the user's 
machine, then it will try to be downloaded from the AirFire database.

On 2020-09-03, available model identifiers include the following:
\itemize{
  \item{PNW-1.33km}
  \item{PNW-4km}
}

Some useful variables provided by WRF:
\itemize{
  \item{XLONG - Longitude (degrees East)}
  \item{XLAT - Latitude (degrees North)}
  \item{XLONG_U - Longitude (degrees East)}
  \item{XLAT_U - Latitude (degrees North)}
  \item{XLONG_V - Longitude (degrees East)}
  \item{XLAT_V - Latitude (degrees North)}
  \item{U - Wind X component (m/s)}
  \item{V - Wind Y component (m/s)}
  \item{W - Wind Z component (m/s)}
  \item{U10 - Wind X component at 10 meters (m/s)}
  \item{V10 - Wind Y component at 10 meters (m/s)}
  \item{ZNU - eta values on half (mass) levels}
  \item{ZNW - eta values on full (w) levels}
  \item{LU_INDEX - Land use category}
  \item{Q2 - QV at 2 M (kg)}
  \item{T - Perturbation potential temperature theta-t0 (K)}
  \item{T2 - Temperature at 2 meters (K)}
  \item{TH2 - Potential temperature at 2 meters (K)}
  \item{HGT - Terrain height (m)}
  \item{RAINNC - Accumulated total grid scale precipitation (mm)}
  \item{CFRACT - Total cloud fraction}
  \item{PBLH - Planetary boundary layer height (m)}
}
}
\examples{
\donttest{
library(AirFireWRF)
setWRFDataDir("~/Data/WRF")

raster <- wrf_load(
  modelName = "PNW-4km",
  modelRun = "2020082612",
  modelRunHour = 9,
  varNames = c("HGT", "TSK", "U10", "V10"),
  res = 0.1,
  xlim = c(-125, -116),
  ylim = c(45, 50)
)

print(raster)
}
}
