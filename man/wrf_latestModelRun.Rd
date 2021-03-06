% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wrf_latestModelRun.R
\name{wrf_latestModelRun}
\alias{wrf_latestModelRun}
\title{Find latest WRF model run from AirFire}
\usage{
wrf_latestModelRun(
  modelName = NULL,
  count = 1,
  baseUrl = "http://m2.airfire.org"
)
}
\arguments{
\item{modelName}{Model identifier.}

\item{count}{Number of most recent \code{modelRun} strings to return. Set to
\code{Inf} to get \emph{all} of the available model runs.}

\item{baseUrl}{Base URL for WRF output.}
}
\value{
Timestamp of the latest model run.
}
\description{
Scans the directory of WRF model output and returns the most 
recent \code{modelRun} string(s). If \code{count > 1}, the most recent
\code{count} will be returned in low-hi order.

On 2020-08-27, available model identifiers include the following:
\itemize{
  \item{PNW-4km}
}
}
\examples{
\donttest{
library(AirFireWRF)
wrf_latestModelRun("PNW-4km")
}
}
