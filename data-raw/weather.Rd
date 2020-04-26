\docType{data}
\name{weather}
\alias{weather}
\title{Hourly weather data}
\format{A data frame with columns
\describe{
\item{\code{origin}}{Weather station. Named \code{origin} to facilitate 
  merging with flights data}
\item{\code{year, month, day, hour}}{Time of recording, UTC}
\item{\code{temp, dewp}}{Temperature and dewpoint in F}
\item{\code{humid}}{Relative humidity}
\item{\code{wind_dir, wind_speed, wind_gust}}{Wind direction (in degrees), 
  speed and gust speed (in mph)}
\item{\code{precip}}{Precipitation, in inches}
\item{\code{pressure}}{Sea level pressure in millibars}
\item{\code{visib}}{Visibility in miles}
\item{\code{time_hour}}{Date and hour of the recording as a \code{POSIXct} 
  date, UTC}
}}
\source{
ASOS download from Iowa Environmental Mesonet,
\url{https://mesonet.agron.iastate.edu/request/download.phtml}.
}
\description{
Hourly meterological data.
}
