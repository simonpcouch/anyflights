#' Hourly weather data
#'
#' Hourly meteorological data for the selected airport.
#'
#' @source ASOS download from Iowa Environmental Mesonet,
#'   https://mesonet.agron.iastate.edu/request/download.phtml.
#' @format A data frame with ~25,000 rows and 15 variables:
#' \describe{
#' \item{origin}{Weather station. Named origin to facilitate merging with
#'   \code{\link{flights}} data}
#' \item{year,month,day,hour}{Time of recording}
#' \item{temp,dewp}{Temperature and dewpoint in F}
#' \item{humid}{Relative humidity}
#' \item{wind_dir,wind_speed,wind_gust}{Wind direction (in degrees), speed
#'   and gust speed (in mph)}
#' \item{precip}{Precipitation, in inches}
#' \item{pressure}{Sea level pressure in millibars}
#' \item{visib}{Visibility in miles}
#' \item{time_hour}{Date and hour of the recording as a \code{POSIXct} date}
#' }
