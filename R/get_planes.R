#' Query nycflights13-Like Planes Data
#' 
#' This function generates a dataframe similar to the 
#' \code{\link[nycflights13]{planes}} dataset from \code{nycflights13} 
#' for any US airports and time frame. Please 
#' note that, even with a strong internet connection, this function 
#' may take several minutes to download relevant data.
#' 
#' @inheritParams anyflights 
#' 
#' @inheritParams get_airlines
#' 
#' @return A data frame with ~3500 rows and 9 variables:
#' \describe{
#' \item{tailnum}{Tail number}
#' \item{year}{Year manufactured}
#' \item{type}{Type of plane}
#' \item{manufacturer, model}{Manufacturer and model}
#' \item{engines, seats}{Number of engines and seats}
#' \item{speed}{Average cruising speed in mph}
#' \item{engine}{Type of engine}
#' }
#' @source FAA Aircraft registry,
#'  \url{https://www.faa.gov/licenses_certificates/aircraft_certification/aircraft_registry/releasable_aircraft_download/}
#' 
#' @examples
#' 
#' # grab airplanes data for 2018
#' \donttest{\dontrun{get_planes(2018)}}
#' 
#' # if you'd like to only return the planes that appear 
#' # in \code{flights}, query your flights dataset first, 
#' # and then supply it as a \code{flights_data} argument
#' \donttest{\dontrun{get_planes(2018, 
#'                  flights_data = get_flights("PDX", 2018, 6))}}
#'
#' @seealso \code{\link{get_flights}} for flight data,
#' \code{\link{get_weather}} for weather data, 
#' \code{\link{get_airlines}} for airlines data,
#' \code{\link{get_airports}} for airports data,
#' or \code{\link{anyflights}} for a wrapper function.
#'
#' Use the \code{\link{as_flights_package}} function to convert this dataset 
#' to a data-only package.
#'
#' @export
get_planes <- function(year, dir = NULL, flights_data = NULL) {

  # check user inputs
  check_arguments(year = year,
                  dir = dir,
                  context = "planes")
  flights_data <- parse_flights_data_arg(flights_data)
  
  # create a temporary directory if need be
  if (is.null(dir)) {
    dir_is_null <- TRUE
    dir <- tempdir()
  } else {
    dir_is_null <- FALSE
  }
  
  # grab the planes data for the relevant year
  planes <- get_planes_data(year, dir, flights_data)
  
  # save the data if a directory was supplied
  if (!dir_is_null) {
    save(planes, file = paste0(dir, "/planes.rda"), compress = "xz")
  }
  
  # ...and return the data!
  planes
}
