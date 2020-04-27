#' Query nycflights13-Like Air Travel Data
#' 
#' This function generates a list of dataframes similar to those found in the
#' nycflights13 data package for any US airports
#' and time frames. Please note that, even with a strong internet connection, 
#' this function may take several minutes to download relevant data.
#' 
#' The \code{anyflights} function is a wrapper around the following functions:
#' \itemize{
#'   \item \code{\link{get_airlines}}: Grab data to translate between two letter 
#'   carrier codes and names
#'   \item \code{\link{get_airports}}: Grab data on airport names and locations
#'   \item \code{\link{get_flights}}: Grab data on all flights that departed 
#'   given US airports in a given year and month
#'   \item \code{\link{get_planes}}: Grab construction information about each 
#'   plane
#'   \item \code{\link{get_weather}}: Grab hourly meterological data for a given 
#'   airport in a given year and month
#' }
#' 
#' @param station A character vector giving the origin US airports of interest
#'  (as the FAA LID airport code).
#'  
#' @param year A numeric giving the year of interest. This argument is currently
#' not vectorized, as dataset sizes for single years are significantly large.
#' Information for the most recent year is usually available by February or 
#' March in the following year.
#' 
#' @param month A numeric giving the month(s) of interest.
#' 
#' @param dir An optional character string giving the directory
#' to save datasets in. By default, datasets will not be saved to file.
#' 
#' @return A list of dataframes (and, optionally, a directory of datasets) 
#' similar to those found in the \code{nycflights13} data package.
#' 
#' @examples
#' # grab data on all flights departing from 
#' # Portland International Airport in June 2019 and 
#' # other useful metadata without saving to file
#' \donttest{\dontrun{anyflights("PDX", 2018, 6)}}
#' 
#' # ...or, grab that same data and opt to save the 
#' # file as well! (tempdir() can usually be specified 
#' # as a character string giving the path to a folder)
#' \donttest{\dontrun{anyflights("PDX", 2018, 6, tempdir())}}
#' 
#' @seealso \code{\link{get_flights}} for flight data,
#' \code{\link{get_weather}} for weather data, 
#' \code{\link{get_airlines}} for airlines data,
#' \code{\link{get_airports}} for airports data,
#' or \code{\link{get_planes}} for planes data.
#' 
#' Use the \code{\link{as_flights_package}} function to convert the output
#' of this function to a data-only package.
#' 
#' @export
anyflights <- function(station, year, month = 1:12, dir = NULL) {
  
  if (!is.null(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  flights <- get_flights(station, year, month, dir)
  airlines <- get_airlines(dir, flights)
  planes <- get_planes(year, dir, flights)
  airports <- get_airports(dir)
  weather <- get_weather(station, year, month, dir)
  
  return(list(airlines = airlines,
              airports = airports,
              flights = flights,
              planes = planes,
              weather = weather))

}
