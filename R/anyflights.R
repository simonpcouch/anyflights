#' Query nycflights13-Like Air Travel Data
#' 
#' This function generates a list of dataframes similar to those found in the
#' \code{nycflights13} data package for any US airports
#' and time frames. Please note that, even with a strong internet connection, 
#' this function may take several minutes to download relevant data.
#' 
#' The \code{anyflights()} function is a wrapper around the following functions:
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
#' The recommended approach to download data for many stations (airports)
#' is to supply a vector of stations to the \code{station} argument rather than 
#' iterating over many calls to \code{anyflights()}. The \code{faa} column
#' in dataframes outputted by \code{get_airports()} provides the FAA LID
#' codes for all supported airports. See 
#' ?\code{\link{get_flights}} for more details on implementation.
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
  
  # create a function, unique to this call to anyflights,
  # that returns the difference in time from when the function was called
  diff_from_start <- create_diff_from_start()
  
  # initiate the progress bar
  pb <- progress::progress_bar$new(
    format = ":what",
    clear = FALSE, width = 60, show_after = 0)
  pb$tick(0)
  
  pb$message(stringr::str_pad("Total Time Elapsed", 50, side = "left"))
  
  if (!is.null(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  write_tick(pb, "  Processing Arguments...")
  flights <- get_flights(station, year, month, dir, 
                         pb = pb, diff_fn = diff_from_start)
  write_message(pb, "Finished Downloading Flights Data", diff_from_start)
  
  write_tick(pb, "  Downloading Airlines...")
  airlines <- get_airlines(dir, flights)
  write_message(pb, "Finished Downloading Airlines Data", diff_from_start)
  
  write_tick(pb, "  Downloading Planes...")
  planes <- get_planes(year, dir, flights)
  write_message(pb, "Finished Downloading Planes Data", diff_from_start)
  
  write_tick(pb, "  Downloading Airports...")
  airports <- get_airports(dir)
  write_message(pb, "Finished Downloading Airports Data", diff_from_start)
  
  write_tick(pb, "  Downloading Weather...")
  weather <- get_weather(station, year, month, dir)
  write_message(pb, "Finished Downloading Weather Data", diff_from_start)
  write_tick(pb, "All Done!")
  return(list(airlines = airlines,
              airports = airports,
              flights = flights,
              planes = planes,
              weather = weather))

}
