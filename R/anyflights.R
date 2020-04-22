#' Easily Query nycflights13-Like Air Travel Data
#' 
#' The \code{anyflights} function is a wrapper around the following functions:
#' \itemize{
#'   \item \code{get_airlines}: Grab data to translate between two letter 
#'   carrier codes and names
#'   \item \code{get_airports}: Grab data on airport names and locations
#'   \item \code{get_flights}: Grab data on all flights that departed given 
#'   airports in a given year and month
#'   \item \code{get_planes}: Grab construction information about each plane
#'   \item \code{get_weather}: Grab hourly meterological data for a given 
#'   airport in a given year and month
#' }
#' 
#' The function returns a list of dataframes (or directory of datasets) 
#' similar to those found in the \code{nycflights13} data package. Please 
#' note that, even with a strong internet connection, this function 
#' may take several minutes to download relevant data.
#' 
#' @param station A character vector giving the origin airports of interest
#'  (as the FAA LID airport code).
#'  
#' @param year A numeric giving the year of interest. This argument is currently
#' not vectorized, as dataset sizes for single years are significantly large.
#' Information for the most recent year is usually available by February or 
#' March in the following year.
#' 
#' @param month A numeric giving the month(s) of interest.
#' 
#' @param dir An (always) optional character string giving the directory
#' to save datasets in. By default, datasets will not be saved to file.
#' 
#' @param flights_data An optional argument to \code{get_airlines} and 
#' \code{get_planes}so that the function will only return carriers
#' that appear in the flight data of interest---either a filepath as a 
#' character string or a dataframe outputted by \code{get_flights}. If 
#' not supplied, all carriers and planes will be returned.
#' 
#' @return A named list of dataframes and, optionally, a folder containing the 
#' datasets saved to file.
#' 
#' @examples
#' # grab data on all flights departing from Portland International Airport
#' # in June 2019 without saving to file as well as other useful metadata
#' \dontrun{\donttest{anyflights("PDX", 2018, 6)}}
#' 
#' # ...or, grab that same data and opt to save the file as well! (tempdir() 
#' # can usually be specified as a character string)
#' \dontrun{\donttest{anyflights("PDX", 2018, 6, tempdir())}}
#' 
#' # equivalently to the first call, grab each of the datasets individually
#' \dontrun{\donttest{get_flights("PDX", 2018, 6)}}
#' \dontrun{\donttest{get_airlines(2018)}}
#' \dontrun{\donttest{get_airports()}}
#' 
#' # get_flights is vectorized on the month argument! to grab both june and
#' # July, you could call
#' \dontrun{\donttest{flights <- get_flights("PDX", 2018, 6:7)}}
#' 
#' # the get_airlines function optionally takes in a flights dataframe
#' # to automatically subset the airlines data down to carriers
#' # that appear in the flights data
#' \dontrun{\donttest{get_airlines(2018, flights)}}
#' 
#' # the same goes for the get_planes function!
#' \dontrun{\donttest{get_planes(2018, flights)}}
#' 
#' # the months provided to get_weather (and get_flights) don't necessarily
#' # have to be back-to-back---to just grab the weather for June and August
#' # at Portland International in 2018, you could call
#' \dontrun{\donttest{get_weather("PDX", 2018, c(6, 8))}
#'
#' 
#' # if the flights_data argument isn't provided to the functions above,
#' # unsubsetted data will be returned
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
