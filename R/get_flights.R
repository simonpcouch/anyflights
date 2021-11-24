#' Query nycflights13-Like Flights Data
#' 
#' This function generates a dataframe similar to the 
#' \code{\link[nycflights13]{flights}} dataset from \code{nycflights13} 
#' for any US airport and time frame. Please 
#' note that, even with a strong internet connection, this function 
#' may take several minutes to download relevant data.
#' 
#' This function currently downloads data for \emph{all} stations for each month
#' supplied, and \emph{then} filters out data for relevant stations. Thus, 
#' the recommended approach to download data for many airports is to supply 
#' a vector of airport codes to the \code{station} argument rather than 
#' iterating over many calls to \code{get_flights()}.
#' 
#' @inheritParams anyflights 
#' 
#' @param ... Currently only used internally.
#' 
#' @return A data frame with ~1k-500k rows and 19 variables:
#' \describe{
#' \item{\code{year, month, day}}{Date of departure}
#' \item{\code{dep_time, arr_time}}{Actual departure and arrival times, UTC.}
#' \item{\code{sched_dep_time, sched_arr_time}}{Scheduled departure and arrival 
#'   times, UTC.}
#' \item{\code{dep_delay, arr_delay}}{Departure and arrival delays, in minutes.
#'   Negative times represent early departures/arrivals.}
#' \item{\code{hour, minute}}{Time of scheduled departure broken into hour and 
#'   minutes.}
#' \item{\code{carrier}}{Two letter carrier abbreviation. See 
#'   \code{\link{get_airlines}} to get full name}
#' \item{\code{tailnum}}{Plane tail number}
#' \item{\code{flight}}{Flight number}
#' \item{\code{origin, dest}}{Origin and destination. See 
#'   \code{\link{get_airports}} for additional metadata.}
#' \item{\code{air_time}}{Amount of time spent in the air, in minutes}
#' \item{\code{distance}}{Distance between airports, in miles}
#' \item{\code{time_hour}}{Scheduled date and hour of the flight as a 
#'   \code{POSIXct} date. Along with \code{origin}, can be used to join 
#'   flights data to weather data.}
#' }
#' 
#' @note
#' If you are repeatedly getting a timeout error when downloading flights,
#' this could be because your download is taking longer than the default timeout
#' R option. You can change the timeout value for your R session by running the
#' code \code{option(timeout = timeout_value_in_seconds)} in your console.
#' 
#' @source RITA, Bureau of transportation statistics,
#'  \url{https://www.transtats.bts.gov}
#' 
#' @examples
#' 
#' # flights out of Portland International in June 2018
#' \donttest{\dontrun{get_flights("PDX", 2018, 6)}}
#' 
#' # ...or the original nycflights13 flights dataset
#' \donttest{\dontrun{get_flights(c("JFK", "LGA", "EWR"), 2013)}}
#' 
#' # use the dir argument to indicate the folder to 
#' # save the data in \code{dir} as "flights.rda"
#' \donttest{\dontrun{get_flights("PDX", 2018, 6, dir = tempdir())}}
#'
#' @seealso \code{\link{get_weather}} for weather data, 
#' \code{\link{get_airlines}} for airlines data,
#' \code{\link{get_airports}} for airports data,
#' \code{\link{get_planes}} for planes data,
#' or \code{\link{anyflights}} for a wrapper function.
#'
#' Use the \code{\link{as_flights_package}} function to convert this dataset 
#' to a data-only package.
#'
#' @export
get_flights <- function(station, year, month = 1:12, dir = NULL, ...) {
  
  if (!hasArg(pb)) {
    # if get_flights isn't supplied a progress bar from the anyflights
    # wrapper, make one!
    diff_from_start <- create_diff_from_start()
    pb <- progress::progress_bar$new(
      format = ":what",
      clear = FALSE, width = 60, show_after = 0)
    pb$tick(0)
    pb$message(stringr::str_pad("Total Time Elapsed", 50, side = "left"))
    write_tick(pb, "  Processing Arguments...")
    in_anyflights <- FALSE
  } else {
    pb <- list(...)$pb
    diff_from_start <- list(...)$diff_fn
    in_anyflights <- TRUE
  }
  
  # check user inputs
  check_arguments(station = station, 
                  year = year, 
                  month = month, 
                  dir = dir,
                  context = "flights")
  
  # create a temporary directory if need be
  if (is.null(dir)) {
    dir_is_null <- TRUE
    dir <- tempdir()
  } else {
    dir_is_null <- FALSE
  }
  
  # make a subdirectory inside the directory to download the raw data into
  flight_exdir <- paste0(dir, "/flights")
  
  write_message(pb, "Finished Processing Arguments", diff_from_start)
  
  # download flight data for each month
  purrr::map(sort(month), download_month,
             year = year, dir = dir, flight_exdir = flight_exdir, 
             pb = pb, diff_fn = diff_from_start)

  write_tick(pb, "  Processing Flights Data")
  
  # load in the flights data for each month, tidy it, and rowbind it
  flights <- purrr::map(dir(flight_exdir, full.names = TRUE),
                        get_flight_data,
                        station = station) %>%
    dplyr::bind_rows() %>%
    dplyr::arrange(year, month, day, dep_time)
  
  # get rid of the "raw" data
  unlink(x = flight_exdir, recursive = TRUE)
    
  if (!dir_is_null) {
    # ...and save the flights data
    save(flights, 
         file = paste0(dir, "/flights.rda"), 
         compress = "bzip2")
  }
  
  if (!in_anyflights) {
    write_message(pb, "Finished Processing Flights Data", diff_from_start)
    write_tick(pb, "All Done!")
  }
  
  return(flights)
}
