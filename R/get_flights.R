#' Generate a flights dataset for a specified year and airport
#' 
#' Please note that, even with a strong internet connection, this function 
#' may take several minutes to download relevant data, and temporarily 
#' requires up to 2GB of storage (the file size is trimmed down significantly 
#' after some post-processing---to the order of a couple MB---and the larger 
#' files are deleted before termination)
#' 
#' 
#' @param station A character vector giving the airports of interest (using the 
#' FAA LID airport code).
#' @param year The years of interest. Currently, years 2015 and on are 
#' supported. Information for the most recent year is usually available by 
#' February or March in the following year.
#' @param dir A character string--the folder for the dataset to be saved in
#' @return A data frame with ~10k-500k rows and 19 variables:
#' \describe{
#' \item{year,month,day}{Date of departure}
#' \item{dep_time,arr_time}{Actual departure and arrival times, local tz.}
#' \item{sched_dep_time,sched_arr_time}{Scheduled departure and arrival times, local tz.}
#' \item{dep_delay,arr_delay}{Departure and arrival delays, in minutes.
#'   Negative times represent early departures/arrivals.}
#' \item{hour,minute}{Time of scheduled departure broken into hour and minutes.}
#' \item{carrier}{Two letter carrier abbreviation. See \code{\link{get_airlines}}
#'   to get name}
#' \item{tailnum}{Plane tail number}
#' \item{flight}{Flight number}
#' \item{origin,dest}{Origin and destination. See \code{\link{get_airports}} for
#'   additional metadata.}
#' \item{air_time}{Amount of time spent in the air, in minutes}
#' \item{distance}{Distance between airports, in miles}
#' \item{time_hour}{Scheduled date and hour of the flight as a \code{POSIXct} date.
#'   Along with \code{origin}, can be used to join flights data to weather data.}
#' }
#' @source RITA, Bureau of transportation statistics,
#'  \url{http://www.transtats.bts.gov}

#' @examples
#' \donttest{get_flights(station = "MCI", year = 2016, dir = tempdir())}
#' @seealso \code{\link{get_airports}} for airport data, 
#' \code{\link{get_weather}} for weather data, \code{\link{get_airlines}} 
#' for airline data, and \code{\link{anyflights}} for a wrapper function  
#' @export
get_flights <- function(station, year, month, dir = NULL) {
  
  # create a temporary directory if need be
  if (is.null(dir)) {
    return_data <- TRUE
    dir <- tempdir()
  } else {
    return_data <- FALSE
  }
  
  # if the directory doesn't exist, make it!
  if (!dir.exists(dir)) {dir.create(dir)}
  
  # make a subdirectory inside the directory to download the raw data into
  flight_exdir <- paste0(dir, "/flights")
  
  # download flight data for the relevant time range
  purrr::map(month, download_month, year = year, flight_exdir)
  
  # load in the flights data for each month, tidy it, and rowbind it
  tidy_flights_data <- map(dir(flight_exdir, full.names = TRUE),
                           get_flight_data) %>%
    bind_rows() %>%
    dplyr::arrange(year, month, day, dep_time)
  
  if (return_data) {
    # get rid of the tempdir
    unlink(x = dir, recursive = TRUE)
    
    # return the data
    return(tidy_flights_data)
  } else {
    # get rid of the "raw" data
    unlink(x = flight_exdir, recursive = TRUE)
    
    # ...and save the flights data
    save(tidy_flights_data, 
         file = paste0(dir, "/flights.rda"), 
         compress = "bzip2")
    
    return(TRUE)
  }
}
