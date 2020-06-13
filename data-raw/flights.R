#' Flights data
#'
#' On-time data for all flights that departed from the given airports.
#'
#' @name flights
#' @docType data
#' @format A data frame with columns:
#' \describe{ 
#'   \item{year, month, day}{Date of departure.} 
#'   \item{dep_time, arr_time}{Actual departure and arrival times, UTC.} 
#'   \item{sched_dep_time, sched_arr_time}{Scheduled departure and arrival 
#'     times, UTC.}
#'   \item{dep_delay, arr_delay}{Departure and arrival delays, in
#'     minutes. Negative times represent early departures/arrivals.}
#'   \item{hour, minute}{Time of scheduled departure broken into hour and
#'     minutes.} 
#'   \item{carrier}{Two letter carrier abbreviation. See
#'     \code{get_airlines} to get the full name.} 
#'   \item{tailnum}{Plane tail number.} 
#'   \item{flight}{Flight number.} 
#'   \item{origin, dest}{Origin and destination airport. 
#'     See \code{get_airports} for additional metadata.} 
#'   \item{air_time}{Amount of time spent in the air, in minutes.} 
#'   \item{distance}{Distance between airports, in miles.} 
#'   \item{time_hour}{Scheduled date and hour of the flight as a
#'     \code{POSIXct} date. Along with \code{origin}, can be used to join 
#'     flights data to weather data.} 
#' }
#' @source RITA, Bureau of transportation statistics,
#' \url{https://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=236}
#' @keywords datasets
NULL
