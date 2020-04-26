\docType{data}
\name{flights}
\alias{flights}
\title{Flights data}
\format{Data frame with columns
\describe{
\item{\code{year, month, day}}{Date of departure}
\item{\code{dep_time, arr_time}}{Actual departure and arrival times, UTC.}
\item{\code{sched_dep_time, sched_arr_time}}{Scheduled departure and arrival 
  times, UTC.}
\item{\code{dep_delay, arr_delay}}{Departure and arrival delays, in minutes.
  Negative times represent early departures/arrivals.}
\item{\code{hour, minute}}{Time of scheduled departure broken into hour and 
  minutes.}
\item{\code{carrier}}{Two letter carrier abbreviation. See 
  \code{\link{get_airlines}} to get full name}
\item{\code{tailnum}}{Plane tail number}
\item{\code{flight}}{Flight number}
\item{\code{origin, dest}}{Origin and destination. See 
  \code{\link{get_airports}} for additional metadata.}
\item{\code{air_time}}{Amount of time spent in the air, in minutes}
\item{\code{distance}}{Distance between airports, in miles}
\item{\code{time_hour}}{Scheduled date and hour of the flight as a 
  \code{POSIXct} date. Along with \code{origin}, can be used to join 
  flights data to weather data.}
}}
\source{
RITA, Bureau of transportation statistics,
\url{https://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=236}
}
\description{
On-time data for all flights that departed from the given airports.
}
