#' Package: anyflights
#'
#' Query nycflights13-Like Air Travel Data for Specified Years and Airports
#'
#' The anyflights package supplies a set of functions to query air travel
#' information for specified years and airports. The output of the wrapper
#' function, \code{anyflights()}, is a list of dataframe similar to those
#' found in the data-only package nycflights13.
#'
#' @docType package
#' @name anyflights_description
#' @aliases anyflights_package
#' @importFrom dplyr %>%
"_PACKAGE"

utils::globalVariables(c(".", "AirTime", "ArrDelay", "ArrTime", "CRSArrTime", 
                         "CRSDepTime", "Carrier", "Code", "DayofMonth", 
                         "DepDelay", "DepTime", "Description", "Dest", 
                         "Distance", "FlightNum", "Month", "Origin", "TailNum", 
                         "Year", "aircraftmfrmdlcode", "alt", "carrier", 
                         "code", "col_character", "col_integer", "cols", 
                         "country", "day", "dep_time", "drct", "dst", "dwpf", 
                         "faa", "flights", "gust", "hour", "lat", "lon", "mfr", 
                         "mfr.mdl.code", "model", "month", "mslp", "n.number", 
                         "name", "no.eng", "no.seats", "origin", "p01i", 
                         "read_csv", "relh", "row_number", "sched_dep_time", 
                         "sknt", "speed", "tailnum", "temp", "tmpf", "type", 
                         "type.acft", "type.eng", "tz", "tzone", "valid", 
                         "visib", "vsby", "wind_speed", "year.mfr", 
                         "Flight_Number_Reporting_Airline", 
                         "Reporting_Airline", "Tail_Number", "time",
                         "everything", "feel", "flight", "metar", "nnum", 
                         "no_eng", "no_seats", "planes_lcl", "station", 
                         "type_acft", "type_eng", "year", "Reporting_Airline",
                         "arr_time", "sched_arr_time"))