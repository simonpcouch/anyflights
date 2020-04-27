#' anyflights: `nycflights13`-Like Data for Specified Years and Airports
#'
#' @description 
#'
#' \if{html}{\figure{logo.png}{options: align='right'}}
#'
#' The anyflights package supplies a set of functions to generate
#' \code{nycflights13}-like datasets and data packages for specified years and 
#' airports.
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
                         "arr_time", "sched_arr_time", "dewp", "engine",
                         "engines", "humid", "manufacturer", "precip",
                         "pressure", "seats", "time_hour", "wind_dir",
                         "wind_gust"))

