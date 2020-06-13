#' Airport metadata
#'
#' Useful metadata about airports.
#'
#' @name airports
#' @docType data
#' @format A data frame with columns: 
#' \describe{ 
#'   \item{faa}{FAA airport code.}
#'   \item{name}{Usual name of the airport.} 
#'   \item{lat, lon}{Location of airport.}
#'   \item{alt}{Altitude, in feet.} 
#'   \item{tz}{Timezone offset from GMT/UTC.}
#'   \item{dst}{Daylight savings time zone. A = Standard US DST: starts on the
#'     second Sunday of March, ends on the first Sunday of November.  
#'     U = unknown. N = no dst.} 
#'   \item{tzone}{IANA time zone, as determined by GeoNames webservice.} 
#' }
#' @source \url{http://openflights.org/data.html}
#' @keywords datasets
NULL
