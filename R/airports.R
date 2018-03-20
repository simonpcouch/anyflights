#' Airport metadata
#'
#' Useful metadata about airports.
#'
#' @source \url{http://openflights.org/data.html}, downloaded 2018-03-15
#' @format A data frame with 1333 rows and 8 variables:
#' \describe{
#'  \item{faa}{FAA airport code}
#'  \item{name}{Usual name of the aiport}
#'  \item{lat,lon}{Location of airport}
#'  \item{alt}{Altitude, in feet}
#'  \item{tz}{Timezone offset from GMT}
#'  \item{dst}{Daylight savings time zone. A = Standard US DST: starts on the
#'     second Sunday of March, ends on the first Sunday of November.
#'     U = unknown. N = no dst.}
#'  \item{tzone}{IANA time zone, as determined by GeoNames webservice}
#' }
#' @examples
#' if (require("dplyr")) {
#' airports
#'
#' airports %>% mutate(dest = faa) %>% semi_join(flights)
#' flights %>% anti_join(airports %>% mutate(dest = faa))
#' airports %>% mutate(origin = faa) %>% semi_join(flights)
#'
#' }
"airports"
