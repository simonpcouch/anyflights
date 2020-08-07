#' Query nycflights13-Like Airports Data
#' 
#' This function generates a dataframe similar to the 
#' \code{\link[nycflights13]{airports}} dataset from \code{nycflights13} 
#' for any US airports and time frame. Please 
#' note that, even with a strong internet connection, this function 
#' may take several minutes to download relevant data.
#' 
#' @inheritParams anyflights 
#' 
#' @return A data frame with ~1350 rows and 8 variables:
#' \describe{
#'  \item{faa}{FAA airport code}
#'  \item{name}{Usual name of the airport}
#'  \item{lat, lon}{Location of airport}
#'  \item{alt}{Altitude, in feet}
#'  \item{tz}{Timezone offset from GMT/UTC}
#'  \item{dst}{Daylight savings time zone. A = Standard US DST: starts on the
#'     second Sunday of March, ends on the first Sunday of November.
#'     U = unknown. N = no dst.}
#'  \item{tzone}{IANA time zone, as determined by GeoNames webservice}
#' }
#' @source \url{https://openflights.org/data.html}
#' 
#' @examples
#' 
#' # grab airports data
#' \donttest{\dontrun{get_airports()}}
#'
#' @seealso \code{\link{get_flights}} for flight data,
#' \code{\link{get_weather}} for weather data, 
#' \code{\link{get_airlines}} for airlines data,
#' \code{\link{get_planes}} for planes data,
#' or \code{\link{anyflights}} for a wrapper function.
#'
#' Use the \code{\link{as_flights_package}} function to convert this dataset 
#' to a data-only package.
#'
#' @export
get_airports <- function(dir = NULL) {
  
  # check the arguments
  check_arguments(dir = dir)
  
  # store the airports data url
  airports_url <- paste0("https://raw.githubusercontent.com/jpatokal",
                         "/openflights/master/data/airports.dat")
  
  # check if the url exists
  if (!url_exists(airports_url)) {
    return("Can't access airports data.")
  }
    
  # read in the data!
  airports_raw <- readr::read_delim(airports_url, ",",
                                    col_names = c("id", "name", "city", 
                                                    "country", "faa", "icao", 
                                                    "lat", "lon", "alt", "tz", 
                                                    "dst", "tzone", "type", 
                                                    "source"),
                                    escape_double = FALSE, na = c("", "NA", 
                                                                    "\\N"), 
                                    col_types = airports_cols)
    
  # tidy up the data a bit
  airports <- airports_raw %>%
    dplyr::filter(country == "United States", faa != "") %>%
    dplyr::select(faa, name, lat, lon, alt, tz, dst, tzone) %>%
    dplyr::group_by(faa) %>% 
    dplyr::slice(1) %>% 
    dplyr::ungroup()
    
  # save the data to file if a directory is supplied
  if (!is.null(dir)) {
    save(airports, file = paste0(dir, "/airports.rda"), compress = "xz")
  }

  # and return the data :-)
  return(airports)
  
}
