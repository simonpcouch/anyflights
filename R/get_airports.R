#' Generate an airports dataset
#' 
#' Please note that, even with a strong internet connection, this function 
#' may take several minutes to download relevant data.
#' 
#' @param dir A character string---the folder for the dataset to be saved in
#' @return A data frame with ~1350 rows and 8 variables:
#' \describe{
#'  \item{faa}{FAA airport code}
#'  \item{name}{Usual name of the airport}
#'  \item{lat,lon}{Location of airport}
#'  \item{alt}{Altitude, in feet}
#'  \item{tz}{Timezone offset from GMT}
#'  \item{dst}{Daylight savings time zone. A = Standard US DST: starts on the
#'     second Sunday of March, ends on the first Sunday of November.
#'     U = unknown. N = no dst.}
#'  \item{tzone}{IANA time zone, as determined by GeoNames webservice}
#' }
#' @source \url{http://openflights.org/data.html}
#' @examples
#' \donttest{get_airports(dir = tempdir())}
#' @seealso \code{\link{get_flights}} for flight data, \code{\link{get_weather}}
#' for weather data, \code{\link{get_airlines}} for airline
#' data, and \code{\link{anyflights}} for a wrapper function  
#' @export
get_airports <- function(dir = NULL) {
  
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
    if (!dir.exists(dir)) {dir.create(dir)}
    save(airports, file = paste0(dir, "/airports.rda"), compress = "xz")
  }

  # and return the data :-)
  return(airports)
  
}
