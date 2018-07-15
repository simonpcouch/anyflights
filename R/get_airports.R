#' Generate an airports dataset
#' 
#' @param dir The folder for the dataset to be saved in
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
#' get_airports(dir = tempdir())
#' @seealso \code{\link{get_flights}} for flight data, \code{\link{get_weather}}
#' for weather data, \code{\link{get_airlines}} for airline
#' data, and \code{\link{anyflights}} for a wrapper function  
#' @export


get_airports <- function(dir) {
  
  # Download Airports Data -------------------
  
  if (!RCurl::url.exists("https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat")) {
    return("Can't access airports data.")}
    
    if (!dir.exists(dir)) {dir.create(dir)}
    
    airports_data_path <- paste0(dir, "/airports.dat")
    utils::download.file(
      "https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat",
      airports_data_path)
    
    airports_cols <- readr::cols(
      id = readr::col_integer(),
      name = readr::col_character(),
      city = readr::col_character(),
      country = readr::col_character(),
      faa = readr::col_character(),
      icao = readr::col_character(),
      lat = readr::col_double(),
      lon = readr::col_double(),
      alt = readr::col_integer(),
      tz = readr::col_double(),
      dst = readr::col_character(),
      tzone = readr::col_character(),
      type = readr::col_character(),
      source = readr::col_character()
    )
    
    airports_raw <- readr::read_delim(airports_data_path, ",",
                                      col_names = c("id", "name", "city", 
                                                    "country", "faa", "icao", 
                                                    "lat", "lon", "alt", "tz", 
                                                    "dst", "tzone", "type", 
                                                    "source"),
                                      escape_double = FALSE, na = c("", "NA", 
                                                                    "\\N"), 
                                      col_types = airports_cols)
    
    
    airports <- airports_raw %>%
      dplyr::filter(country == "United States", faa != "") %>%
      dplyr::select(faa, name, lat, lon, alt, tz, dst, tzone) %>%
      dplyr::group_by(faa) %>% dplyr::slice(1) %>% dplyr::ungroup()
    
    
    save(airports, file = paste0(dir, "/airports.rda"), compress = "xz")
    unlink(x = paste0(dir, "/airports.dat"))

}
