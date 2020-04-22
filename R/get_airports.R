#' @rdname anyflights
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
