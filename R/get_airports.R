get_airports <- function(dir) {
  
  # Download Airports Data -------------------
  
  if (RCurl::url.exists("https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat")) {
    
    airports_dir <- paste0(dir, "/airports")
    dir.create(airports_dir)
    
    airports_data_path <- paste0(airports_dir, "/airports.dat")
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
                                      col_names = c("id", "name", "city", "country", "faa", "icao", "lat", "lon", "alt", "tz", "dst", "tzone", "type", "source"),
                                      escape_double = FALSE, na = c("", "NA", "\\N"), 
                                      col_types = airports_cols)
    
    
    airports <- airports_raw %>%
      dplyr::filter(country == "United States", faa != "") %>%
      dplyr::select(faa, name, lat, lon, alt, tz, dst, tzone) %>%
      dplyr::group_by(faa) %>% dplyr::slice(1) %>% dplyr::ungroup()
    
    
    save(airports, file = paste0(dir, "/airports.rda"), compress = "xz")
    unlink(x = airports_dir, recursive = TRUE)
    
  } else {return("Can't access airports data.")}
}