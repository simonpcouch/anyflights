get_airlines <- function(subdir) {
  
  # Download Airlines Data -------------------
  
  if (RCurl::url.exists("http://www.transtats.bts.gov/Download_Lookup.asp?Lookup=L_UNIQUE_CARRIERS")) {
    
    airlines_cols <- readr::cols(
      Code = readr::col_character(),
      Description = readr::col_character()
    )
    
    airlines_raw <- readr::read_csv("http://www.transtats.bts.gov/Download_Lookup.asp?Lookup=L_UNIQUE_CARRIERS",
                                    col_types = airlines_cols)
    
    airlines <- airlines_raw %>%
      dplyr::select(carrier = Code, name = Description) %>%
      dplyr::semi_join(flights, by = "carrier") %>%
      dplyr::arrange(carrier)
    
    airlines_filepath <- paste0(subdir, "/airlines.rda")
    save(airlines, file = airlines_filepath, compress = "bzip2")
    
  } else {return("Can't access link for airlines data.")}
  
}