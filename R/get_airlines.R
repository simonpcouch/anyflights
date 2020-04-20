#' @rdname anyflights
#' @export
get_airlines <- function(dir = NULL, flights_data = NULL) {
  
  # check if the flights_data argument references/is a dataset
  flights_data <- parse_flights_data_arg(flights_data)
  
  # base url for the airlines dataset
  airlines_url <- paste0("http://www.transtats.bts.gov/Download_Lookup.asp?",
                         "Lookup=L_UNIQUE_CARRIERS")
  
  # check if the url is active
  if(!url_exists(airlines_url)) {
    return("Can't access link for airlines data.")
  }
    
  # read in both columns as characters
  airlines_cols <- readr::cols(
    Code = readr::col_character(),
    Description = readr::col_character()
  )
  
  # grab the data, rename columns, and arrange by carrier
  airlines <- readr::read_csv(airlines_url, 
                              col_types = airlines_cols) %>%
    dplyr::select(carrier = Code, name = Description) %>%
    dplyr::arrange(carrier)
  
  # if the directory argument isn't null, check for a flights.rda
  # file inside the directory. if there is one there, load it in and
  # semi_join on carrier. otherwise, just return the whole dataset.
  if (!is.null(flights_data)) {
    # filter out inactive airlines
    airlines <- airlines %>% 
      dplyr::semi_join(flights_data, by = "carrier")
  }
  
  if (!is.null(dir)) {
    # save the airlines data in the dir
    save(airlines, 
         file = paste0(dir, "/airlines.rda"), 
         compress = "bzip2")
  }
  
  return(airlines)
  
}
    
