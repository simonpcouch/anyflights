#' Generate an airlines dataset
#' 
#' @param dir The folder for the dataset to be saved in
#' @return A data frame with ~12 rows and 2 variables:
#' \describe{
#' \item{carrier}{Two letter abbreviation}
#' \item{name}{Full name}
#' }
#' @source http://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=236
#' @examples
#' get_airlines(dir = tempdir())
#' @seealso \code{\link{get_flights}} for flight data, \code{\link{get_planes}} for plane
#' data, \code{\link{get_weather}} for weather data, \code{\link{get_airports}} for airport
#' data, and \code{\link{make_flights}} for a wrapper function  
#' @export

get_airlines <- function(dir) {
  
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
    
    airlines_filepath <- paste0(dir, "/airlines.rda")
    save(airlines, file = airlines_filepath, compress = "bzip2")
    
  } else {return("Can't access link for airlines data.")}
  
}