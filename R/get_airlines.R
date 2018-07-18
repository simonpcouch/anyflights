#' Generate an airlines dataset
#' 
#' Please note that, even with a strong internet connection, this function 
#' may take several minutes to download relevant data.
#' 
#' @param dir A character string---the folder for the dataset to be saved in
#' @return A data frame with ~12 rows and 2 variables:
#' \describe{
#' \item{carrier}{Two letter abbreviation}
#' \item{name}{Full name}
#' }
#' @source \url{http://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=236}
#' @examples
#' \donttest{get_airlines(dir = tempdir())}
#' @seealso \code{\link{get_flights}} for flight data, \code{\link{get_weather}} 
#' for weather data, \code{\link{get_airports}} for airport data, and 
#' \code{\link{anyflights}} for a wrapper function  
#' @export

get_airlines <- function(dir) {
  
  if (!RCurl::url.exists("http://www.transtats.bts.gov/Download_Lookup.asp?Lookup=L_UNIQUE_CARRIERS")) {
    return("Can't access link for airlines data.")}

  if (!file.exists(paste0(dir, "/flights.rda"))) stop("`flights.rda` dataset not found. Please run
                                                      get_flights() with the same `dir` argument first.")
  
      airlines_cols <- readr::cols(
      Code = readr::col_character(),
      Description = readr::col_character()
    )
    
    airlines_raw <- readr::read_csv("http://www.transtats.bts.gov/Download_Lookup.asp?Lookup=L_UNIQUE_CARRIERS",
                                    col_types = airlines_cols)
    
    load(paste0(dir, "/flights.rda"))
    
    airlines <- airlines_raw %>%
      dplyr::select(carrier = Code, name = Description) %>%
      dplyr::semi_join(flights, by = "carrier") %>%
      dplyr::arrange(carrier)
    
    airlines_filepath <- paste0(dir, "/airlines.rda")
    save(airlines, file = airlines_filepath, compress = "bzip2")
    
  
}