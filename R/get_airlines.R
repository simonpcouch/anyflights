#' Query nycflights13-Like Airlines Data
#' 
#' This function generates a dataframe similar to the 
#' \code{\link[nycflights13]{airlines}} dataset from \code{nycflights13} 
#' for any US airports and time frame. Please 
#' note that, even with a strong internet connection, this function 
#' may take several minutes to download relevant data.
#' 
#' @inheritParams anyflights 
#' 
#' @param flights_data Optional---either a filepath as a 
#' character string or a dataframe outputted by \code{\link{get_flights}} that
#' will be used to subset the output to only include relevant carriers/planes.
#' If not supplied, all carriers/planes will be returned.
#' 
#' @return A data frame with <2k rows and 2 variables:
#' \describe{
#' \item{carrier}{Two letter abbreviation}
#' \item{name}{Full name}
#' }
#' @source \url{http://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=236}
#' 
#' @examples
#' 
#' # run with defaults
#' \donttest{\dontrun{get_airlines()}}
#' 
#' # if you'd like to only return the airline 
#' # abbreviations only for airlines that appear in 
#' # \code{flights}, query your flights dataset first, 
#' # and then supply it as a flights_data argument
#' \donttest{\dontrun{get_airlines(flights_data = get_flights("PDX", 2018, 6))}}
#'
#' @seealso \code{\link{get_flights}} for flight data,
#' \code{\link{get_weather}} for weather data, 
#' \code{\link{get_airports}} for airports data,
#' \code{\link{get_planes}} for planes data,
#' or \code{\link{anyflights}} for a wrapper function.
#'
#' Use the \code{\link{as_flights_package}} function to convert this dataset 
#' to a data-only package.
#'
#' @export
get_airlines <- function(dir = NULL, flights_data = NULL) {
  
  # check input arguments!
  check_arguments(dir = dir)
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
    
