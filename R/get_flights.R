#' @rdname anyflights
#' @export
get_flights <- function(station, year, month = 1:12, dir = NULL) {
  
  # create a temporary directory if need be
  if (is.null(dir)) {
    dir_is_null <- TRUE
    dir <- tempdir()
  } else {
    dir_is_null <- FALSE
  }
  
  # if the directory doesn't exist, make it!
  if (!dir.exists(dir)) {dir.create(dir)}
  
  # make a subdirectory inside the directory to download the raw data into
  flight_exdir <- paste0(dir, "/flights")
  
  # download flight data for the relevant time range
  purrr::map(month, download_month, 
             year = year, dir = dir, flight_exdir = flight_exdir)
  
  # load in the flights data for each month, tidy it, and rowbind it
  flights <- purrr::map(dir(flight_exdir, full.names = TRUE),
                        get_flight_data,
                        station = station) %>%
    dplyr::bind_rows() %>%
    dplyr::arrange(year, month, day, dep_time)
  
  # get rid of the "raw" data
  unlink(x = flight_exdir, recursive = TRUE)
    
  if (!dir_is_null) {
    # ...and save the flights data
    save(flights, 
         file = paste0(dir, "/flights.rda"), 
         compress = "bzip2")
  }
  
  return(flights)
}
