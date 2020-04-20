# general utilities --------------------------------------------------------

# a function derived from simonpcouch/gbfs to check if a URL exists
url_exists <- function(x, quiet = FALSE, ...) {
  
  capture_error <- function(code, otherwise = NULL, quiet = TRUE) {
    tryCatch(
      list(result = code, error = NULL),
      error = function(e) {
        list(result = otherwise, error = e)
      }
    )
  }
  
  safely <- function(.f, otherwise = NULL, quiet = TRUE) {
    function(...) capture_error(.f(...), otherwise, quiet)
  }
  
  sHEAD <- safely(httr::HEAD)
  sGET <- safely(httr::GET)
  
  if (!stringr::str_detect(x, "http")) {
    x <- paste0("https://", x)
  }
  
  res <- sHEAD(x, ...)
  
  if (is.null(res$result)) {
    
    res <- sGET(x, ...)
    
    if (is.null(res$result)) { 
      return(FALSE)
    }
  }
  
  return(TRUE)
  
}


# get_flights utilities --------------------------------------------------

download_month <- function(year, month, dir, flight_exdir) {
  
  # put together the url for the relevant year and month
  fl_url <- make_flights_url(year, month)
  
  # make a temporary file to download to
  flight_temp <- tempfile(fileext = ".zip")
  
  # download the file
  utils::download.file(fl_url, flight_temp)
  
  # ...and unzip it
  flight_files <- utils::unzip(flight_temp, list = TRUE)
  
  # only extract biggest file (its the one we want!)
  flight_csv <- flight_files$Name[order(flight_files$Length, 
                                        decreasing = TRUE)[1]]
  utils::unzip(flight_temp, exdir = flight_exdir, 
               junkpaths = TRUE, files = flight_csv)
  
  # rename the file so that it's easier to find elsewhere
  flight_src <- paste0("flights/", flight_csv)
  flight_dst <- paste0("flights/", year, "-", month, ".csv")
  file.rename(flight_src, flight_dst)
}

get_flight_data <- function(path) {
  
  # read in the data
  suppressMessages(vroom::vroom(path)) %>%
    # select relevant columns
    dplyr::select(
      year = Year, 
      month = Month, 
      day = DayofMonth,
      dep_time = DepTime, 
      sched_dep_time = CRSDepTime, 
      dep_delay = DepDelay,
      arr_time = ArrTime, 
      sched_arr_time = CRSArrTime, 
      arr_delay = ArrDelay,
      carrier = Reporting_Airline,  
      flight = Flight_Number_Reporting_Airline, 
      tailnum = Tail_Number, 
      origin = Origin, 
      dest = Dest, 
      air_time = AirTime, 
      distance = Distance) %>%
    # only keep the relevant rows
    dplyr::filter(origin %in% station) %>%
    dplyr::mutate(
      # convert column classes
      dep_time = as.numeric(dep_time),
      sched_dep_time = as.numeric(sched_dep_time),
      flight = as.factor(flight),
      # mutate some help time columns
      hour = sched_dep_time %/% 100,
      minute = sched_dep_time %% 100,
      time_hour = lubridate::make_datetime(year, month, day, hour, 0, 0),
      # cleanup NAs in the tailnum column
      tailnum = case_when(
        tailnum == "" ~ NA_character_,
        TRUE ~ tailnum)
      )
}

# given a year and month, this function returns the URL to query for the data
make_flights_url <- function(year, month) {
  base_url <- "https://transtats.bts.gov/PREZIP/"
  paste0(base_url, 
         "On_Time_Reporting_Carrier_On_Time_Performance_1987_present_", 
         year, "_", month, ".zip")
}