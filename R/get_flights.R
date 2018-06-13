get_flights <- function(station, year, dir) {
  
  # Download Flights Data --------------------
  
  flight_url <- function(year, month) {
    base_url <- "http://www.transtats.bts.gov/PREZIP/"
    paste0(base_url, "On_Time_On_Time_Performance_", year, "_", month, ".zip")
  }
  
  flight_exdir <- paste0(dir, "/flights")
  
  download_month <- function(year = year, month) {
    fl_url <- flight_url(year, month)
    if (RCurl::url.exists(fl_url)) {
      flight_temp <- tempfile(fileext = ".zip")
      utils::download.file(fl_url, flight_temp)
    } else stop(sprintf("Can't access flight data for supplied year. Check date of 'Latest Available Data' for 'Airline On-Time Performance Data' on \n https://www.transtats.bts.gov/releaseinfo.asp"))
    
    flight_files <- utils::unzip(flight_temp, list = TRUE)
    # Only extract biggest file
    flight_csv <- flight_files$Name[order(flight_files$Length, decreasing = TRUE)[1]]
    utils::unzip(flight_temp, exdir = flight_exdir, junkpaths = TRUE, files = flight_csv)
    
    flight_src <- paste0(dir, "/flights/", flight_csv)
    flight_dst <- paste0(dir, "/flights/", year, "-", month, ".csv")
    file.rename(flight_src, flight_dst)
  }
  
  months <- 1:12
  lapply(months, download_month, year = year)
  
  get_flight_data <- function(path) {
    col_types <- readr::cols(
      DepTime = readr::col_integer(),
      ArrTime = readr::col_integer(),
      CRSDepTime = readr::col_integer(),
      CRSArrTime = readr::col_integer(),
      Carrier = readr::col_character(),
      UniqueCarrier = readr::col_character()
    )
    
    suppressWarnings(readr::read_csv(path, col_types = col_types)) %>%
      dplyr::select(
        year = Year, month = Month, day = DayofMonth,
        dep_time = DepTime, sched_dep_time = CRSDepTime, dep_delay = DepDelay,
        arr_time = ArrTime, sched_arr_time = CRSArrTime, arr_delay = ArrDelay,
        carrier = Carrier,  flight = FlightNum, tailnum = TailNum,
        origin = Origin, dest = Dest,
        air_time = AirTime, distance = Distance
      ) %>%
      dplyr::filter(origin %in% station) %>%
      dplyr::mutate(
        hour = sched_dep_time %/% 100,
        minute = sched_dep_time %% 100,
        time_hour = lubridate::make_datetime(year, month, day, hour, 0, 0)
      ) %>%
      dplyr::arrange(year, month, day, dep_time)
  }
  
  all <- lapply(dir(flight_exdir, full.names = TRUE), get_flight_data)
  flights <- dplyr::bind_rows(all)
  flights$tailnum[flights$tailnum == ""] <- NA
  flight_file_path <- paste0(dir, "/flights.rda")
  save(flights, file = flight_file_path, compress = "bzip2")
  unlink(x = flight_exdir, recursive = TRUE)
  
}