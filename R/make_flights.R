make_flights <- function(station, year) {
  
  # Create Subdirectory ----------------------
  station_low <- tolower(station)
  year_substr <- substr(year[1], 3, 4)
  subdir <- paste0(station_low, "flights", year_substr)
  dir.create(subdir, showWarnings = FALSE)
  # Download Flights Data --------------------
  
    flight_url <- function(year, month) {
      base_url <- "http://www.transtats.bts.gov/PREZIP/"
      paste0(base_url, "On_Time_On_Time_Performance_", year, "_", month, ".zip")
    }
    
    flight_exdir <- paste0(subdir, "/flights")
  
    download_month <- function(year = year, month) {
      fl_url <- flight_url(year, month)
      if (url.exists(fl_url)) {
        flight_temp <- tempfile(fileext = ".zip")
        download.file(fl_url, flight_temp)
      } else stop(sprintf("Can't access flight data for supplied year. Check date of 'Latest Available Data' for 'Airline On-Time Performance Data' on \n https://www.transtats.bts.gov/releaseinfo.asp"))
      
      flight_files <- unzip(flight_temp, list = TRUE)
      # Only extract biggest file
      flight_csv <- flight_files$Name[order(flight_files$Length, decreasing = TRUE)[1]]
      unzip(flight_temp, exdir = flight_exdir, junkpaths = TRUE, files = flight_csv)
      
      flight_src <- paste0(subdir, "/flights/", flight_csv)
      flight_dst <- paste0(subdir, "/flights/", year, "-", month, ".csv")
      file.rename(flight_src, flight_dst)
    }
    
    months <- 1:12
    lapply(months, download_month, year = year)
    
    get_flight_data <- function(path) {
      col_types <- cols(
        DepTime = col_integer(),
        ArrTime = col_integer(),
        CRSDepTime = col_integer(),
        CRSArrTime = col_integer(),
        Carrier = col_character(),
        UniqueCarrier = col_character()
      )
      suppressWarnings(read_csv(path, col_types = col_types)) %>%
        select(
          year = Year, month = Month, day = DayofMonth,
          dep_time = DepTime, sched_dep_time = CRSDepTime, dep_delay = DepDelay,
          arr_time = ArrTime, sched_arr_time = CRSArrTime, arr_delay = ArrDelay,
          carrier = Carrier,  flight = FlightNum, tailnum = TailNum,
          origin = Origin, dest = Dest,
          air_time = AirTime, distance = Distance
        ) %>%
        filter(origin %in% station) %>%
        mutate(
          hour = sched_dep_time %/% 100,
          minute = sched_dep_time %% 100,
          time_hour = lubridate::make_datetime(year, month, day, hour, 0, 0)
        ) %>%
        arrange(year, month, day, dep_time)
    }
    
    all <- lapply(dir(flight_exdir, full.names = TRUE), get_flight_data)
    flights <- bind_rows(all)
    flights$tailnum[flights$tailnum == ""] <- NA
    
    flight_file_path <- paste0(subdir, "/flights.rda")
    save(flights, file = flight_file_path, compress = "bzip2")
    unlink(x = flight_exdir, recursive = TRUE)
  # Download Weather Data --------------------
  # Download Planes Data ---------------------
  # Download Airports Data -------------------
  # Download Airlines Data -------------------
}
