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
    
    weather_url <- "http://mesonet.agron.iastate.edu/cgi-bin/request/asos.py?"
    if(!(url.exists(weather_url))) stop("Can't access `weather` querying link")
    weather_query <- list(
      station = station, data = "all",
      year1 = as.character(year), month1 = "1", day1 = "1",
      year2 = as.character(year), month2 = "12", day2 = "31", tz = "GMT",
      format = "comma", latlon = "no", direct = "yes")
    
    weather_subdir <- paste(subdir, "/weather")
    dir.create(weather_subdir, showWarnings = FALSE, recursive = FALSE)
    
    r <- GET(weather_url, query = weather_query, write_disk(paste0("./", weather_subdir, "/", station, ".csv"), overwrite = TRUE))
    stop_for_status(r, "Can't access `weather` link for requested location and date range. \n Check data availability at `https://mesonet.agron.iastate.edu`")
    
    weather_paths <- dir(weather_subdir, full.names = TRUE)
    weather_col_types <- cols(
      .default = col_double(),
      station = col_character(),
      valid = col_datetime(format = ""),
      skyc1 = col_character(),
      skyc2 = col_character(),
      skyc3 = col_character(),
      skyc4 = col_character(),
      wxcodes = col_character(),
      metar = col_character()
    )
    
    weather_all <- lapply(weather_paths, read_csv, comment = "#", na = "M",
                          col_names = TRUE, col_types = weather_col_types)
    weather_raw <- bind_rows(weather_all)
    names(weather_raw) <- c("origin", "valid", "tmpf", "dwpf", "relh", "drct", "sknt",
                            "p01i", "alti", "mslp", "vsby", "gust",
                            "skyc1", "skyc2", "skyc3", "skyc4",
                            "skyl1", "skyl2", "skyl3", "skyl4", "wxcodes", "metar")
    
    weather <-
      weather_raw %>%
      rename_(time = ~valid) %>%
      select_(
        ~origin, ~time, temp = ~tmpf, dewp = ~dwpf, humid = ~relh,
        wind_dir = ~drct, wind_speed = ~sknt, wind_gust = ~gust,
        precip = ~p01i, pressure = ~mslp, visib = ~vsby
      ) %>%
      mutate_(
        time = ~as.POSIXct(strptime(time, "%Y-%m-%d %H:%M")),
        wind_speed = ~as.numeric(wind_speed) * 1.15078, # convert to mpg
        wind_gust = ~as.numeric(wind_speed) * 1.15078,
        year = year,
        month = ~lubridate::month(time),
        day = ~lubridate::mday(time),
        hour = ~lubridate::hour(time)) %>%
      group_by_(~origin, ~month, ~day, ~hour) %>%
      filter_(~ row_number() == 1) %>%
      select_(~origin, ~year:hour, ~temp:visib) %>%
      ungroup() %>%
      filter_(~!is.na(month)) %>%
      mutate_(
        time_hour = ~ISOdatetime(year, month, day, hour, 0, 0))
    
    file_path <- paste0(subdir, "/weather.rda")
    
    save(weather, file = file_path, compress = "xz")
    
  # Download Planes Data ---------------------
  # Download Airports Data -------------------
  # Download Airlines Data -------------------
}
