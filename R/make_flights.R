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
    
    # Update URL from
    # http://www.faa.gov/licenses_certificates/aircraft_certification/aircraft_registry/releasable_aircraft_download/
    planes_src <- paste0("http://registry.faa.gov/database/yearly/ReleasableAircraft.", year, ".zip")
    planes_lcl <- paste0(subdir, "/planes")
    
    planes_tmp <- tempfile(fileext = ".zip")
    download.file(planes_src, planes_tmp) 
    
    dir.create(planes_lcl)
    unzip(planes_tmp, exdir = planes_lcl, junkpaths = TRUE)
    
    planes_master <- read.csv(paste0(planes_lcl, "/MASTER.txt"), stringsAsFactors = FALSE, strip.white = TRUE)
    names(planes_master) <- tolower(names(planes_master))
    
    keep <- planes_master %>%
      tbl_df() %>%
      select(nnum = n.number, code = aircraftmfrmodelcode, year = year.mfr)
    
    planes_col_names <- c("mfr", "model", "type.acft", "type.eng", "ac", 
                          "amat", "no.eng", "no.seats", "speed", "na3", "code", "na1", "na2")
    
    
    planes_ref <- read.csv(paste0(planes_lcl, "/AcftRef.txt"), stringsAsFactors = FALSE,
                           strip.white = TRUE, header = FALSE, col.names = planes_col_names)
    
    planes_ref <- planes_ref %>%
      tbl_df() %>%
      select(code, mfr, model, type.acft, type.eng, no.eng, no.seats, speed)
    
    planes_all <- keep %>%
      inner_join(planes_ref, by = "code") %>%
      select(-code)
    planes_all$speed[planes_all$speed == 0] <- NA
    planes_all$no.eng[planes_all$no.eng == 0] <- NA
    planes_all$no.seats[planes_all$no.seats == 0] <- NA
    
    engine <- c("None", "Reciprocating", "Turbo-prop", "Turbo-shaft", "Turbo-jet",
                "Turbo-fan", "Ramjet", "2 Cycle", "4 Cycle", "Unknown", "Electric", "Rotary")
    planes_all$engine <- engine[planes_all$type.eng + 1]
    planes_all$type.eng <- NULL
    
    acft <- c("Glider", "Balloon", "Blimp/Dirigible", "Fixed wing single engine",
              "Fixed wing multi engine", "Rotorcraft", "Weight-shift-control",
              "Powered Parachute", "Gyroplane")
    planes_all$type <- acft[planes_all$type.acft]
    planes_all$type.acft <- NULL
    
    planes_all$tailnum <- paste0("N", planes_all$nnum)
    
    flights_ds <- paste0(subdir, "/flights.rda")
    load(flights_ds)
    
    planes <- planes_all %>%
      select(
        tailnum, year, type, manufacturer = mfr, model = model,
        engines = no.eng, seats = no.seats, speed, engine
      ) %>%
      semi_join(flights, "tailnum") %>%
      arrange(tailnum)
    
    planes_filepath <- paste0(subdir, "/planes.rda")
    save(planes, file = planes_filepath, compress = "xz")
    unlink(x = planes_lcl, recursive = TRUE)
    
  # Download Airports Data -------------------
    
    if (url.exists("https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat")) {
      
      airports_subdir <- paste0(subdir, "/airports")
      dir.create(airports_subdir)
      
      airports_data_path <- paste0(airports_subdir, "/airports.dat")
      download.file(
        "https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat",
        airports_data_path)
      
      airports_cols <- cols(
        id = col_integer(),
        name = col_character(),
        city = col_character(),
        country = col_character(),
        faa = col_character(),
        icao = col_character(),
        lat = col_double(),
        lon = col_double(),
        alt = col_integer(),
        tz = col_double(),
        dst = col_character(),
        tzone = col_character(),
        type = col_character(),
        source = col_character()
      )
      
      airports_raw <- read_delim(airports_data_path, ",",
                                 col_names = c("id", "name", "city", "country", "faa", "icao", "lat", "lon", "alt", "tz", "dst", "tzone", "type", "source"),
                                 escape_double = FALSE, na = c("", "NA", "\\N"), 
                                 col_types = airports_cols)
      
      
      airports <- airports_raw %>%
        filter(country == "United States", faa != "") %>%
        select(faa, name, lat, lon, alt, tz, dst, tzone) %>%
        group_by(faa) %>% slice(1) %>% ungroup()
      
      
      save(airports, file = paste0(subdir, "/airports.rda"), compress = "xz")
      unlink(x = airports_subdir, recursive = TRUE)
      
    } else { return("Can't access airports data.") }
    
  # Download Airlines Data -------------------
    
    if (url.exists("http://www.transtats.bts.gov/Download_Lookup.asp?Lookup=L_UNIQUE_CARRIERS")) {
      airlines_raw <- read_csv("http://www.transtats.bts.gov/Download_Lookup.asp?Lookup=L_UNIQUE_CARRIERS")
      
      airlines <- airlines_raw %>%
        select(carrier = Code, name = Description) %>%
        semi_join(flights) %>%
        arrange(carrier)
      
      airlines_filepath <- paste0(subdir, "/airlines.rda")
      save(airlines, file = "data/airlines.rda", compress = "bzip2")
      
    } else {return("Can't access link for airlines data.")}
    
}
