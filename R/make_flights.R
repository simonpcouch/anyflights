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
      if (RCurl::url.exists(fl_url)) {
        flight_temp <- tempfile(fileext = ".zip")
        utils::download.file(fl_url, flight_temp)
      } else stop(sprintf("Can't access flight data for supplied year. Check date of 'Latest Available Data' for 'Airline On-Time Performance Data' on \n https://www.transtats.bts.gov/releaseinfo.asp"))
      
      flight_files <- utils::unzip(flight_temp, list = TRUE)
      # Only extract biggest file
      flight_csv <- flight_files$Name[order(flight_files$Length, decreasing = TRUE)[1]]
      utils::unzip(flight_temp, exdir = flight_exdir, junkpaths = TRUE, files = flight_csv)
      
      flight_src <- paste0(subdir, "/flights/", flight_csv)
      flight_dst <- paste0(subdir, "/flights/", year, "-", month, ".csv")
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
    flights <- bind_rows(all)
    flights$tailnum[flights$tailnum == ""] <- NA
    flight_file_path <- paste0(subdir, "/flights.rda")
    save(flights, file = flight_file_path, compress = "bzip2")
    unlink(x = flight_exdir, recursive = TRUE)
    
  # Download Weather Data --------------------
    
    weather_url <- "http://mesonet.agron.iastate.edu/cgi-bin/request/asos.py?"
    if(!(RCurl::url.exists(weather_url))) stop("Can't access `weather` querying link")
    weather_query <- list(
      station = station, data = "all",
      year1 = as.character(year), month1 = "1", day1 = "1",
      year2 = as.character(year), month2 = "12", day2 = "31", tz = "GMT",
      format = "comma", latlon = "no", direct = "yes")
    
    weather_subdir <- paste(subdir, "/weather")
    dir.create(weather_subdir, showWarnings = FALSE, recursive = FALSE)
    
    r <- httr::GET(weather_url, query = weather_query, write_disk(paste0("./", weather_subdir, "/", station, ".csv"), overwrite = TRUE))
    httr::stop_for_status(r, "Can't access `weather` link for requested location and date range. \n Check data availability at `https://mesonet.agron.iastate.edu`")
    
    weather_paths <- dir(weather_subdir, full.names = TRUE)
    weather_col_types <- readr::cols(
      .default = readr::col_double(),
      station = readr::col_character(),
      valid = col_datetime(format = ""),
      skyc1 = readr::col_character(),
      skyc2 = readr::col_character(),
      skyc3 = readr::col_character(),
      skyc4 = readr::col_character(),
      wxcodes = readr::col_character(),
      metar = readr::col_character()
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
      dplyr::rename(time = ~valid) %>%
      dplyr::select(
        ~origin, ~time, temp = ~tmpf, dewp = ~dwpf, humid = ~relh,
        wind_dir = ~drct, wind_speed = ~sknt, wind_gust = ~gust,
        precip = ~p01i, pressure = ~mslp, visib = ~vsby
      ) %>%
      dplyr::mutate(
        time = ~as.POSIXct(strptime(time, "%Y-%m-%d %H:%M")),
        wind_speed = ~as.numeric(wind_speed) * 1.15078, # convert to mpg
        wind_gust = ~as.numeric(wind_speed) * 1.15078,
        year = year,
        month = ~lubridate::month(time),
        day = ~lubridate::mday(time),
        hour = ~lubridate::hour(time)) %>%
      dplyr::group_by(~origin, ~month, ~day, ~hour) %>%
      dplyr::filter(~ row_number() == 1) %>%
      dplyr::select(~origin, ~year:hour, ~temp:visib) %>%
      dplyr::ungroup() %>%
      dplyr::filter(~!is.na(month)) %>%
      dplyr::mutate(
        time_hour = ~ISOdatetime(year, month, day, hour, 0, 0))
    
    file_path <- paste0(subdir, "/weather.rda")
    
    save(weather, file = file_path, compress = "xz")
    
  # Download Planes Data ---------------------
    
    # Update URL from
    # http://www.faa.gov/licenses_certificates/aircraft_certification/aircraft_registry/releasable_aircraft_download/
    planes_src <- paste0("http://registry.faa.gov/database/yearly/ReleasableAircraft.", year, ".zip")
    planes_lcl <- paste0(subdir, "/planes")
    
    planes_tmp <- tempfile(fileext = ".zip")
    utils::download.file(planes_src, planes_tmp) 
    
    dir.create(planes_lcl)
    utils::unzip(planes_tmp, exdir = planes_lcl, junkpaths = TRUE)
    
    planes_master <- read.csv(paste0(planes_lcl, "/MASTER.txt"), stringsAsFactors = FALSE, strip.white = TRUE)
    names(planes_master) <- tolower(names(planes_master))
    
    keep <- planes_master %>%
      dplyr::tbl_df() %>%
      dplyr::select(nnum = n.number, code = aircraftmfrmodelcode, year = year.mfr)
    
    planes_col_names <- c("mfr", "model", "type.acft", "type.eng", "ac", 
                          "amat", "no.eng", "no.seats", "speed", "na3", "code", "na1", "na2")
    
    
    planes_ref <- read.csv(paste0(planes_lcl, "/AcftRef.txt"), stringsAsFactors = FALSE,
                           strip.white = TRUE, header = FALSE, col.names = planes_col_names)
    
    planes_ref <- planes_ref %>%
      dplyr::tbl_df() %>%
      dplyr::select(code, mfr, model, type.acft, type.eng, no.eng, no.seats, speed)
    
    planes_all <- keep %>%
      dplyr::inner_join(planes_ref, by = "code") %>%
      dplyr::select(-code)
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
      dplyr::select(
        tailnum, year, type, manufacturer = mfr, model = model,
        engines = no.eng, seats = no.seats, speed, engine
      ) %>%
      dplyr::semi_join(flights, "tailnum") %>%
      dplyr::arrange(tailnum)
    
    planes_filepath <- paste0(subdir, "/planes.rda")
    save(planes, file = planes_filepath, compress = "xz")
    unlink(x = planes_lcl, recursive = TRUE)
    
  # Download Airports Data -------------------
    
    if (RCurl::url.exists("https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat")) {
      
      airports_subdir <- paste0(subdir, "/airports")
      dir.create(airports_subdir)
      
      airports_data_path <- paste0(airports_subdir, "/airports.dat")
      utils::download.file(
        "https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat",
        airports_data_path)
      
      airports_cols <- readr::cols(
        id = readr::col_integer(),
        name = readr::col_character(),
        city = readr::col_character(),
        country = readr::col_character(),
        faa = readr::col_character(),
        icao = readr::col_character(),
        lat = readr::col_double(),
        lon = readr::col_double(),
        alt = readr::col_integer(),
        tz = readr::col_double(),
        dst = readr::col_character(),
        tzone = readr::col_character(),
        type = readr::col_character(),
        source = readr::col_character()
      )
      
      airports_raw <- read_delim(airports_data_path, ",",
                                 col_names = c("id", "name", "city", "country", "faa", "icao", "lat", "lon", "alt", "tz", "dst", "tzone", "type", "source"),
                                 escape_double = FALSE, na = c("", "NA", "\\N"), 
                                 col_types = airports_cols)
      
      
      airports <- airports_raw %>%
        dplyr::filter(country == "United States", faa != "") %>%
        dplyr::select(faa, name, lat, lon, alt, tz, dst, tzone) %>%
        dplyr::group_by(faa) %>% dplyr::slice(1) %>% dplyr::ungroup()
      
      
      save(airports, file = paste0(subdir, "/airports.rda"), compress = "xz")
      unlink(x = airports_subdir, recursive = TRUE)
      
    } else {return("Can't access airports data.")}
    
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
      
      airlines_filepath <- paste0(subdir, "/airlines.rda")
      save(airlines, file = airlines_filepath, compress = "bzip2")
      
    } else {return("Can't access link for airlines data.")}
    
}
