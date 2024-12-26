# general utilities --------------------------------------------------------

# a general argument checking function
check_arguments <- function(station = NULL, year = NULL, 
                            month = NULL, dir = NULL, context = NA) {
  
  # test internet connection
  if (!connected_to_internet()) {
    stop_no_internet()
  }
  
  # checking the "station" argument
  if (context %in% c("flights", "weather")) {
    if (!all(is.character(station))) {
      stop_glue("At least one of the provided station arguments, ",
                "{list(station)}, wasn't a character string. Have you ",
                "surrounded the airport names in quotes?")
    }    
    
    if (!all(station %in% get_airports()$faa)) {
      stop_glue("Couldn't find at least one of the provided origin airports ",
                "{list(station)}. Please consider using the get_airports() function ",
                "to locate the desired FAA LID code!")
    }
  }
  
  # checking the "year" argument
  if (context %in% c("flights", "planes", "weather")) {
    if (!is.numeric(year)) {
      stop_glue("The provided `year` argument has class {class(year)}, but ",
                "it needs to be a numeric. Have you put the year in quotes?")
    }
    if (year > as.numeric(substr(Sys.Date(), 1, 4))) {
      stop_glue("The provided `year` is in the future. Oops. :-)")
    }
  
    if (year == as.numeric(substr(Sys.Date(), 1, 4))) {
      stop_glue("The data for this year isn't quite available yet. The data ",
                "for the previous year usually is released in February ",
                "or March!")
      }
    
    if (year < 1987) {
      stop_glue("Your `year` argument {year} is really far back in time! ",
                "`anyflights` data sources do not provide data this old.")
    }
    if (year < 2013 & context == "planes") {
      warning_glue("Planes data was not formatted consistently before 2013. ",
                   "Please use caution.")
    } else if (context != "planes" & year < 2010) {
      message_glue("Queries before 2010 are untested by the package. ",
                   "Please use caution!")
    }
  }
  
  # checking the "month" argument
  if (context %in% c("flights", "weather")) {
    if (!is.numeric(month)) {
      stop_glue("The provided `month` argument has class {class(month)}, but ",
                "it needs to be a numeric. Have you put the months in quotes?")
    }
    if (any(month > 12 | month < 1)) {
      stop_glue("Please enter only month values within 1 to 12.")
    }
  }
  
  if (!is.null(dir)) {
    if (!dir.exists(dir)) {
      dir_ <- tryCatch(dir.create(dir), error = function(e) e)
      if (inherits(dir_, "error")) {
        stop_glue("anyflights had trouble making the folder specified by ",
                  "the directory argument {dir}. Here's the error: \n {dir_}")
      }
    }
  }
} 


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

# a function to alert the user of no internet connection in a
# more informative/helpful way
stop_no_internet <- function() {
  stop_glue("You don't seem to have an active internet connection. Please ", 
            "connect to the internet to use the anyflights package.")
  return(list())
}

# a wrapper around has internet so that with_mock can be used in tests
connected_to_internet <- function() {
  curl::has_internet()
}

stop_glue <- function(..., .sep = "", .envir = parent.frame(),
                      call. = FALSE, .domain = NULL) {
  stop(
    glue_null(..., .sep = .sep, .envir = .envir),
    call. = call., domain = .domain
  )
}

warning_glue <- function(..., .sep = "", .envir = parent.frame(),
                         call. = FALSE, .domain = NULL) {
  warning(
    glue_null(..., .sep = .sep, .envir = .envir),
    call. = call., domain = .domain
  )
}

message_glue <- function(..., .sep = "", .envir = parent.frame(),
                         .domain = NULL, .appendLF = TRUE) {
  message(
    glue_null(..., .sep = .sep, .envir = .envir),
    domain = .domain, appendLF = .appendLF
  )
}

# glue messages, warnings, and errors
glue_null <- function(..., .sep = "", .envir = parent.frame()) {
  glue::glue(
    ..., .sep = .sep, .envir = .envir, .transformer = null_transformer
  )
}

# actually print NULLs in output 
null_transformer <- function(text, envir) {
  out <- eval(parse(text = text, keep.source = FALSE), envir)
  if (is.null(out)) {
    return("NULL")
  }
  
  out
}

load_as <- function(filepath) {
  new_env <- new.env()
  data <- load(filepath, new_env)[1]
  new_env[[data]]
}

skip_conditions <- function() {
  internet <- curl::has_internet()
  on_mac <- unname(Sys.info()["sysname"]) != "Darwin"
  
  (!internet) | on_mac
}

download_file_wrapper <- function(url, file_path, quiet = TRUE){
  out <- tryCatch(
    utils::download.file(url, file_path, quiet = quiet), 
    error = function(e) {e}
  )
  
  if (inherits(out, "error")) {
    stop_glue(
      "\n\n\nutils::download.file timed out before finishing downloading the file. ", 
      "If you are repeatedly getting a timeout error, try extending the ",
      "timeout period for your R session using ",
      "options(timeout = timeout_value_in_seconds)\n\n\n")
  }
  
  out
}

# get_flights utilities --------------------------------------------------

download_month <- function(year, month, dir, flight_exdir, pb, diff_fn) {
  
  # update the progress bar with the month being downloaded
  write_tick(pb = pb, paste0("  Downloading Flights Data for ", 
                             months[month], 
                             "..."))
  
  # put together the url for the relevant year and month
  fl_url <- make_flights_url(year, month)
  
  # make a temporary file to download to
  flight_temp <- tempfile(fileext = ".zip")
  
  # download the file
  download_file_wrapper(fl_url, flight_temp, quiet = TRUE)
 
  # ...and unzip it
  flight_files <- utils::unzip(flight_temp, list = TRUE)
  
  # only extract biggest file (its the one we want!)
  flight_csv <- flight_files$Name[order(flight_files$Length, 
                                        decreasing = TRUE)[1]]
  utils::unzip(flight_temp, exdir = flight_exdir, 
               junkpaths = TRUE, files = flight_csv)
  
  # rename the file so that it's easier to find elsewhere
  flight_src <- paste0(flight_exdir, "/", flight_csv)
  flight_dst <- paste0(flight_exdir, "/", year, "-", month, ".csv")
  file.rename(flight_src, flight_dst)
  
  write_message(pb, 
                paste0("Downloaded Flights Data for ", 
                months[month]),
                diff_fn)
}

get_flight_data <- function(path, station) {
  
  # read in the data
  suppressWarnings(
    suppressMessages(vroom::vroom(path,
                     progress = FALSE,
                     show_col_types = FALSE))
    ) %>%
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
      dep_time = as.integer(dep_time),
      sched_dep_time = as.integer(sched_dep_time),
      flight = as.factor(flight),
      # mutate some help time columns
      hour = sched_dep_time %/% 100,
      minute = sched_dep_time %% 100,
      time_hour = lubridate::make_datetime(year, month, day, hour, 0, 0),
      # cleanup NAs in the tailnum column
      tailnum = dplyr::case_when(
        tailnum == "" ~ NA_character_,
        TRUE ~ tailnum),
      # convert column types to match the original data
      year = as.integer(year),
      month = as.integer(month),
      day = as.integer(day),
      arr_time = as.integer(arr_time),
      sched_arr_time = as.integer(sched_arr_time),
      flight = as.integer(flight)
      )
}

# given a year and month, this function returns the URL to query for the data
make_flights_url <- function(year, month) {
  base_url <- "https://transtats.bts.gov/PREZIP/"
  paste0(base_url, 
         "On_Time_Reporting_Carrier_On_Time_Performance_1987_present_", 
         year, "_", month, ".zip")
}


# get_airlines utilities ----------------------------------------------------
parse_flights_data_arg <- function(flights_data) {
  
  # if it's a character vector, check if it's a filepath
  if (is.character(flights_data)) {
    if (file.exists(flights_data)) {
      flights_data <- load_as(flights_data)
    }
  }
  
  # now, if flights_data is a dataframe...
  if (is.data.frame(flights_data)) {
    # and carrier is in the column names
    if ("carrier" %in% colnames(flights_data)) {
      # just return the dataframe
      return(flights_data)
    }
  }
  
  # otherwise, return null
  return(NULL)
  
}


# get_airports utilities -----------------------------------------------------

airports_cols <- readr::cols(
    id = readr::col_integer(),
    name = readr::col_character(),
    city = readr::col_character(),
    country = readr::col_character(),
    faa = readr::col_character(),
    icao = readr::col_character(),
    lat = readr::col_double(),
    lon = readr::col_double(),
    alt = readr::col_double(),
    tz = readr::col_double(),
    dst = readr::col_character(),
    tzone = readr::col_character(),
    type = readr::col_character(),
    source = readr::col_character()
)


# get_weather utilities -------------------------------------------------

weather_col_types <- readr::cols(
  .default = readr::col_double(),
  station = readr::col_character(),
  valid = readr::col_datetime(format = ""),
  skyc1 = readr::col_character(),
  skyc2 = readr::col_character(),
  skyc3 = readr::col_character(),
  skyc4 = readr::col_logical(),
  skyl4 = readr::col_logical(),
  wxcodes = readr::col_character(),
  peak_wind_time = readr::col_datetime(format = ""),
  metar = readr::col_character()
)

process_month_arg <- function(month) {
  start_month <- min(month)
  end_month <- max(month)
  last_day <- c(31, 28, 31, 30,
                31, 30, 31, 31,
                30, 31, 30, 31)[end_month]
  
  return(c(start_month, end_month, last_day))
}

get_weather_for_station <- function(station, year, dir, 
                                    month_and_day_range, month) {
  
  # query setup
  weather_url <- "http://mesonet.agron.iastate.edu/cgi-bin/request/asos.py?"
  
  weather_query <- list(
    station = station, 
    data = "all",
    year1 = as.character(year), 
    month1 = as.character(month_and_day_range[1]), 
    day1 = "1",
    year2 = as.character(year), 
    month2 = as.character(month_and_day_range[2]), 
    day2 = as.character(month_and_day_range[3]), 
    tz = "Etc/UTC",
    format = "comma", 
    latlon = "no", 
    direct = "yes"
  )
  
  # query the data!
  request <- httr::GET(weather_url, 
                       query = weather_query, 
                       httr::write_disk(paste0(dir, 
                                               "/weather_",
                                               station,
                                               ".csv"), 
                                        overwrite = TRUE))
  httr::stop_for_status(request) 
  
  # load the data, but fast !
  suppressWarnings(
    weather_raw <- vroom::vroom(file = paste0(dir, 
                                              "/weather_",
                                              station,
                                              ".csv"), 
                                comment = "#", 
                                na = "M", 
                                col_names = TRUE,
                                col_types = weather_col_types)
  )
  
  # delete the raw data
  unlink(paste0(dir, "/weather_", station, ".csv"))
  
  # and return the tidied data object :-)
  weather_raw %>%
    # rename some columns
    dplyr::rename(origin = station, 
                  time = valid, 
                  temp = tmpf, 
                  dewp = dwpf, 
                  humid = relh,
                  wind_dir = drct, 
                  wind_speed = sknt, 
                  wind_gust = gust,
                  precip = p01i, 
                  pressure = mslp, 
                  visib = vsby,
                  feels_like = feel) %>%
    # get rid of the metadata column
    dplyr::select(-metar) %>%
    # mutate some new useful columns
    dplyr::mutate(time = as.POSIXct(strptime(time, "%Y-%m-%d %H:%M")),
                  wind_speed = as.numeric(wind_speed) * 1.15078, # convert to mpg
                  wind_gust = as.numeric(wind_speed) * 1.15078,
                  year = as.integer(year),
                  month = as.integer(lubridate::month(time)),
                  day = lubridate::mday(time),
                  hour = lubridate::hour(time),
                  time_hour = ISOdatetime(year, month, day, hour, 0, 0, tz = "GMT")) %>%
    # filter to only relevant rows - necessary for discontinuous month ranges
    dplyr::filter(month %in% !!month) %>%
    # fill in missing values with the only value given in a particular hour
    dplyr::group_by(time_hour) %>%  # Group data by 'time_hour'
    tidyr::fill(temp, dewp, humid, precip, pressure, .direction = "downup") %>%  # Fill NAs
    dplyr::ungroup() %>% # Ungroup to return to original data structure
    # remove duplicates / incompletes
    dplyr::group_by(origin, month, day, hour) %>%
    dplyr::filter(dplyr::row_number() == 1) %>%
    dplyr::ungroup() %>%
    # reorder columns to match the original dataset
    dplyr::select(origin, year, month, day, hour, temp, dewp, 
                  humid, wind_dir, wind_speed, wind_gust, precip,
                  pressure, visib, time_hour)
}


# get_planes utilities ------------------------------------------------------
get_planes_data <- function(year, dir, flights_data) {
  
  # put together the url to query the planes data at
  planes_src <- paste0(
    "https://registry.faa.gov/database/yearly/ReleasableAircraft.", 
    year, 
    ".zip"
  )
  
  # and a folder to save the planes data to
  planes_lcl <- paste0(dir, "/planes")
  if (!dir.exists(planes_lcl)) {dir.create(planes_lcl)}
  
  # download the planes data
  planes_tmp <- tempfile(fileext = ".zip")
  planes_response <- 
    httr::GET(
      planes_src, 
      httr::user_agent("anyflights"), 
      httr::write_disk(planes_tmp, overwrite = TRUE)
    )

  # ...and unzip it!
  utils::unzip(planes_tmp, exdir = planes_lcl, junkpaths = TRUE)
  
  # loading in and tidying the master planes data
  planes_master <- process_planes_master(planes_lcl)
  
  # loading in and tidying the planes reference data
  planes_ref <- process_planes_ref(planes_lcl)
  
  # join the master and ref data together
  planes <- join_planes_data(planes_master, planes_ref)
  
  # filter the planes data by the flights data, if relevant
  planes <- join_planes_to_flights_data(planes, flights_data)
}


process_planes_master <- function(planes_lcl) {
  suppressMessages(suppressWarnings(
    # read in the data, but fast
    planes_master <- vroom::vroom(paste0(planes_lcl, "/MASTER.txt"),
                                  progress = FALSE) %>%
      # the column names change every year, but the positions have stayed the
      # same -- select by position :-(
      dplyr::select(nnum = 1, code = 3, year = 5)
  ))
  
  # delete the temporary folder
  unlink(x = planes_lcl, recursive = TRUE)
  
  planes_master
}


process_planes_ref <- function(planes_lcl) {
  
  # 99.96% of the tailnumbers that were in the 2013 data are in the
  # 2019 data -- similar numbers hold for 2015, 2017. since formatting
  # is so unstable, just query the 2019 acftref data for now and join
  # to the given year's master.txt data to get the accurate data
  # for tailnums licensed in that year
  
  if (!dir.exists(planes_lcl)) {dir.create(planes_lcl)}
  
  # download the planes acftref data 
  planes_tmp <- tempfile(fileext = ".zip")
  
  planes_response <- 
    httr::GET(
      "https://registry.faa.gov/database/yearly/ReleasableAircraft.2019.zip", 
      httr::user_agent("anyflights"), 
      httr::write_disk(planes_tmp, overwrite = TRUE)
    )
  
  # ...and unzip it!
  utils::unzip(planes_tmp, exdir = planes_lcl, junkpaths = TRUE)
  
  # read in the data, but fast
  suppressMessages(suppressWarnings(
    planes_ref <- vroom::vroom(paste0(planes_lcl, 
                                      "/", 
                                      "ACFTREF.txt"),
                               col_names = planes_ref_col_names,
                               col_types = planes_ref_col_types,
                               progress = FALSE,
                               skip = 1) %>%
      dplyr::select(code, mfr, model, type_acft, 
                    type_eng, no_eng, no_seats, speed)
  ))
  
  
  # delete the temporary folder
  unlink(x = planes_lcl, recursive = TRUE)
  
  planes_ref
}

join_planes_data <- function(planes_master, planes_ref) {
  suppressWarnings(
    planes_master %>%
      dplyr::inner_join(planes_ref, by = "code") %>%
      dplyr::select(-code) %>%
      dplyr::mutate(speed = dplyr::if_else(speed == 0, NA_character_, speed),
                    no_eng = dplyr::if_else(no_eng == 0, NA_integer_, no_eng),
                    no_seats = dplyr::if_else(no_seats == 0, NA_integer_, no_seats),
                    engine = engine_types[type_eng + 1],
                    type = acft_types[type_acft],
                    tailnum = paste0("N", nnum),
                    year = as.integer(year),
                    speed = as.integer(speed)) %>%
      dplyr::rename(manufacturer = mfr,
                    engines = no_eng, 
                    seats = no_seats) %>%
      dplyr::select(tailnum, year, type, manufacturer, model, engines,
                    seats, speed, engine)
  )
}


planes_ref_col_names <- c("code", "mfr", "model", "type_acft", "type_eng", "ac", 
                          "amat", "no_eng", "no_seats", "na1", "speed", "na2")  

engine_types <- c("None", "Reciprocating", "Turbo-prop", "Turbo-shaft", 
                  "Turbo-jet", "Turbo-fan", "Ramjet", "2 Cycle", "4 Cycle", 
                  "Unknown", "Electric", "Rotary")

acft_types <- c("Glider", "Balloon", "Blimp/Dirigible", 
                "Fixed wing single engine", "Fixed wing multi engine", 
                "Rotorcraft", "Weight-shift-control", "Powered Parachute", 
                "Gyroplane")

planes_ref_col_types <- readr::cols(
  code = readr::col_character(),
  mfr = readr::col_character(),
  model = readr::col_character(),
  type_acft = readr::col_integer(),
  type_eng = readr::col_integer(),
  ac = readr::col_integer(),
  amat = readr::col_integer(),
  no_eng = readr::col_integer(),
  no_seats = readr::col_integer(),
  na1 = readr::col_character(),
  speed = readr::col_character(),
  na2 = readr::col_character()
)


# filter the planes data by the flights data, if relevant
join_planes_to_flights_data <- function(planes, flights_data) {
  
  # interpret the flights_data argument
  flights_data <- parse_flights_data_arg(flights_data)
  
  # join to flights data if it was supplied
  if (!is.null(flights_data)) {
    planes <- planes %>%
      dplyr::semi_join(flights_data, "tailnum") %>%
      dplyr::arrange(tailnum)
  }
  
  planes
}


# create_flights_package utilities -------------------------------------

save_flights_data <- function(data, name) {
  
  dir.create(paste0(name, "/data"))
  
  purrr::map2(names(data), data, save_flights_dataset, name)
  
}

save_flights_dataset <- function(dataset_name, data, name) {
  
  # save the dataset to file with the appropriate object name attached to it
  assign(dataset_name, data)
  save(list = dataset_name, 
       file = paste0(name, "/data/", dataset_name,  ".rda"))
  
}


write_flights_documentation <- function(name) {
  
  # check for which datasets are included in the package
  which_data <- dir(paste0(name, "/data")) %>% stringr::str_replace(".rda", "")

  # only write documentation for the relevant datasets
  needed_docs <- sysdata[which_data]
  
  # create the .R files in R/
  purrr::map(paste0(name, "/R/", names(needed_docs), ".R"), file.create)
  
  # write the .R data to them
  purrr::map2(needed_docs, 
              paste0(name, "/R/", names(needed_docs), ".R"),
              writeLines)
  
  # generate .Rd documentation files
  roxygen2::roxygenize(name)
  
  invisible(TRUE)
}


check_as_flights_package_arguments <- function(data, name) {
  
  # check the supplied data
  if (!inherits(data, "list")) {
    stop_glue("The `data` argument to `as_flights_package` must be a named ",
              "list, but you've provided an object with class ",
              "{list(class(data))}.")
  }
  if (is.null(names(data))) {
    stop_glue("The `data` argument must have names.")
  }
  if (any(!names(data) %in% c("flights", "weather", "airlines",
                              "airports", "planes"))) {
    stop_glue('Each of the names of the list for the `data` argument must be ',
              'one of "flights", "weather", "airlines", "airports", ', 
              'or "planes".')
  }
  if (!suppressWarnings(requireNamespace("nycflights13", quietly = TRUE))) {
    warning_glue(
      "Some internal checks in as_flights_package make use of the nycflights13 ",
      "package, but nycflights13 is not installed. To avoid warnings in the ",
      'future, please install nycflights13 with `install.packages("nycflights13")`.'
    )    
  }
  
  if ("flights" %in% names(data)) {
    check_given_data(data[["flights"]], "flights", 19)
  }
  if ("weather" %in% names(data)) {
    check_given_data(data[["weather"]], "weather", 15)
  }
  if ("planes" %in% names(data)) {
    check_given_data(data[["planes"]], "planes", 9)
  }
  if ("airlines" %in% names(data)) {
    check_given_data(data[["airlines"]], "airlines", 2)
  }
  if ("airports" %in% names(data)) {
    check_given_data(data[["airports"]], "airports", 8)
  }
  
  # if the package name isn't valid, error out
  if (!grepl(.standard_regexps()$valid_package_name, name)) {
    stop_glue("The supplied package name isn't valid. See: \n ",
              "http://r-pkgs.had.co.nz/package.html \n",
              "for more information.")
  }
  
}

check_given_data <- function(data_, name, ncols) {
  if (ncol(data_) != ncols) {
    stop_glue("There should be {ncols} columns in the {name} data, but the ",
              "supplied {name} data has {ncol(data_)} columns.")
  }
  
  if (suppressWarnings(requireNamespace("nycflights13", quietly = TRUE))) {
    if (!all(names(data_) %in% names(eval(parse(
        text = paste0("nycflights13::", name)))))) {
      stop_glue("The column names in the {name} data don't match the ",
                "expected column names. See names(nycflights13::{name}) ",
                "for expected column names.")
    }
  }
}

# progress updates utility -----------------------------------------

# A wrapper around str_pad for easier defaults
pad_text <- function(msg, width = 50) {
  stringr::str_pad(msg, width, side = "right")
}

# call tick on pb with an update
write_tick <- function(pb, update) {
  pb$tick(tokens = list(what = paste0(pad_text(update))))
}

# call message on pb with the total elapsed time
write_message <- function(pb, update, diff_fn) {
  pb$message(paste0(pad_text(update, 
                             50 - stringr::str_length(diff_fn())), 
                    diff_fn()))
}

# create a function that returns the difference in time
# from when the function was created, in seconds
create_diff_from_start <- function() {
  start <- Sys.time()
  diff_from_start <- function() {
    difftime(Sys.time(), start, units = "secs") %>%
      as.numeric() %>%
      round() %>%
      as.character() %>%
      paste0("s")
  }
}

# convert month numbers to names for progress updates
months <- c("January", "February", "March", "April",
            "May", "June", "July", "August",
            "September", "October", "November", "December")
