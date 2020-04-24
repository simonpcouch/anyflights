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
    if (!all(station %in% get_airports()$faa)) {
      stop_glue("Couldn't find at least one of the provided origin airports ",
                "{station}. Please consider using the get_airports() function ",
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

# get_flights utilities --------------------------------------------------

download_month <- function(year, month, dir, flight_exdir) {
  
  # put together the url for the relevant year and month
  fl_url <- make_flights_url(year, month)
  
  # make a temporary file to download to
  flight_temp <- tempfile(fileext = ".zip")
  
  # download the file
  utils::download.file(fl_url, flight_temp, quiet = TRUE)

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
}

get_flight_data <- function(path, station) {
  
  # read in the data
  suppressMessages(vroom::vroom(path, progress = FALSE)) %>%
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


# get_planes utilities ------------------------------------------------------
get_planes_data <- function(year, dir, flights_data) {
  
  # put together the url to query the planes data at
  planes_src <- paste0(
    "http://registry.faa.gov/database/yearly/ReleasableAircraft.", 
    year, 
    ".zip"
  )
  
  # and a folder to save the planes data to
  planes_lcl <- paste0(dir, "/planes")
  if (!dir.exists(planes_lcl)) {dir.create(planes_lcl)}
  
  # download the planes data
  planes_tmp <- tempfile(fileext = ".zip")
  utils::download.file(planes_src, planes_tmp, quiet = TRUE) 
  
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
  
  # delete the temporary folder
  unlink(x = planes_lcl, recursive = TRUE)
  
  planes %>%
    dplyr::select(tailnum, everything(), -nnum)
}


process_planes_master <- function(planes_lcl) {
  suppressMessages(
    # read in the data, but fast
    planes_master <- vroom::vroom(paste0(planes_lcl, "/MASTER.txt"),
                                  progress = FALSE) %>%
      # the column names change every year, but the positions have stayed the
      # same -- select by position :-(
      dplyr::select(nnum = 1, code = 3, year = 5)
  )
  
  planes_master
}


process_planes_ref <- function(planes_lcl) {
  
  # find the file called "acftref" in the folder -- the filename
  # is capitalized differently from year to year, which is great
  acftref_loc <- dir(planes_lcl) %>%
    tolower() %>%
    stringr::str_detect("acftref.txt") %>%
    which()
  
  if (length(acftref_loc) != 1) {
    stop_glue("Couldn't process the planes data for the given year.")
  }
  
  # read in the data, but fast
  planes_ref <- vroom::vroom(paste0(planes_lcl, 
                                    "/", 
                                    dir(planes_lcl)[acftref_loc]),
                             col_names = planes_ref_col_names,
                             col_types = planes_ref_col_types,
                             progress = FALSE) %>%
    dplyr::select(code, mfr, model, type_acft, 
                  type_eng, no_eng, no_seats, speed)
}

join_planes_data <- function(planes_master, planes_ref) {
  planes_master %>%
    dplyr::inner_join(planes_ref, by = "code") %>%
    dplyr::select(-code) %>%
    dplyr::mutate(speed = dplyr::if_else(speed == 0, NA_character_, speed),
                  no_eng = dplyr::if_else(no_eng == 0, NA_integer_, no_eng),
                  no_seats = dplyr::if_else(no_seats == 0, NA_integer_, no_seats),
                  engine = engine_types[type_eng + 1],
                  type = acft_types[type_acft],
                  tailnum = paste0("N", nnum)) %>%
    dplyr::select(-c(type_eng, type_acft)) %>%
    dplyr::rename(manufacturer = mfr,
                  engines = no_eng, 
                  seats = no_seats)
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
