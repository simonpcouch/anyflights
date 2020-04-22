#' @rdname anyflights
#' @export
get_weather <- function(station, year, month, dir = NULL) {
  
  # check user inputs
  check_arguments(station = station, 
                  year = year, 
                  month = month, 
                  dir = dir, 
                  context = "weather")
  month_and_day_range <- process_month_arg(month)
  
  # set up the directory to download data to
  if (is.null(dir)) {
    dir_is_null <- TRUE
    dir <- tempdir()
  } else {
    dir_is_null <- FALSE
  }
  
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
                       httr::write_disk(paste0(dir, "/weather.csv"), 
                                        overwrite = TRUE))
  httr::stop_for_status(request)                     
    
  # load in the data as an object                    
  weather_raw <- vroom::vroom(file = paste0(dir, "/weather.csv"), 
                              comment = "#", 
                              na = "M", 
                              col_names = TRUE,
                              col_types = weather_col_types)
  
  # tidy the data
  weather <- weather_raw %>%
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
                  year = year,
                  month = lubridate::month(time),
                  day = lubridate::mday(time),
                  hour = lubridate::hour(time),
                  time_hour = ISOdatetime(year, month, day, hour, 0, 0)) %>%
    # filter to only relevant rows - necessary for discontinuous month ranges
    dplyr::filter(month %in% !!month) %>%
    # remove duplicates / incompletes
    dplyr::group_by(origin, month, day, hour) %>%
    dplyr::filter(dplyr::row_number() == 1) %>%
    # reorder columns
    dplyr::select(origin, year:hour, temp:visib, dplyr::everything()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(month))
  
  # delete the raw data
  unlink(paste0(dir, "/weather.csv"))
  
  # save the data if the user supplied a directory
  if (!dir_is_null) {
    save(weather, file = paste0(dir, "/weather.rda"), compress = "xz")
  }
  
  # ...and return the data :-)
  return(weather)
}
