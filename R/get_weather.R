#' Query nycflights13-Like Weather Data
#' 
#' This function generates a dataframe similar to the 
#' \code{\link[nycflights13]{weather}} dataset from \code{nycflights13} 
#' for any US airports and time frame. Please 
#' note that, even with a strong internet connection, this function 
#' may take several minutes to download relevant data.
#' 
#' @inheritParams anyflights 
#' 
#' @return A data frame with ~1k-25k rows and 15 variables:
#' \describe{
#' \item{\code{origin}}{Weather station. Named \code{origin} to facilitate 
#'   merging with flights data}
#' \item{\code{year, month, day, hour}}{Time of recording, UTC}
#' \item{\code{temp, dewp}}{Temperature and dewpoint in F}
#' \item{\code{humid}}{Relative humidity}
#' \item{\code{wind_dir, wind_speed, wind_gust}}{Wind direction (in degrees), 
#'   speed and gust speed (in mph)}
#' \item{\code{precip}}{Precipitation, in inches}
#' \item{\code{pressure}}{Sea level pressure in millibars}
#' \item{\code{visib}}{Visibility in miles}
#' \item{\code{time_hour}}{Date and hour of the recording as a \code{POSIXct} 
#'   date, UTC}
#' }
#' @source ASOS download from Iowa Environmental Mesonet,
#'   \url{https://mesonet.agron.iastate.edu/request/download.phtml}
#' 
#' @examples
#' 
#' # query weather at Portland International in June 2018
#' \donttest{\dontrun{get_weather("PDX", 2018, 6)}}
#' 
#' # ...or the original nycflights13 weather dataset
#' \donttest{\dontrun{get_weather(c("JFK", "LGA", "EWR"), 2013)}}
#' 
#' # use the dir argument to indicate the folder to save the 
#' # data in as "weather.rda"
#' \donttest{\dontrun{get_weather("PDX", 2018, 6, dir = tempdir())}}
#'
#' @seealso \code{\link{get_flights}} for flight data,
#' \code{\link{get_airlines}} for airlines data,
#' \code{\link{get_airports}} for airports data,
#' \code{\link{get_planes}} for planes data,
#' or \code{\link{anyflights}} for a wrapper function
#'
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
  
  weather_raw <- purrr::map(station, 
                            get_weather_for_station, 
                            year = year,
                            dir = dir, 
                            month_and_day_range = month_and_day_range) %>%
    dplyr::bind_rows()
  
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
  
  # save the data if the user supplied a directory
  if (!dir_is_null) {
    save(weather, file = paste0(dir, "/weather.rda"), compress = "xz")
  }
  
  # ...and return the data :-)
  return(weather)
}
