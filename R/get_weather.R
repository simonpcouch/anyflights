#' Generate a weather dataset for the specified year and airport
#' 
#' @param station The airport of interest (use the airport code)
#' @param year The year of interest, as an integer
#' @param dir The folder for the dataset to be saved in
#' @return A data frame with ~25,000 rows and 15 variables:
#' \describe{
#' \item{origin}{Weather station. Named origin to facilitate merging with
#'   flights data}
#' \item{year,month,day,hour}{Time of recording}
#' \item{temp,dewp}{Temperature and dewpoint in F}
#' \item{humid}{Relative humidity}
#' \item{wind_dir,wind_speed,wind_gust}{Wind direction (in degrees), speed
#'   and gust speed (in mph)}
#' \item{precip}{Precipitation, in inches}
#' \item{pressure}{Sea level pressure in millibars}
#' \item{visib}{Visibility in miles}
#' \item{time_hour}{Date and hour of the recording as a \code{POSIXct} date}
#' }
#' @source ASOS download from Iowa Environmental Mesonet,
#'   \url{https://mesonet.agron.iastate.edu/request/download.phtml}
#' @examples
#' get_weather(station = "PDX", year = 2015, dir = tempdir())
#' @seealso \code{\link{get_flights}} for flight data, \code{\link{get_airports}} for airport
#' data, \code{\link{get_planes}} for plane data, \code{\link{get_airlines}} for airline
#' data, and \code{\link{make_flights}} for a wrapper function  
#' @export

get_weather <- function(station, year, dir) {
  
  # Download Weather Data --------------------
  
  weather_url <- "http://mesonet.agron.iastate.edu/cgi-bin/request/asos.py?"

  weather_query <- list(
    station = station, data = "all",
    year1 = as.character(year), month1 = "1", day1 = "1",
    year2 = as.character(year), month2 = "12", day2 = "31", tz = "GMT",
    format = "comma", latlon = "no", direct = "yes")
  
  if (!dir.exists(dir)) {dir.create(dir, showWarnings = FALSE, recursive = TRUE)}
  r <- httr::GET(weather_url, query = weather_query, 
                 write_disk(paste0("./", dir, "/weather.csv"), overwrite = TRUE))
  httr::stop_for_status(r)                     
                        
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
  
  weather_raw <- read_csv(file = paste0("./", dir, "/weather.csv"), comment = "#", na = "M",
           col_names = TRUE, col_types = weather_col_types)
  names(weather_raw) <- c("station", "valid", "tmpf", "dwpf", "relh", "drct", "sknt",
                          "p01i", "alti", "mslp", "vsby", "gust",
                          "skyc1", "skyc2", "skyc3", "skyc4",
                          "skyl1", "skyl2", "skyl3", "skyl4", "wxcodes", "metar")
  
  weather <-
    weather_raw %>%
    dplyr::rename(time = valid) %>%
    dplyr::select(
      origin = station, time, temp = tmpf, dewp = dwpf, humid = relh,
      wind_dir = drct, wind_speed = sknt, wind_gust = gust,
      precip = p01i, pressure = mslp, visib = vsby
    ) %>%
    dplyr::mutate(
      time = as.POSIXct(strptime(time, "%Y-%m-%d %H:%M")),
      wind_speed = as.numeric(wind_speed) * 1.15078, # convert to mpg
      wind_gust = as.numeric(wind_speed) * 1.15078,
      year = year,
      month = lubridate::month(time),
      day = lubridate::mday(time),
      hour = lubridate::hour(time)) %>%
    dplyr::group_by(origin, month, day, hour) %>%
    dplyr::filter(row_number() == 1) %>%
    dplyr::select(origin, year:hour, temp:visib) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(month)) %>%
    dplyr::mutate(
      time_hour = ISOdatetime(year, month, day, hour, 0, 0))
  
  unlink(paste0("./", dir, "/weather.csv"))
  weather_file_path <- paste0(dir, "/weather.rda")
  save(weather, file = weather_file_path, compress = "xz")
  
}
