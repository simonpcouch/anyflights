get_weather <- function(station, year) {

# From https://mesonet.agron.iastate.edu/request/download.phtml?network=NY_ASOS

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

}
