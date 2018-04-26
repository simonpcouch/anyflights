get_flights <- function(station, year) {

flight_url <- function(year, month) {
  base_url <- "http://www.transtats.bts.gov/PREZIP/"
  paste0(base_url, "On_Time_On_Time_Performance_", year, "_", month, ".zip")
}

download_month <- function(year = year, month) {
  fl_url <- flight_url(year, month)
  if (url.exists(fl_url)) {
    temp <- tempfile(fileext = ".zip")
    download.file(fl_url, temp)
  } else stop(sprintf("Can't access flight data for supplied year. Check date of 'Latest Available Data' for 'Airline On-Time Performance Data' on \n https://www.transtats.bts.gov/releaseinfo.asp"))

  files <- unzip(temp, list = TRUE)
  # Only extract biggest file
  csv <- files$Name[order(files$Length, decreasing = TRUE)[1]]

  unzip(temp, exdir = "data-raw/flights", junkpaths = TRUE, files = csv)

  src <- paste0("data-raw/flights/", csv)
  dst <- paste0("data-raw/flights/", year, "-", month, ".csv")
  file.rename(src, dst)
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

all <- lapply(dir("data-raw/flights", full.names = TRUE), get_flight_data)
flights <- bind_rows(all)
flights$tailnum[flights$tailnum == ""] <- NA

dir.create("data", showWarnings = FALSE)
station_low <- tolower(station)
year_substr <- substr(year[1], 3, 4)
subdir <- paste0("data/", station_low, "flights", year_substr)
file_path <- paste0(subdir, "/flights.rda")
if (!dir.exists(path = subdir)) {
  dir.create(path = subdir) }
save(flights, file = file_path, compress = "bzip2")

save(flights, file = "data/flights.rda", compress = "bzip2")
}
