make_flights <- function(station, year) {
  
  # Create Subdirectory ----------------------
  
  station_low <- tolower(station)
  year_substr <- substr(year[1], 3, 4)
  subdir <- paste0(station_low, "flights", year_substr)
  dir.create(subdir, showWarnings = FALSE)
  
  get_flights(station = station, year = year, subdir = subdir)
  get_airlines(subdir = subdir)
  get_airports(subdir = subdir)
  get_weather(station = station, year = year, subdir = subdir)
  get_planes(year = year, subdir = subdir)
}
