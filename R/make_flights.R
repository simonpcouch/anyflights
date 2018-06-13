#' Generate a folder of flights datasets for a given year and airport
#' 
#' @param station The airport of interest (use the airport code)
#' @param year The year of interest, as an integer
#' @return A folder named with "airport code" "flights" "year" convention 
#' (i.e. pdxflights15) containing datasets about flights & relevant metadata
#' @examples
#' make_flights(station = "PDX", year = 2015)
#' @export
make_flights <- function(station, year, dir = tempdir()) {
  
  # Create Subdirectory ----------------------
  dir.create(dir, showWarnings = FALSE)
  
  # Call get_ Functions
  get_flights(station = station, year = year, subdir = dir)
  get_airlines(subdir = dir)
  get_airports(subdir = dir)
  get_weather(station = station, year = year, subdir = dir)
  get_planes(year = year, subdir = dir)
}
