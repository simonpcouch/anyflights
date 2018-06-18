#' Generate a folder of flights datasets for a given year and airport
#' 
#' \code{anyflights} is a wrapper function for \code{get_flights},
#' \code{get_airports}, \code{get_planes}, \code{get_weather}, and
#' \code{get_airlines}
#' 
#' @param station The airport of interest (use the airport code)
#' @param year The year of interest, as an integer
#' @param dir The folder for the datasets to be saved in
#' @return A folder containing datasets about air travel
#' @examples
#' anyflights(station = "PDX", year = 2015)
#' @seealso \code{\link{get_flights}} for flight data, \code{\link{get_airports}} for airport
#' data, \code{\link{get_weather}} for weather data, \code{\link{get_airlines}} for airline
#' data, and \code{\link{get_planes}} for plane data
#' @export

anyflights <- function(station, year, dir = tempdir()) {
  
  # Create Subdirectory ----------------------
  dir.create(dir, showWarnings = FALSE)
  
  # Call get_ Functions
  get_flights(station = station, year = year, subdir = dir)
  get_airlines(subdir = dir)
  get_airports(subdir = dir)
  get_weather(station = station, year = year, subdir = dir)
  get_planes(year = year, subdir = dir)
}