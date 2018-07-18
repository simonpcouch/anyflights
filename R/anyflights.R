#' Generate a folder of air travel datasets for a given year and airport
#' 
#' \code{anyflights} is a wrapper function for \code{get_flights},
#' \code{get_airports}, \code{get_weather}, and \code{get_airlines}. Please 
#' note that, even with a strong internet connection, this function 
#' may take several minutes to download relevant data, and temporariliy 
#' requires up to 2GB of storage (the file size is trimmed down significantly 
#' after some post-processing---to the order of a couple MB---and the larger 
#' files are deleted before termination)
#' 
#' @param station A character string---the airport of interest (use the FAA 
#' LID airport code).
#' @param year The year of interest, as an integer (unquoted). Currently, years 
#' 2015 and on are supported. Information for the most recent year is usually 
#' available by February or March in the following year.
#' @param dir A character string---the folder for the dataset to be saved in
#' @return A folder containing datasets about air travel
#' @examples
#' \donttest{anyflights(station = "PDX", year = 2015, dir = tempdir())}
#' @seealso \code{\link{get_flights}} for flight data, 
#' \code{\link{get_airports}} for airport data, \code{\link{get_weather}} 
#' for weather data, \code{\link{get_flights}} for flight data, and 
#' \code{\link{get_airlines}} for airline data
#' @export

anyflights <- function(station, year, dir) {
  
  # Create Subdirectory 
  dir.create(dir, showWarnings = FALSE)
  
  # Call get_ Functions
  get_flights(station = station, year = year, dir = dir)
  get_airlines(dir = dir)
  get_airports(dir = dir)
  get_weather(station = station, year = year, dir = dir)
  print("All done!")
}
