#' @rdname anyflights
#' @export
get_planes <- function(year, dir = NULL, flights_data = NULL) {

  # check user inputs
  check_arguments(station = station,
                  year = year,
                  month = month,
                  dir = dir,
                  context = "planes")
  flights_data <- parse_flights_data_arg(flights_data)
  
  # create a temporary directory if need be
  if (is.null(dir)) {
    dir_is_null <- TRUE
    dir <- tempdir()
  } else {
    dir_is_null <- FALSE
  }
  
  # grab the planes data for the relevant year
  planes <- get_planes_data(year, dir, flights_data)
  
  # save the data if a directory was supplied
  if (!dir_is_null) {
    save(planes, file = paste0(dir, "/planes.rda"), compress = "xz")
  }
  
  # delete the temporary folder
  unlink(x = planes_lcl, recursive = TRUE)
  
  # ...and return the data!
  planes
}
