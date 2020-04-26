context("as_flights_package")

test_that("as_flights_package works", {

  skip_if(TRUE)
  
  toy_data <- list(flights = dplyr::sample_n(nycflights13::flights, 30),
                   weather = dplyr::sample_n(nycflights13::weather, 30),
                   airports = dplyr::sample_n(nycflights13::airports, 30),
                   planes = dplyr::sample_n(nycflights13::planes, 30),
                   airlines = nycflights13::airlines)
  
  as_flights_package(toy_data, "toyflights13")

})
