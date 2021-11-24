context("as_flights_package")

test_that("as_flights_package works", {

  skip_on_cran()
  skip_on_ci()
  skip_on_os("windows")
  
  test_data <- list(flights = dplyr::sample_n(nycflights13::flights, 30),
                    weather = dplyr::sample_n(nycflights13::weather, 30),
                    airports = dplyr::sample_n(nycflights13::airports, 30),
                    planes = dplyr::sample_n(nycflights13::planes, 30),
                    airlines = nycflights13::airlines)
  
  create_path <- tempdir()
  
  as_flights_package(test_data, "testflights13", 
                     create_path = create_path,
                     check_name = FALSE,
                     use_rstudio = FALSE)
  
  package_path = paste0(create_path, "/testflights13")
  
  expect_true(file.exists(paste0(package_path, "/R/flights.R")))
  expect_true(file.exists(paste0(package_path, "/man/flights.Rd")))
  expect_true(file.exists(paste0(package_path, "/data/flights.rda")))
  
  unlink(package_path, recursive = TRUE)

})
