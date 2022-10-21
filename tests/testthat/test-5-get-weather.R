context("get weather")

test_that("standard get_weather (PDX, June 2018)", {
  skip_on_cran()
  skip_if_offline()
  skip_on_os("windows")
  
  weather_ <- get_weather("PDX", 2018, 6)
})

test_that("standard get_weather (NYC, February 2013)", {
  skip_on_cran()
  skip_if_offline()
  skip_on_os("windows")
  
  weather_2 <- get_weather(c("JFK", "LGA", "EWR"), 2013, 2)
  
  weather_orig <- nycflights13::weather %>% dplyr::filter(month == 2)
  
  # expect the same number of columns, column names, and column types
  expect_equal(ncol(weather_2), ncol(weather_orig))
  expect_equal(colnames(weather_2), colnames(weather_orig))
  expect_equal(purrr::map(weather_2, class) %>% unlist(),
               purrr::map(weather_orig, class) %>% unlist())
})