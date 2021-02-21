context("utilities")

test_that("checking station argument", {
  skip_on_cran()
  skip_if_offline()
  
  expect_error(anyflights("Portland", 2018, 1),
               "consider using the get_airports")
  expect_error(anyflights(c("PDX", "Portland", "JFK"), 2018, 1),
               "consider using the get_airports")
  expect_error(anyflights(1, 2018, 1),
               "wasn't a character")
})


test_that("checking month argument", {
  skip_on_cran()
  skip_if_offline()
  
  expect_error(anyflights("PDX", 2018, -1),
               "month values within")
  expect_error(anyflights("PDX", 2018, 13),
               "month values within")
  expect_error(anyflights("PDX", 2018, c(12, 13)),
               "month values within")
  expect_error(anyflights("PDX", 2018, "1"),
               "has class character")
  expect_error(anyflights("PDX", 2018, c("1", "2")),
               "has class character")
  expect_error(anyflights("PDX", 2018, list(1)),
               "has class list")
})

test_that("checking year argument", {
  skip_if(skip_conditions())
  
  expect_error(anyflights("PDX", as.numeric(substr(Sys.Date(), 1, 4)) + 1, 1),
               "in the future")
  expect_error(anyflights("PDX", as.numeric(substr(Sys.Date(), 1, 4)), 1),
               "for this year")
  expect_error(anyflights("PDX", 1980, 1),
               "argument 1980 is really")
})

test_that("checking download file wrapper", {
  skip_on_cran()
  skip_if_offline()
  
  # set timeout option 1 second
  original_timeout_value <- options()[['timeout']]
  options(timeout = 1)
  expect_warning(expect_error(anyflights("LAX", 2020, 1), 
               "utils::download.file timed out before finishing downloading the file"
               ))
  expect_warning(expect_error(get_planes(2018), 
               "utils::download.file timed out before finishing downloading the file"
  ))
  options(timeout = original_timeout_value)
})
