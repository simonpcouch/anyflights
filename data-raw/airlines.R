library(dplyr)
library(readr)

if (url.exists("http://www.transtats.bts.gov/Download_Lookup.asp?Lookup=L_UNIQUE_CARRIERS")) {
  raw <- read_csv("http://www.transtats.bts.gov/Download_Lookup.asp?Lookup=L_UNIQUE_CARRIERS")
} else stop("Can't access `airlines` link in 'data-raw/airlines.R'")


load("data/flights.rda")

airlines <- raw %>%
  select(carrier = Code, name = Description) %>%
  semi_join(flights) %>%
  arrange(carrier)

write_csv(airlines, "data-raw/airlines.csv")
save(airlines, file = "data/airlines.rda", compress = "bzip2")
