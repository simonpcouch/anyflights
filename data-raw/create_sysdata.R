# the following script allows for reproducible recreation of the
# R/sysdata.rda file used to automatically generate roxygen
# documentation for packages outputted by the package.

# read in the raw .Rds for the desired documentation
weather_lines <- readLines("data-raw/weather.Rd")
planes_lines <- readLines("data-raw/planes.Rd")
flights_lines <- readLines("data-raw/flights.Rd")
airlines_lines <- readLines("data-raw/airlines.Rd")
airports_lines <- readLines("data-raw/airports.Rd")

# create the sysdata .Rda object that will be built internally
sysdata <- list(weather = weather_lines,
                planes = planes_lines,
                flights = flights_lines,
                airlines = airlines_lines,
                airports = airports_lines)

# save sysdata to file
save(sysdata, file = "R/sysdata.rda")


