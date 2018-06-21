#' Generate a planes dataset for the specified year
#' 
#' @param year The year of interest, as an integer
#' @param dir The folder for the dataset to be saved in
#' @return A data frame with ~3500 rows and 9 variables:
#' \describe{
#' \item{tailnum}{Tail number}
#' \item{year}{Year manufactured}
#' \item{type}{Type of plane}
#' \item{manufacturer,model}{Manufacturer and model}
#' \item{engines,seats}{Number of engines and seats}
#' \item{speed}{Average cruising speed in mph}
#' \item{engine}{Type of engine}
#' }
#' @source FAA Aircraft registry,
#'  \url{http://www.faa.gov/licenses_certificates/aircraft_certification/aircraft_registry/releasable_aircraft_download/}
#' @examples
#' get_planes(year = 2015, dir = tempdir())
#' @seealso \code{\link{get_flights}} for flight data, \code{\link{get_airports}} for airport
#' data, \code{\link{get_weather}} for weather data, \code{\link{get_airlines}} for airline
#' data, and \code{\link{anyflights}} for a wrapper function  
#' @export


get_planes <- function(year, dir) {
  
  # Check that the flights dataset exists before running
  if (!file.exists(paste0(dir, "/flights.rda"))) {stop(
    "The corresponding flights dataset does not seem to exist. Please run get_flights()
    with the same dir argument."
  )}
  
  # Download Planes Data ---------------------
  
  # Update URL from
  # http://www.faa.gov/licenses_certificates/aircraft_certification/aircraft_registry/releasable_aircraft_download/
  planes_src <- paste0("http://registry.faa.gov/database/yearly/ReleasableAircraft.", year, ".zip")
  planes_lcl <- paste0(dir, "/planes")
  
  planes_tmp <- tempfile(fileext = ".zip")
  utils::download.file(planes_src, planes_tmp) 
  
  dir.create(planes_lcl)
  utils::unzip(planes_tmp, exdir = planes_lcl, junkpaths = TRUE)
  
  planes_master <- read.csv(paste0(planes_lcl, "/MASTER.txt"), stringsAsFactors = FALSE, strip.white = TRUE)
  names(planes_master) <- tolower(names(planes_master))
  
  keep <- planes_master %>%
    dplyr::tbl_df() %>%
    dplyr::select(nnum = n.number, code = mfr.mdl.code, year = year.mfr)
  
  planes_col_names <- c("mfr", "model", "type.acft", "type.eng", "ac", 
                        "amat", "no.eng", "no.seats", "speed", "na3", "code", "na1", "na2")
  
  
  planes_ref <- read.csv(paste0(planes_lcl, "/AcftRef.txt"), stringsAsFactors = FALSE,
                         strip.white = TRUE, header = FALSE, col.names = planes_col_names)
  
  planes_ref <- planes_ref %>%
    dplyr::tbl_df() %>%
    dplyr::select(code, mfr, model, type.acft, type.eng, no.eng, no.seats, speed)
  
  planes_all <- keep %>%
    dplyr::inner_join(planes_ref, by = "code") %>%
    dplyr::select(-code)
  planes_all$speed[planes_all$speed == 0] <- NA
  planes_all$no.eng[planes_all$no.eng == 0] <- NA
  planes_all$no.seats[planes_all$no.seats == 0] <- NA
  
  engine <- c("None", "Reciprocating", "Turbo-prop", "Turbo-shaft", "Turbo-jet",
              "Turbo-fan", "Ramjet", "2 Cycle", "4 Cycle", "Unknown", "Electric", "Rotary")
  planes_all$engine <- engine[planes_all$type.eng + 1]
  planes_all$type.eng <- NULL
  
  acft <- c("Glider", "Balloon", "Blimp/Dirigible", "Fixed wing single engine",
            "Fixed wing multi engine", "Rotorcraft", "Weight-shift-control",
            "Powered Parachute", "Gyroplane")
  planes_all$type <- acft[planes_all$type.acft]
  planes_all$type.acft <- NULL
  
  planes_all$tailnum <- paste0("N", planes_all$nnum)
  
  flights_ds <- paste0(dir, "/flights.rda")
  load(flights_ds)
  
  planes <- planes_all %>%
    dplyr::select(
      tailnum, year, type, manufacturer = mfr, model = model,
      engines = no.eng, seats = no.seats, speed, engine
    ) %>%
    dplyr::semi_join(flights, "tailnum") %>%
    dplyr::arrange(tailnum)
  
  planes_filepath <- paste0(dir, "/planes.rda")
  save(planes, file = planes_filepath, compress = "xz")
  unlink(x = planes_lcl, recursive = TRUE)
  
}