# this function is deprecated for now--the file format changes every year,
# and for now, it doesn't seem worth it to make queries for every year.

get_planes <- function(year, dir) {
 # main_dir <- file.path(getwd(), dir, fsep = "/")
  
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
  
  if (!dir.exists(planes_lcl)) {dir.create(planes_lcl)}
  utils::unzip(planes_tmp, exdir = planes_lcl, junkpaths = TRUE)
  
  planes_master <- read.csv(paste0(planes_lcl, "/MASTER.txt"), 
                            stringsAsFactors = FALSE, strip.white = TRUE)
  names(planes_master) <- tolower(names(planes_master))
  
  if (year == 2015)
  { keep <- planes_master %>%
    dplyr::tbl_df() %>%
    dplyr::select(nnum = n.number, code = aircraftmfrmodelcode, year = year.mfr) 
  } else {
    keep <- planes_master %>%
      dplyr::tbl_df() %>%
      dplyr::select(nnum = n.number, code = mfr.mdl.code, year = year.mfr) 
  }
  
  # the file structure changed in 2017.. thus, arguments need to be a bit different
  if (year >= 2017) {
  planes_col_names <- c("code", "mfr", "model", "type.acft", "type.eng", "ac", 
                        "amat", "no.eng", "no.seats", "na1", "speed", "na2")   

  planes_col_types <- cols(
    code = col_character(),
    mfr = col_character(),
    model = col_character(),
    type.acft = col_integer(),
    type.eng = col_integer(),
    ac = col_integer(),
    amat = col_integer(),
    no.eng = col_integer(),
    no.seats = col_integer(),
    na1 = col_character(),
    speed = col_character(),
    na2 = col_character()
  )       
  
  planes_ref <- read_csv(paste0(planes_lcl, "/ACFTREF.txt"),
                         col_names = planes_col_names,
                         col_types = planes_col_types,
                         skip = 1,
                         trim_ws = TRUE,
                         guess_max = Inf)
  
  planes_ref <- read.csv(paste0(planes_lcl, "/ACFTREF.txt"))
  
  
  } else {
  planes_col_names <- c("mfr", "model", "type.acft", "type.eng", "ac", 
                        "amat", "no.eng", "no.seats", "speed", "na3", 
                        "code", "na1", "na2")   
    
  planes_ref <- read.csv(paste0(planes_lcl, "/AcftRef.txt"), 
                         stringsAsFactors = FALSE, strip.white = TRUE, 
                         header = FALSE, col.names = planes_col_names) 
  }
  
  planes_ref <- planes_ref %>%
    dplyr::tbl_df() %>%
    dplyr::select(code, mfr, model, type.acft, type.eng, 
                  no.eng, no.seats, speed)
  
  planes_all <- keep %>%
    dplyr::inner_join(planes_ref, by = "code") %>%
    dplyr::select(-code)
  planes_all$speed[planes_all$speed == 0] <- NA
  planes_all$no.eng[planes_all$no.eng == 0] <- NA
  planes_all$no.seats[planes_all$no.seats == 0] <- NA
  
  engine <- c("None", "Reciprocating", "Turbo-prop", "Turbo-shaft", 
              "Turbo-jet", "Turbo-fan", "Ramjet", "2 Cycle", "4 Cycle", 
              "Unknown", "Electric", "Rotary")
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
