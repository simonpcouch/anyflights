file.sources <- list.files("data-raw", pattern="*.R$", full.names = TRUE)[c(3,1,2,4,5)]
sapply(file.sources,source,.GlobalEnv)
