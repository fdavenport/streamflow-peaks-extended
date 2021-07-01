## -----------------------------------------------------------------------------
## SET PARAMETERS FOR DATA PROCESSING
## -----------------------------------------------------------------------------

## NLDAS DATE RANGE
nldas_start_day <- 19790102
nldas_end_day <- 20201231

## PROJECT DATE PARAMETERS
start_year <- 1980
end_year <- 2020
start_date <- as.Date(paste0(start_year-1, "-10-01"))
end_date <- as.Date(paste0(end_year, "-09-30"))

## STREAMFLOW PEAK PARAMETERS
flow_window <- 8
min_flow_thr <- 0.5 ## 50th percentile, i.e. peak must exceed median
base_period_start <- 1980
base_period_end <- 2016
