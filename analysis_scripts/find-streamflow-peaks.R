library(dplyr)
library(data.table)

source("./input_file_paths.R")
source("./parameters.R")
source("./functions.R")

## THIS SCRIPT: ----------------------------------------------------------------
## Open daily streamflow data files
## Finds peaks based on daily mean flow --> saves results
## -----------------------------------------------------------------------------

## OUTPUT FILES: ---------------------------------------------------------------
peak_folder <- "../processed_data/streamflow_peaks/"

## -----------------------------------------------------------------------------
sites <- as.data.frame(read.csv(site_file)) %>%
    dplyr::rename(staInt = x) %>%
    mutate(STAID = sta2char(staInt))

for(i in seq_along(sites$STAID)) {
    print(i)
    STAID <- sites$STAID[i]
    ## -------------------------------------------------------------------------
    ## calculate peaks from Daily Mean Flow ----------------------
    daily_data_file <- paste0(daily_streamflow_folder, STAID, "_dailyFlow.txt")
    data <- read.csv(daily_data_file)
    ## check for multiple streamgages
    if(ncol(data) == 7) {
        ## if there is more than one gage, gage with most recent data supercedes
        ## during overlapping record
        if (max(is.na(data[,4])) < max(is.na(data[,6]))) {
            data[,"X_00060_00003"] <- ifelse(!is.na(data[,4]), data[,4], data[,6])
        } else {
            data[,"X_00060_00003"] <- ifelse(!is.na(data[,6]), data[,4], data[,4])
        }
    }
    ## convert dates back to POSIXct format
    data$Date <- as.POSIXct(data$Date, "UTC")
    ## subset for datces within range of interest
    date_index <- which(data$Date >=
                        as.POSIXct(paste0(start_year-1, "-10-01", "tz_cd")) &
                        data$Date <=
                        as.POSIXct(paste0(end_year, "-09-30", "tz_cd")))
    date_index_base <- which(data$Date >=
                        as.POSIXct(paste0(base_period_start-1, "-10-01", "tz_cd")) &
                        data$Date <=
                        as.POSIXct(paste0(base_period_end, "-09-30", "tz_cd")))

    if(length(date_index) > 1){

        ## find min threshold using baseline period
        thr <- quantile(data$X_00060_00003[date_index_base], min_flow_thr, na.rm = TRUE)

        daily_peaks <- findPeaks(data$X_00060_00003[date_index],
                          data$Date[date_index],
                          peak_window = flow_window,
                          thr = thr)
        if(nrow(daily_peaks) > 0){
            daily_peaks$site_no <- data$site_no[1]
            outfile <- paste0(peak_folder, STAID, "_",
                              flow_window, "_MeanPeaks.txt")
            write.csv(daily_peaks, file = outfile, row.names = FALSE)
        } else {
            cat(sprintf("No daily mean peaks found for station %s", STAID))
        }
    }
}
