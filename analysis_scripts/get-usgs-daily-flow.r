library(dataRetrieval)
library(dplyr)

source("./input_file_paths.R")
source("./functions.R")
source("./parameters.R")

## THIS SCRIPT: ----------------------------------------------------------------
## Read and saves daily flow data through water year 2020
## DOES NOT OVERWRITE existing files

## -----------------------------------------------------------------------------
sites <- as.data.frame(read.csv(site_file)) %>%
    dplyr::rename(staInt = x) %>%
    mutate(STAID = sta2char(staInt))


for(i in seq_along(sites$STAID)){
    current_file <- paste0(daily_streamflow_folder, sites$STAID[i], "_dailyFlow.txt")
    if(!file.exists(current_file)){
        cat(sprintf("Retrieving data for station %s\n", sites$STAID[i]))
        daily_data <- try(readNWISdv(sites$STAID[i], "00060",
                                     startDate = "",
                                     endDate = ""))
        write.csv(daily_data, file = current_file, row.names = F)
    }
}
