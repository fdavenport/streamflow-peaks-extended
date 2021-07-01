library(stringr)
library(raster)
library(velox)
library(rgdal)

## FUNCTIONS USED IN DATA PROCESSING AND ANALYSIS

## convert USGS IDs to 8-digit character format
sta2char <- function(site_vec){
    ## INPUT
    ## site_vec is an integer vector of USGS station IDs

    ## RETURN
    ## character vector of station IDs in 8-digit character format

    for(i in 1:length(site_vec)){
        station <- toString(site_vec[i])
        len <- nchar(station)
        ## add leading zeros for station with <8 char
        if (len < 8){
            station <- str_pad(station, 8, pad = "0")
        }
        ## two stations have 9-digit format
        if(station == "94196783") station <- "094196783"
        if(station == "94985005") station <- "094985005"
        site_vec[i] <- station
    }
    site_vec
}

## -----------------------------------------------------------------------------
hrGRIB2dayRaster <- function(filepath, fileprefix, dt,
                       model, vars = NULL, fun = "sum", outdirectory){
    ## WHAT: reads hourly NLDAS GRIB files and saves as daily raster files
    ## INPUT
    ## filepaths = path to files
    ## fileprefix = prefix of filename before date
    ## dt = date to aggregate, YearMonDay format: 19801001
    ## model = FORA or VIC
    ## vars = variables to include
    ## fun = function to aggregate each variable ("sum", "mean", or "inst")
    ##       either vector with function for each variable or one function to use for all variables
    ## outdirectory = directory to save new daily files
    ## RETURN
    ## NULL, saves day file as raster file

    hours <- str_pad(0:23, 2, pad = "0")

    yr <- as.integer(dt) %/% 10000 ## determine year for correct subfolder

    if (length(fun) == 1) fun <- rep(fun, nVars)

    if (model == "FORA") {
        GRIBvars <- c("TMP", "SPFH", "PRES", "UGRD", "VGRD", "DLWRF",
                      "var153", "CAPE", "PEVAP", "APCP", "DSWRF")
        suffix <- "00.002.grb"
    } else if (model == "VIC"){
        if (yr >= 2018){
            ## different file contents for 2018-2020 files!
            GRIBvars <- c("ASNOW", "ARAIN", "SSRUN", "BGRUN", "SNOM", "SNOT", "AVSFT",
                       "WEASD", "TSOIL1", "TSOIL2", "TSOIL3", "SOILM5","SOILM0-100",
                      "SOILM1", "SOILM2", "SOILM3", "LSOIL1", "LSOIL2", "LSOIL3", "MSTAV5",
                      "MSTAV0-100", "TRANS", "SBSNO", "SNOD", "SNOWC")
        } else {
            GRIBvars <- c("ASNOW", "ARAIN", "EVP", "SSRUN", "BGRUN", "SNOM", "SNOT", "AVSFT",
                      "RADT", "WEASD", "TSOIL1", "TSOIL2", "TSOIL3", "SOILM5", "RZS",
                      "SOILM1", "SOILM2", "SOILM3", "LSOIL1", "LSOIL2", "LSOIL3", "MSTAV5",
                      "TRANS", "SBSNO", "LAI", "SNOD", "SNOWC")
        }
        suffix <- "00.002.grb.SUB.grb"
    }

    hour.raster.list <- vector("list", length=24)
    for (j in seq_along(hours)){
        ## chose file
        nldasFilename <- paste0(filepath, yr, "/", fileprefix, dt,
                                ".", hours[j], suffix)

        ## read in rasters and assign variable names
        oldw <- getOption("warn")
        options(warn = -1)
        hour.raster.list[[j]] <- readGDAL(nldasFilename)
        options(warn = oldw)
        names(hour.raster.list[[j]]) <- GRIBvars
    }
    var.raster.list <- vector("list", length = length(vars))
    for (k in seq_along(vars)){
        temp.raster.list <- lapply(1:24,
                                   function(x) raster(hour.raster.list[[x]][vars[k]]))
        if(fun[k] == "mean") {
            temp <- mean(brick(temp.raster.list), na.rm = TRUE)
        } else if (fun[k] == "sum") {
            temp <- sum(brick(temp.raster.list), na.rm = TRUE)
        } else if (fun[k] == "inst") {
            temp <- temp.raster.list[[24]]
        } else {
            stop("incorrect function used. options are sum, mean or inst")
        }
        names(temp) <- vars[k]
        var.raster.list[[k]] <- temp
    }
    dayRaster <- brick(var.raster.list)
    dayOut <- writeRaster(dayRaster, filename = paste0(outdirectory, model, "0125_D.A", dt,
                                                        ".grd"), overwrite = TRUE)
    return(1)
}

## -----------------------------------------------------------------------------
extract_raster_daily <- function(boundaries, fileprefix, startDay = 19801001,
                               endDay = 20160930, vars, res = 1){
    ## WHAT: calculates mean value of NLDAS variable within basin boundary for hourly data from
    ##       daily raster files created in hrGRIB2dayRaster()
    ## INPUT
    ## boundaries = SpatialPolygonsDataFrame with feature boundaries to use for extraction
    ## fileprefix = path and prefix of raster files,
    ##              eg "/scratch/users/fvdav/NLDAS/VIC0125_D/VIC0125_D.A"
    ## startDay = starting day,YearMonDay format: 19801001
    ## endDay =  end day, YearMonDay format: 20160930
    ## vars = character vector listing variables (short form) to extract
    ## res = resolution to increase raster by, default of 1 is no change

    ## RETURN
    ## array of mean variable value for each year/mon for each station
    ## with dimensions of (watersheds) X (days) X (variables)
    if(is.na(startDay)) return(NA)

    days <- seq.Date(as.Date(as.character(startDay), format = "%Y%m%d"),
                     as.Date(as.character(endDay), format = "%Y%m%d"),
                     "days")
    climateData <- array(,dim=c(nrow(boundaries), length(days), length(vars)),
                    dimnames=list(boundaries$GAGE_ID, days, vars))

    for (i in seq_along(days)){
        cat(sprintf("day = %i\n", days[i]))
        dt <- gsub("-", "", days[i])

        ## chose file for correct year/month combo
        rasterFilename <- paste0(fileprefix, dt, ".grd")

        ## initialize raster list
        rasters <- brick(rasterFilename)
        rasters <- rasters[[vars]]

        rasters <- velox(disaggregate(rasters, res))
        tempData.list <- rasters$extract(sp=boundaries, small=TRUE)
        ## mean is calculated outside of extract to allow for NA remove
        meanData.list <- lapply(1:length(tempData.list),
                                function(x) colMeans(tempData.list[[x]], na.rm = TRUE))
        ## convert mean Data list into array and store in climateData array
        climateData[,i,] <- t(simplify2array(meanData.list))
    }
    climateData
}

## -----------------------------------------------------------------------------

findPeaks <- function(flowData, timeData, peak_window = 7, thr = 0, allowEndPeaks = TRUE){
    ## WHAT: returns date and magnitude of peak flow events
    ## INPUT:
    ## flowData = time series of flow data
    ## timeData = timesteps corresponding to flow data
    ##            NOTE: assumes time series is in order and has uniform spacing
    ## window = minimum time separation between peaks (whatever unit of timeData is)
    ## thr = minimum threshold (abs value) for peaks
    ## allowEndPeaks = whether or not first or last day in time series can be peak
    ## RETURN:
    ## data.frame with peak magnitude and date of occurence

    ## create list of indices within window of peak, e.g. -6:-1,1:6
    windowInd <- -(peak_window-1):(peak_window-1)

    ## max must be greater than previous day, and >= following day
    localMaxCheck <- sapply(1:length(flowData),
                            function(x) flowData[x]>flowData[x-1] &&
                                        flowData[x]>=flowData[x+1])
    if(allowEndPeaks){
        ## check for max at first or last data point
        localMaxCheck[1] <- ifelse(flowData[1]>=flowData[2],TRUE,FALSE)
        localMaxCheck[length(flowData)] <- ifelse(flowData[length(flowData)] >
                                                  flowData[length(flowData)-1],
                                                  TRUE, FALSE)
    } else {
            localMaxCheck[1] <- FALSE
            localMaxCheck[length(flowData)] <- FALSE
    }
    peakIndex <- which(localMaxCheck & flowData > thr)
    j <- 1
    goodPeaks <- vector(,length(peakIndex))

    while(length(peakIndex)>0){
        tempMaxInd <- peakIndex[which.max(flowData[peakIndex])]
        goodPeaks[j] <- tempMaxInd
        ## remove peaks within 7 days
        peakIndex <- peakIndex[-(which(peakIndex %in% (windowInd+tempMaxInd)))]
        j <- j+1
    }
    result <- data.frame(timeData[sort(goodPeaks)], flowData[sort(goodPeaks)])
    names(result) <- c("peak_dt", "flow")
    return(result)
}
