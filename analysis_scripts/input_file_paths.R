## -----------------------------------------------------------------------------
## SET FILEPATHS FOR INPUT DATA
## -----------------------------------------------------------------------------

## smaller raw data files are included in repository
## larger files must be downloaded separately

input_data_folder <- "../input_data"

## -----------------------------------------------------------------------------
## FILES INCLUDED IN REPOSITORY:
## -----------------------------------------------------------------------------

## list of stations to use:
site_file <- paste0(input_data_folder, "/site_list.txt")

## -----------------------------------------------------------------------------
## FILES NOT INCLUDED IN REPOSITORY (MUST BE DOWNLOADED SEPARATELY)
## -----------------------------------------------------------------------------

## daily USGS streamflow data:
daily_streamflow_folder <- paste0(input_data_folder, "/Daily_Flow_2020update/")


## NLDAS-2 hourly data:
nldas_FORA_H_path <- paste0(input_data_folder, "/FORA0125_H/")
nldas_VIC_H_path <- paste0(input_data_folder, "/VIC0125_H/")


watershed_boundary_file <- paste0(input_data_folder, "/watershed_boundaries.shp")

