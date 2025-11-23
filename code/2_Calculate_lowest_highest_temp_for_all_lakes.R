setwd("C:/Scratch/LLX/Project_3")
library(data.table)
library(RNetCDF)
library(ncmeta) 
library(tidyverse)
library(ncdfgeom) 
library(sf)
library(rgdal)
library(stringr)
library(lwgeom)
library(dplyr)
library(lubridate)
library(purrr)
library(dplyr)
library(tools)

# Before you can run this script, make sure that you have also downloaded
# the `netCDF_extract_utils.R` script from ScienceBase 
# It contains all the functions required for this code to work. Edit the filepath
# below depending on where you saved it relative to this file.
source('Code/netCDF_extract_utils.R')

####read temperature profile data and calcualte the No of days in each year with optimal temperature####
path <- "Data/Environment/compress_tmp/"
files <- list.files(path, pattern = "\\.nc$", full.names = TRUE)

# creat the file for the data
output_file_hot <- "Output/highest_lowest_temp_hot_11.rds"
output_file_cold <- "Output/highest_lowest_temp_cold_11.rds"
output_file_good <- "Output/highest_lowest_temp_good_11.rds"

# Initialize an empty list to store results
results_list_hot <- list()
results_list_cold <- list()
results_list_good <- list()

# Loop over each file
for(i in 11) {
  
  nc_file <- files[i]
  
  # Read in information about netCDF (variables, dates, etc.)
  nc_info <- read_timeseries_profile_dsg(nc_file, read_data = FALSE)
  
  # Define lake sites of interest
  lake_sites <- c(nc_info$timeseries_id) # change to the lake site 
  
  # Split lake_sites into 5 equal-sized chunks
  lake_sites_split <- split(lake_sites, cut(1:length(lake_sites), 5))
  
  # Loop over each chunk of lake_sites
  for (j in 1:length(lake_sites_split)) {
    
    # Pull temperature predictions (for all dates and all depths) for those lakes.
    species_temp_data <- pull_data_for_sites(nc_file, nc_info, var = 'temp', sites = lake_sites_split[[j]], long_format = TRUE)
    species_temp_data = species_temp_data[complete.cases(species_temp_data),]
    
    result <- species_temp_data %>%
      group_by(site_id, time) %>%
      summarise(
        min_temp = min(temperature, na.rm = TRUE),
        max_temp = max(temperature, na.rm = TRUE)
      ) 
    
    #result <- result[result$max_temp>10,]
    
    result$year = year(result$time)
    result$date = format(as.Date(result$time), "%m-%d")
    
    result = result %>%
      group_by(site_id,date) %>%
      mutate(list = ifelse(max(max_temp)<11, "cold", ifelse(min(min_temp)>30.3, "hot", "good")))
    
    print(c(i,j))
    
    # Add result to list with a unique identifier
    results_list_hot[[paste(i,j, sep = "_")]] <- result[result$list=="hot",]
    results_list_cold[[paste(i,j, sep = "_")]] <- result[result$list=="cold",]
    results_list_good[[paste(i,j, sep = "_")]] <- result[result$list=="good",]
    
  }
}  # ignore the two errors. it came out because of Rstudio


final_result_hot <- do.call(rbind, results_list_hot)
saveRDS(final_result_hot, file = output_file_hot)

final_result_cold <- do.call(rbind, results_list_cold)
saveRDS(final_result_cold, file = output_file_cold)

final_result_good <- do.call(rbind, results_list_good)
saveRDS(final_result_good, file = output_file_good)
