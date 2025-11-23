setwd("C:/Scratch/LLX/Project_X")
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

#Sys.setenv(TZ = "UTC") 

#### read  1. daily lake highest and lowest data, 2. fish thermal metric data and 3. fish spatial range data ####
### read the daily lake highest and lowest data ###
rds_files <- list.files("D:/Project_X/Output", pattern = "highest_lowest_temp.*\\.rds$", full.names = TRUE)
lake_temp <- lapply(rds_files, readRDS) %>%
  bind_rows() 
lake_temp <- lake_temp %>%
  mutate(month = month(time))
lake_temp_spring <- lake_temp[lake_temp$month%in%c(3,4,5),]
lake_temp_fall <- lake_temp[lake_temp$month%in%c(9,10,11),]

###read the fish thermal metrics ###
fish_thermal_metrics = readxl::read_xlsx('Data/thermal_metrics.xlsx')
lake_species_name = read.csv("Data/lake_species_name.csv")
lake_species_name$fish_names <- sub("\\(.*\\)", "", lake_species_name$Species_label)
fish_thermal_metrics = fish_thermal_metrics[fish_thermal_metrics$"Scientific Name"%in%lake_species_name$fish_names,]

### read the fish_presnet_lake ###
fish_present_lake = read.csv("Output/fish_present_lake.csv")
fish_present_lake = fish_present_lake[fish_present_lake$presence==1,]

### read the spawning season data ###
fish_thermal_metrics <- read.csv("fish_thermal_metrics.csv")
#### read data ####

#### calculate the annual preferred days ####
## the species both has os data and spatial range data
include_species = unique(fish_thermal_metrics$`Scientific Name`)[unique(fish_thermal_metrics$`Scientific Name`)%in%fish_present_lake$species]

# Initialize an empty data frame to store all results
all_results <- data.frame()

for (i in 1:length(include_species)) {
  #time1=Sys.time()
  species_name = fish_thermal_metrics[i,]$`Scientific Name`
  optimal_temp_growth <- fish_thermal_metrics[i,]$OGT
  #optimal_temp_egg <- fish_thermal_metrics[i,]$OE
  species_present_lake <- fish_present_lake[fish_present_lake$species == species_name,]
  data_length = length(species_present_lake$lake_id)
  
  species_temp_data <- lake_temp[lake_temp$site_id %in% species_present_lake$lake_id,]
    # Calculate summary statistics
  result <- species_temp_data %>%
    group_by(year, site_id) %>%
    summarise(
    good_days_growth = sum(min_temp < optimal_temp_growth & max_temp > optimal_temp_growth),
        .groups = "drop")
  
  # Add the species name to the result
  result <- result %>%
    mutate(species = species_name)
  
  # Combine results into the all_results data frame
  all_results <- bind_rows(all_results, result)
  #time2=Sys.time()
  result<-data.frame()
  print(paste( "processing lake number", data_length,"for",i))
  gc()
}

saveRDS(all_results, "Output/preferred_days_for_growth_1.rds")

for (i in 1:length(include_species)) {
  #time1=Sys.time()
  species_name = fish_thermal_metrics[i,]$`Scientific Name`
  #optimal_temp_growth <- fish_thermal_metrics[i,]$OGT
  optimal_temp_egg <- fish_thermal_metrics[i,]$OE
  species_present_lake <- fish_present_lake[fish_present_lake$species == species_name,]
  data_length = length(species_present_lake$lake_id)
  if (fish_thermal_metrics$`Spawning Season`[i]=="Spring"){
    species_temp_data <- lake_temp_spring[lake_temp_spring$site_id %in% species_present_lake$lake_id,]
  } else {
    species_temp_data <- lake_temp_fall[lake_temp_fall$site_id %in% species_present_lake$lake_id,]
  }
  # Calculate summary statistics
  result <- species_temp_data %>%
    group_by(year, site_id) %>%
    summarise(
      good_days_rep = sum(min_temp < optimal_temp_egg & max_temp > optimal_temp_egg),
      .groups = "drop")
  
  # Add the species name to the result
  result <- result %>%
    mutate(species = species_name )
  
  # Combine results into the all_results data frame
  all_results <- bind_rows(all_results, result)
  #time2=Sys.time()
  result<-data.frame()
  print(paste( "processing lake number", data_length,"for",i))
  gc()
}

saveRDS(all_results, "Output/preferred_days_for_rep_1.rds")
