# the data needed for this code includes:
# 1. lake_metadata.csv; it is lake information (e.g., lon and lat, size, max depth...) data
# 2. all the fish range data stored in the folder Fish_range; it is big data 22.7 GB download from https://www.sciencebase.gov/catalog/item/get/5e2ef8e3e4b0a79317d421ac
# 3. HUC8 shape file with polygon infor HUC8_US.shp
# 4. spp_thermal_metrics.csv; fish species that have the thermal metric information 

# Load the necessary libraries
library(doParallel)
library(foreach)
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
library(tools)
library(purrr)
setwd("C:/Users/user/Desktop/OneDrive/Postdoctor/Project_3")


#### Function to match fish spatial range with lake locations ####
match_fish_to_lake <- function(species_name, lakes_sf, fish_range_df, huc8_shapefile) {
  species_spatial_range <- fish_range_df[fish_range_df$latin_name == species_name,]
  species_spatial_range$HUC8 <- str_pad(species_spatial_range$HUC8, width = 8, side = "left", pad = "0") # fix the HUC8 when some of the code only have 7 numbers
  
  # sf data for the species fish range
  huc8_species_sf <- huc8_shapefile[huc8_shapefile$HUC8 %in% unique(species_spatial_range$HUC8), ]
  
  # prevent loop polygons
  huc8_species_sf <- st_make_valid(huc8_species_sf)
  
  # Check if the points are within any HUC polygon in the group using st_join
  results_species <-  st_join(lakes_sf, huc8_species_sf, join = st_intersects)
  
  return(results_species)
}

#### 1. read lake location with nhdrn and lon, lat ####
lake_location = read.csv("Data/Environment/lake_metadata.csv")

# remove the lakes that is not included in the GLM analysis, it will get you 12679 lakes in total
lake_location = lake_location[lake_location$model_preds_glm_nldas=="TRUE",]
N_lakes = length(lake_location$site_id)

# sf data created for all lakes
lakes_sf <- st_as_sf(lake_location, coords = c("centroid_lon", "centroid_lat"), crs = 4326)

#### 2. read the fish spatial range with HUC8 ####
# Specify the path to the directory
path <- "C:/Users/user/Desktop/OneDrive/Postdoctor/Project_3/Data/Fish/Fish_range"

# Get a list of all csv file names in the directory
files <- list.files(path, pattern = "*.csv", full.names = TRUE)

# Read all csv files into a list of data frames
list_of_df <- lapply(files, read.csv)

# make the column in the same data type 
list_of_df <- lapply(list_of_df, function(df) {
  if ("PointData" %in% names(df)) {
    # Convert PointData column to character type
    df$PointData <- as.character(df$PointData)
  }
  
  if ("POINTDATA" %in% names(df)) {
    # Convert POINTDATA column to character type
    df$POINTDATA <- as.character(df$POINTDATA)
  }
  
  return(df)
})

# convert list to dataframe 
fish_range_df <- bind_rows(list_of_df)

#### 3. read HUC8 shape file with polygon infor ####
huc8_shapefile <- st_read("Data/Environment/HUC8/HUC8_US.shp")

#### 4. read the fish FTP ####
fish_ftp = read.csv('Data/spp_thermal_metrics.csv')
fish_ftp = fish_ftp[!is.na(fish_ftp$ftp),]

# check any rows with the same latin name but different ftp (because of different life stages)
# Add a row_number for later use
fish_ftp <- fish_ftp %>% mutate(row = row_number())

# Check if the same latin.name have different ftp
result <- fish_ftp %>%
  group_by(latin.name) %>%
  filter(n_distinct(ftp) > 1) %>%
  arrange(latin.name, row)

# print out the result
length(result$order) # it is empty

# Keep only unique latin.name
fish_ftp <- fish_ftp %>%
  distinct(latin.name, .keep_all = TRUE)

# Replace underscore with space and convert to title case
fish_ftp$latin.name <- toTitleCase(fish_ftp$latin.name)
fish_ftp$latin.name <- gsub("_", " ", fish_ftp$latin.name)

# check if the latin name is included in the fish range file 
# consolidate all species into one column
fish_range_df <- fish_range_df %>% 
  mutate(latin_name = coalesce(Species, SPECIES_NAME, SPECIES))
# the available species is 73
sum(fish_ftp$latin.name %in% fish_range_df$latin_name)
available_fish = fish_ftp[fish_ftp$latin.name%in%fish_range_df$latin_name,]$latin.name

#### 5. lake location matches fish range ####
# Initialize an empty list to store the results
results <- list()

# Run the function for each fish in the vector and store the results in the list
for (i in 1:length(available_fish)) {
  results[[available_fish[i]]] <- match_fish_to_lake(available_fish[i], lakes_sf, fish_range_df, huc8_shapefile)
  print(i)
}

#### 5. save the results ####
# Create an empty data frame to store results
fish_present_lake = data.frame(lake_id = rep(NA, length(results)*N_lakes))

# save the results
for (i in 1:length(results)){
  fish_present_lake$lake_id[((i-1)*N_lakes+1):(i*N_lakes)] = results[[i]]$site_id
  fish_present_lake$species[((i-1)*N_lakes+1):(i*N_lakes)] = available_fish[i]
  fish_present_lake$presence[((i-1)*N_lakes+1):(i*N_lakes)] = ifelse(is.na(results[[i]]$HUC8), 0, 1)
}

# write the results
write.csv(fish_present_lake,"Output/Fish_present_lake.csv",row.names = F)
