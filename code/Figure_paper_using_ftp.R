setwd("C:/Scratch/LLX/Project_X")
library(dplyr)
library(tools)
library(ggplot2)
library(ggmap)
library(RColorBrewer)
library(scales)
library(grid)
library(gridExtra)
library(patchwork)
library(sf)
library(dplyr)
library(ggplot2)
library(egg)
library(cowplot)
library(tidyverse)
library(data.table)
library(viridis)
library(maps)

#### read the results 
### read the preferred days for reproduction and growth
rep_days = readRDS("Output/preferred_days_for_rep_1.rds")
growth_days = readRDS("Output/preferred_days_for_growth_1.rds")

#### read fish thermal metrics and process the data
###read the fish thermal metrics ###
fish_thermal_metrics = readxl::read_xlsx('Data/thermal_metrics.xlsx')
lake_species_name = read.csv("Data/lake_species_name.csv")
lake_species_name$fish_names <- sub("\\(.*\\)", "", lake_species_name$Species_label)
fish_thermal_metrics = fish_thermal_metrics[fish_thermal_metrics$"Scientific Name"%in%lake_species_name$fish_names,]

### assign fish to thermal guilds ###
fish_thermal_metrics <- fish_thermal_metrics %>%
  mutate(Temp_Category = cut(FTP, breaks = c(-Inf, 19, 24.99, Inf), labels = c("Cold-water fish", "Cool-water fish", "Warm-water fish")))

#### read meta data of the lake model (for plotting the map)
lake_meta_df = read.csv("Data/lake_metadata.csv")

### data processing ###
nod_result = left_join(rep_days,growth_days,c("year","site_id","species"))
nod_result <- nod_result %>%
  left_join(
    fish_thermal_metrics %>% select("Scientific Name", FTP, OGT, OE,Temp_Category),
    by = c("species" = "Scientific Name")
  ) %>%
  left_join(
    lake_meta_df %>% select(site_id, state, centroid_lon, centroid_lat, max_depth, area, elevation, clarity),
    by = "site_id"
  )

# 1980-2000 mean optimal days for each stock
mean_result_1980 <- nod_result %>%
  group_by(site_id, species) %>%
  filter(year %in% (1980:2000)) %>%  # filter for the first 21 years
  summarise(mean_rep_days_1980 = mean(good_days_rep, na.rm = TRUE),
            mean_growth_days_1980 = mean(good_days_growth, na.rm = TRUE),
            ftp = first(FTP),
            Temp_Category =  first(Temp_Category),
            ogt = first(OGT),
            oe =  first(OE),
            .groups = "drop")

# 2001-2021 mean optimal days for each stock
mean_result_2001 <- nod_result %>%
  group_by(site_id, species) %>%
  filter(year %in% (2001:2021)) %>%  # filter for the last 21 years
  summarise(mean_rep_days_2001 = mean(good_days_rep, na.rm = TRUE),
            mean_growth_days_2001 = mean(good_days_growth, na.rm = TRUE),
            .groups = "drop")

# Merge the two data frames to have a combined result
mean_result_every_21_years <- mean_result_1980 %>%
  left_join(mean_result_2001, by = c("site_id", "species"))

#### calculate the difference of preferred days for reproduction and growth
mean_result_every_21_years$dif_rep = mean_result_every_21_years$mean_rep_days_2001-mean_result_every_21_years$mean_rep_days_1980
mean_result_every_21_years$dif_rep_per = mean_result_every_21_years$dif_rep/max(mean_result_every_21_years$mean_rep_days_2001,mean_result_every_21_years$mean_rep_days_1980) # percentage change

mean_result_every_21_years$dif_growth = mean_result_every_21_years$mean_growth_days_2001-mean_result_every_21_years$mean_growth_days_1980
mean_result_every_21_years$dif_growth_per = mean_result_every_21_years$dif_growth/max(mean_result_every_21_years$mean_growth_days_2001,mean_result_every_21_years$mean_growth_days_1980) # percentage change

# Calculate median and standard deviation for each species within each Temp_Category
summary_data <- mean_result_every_21_years %>%
  group_by(Temp_Category, species) %>%
  summarize(
    median_dif_rep = mean(dif_rep,na.rm=TRUE),
    sd_dif_rep = sd(dif_rep,na.rm=TRUE),
    median_dif_growth = mean(dif_growth, na.rm=TRUE),
    sd_dif_growth = sd(dif_growth, na.rm=TRUE),
    
    median_dif_rep_per = mean(dif_rep_per,na.rm=TRUE),
    sd_dif_rep_per = sd(dif_rep_per,na.rm=TRUE),
    median_dif_growth_per = mean(dif_growth_per, na.rm=TRUE),
    sd_dif_growth_per = sd(dif_growth_per, na.rm=TRUE),
    
  )

summary_data <- summary_data %>%
  mutate(quadrant = case_when(
    median_dif_growth > 0 & median_dif_rep > 0 ~ "Q1",
    median_dif_growth < 0 & median_dif_rep > 0 ~ "Q2",
    median_dif_growth < 0 & median_dif_rep < 0 ~ "Q3",
    median_dif_growth > 0 & median_dif_rep < 0 ~ "Q4"
  ))

mean_result_every_21_years <- mean_result_every_21_years %>%
  mutate(
    quadrant = case_when(
      dif_growth >= 0 & dif_rep >= 0 ~ "Q1",
      dif_growth < 0 & dif_rep >= 0 ~ "Q2",
      dif_growth < 0 & dif_rep < 0 ~ "Q3",
      dif_growth >= 0 & dif_rep < 0 ~ "Q4"
    )
  )

# Calculate the percentage of each quadrant for each Temp_Category
quadrant_percentages <- mean_result_every_21_years %>%
  group_by(Temp_Category, quadrant) %>%
  summarize(count = n()) %>%
  group_by(Temp_Category) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()

### 
label_positions <- tibble(
  quadrant = c("Q1", "Q2", "Q3", "Q4"),
  x = c(8, -8, -8, 8),
  y = c(8, 8, -8, -8)
)


### 

# All combinations of Temp_Category and quadrant
quadrant_all <- expand.grid(
  Temp_Category = unique(summary_data$Temp_Category),
  quadrant = c("Q1", "Q2", "Q3", "Q4"),
  stringsAsFactors = FALSE
)

# Count species in each quadrant
quadrant_counts <- summary_data %>%
  count(Temp_Category, quadrant, name = "count") %>%
  right_join(quadrant_all, by = c("Temp_Category", "quadrant")) %>%
  mutate(count = replace_na(count, 0))


label_positions <- tibble(
  quadrant = c("Q1", "Q2", "Q3", "Q4"),
  x = c(8, -8, -8, 8),
  y = c(8, 8, -8, -8)+0.5
)

label_positions_per <- tibble(
  quadrant = c("Q1", "Q2", "Q3", "Q4"),
  x = c(8, -8, -8, 8),
  y = c(8, 8, -8, -8)-0.2
)

# label for species number 
quadrant_labels <- quadrant_counts %>%
  left_join(label_positions, by = "quadrant") %>%
  mutate(label = as.character(count))  

# label for population percentage 
quadrant_labels_per <- quadrant_percentages %>%
  left_join(label_positions_per, by = "quadrant") %>%
  mutate(label = sprintf("%.1f%%", percentage))
# Calculate dynamic x and y limits with padding
x_limits <- range(summary_data$median_dif_growth + summary_data$sd_dif_growth, 
                  summary_data$median_dif_growth - summary_data$sd_dif_growth, 
                  quadrant_labels$x, na.rm = TRUE)
x_pad <- 0.01 * diff(x_limits)
x_limits <- x_limits + c(-x_pad, x_pad)

y_limits <- range(summary_data$median_dif_rep + summary_data$sd_dif_rep, 
                  summary_data$median_dif_rep - summary_data$sd_dif_rep, 
                  quadrant_labels$y, na.rm = TRUE)
y_pad <- 0.01 * diff(y_limits)
y_limits <- y_limits + c(-y_pad, y_pad)

summary_data <- summary_data %>%
  mutate(quadrant_label = case_when(
    quadrant %in% c("Q1", "Q3") ~ "Aligned impact",
    quadrant %in% c("Q2", "Q4") ~ "Divergent impact"
  ))

summary_data$quadrant_label <- factor(summary_data$quadrant_label, levels = c("Aligned impact", "Divergent impact"))

summary_data <- summary_data %>%
  mutate(
    quadrant = case_when(
      median_dif_growth >= 0 & median_dif_rep >= 0 ~ "Q1",
      median_dif_growth < 0  & median_dif_rep >= 0 ~ "Q2",
      median_dif_growth < 0  & median_dif_rep <  0 ~ "Q3",
      median_dif_growth >= 0 & median_dif_rep <  0 ~ "Q4"
    )
  )


pic_result1_1 = ggplot(summary_data, aes(x = median_dif_growth, y = median_dif_rep, color = quadrant_label))+
  # Error bars first (in the background)
  geom_errorbar(aes(ymin = median_dif_rep - sd_dif_rep, ymax = median_dif_rep + sd_dif_rep),alpha = 0.3) +
  geom_errorbarh(aes(xmin = median_dif_growth - sd_dif_growth, xmax = median_dif_growth + sd_dif_growth),alpha = 0.3) +
  
  # Then the points (in the foreground)
  geom_point(size = 1) +
  
  # Reference lines
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  
  # Facet and axis settings
  facet_wrap(~ Temp_Category, nrow = 1) +
  labs(x = "Change in optimal days for growth", y = "Change in optimal days for reproduction") +
  theme_minimal() +
  
  # Color by quadrant
  scale_color_manual(
    values = c("Aligned impact" = "blue", "Divergent impact" = "red"),
    name = NULL  # or "Impact Type" if you want a title
  )  +
  
  geom_text(
    data = quadrant_labels,
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    size = 3
  )+
  geom_text(
    data = quadrant_labels_per,
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    size = 3
  )+
  
  # Dynamic axis limits
  coord_cartesian(xlim = c(-15,15), ylim = y_limits)+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

jpeg("Output/Figure/FTP/pic_result1_1_four_category_USING_FTP.jpeg",width=183,height=120,units = "mm",res=600)
pic_result1_1
dev.off()

### the overlap lakes change ###
setDT(mean_result_every_21_years)
medians_ninety <- mean_result_every_21_years [, .(
  median_day_growth_1980 = mean(mean_growth_days_1980, na.rm = TRUE),
  median_day_growth_2001 = mean(mean_growth_days_2001, na.rm = TRUE),
  median_day_rep_1980 = mean(mean_rep_days_1980, na.rm = TRUE),
  median_day_rep_2001 = mean(mean_rep_days_2001, na.rm = TRUE),
  
  sixty_day_growth_1980 = quantile(mean_growth_days_1980, 0.6, na.rm = TRUE),
  sixty_day_growth_2001 = quantile(mean_growth_days_2001, 0.6, na.rm = TRUE),
  sixty_day_rep_1980 = quantile(mean_rep_days_1980, 0.6, na.rm = TRUE),
  sixty_day_rep_2001 = quantile(mean_rep_days_2001, 0.6, na.rm = TRUE),
  
  seventy_day_growth_1980 = quantile(mean_growth_days_1980, 0.7, na.rm = TRUE),
  seventy_day_growth_2001 = quantile(mean_growth_days_2001, 0.7, na.rm = TRUE),
  seventy_day_rep_1980 = quantile(mean_rep_days_1980, 0.7, na.rm = TRUE),
  seventy_day_rep_2001 = quantile(mean_rep_days_2001, 0.7, na.rm = TRUE),
  
  eighty_day_growth_1980 = quantile(mean_growth_days_1980, 0.8, na.rm = TRUE),
  eighty_day_growth_2001 = quantile(mean_growth_days_2001, 0.8, na.rm = TRUE),
  eighty_day_rep_1980 = quantile(mean_rep_days_1980, 0.8, na.rm = TRUE),
  eighty_day_rep_2001 = quantile(mean_rep_days_2001, 0.8, na.rm = TRUE),
  
  ninety_day_growth_1980 = quantile(mean_growth_days_1980, 0.9, na.rm = TRUE),
  ninety_day_growth_2001 = quantile(mean_growth_days_2001, 0.9, na.rm = TRUE),
  ninety_day_rep_1980 = quantile(mean_rep_days_1980, 0.9, na.rm = TRUE),
  ninety_day_rep_2001 = quantile(mean_rep_days_2001, 0.9, na.rm = TRUE)
), by=.(species)]

overlap_lake = left_join(mean_result_every_21_years,medians_ninety,"species")
above_median_lake <- overlap_lake [
  , .(
    above_median_day_growth_1980 = sum(mean_growth_days_1980 > median_day_growth_1980, na.rm = TRUE),
    above_median_day_growth_2001 = sum(mean_growth_days_2001 > median_day_growth_2001, na.rm = TRUE), 
    above_median_day_rep_1980 = sum(mean_rep_days_1980 > median_day_rep_1980, na.rm = TRUE),
    above_median_day_rep_2001 = sum(mean_rep_days_2001 > median_day_rep_2001, na.rm = TRUE), 
    overlap_1980_median = sum(
      mean_growth_days_1980 > median_day_growth_1980 & 
        mean_rep_days_1980 > median_day_rep_1980, 
      na.rm = TRUE
    ),
    overlap_2001_median = sum(
      mean_growth_days_2001 > median_day_growth_2001 & # switch year 1980 and 2001
        mean_rep_days_2001 > median_day_rep_2001, # switch year 1980 and 2001
      na.rm = TRUE
    ),
    
    above_sixty_day_growth_1980 = sum(mean_growth_days_1980 > sixty_day_growth_1980, na.rm = TRUE),
    above_sixty_day_growth_2001 = sum(mean_growth_days_2001 > sixty_day_growth_2001, na.rm = TRUE),
    above_sixty_day_rep_1980 = sum(mean_rep_days_1980 > sixty_day_rep_1980, na.rm = TRUE),
    above_sixty_day_rep_2001 = sum(mean_rep_days_2001 > sixty_day_rep_2001, na.rm = TRUE),
    overlap_1980_sixty = sum(
      mean_growth_days_1980 > sixty_day_growth_1980 & 
        mean_rep_days_1980 > sixty_day_rep_1980, 
      na.rm = TRUE
    ),
    overlap_2001_sixty = sum(
      mean_growth_days_2001 > sixty_day_growth_2001 & 
        mean_rep_days_2001 > sixty_day_rep_2001, 
      na.rm = TRUE
    ),
    
    
    above_seventy_day_growth_1980 = sum(mean_growth_days_1980 > seventy_day_growth_1980, na.rm = TRUE),
    above_seventy_day_growth_2001 = sum(mean_growth_days_2001 > seventy_day_growth_2001, na.rm = TRUE),
    above_seventy_day_rep_1980 = sum(mean_rep_days_1980 > seventy_day_rep_1980, na.rm = TRUE),
    above_seventy_day_rep_2001 = sum(mean_rep_days_2001 > seventy_day_rep_2001, na.rm = TRUE),
    overlap_1980_seventy = sum(
      mean_growth_days_1980 > seventy_day_growth_1980 & 
        mean_rep_days_1980 > seventy_day_rep_1980, 
      na.rm = TRUE
    ),
    overlap_2001_seventy = sum(
      mean_growth_days_2001 > seventy_day_growth_2001 & 
        mean_rep_days_2001 > seventy_day_rep_2001, 
      na.rm = TRUE
    ),
    
    
    above_eighty_day_growth_1980 = sum(mean_growth_days_1980 > eighty_day_growth_1980, na.rm = TRUE),
    above_eighty_day_growth_2001 = sum(mean_growth_days_2001 > eighty_day_growth_2001, na.rm = TRUE),
    above_eighty_day_rep_1980 = sum(mean_rep_days_1980 > eighty_day_rep_1980, na.rm = TRUE),
    above_eighty_day_rep_2001 = sum(mean_rep_days_2001 > eighty_day_rep_2001, na.rm = TRUE),
    overlap_1980_eighty = sum(
      mean_growth_days_1980 > eighty_day_growth_1980 & 
        mean_rep_days_1980 > eighty_day_rep_1980, 
      na.rm = TRUE
    ),
    overlap_2001_eighty = sum(
      mean_growth_days_2001 > eighty_day_growth_2001 & 
        mean_rep_days_2001 > eighty_day_rep_2001, 
      na.rm = TRUE
    ),
    
    
    above_ninety_day_growth_1980 = sum(mean_growth_days_1980 > ninety_day_growth_1980, na.rm = TRUE),
    above_ninety_day_growth_2001 = sum(mean_growth_days_2001 > ninety_day_growth_2001, na.rm = TRUE),
    above_ninety_day_rep_1980 = sum(mean_rep_days_1980 > ninety_day_rep_1980, na.rm = TRUE),
    above_ninety_day_rep_2001 = sum(mean_rep_days_2001 > ninety_day_rep_2001, na.rm = TRUE),
    overlap_1980_ninety = sum(
      mean_growth_days_1980 > ninety_day_growth_1980 & 
        mean_rep_days_1980 > ninety_day_rep_1980, 
      na.rm = TRUE
    ),
    overlap_2001_ninety = sum(
      mean_growth_days_2001 > ninety_day_growth_2001 & 
        mean_rep_days_2001 > ninety_day_rep_2001, 
      na.rm = TRUE
    )
    
  ), by=.(species)]


summary(above_median_lake$overlap_2001_median-above_median_lake$overlap_1980_median)

sum(above_median_lake$overlap_2001_median-above_median_lake$overlap_1980_median>0)


above_median_lake <- above_median_lake %>%
  left_join(
    fish_thermal_metrics %>% select("Scientific Name", FTP, OGT, OE),
    by = c("species" = "Scientific Name")
  ) 

above_median_lake <- above_median_lake %>%
  mutate(Temp_Category = cut(FTP, breaks = c(-Inf, 19, 24.99, Inf), labels = c("Cold-water fish", "Cool-water fish", "Warm-water fish")))
above_median_lake$Species_label = paste(above_median_lake$species, "(", sprintf("%.1f", above_median_lake$FTP),")", sep = "")

# data for figure
overlap_lake_figure_1 = above_median_lake[,c("FTP","OGT","OE","Temp_Category","species","Species_label")]
overlap_lake_figure = bind_rows(replicate(5, overlap_lake_figure_1, simplify = FALSE))
overlap_lake_figure$quan = rep(c("10%","20%","30%","40%","50%"), each=length(above_median_lake$species))
overlap_lake_figure$change = NA


species_number = length(above_median_lake$species)

overlap_lake_figure$change[1:species_number] = (above_median_lake$overlap_2001_ninety-above_median_lake$overlap_1980_ninety)/pmax(above_median_lake$overlap_2001_ninety,above_median_lake$overlap_1980_ninety)
overlap_lake_figure$change[(species_number+1):(2*species_number)] = (above_median_lake$overlap_2001_eighty-above_median_lake$overlap_1980_eighty)/pmax(above_median_lake$overlap_2001_eighty,above_median_lake$overlap_1980_eighty)
overlap_lake_figure$change[(2*species_number+1):(3*species_number)] = (above_median_lake$overlap_2001_seventy-above_median_lake$overlap_1980_seventy)/pmax(above_median_lake$overlap_2001_seventy,above_median_lake$overlap_1980_seventy)
overlap_lake_figure$change[(3*species_number+1):(4*species_number)] = (above_median_lake$overlap_2001_sixty-above_median_lake$overlap_1980_sixty)/pmax(above_median_lake$overlap_2001_sixty,above_median_lake$overlap_1980_sixty)
overlap_lake_figure$change[(4*species_number+1):(5*species_number)] = (above_median_lake$overlap_2001_median-above_median_lake$overlap_1980_median)/pmax(above_median_lake$overlap_2001_median,above_median_lake$overlap_1980_median)

# overlap_lake_figure$change[1:species_number] = (above_median_lake$overlap_2001_ninety-above_median_lake$overlap_1980_ninety)
# overlap_lake_figure$change[(species_number+1):(2*species_number)] = (above_median_lake$overlap_2001_eighty-above_median_lake$overlap_1980_eighty)
# overlap_lake_figure$change[(2*species_number+1):(3*species_number)] = (above_median_lake$overlap_2001_seventy-above_median_lake$overlap_1980_seventy)
# overlap_lake_figure$change[(3*species_number+1):(4*species_number)] = (above_median_lake$overlap_2001_sixty-above_median_lake$overlap_1980_sixty)
# overlap_lake_figure$change[(4*species_number+1):(5*species_number)] = (above_median_lake$overlap_2001_median-above_median_lake$overlap_1980_median)


pic_result2_1 <- ggplot(overlap_lake_figure[overlap_lake_figure$quan=="50%",], aes(x = reorder(Species_label, FTP), y = change*100 )) +
  geom_col(aes(fill = ifelse(change > 0, "blue", "red")), width = 0.9) +
  #geom_hline(yintercept = mean_positive, linetype = "dashed", color = "blue", size = 0.5) +
  #geom_hline(yintercept = mean_negative, linetype = "dashed", color = "red", size = 0.5) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = "none",
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.grid = element_blank(),
        legend.title = element_blank()) +
  scale_fill_identity() +
  labs(title = "",
       x = "Species",
       y = "Change in lakes with above-average habitats\n for both growth and reproduction(%)") +
  #ylim(-13,13)+
  #coord_cartesian(ylim = c(-10, 10)) +
  #facet_wrap(~ quan, 5,scales = "free_y") 
  annotate("rect", xmin = -Inf, xmax = 16.5, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "blue") +
  annotate("rect", xmin = 16.5, xmax = 39.5, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "springgreen1") +
  annotate("rect", xmin = 39.5 , xmax = Inf , ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "coral") +
  geom_text(aes(x = 5, y = 21, label = "4 species"),color="blue") +
  geom_text(aes(x = 5, y = -21, label = "12 species"),color="red") +
  geom_text(aes(x = 22, y = 21, label = "6 species"),color="blue") +
  geom_text(aes(x = 22, y = -21, label = "17 species"),color="red") +
  geom_text(aes(x = 45, y = 21, label = "11 species"),color="blue") +
  geom_text(aes(x = 45, y = -21, label = "10 species"),color="red") 

jpeg("Output/Figure/FTP/pic_result2_1_change_overlap_lakes.jpeg",width=183,height=130,units = "mm",res=600)
pic_result2_1
dev.off()

### map ###
n_distinct_pairs <- mean_result_every_21_years %>%
  distinct(site_id, species) %>%
  nrow()

mean_result_every_21_years %>%
  distinct(site_id, species, Temp_Category) %>%
  count(Temp_Category)

map_figure_df <- mean_result_every_21_years %>%
  left_join(
    fish_thermal_metrics %>% select("Scientific Name"),
    by = c("species" = "Scientific Name")
  ) %>%
  left_join(
    lake_meta_df %>% select(site_id, state, centroid_lon, centroid_lat, max_depth, area, elevation, clarity),
    by = "site_id"
  )

map_guild_rep_df  <- map_figure_df %>%
  group_by(site_id, Temp_Category) %>%
  summarise(
    mean_Trend = mean(dif_rep, na.rm = TRUE),
    centroid_lat = first(centroid_lat),
    centroid_lon = first(centroid_lon)
  )
quantile(map_guild_rep_df$mean_Trend,c(0.0005,0.1,0.9,0.9995))
# change the extreme values to 20 or -20
map_guild_rep_df  <- map_guild_rep_df  %>%
  mutate(mean_Trend_adjust_value_10 = case_when(
    mean_Trend > 5 ~ 5,
    mean_Trend < -5 ~ -5,
    TRUE ~ mean_Trend
  ))

wi <- map_data("state")

g_rep = ggplot() +
  geom_polygon(data = wi, aes(long, lat, group = group), fill = "gray", color = "black") +  # US states
  geom_point(data = map_guild_rep_df, aes(x = centroid_lon, y = centroid_lat, color = mean_Trend_adjust_value_10), size = 0.025) +
  coord_fixed(ratio = 1, xlim = c(-105, -80), ylim = c(30, 48.5)) +  # Adjust xlim and ylim as needed
  facet_wrap(~Temp_Category, nrow = 3) +  # Use custom labeller for panel titles
  xlab("Longitude (°W)") +
  ylab("Latitude (°N)") +
  scale_color_viridis_c(
    option = "cividis",  # or "viridis", "magma", "cividis"
    name = "",
    limits = c(-5, 5),
    breaks = c(-5, -2.5, 0, 2.5,5),
    labels = c("-5>", "-2.5", "0", "2.5", "5<"),
    direction = -1  # flip if needed to match visual preference
  )+
  theme(
    panel.background = element_blank(),
    plot.background = element_blank(),
    panel.grid = element_blank(),
    legend.position = "top"
  ) +
  labs(tag="a")

map_guild_growth_df  <- map_figure_df %>%
  group_by(site_id, Temp_Category) %>%
  summarise(
    mean_Trend = mean(dif_growth, na.rm = TRUE),
    centroid_lat = first(centroid_lat),
    centroid_lon = first(centroid_lon)
  )
quantile(map_guild_growth_df$mean_Trend,c(0.05,0.1,0.9,0.95))
# change the extreme values to 20 or -20
map_guild_growth_df  <- map_guild_growth_df  %>%
  mutate(mean_Trend_adjust_value_10 = case_when(
    mean_Trend > 10 ~ 10,
    mean_Trend < -10 ~ -10,
    TRUE ~ mean_Trend
  ))

g_growth <- ggplot() +
  geom_polygon(data = wi, aes(long, lat, group = group), fill = "gray", color = "black") +  # US states
  geom_point(data = map_guild_growth_df, aes(x = centroid_lon, y = centroid_lat, color = mean_Trend_adjust_value_10), size = 0.025) +
  coord_fixed(ratio = 1, xlim = c(-105, -80), ylim = c(30, 48.5)) +  # Adjust xlim and ylim as needed
  facet_wrap(~Temp_Category, nrow = 3) +  # Use custom labeller for panel titles
  xlab("Longitude (°W)") +
  ylab("Latitude (°N)") +
  scale_color_viridis_c(
    option = "cividis",  # or "viridis", "magma", "cividis"
    name = "",
    limits = c(-10, 10),
    breaks = c(-10, -5, 0, 5, 10),
    labels = c("-10>", "-5", "0", "5", "10<"),
    direction = -1  # flip if needed to match visual preference
  )+
  theme(
    panel.background = element_blank(),
    plot.background = element_blank(),
    panel.grid = element_blank(),
    legend.position = "top"
  ) +
  labs(tag="b")

# Combine the plots
combined_plot <- plot_grid(g_rep, g_growth, ncol = 2, rel_heights = c(1,1), rel_widths = c(1, 1))

# figure for positive and negative only
map_guild_rep_df2 <- map_guild_rep_df %>%
  mutate(Direction = case_when(
    mean_Trend_adjust_value_10 > (-0.0001) ~ "Increase",
    mean_Trend_adjust_value_10 < 0 ~ "Decrease",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(Direction))

g_rep <- ggplot() +
  geom_polygon(data = wi, aes(long, lat, group = group),
               fill = "gray90", color = "gray40", linewidth = 0.2) +
  geom_point(data = map_guild_rep_df2,
             aes(x = centroid_lon, y = centroid_lat, color = Direction),
             size = 0.25, alpha = 0.8) +
  coord_fixed(ratio = 1, xlim = c(-105, -80), ylim = c(30, 48.5)) +
  facet_wrap(~ Temp_Category, nrow = 3) +
  xlab("Longitude (°W)") + ylab("Latitude (°N)") +
  scale_color_manual(
    values = c("Increase" = "#2C7BB6",  # colorblind-friendly blue
               "Decrease" = "#D7191C"), # colorblind-friendly red
    name = ""
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.background = element_blank(),
    plot.background  = element_blank(),
    panel.grid       = element_blank(),
    legend.position  = c(0.13, 0.79),
    legend.spacing.y = unit(-0.3, "cm") 
  ) +
  guides(color = guide_legend(
    override.aes = list(size = 0.6)  # legend point size only
  ))

# --- GROWTH MAP (recompute mean, cap, two colors) ---
map_guild_growth_df <- map_figure_df %>%
  group_by(site_id, Temp_Category) %>%
  summarise(
    mean_Trend = mean(dif_growth, na.rm = TRUE),
    centroid_lat = first(centroid_lat),
    centroid_lon = first(centroid_lon),
    .groups = "drop"
  ) %>%
  mutate(
    mean_Trend_adjust_value_10 = pmax(pmin(mean_Trend, 10), -10),
    Direction = case_when(
      mean_Trend_adjust_value_10 > (-0.0001) ~ "Increase",
      mean_Trend_adjust_value_10 < 0 ~ "Decrease",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Direction))

g_growth <- ggplot() +
  geom_polygon(data = wi, aes(long, lat, group = group),
               fill = "gray90", color = "gray40", linewidth = 0.2) +
  geom_point(data = map_guild_growth_df,
             aes(x = centroid_lon, y = centroid_lat, color = Direction),
             size = 0.25, alpha = 0.8) +
  coord_fixed(ratio = 1, xlim = c(-105, -80), ylim = c(30, 48.5)) +
  facet_wrap(~ Temp_Category, nrow = 3) +
  xlab("Longitude (°W)") + ylab("Latitude (°N)") +
  scale_color_manual(
    values = c("Increase" = "#2C7BB6",
               "Decrease" = "#D7191C"),
    name = ""
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.background = element_blank(),
    plot.background  = element_blank(),
    panel.grid       = element_blank(),
    legend.position  = c(0.13, 0.79),
    legend.spacing.y = unit(-13, "cm") 
  ) +
  guides(color = guide_legend(
    override.aes = list(size = 0.6)  # legend point size only
  ))

# --- Combine and save ---
combined_plot <- plot_grid(g_rep, g_growth, ncol = 2, rel_widths = c(1, 1))

# Add main titles directly to each plot
g_rep  <- g_rep  + ggtitle("Reproduction")
g_growth <- g_growth + ggtitle("Growth")

# Combine side by side
combined_plot <- plot_grid(
  g_rep, g_growth,
  ncol = 2,
  labels = c("A", "B"),      # keep subplot tags
  label_size = 10,
  rel_widths = c(1, 1)
)

# save the plot
jpeg("Output/Figure/FTP/Figure_map_result2_2.jpeg",width=183,height=183,units = "mm",res=600)
combined_plot 
dev.off()

### map for each species ###
# Calculate species order based on mean FTP
species_ftp_order <- map_figure_df %>%
  group_by(species) %>%
  summarize(mean_ftp = mean(ftp, na.rm = TRUE)) %>%
  arrange(mean_ftp) %>%
  pull(species)

map_species_growth_df <- map_figure_df %>%
  mutate(
    Direction = case_when(
      dif_growth > (-0.0001) ~ "Increase",
      dif_growth < 0 ~ "Decrease",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Direction))


map_species_rep_df <- map_figure_df %>%
  mutate(
    Direction = case_when(
      dif_rep > -(0.0001) ~ "Increase",
      dif_rep < 0 ~ "Decrease",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Direction))

# Function to create a plot for a given species group, ensuring species order by FTP
create_plots_for_group <- function(data_growth, data_rep, group_id, species_order) {
  # Reorder species labels in the data subset according to the specified order
  data_growth$species<- factor(data_growth$species, levels = species_order)
  data_rep$species<- factor(data_rep$species, levels = species_order)
  
  g_growth <- ggplot() +
    geom_polygon(data = wi, aes(long, lat, group = group),
                 fill = "gray90", color = "gray40", linewidth = 0.2) +
    geom_point(data = data_growth,
               aes(x = centroid_lon, y = centroid_lat, color = Direction),
               size = 0.25, alpha = 0.8) +
    coord_fixed(ratio = 1, xlim = c(-105, -80), ylim = c(30, 48.5)) +
    facet_wrap(~ species, nrow = 3) +
    xlab("Longitude (°W)") + ylab("Latitude (°N)") +
    scale_color_manual(
      values = c("Increase" = "#2C7BB6",
                 "Decrease" = "#D7191C"),
      name = ""
    ) +
    theme_minimal(base_size = 10) +
    theme(
      panel.background = element_blank(),
      plot.background  = element_blank(),
      panel.grid       = element_blank(),
      legend.position  = c(0.13, 0.79),
      legend.spacing.y = unit(-13, "cm") 
    ) +
    guides(color = guide_legend(
      override.aes = list(size = 0.6)  # legend point size only
    ))
  
  g_rep <- ggplot() +
    geom_polygon(data = wi, aes(long, lat, group = group),
                 fill = "gray90", color = "gray40", linewidth = 0.2) +
    geom_point(data = data_rep,
               aes(x = centroid_lon, y = centroid_lat, color = Direction),
               size = 0.25, alpha = 0.8) +
    coord_fixed(ratio = 1, xlim = c(-105, -80), ylim = c(30, 48.5)) +
    facet_wrap(~ species, nrow = 3) +
    xlab("Longitude (°W)") + ylab("Latitude (°N)") +
    scale_color_manual(
      values = c("Increase" = "#2C7BB6",
                 "Decrease" = "#D7191C"),
      name = ""
    ) +
    theme_minimal(base_size = 10) +
    theme(
      panel.background = element_blank(),
      plot.background  = element_blank(),
      panel.grid       = element_blank(),
      legend.position  = c(0.13, 0.79),
      legend.spacing.y = unit(-13, "cm") 
    ) +
    guides(color = guide_legend(
      override.aes = list(size = 0.6)  # legend point size only
    ))
  
  # Add main titles directly to each plot
  g_rep  <- g_rep  + ggtitle("Reproduction")
  g_growth <- g_growth + ggtitle("Growth")
  
  # Combine side by side
  combined_plot <- plot_grid(
    g_rep, g_growth,
    ncol = 2,
    labels = c("A", "B"),      # keep subplot tags
    label_size = 10,
    rel_widths = c(1, 1)
  )
  return(combined_plot)
}

# Create and save plots for each group, ensuring correct species order within each plot
for (i in 1:20) {
  species_subset <- species_ftp_order[((i-1)*3+1):(i*3)]
  data_subset_growth <- map_species_growth_df[map_species_growth_df$species %in% species_subset,]
  data_subset_rep <- map_species_rep_df[map_species_rep_df$species %in% species_subset,]
  plot <- create_plots_for_group(data_subset_growth, data_subset_rep,i, species_subset)
  o = 100+i
  ggsave(paste0("Output/Figure/FTP/species_group_", o, ".png"), plot, width = 7, height = 7)
}

### Figure_histogram_big_loss_small_win ###
hist_figure_df <- mean_result_every_21_years %>%
  group_by(site_id, Temp_Category) %>%
  summarise(
    dif_rep = mean(dif_rep, na.rm = TRUE),
    dif_growth = mean(dif_growth, na.rm = TRUE)
  )
quantile(hist_figure_df$dif_rep,c(0.05,0.1,0.9,0.95))
quantile(hist_figure_df$dif_growth,c(0.05,0.1,0.9,0.95))

hist_figure_df <- hist_figure_df  %>%
  mutate(dif_rep_adjust = case_when(
    dif_rep > 3 ~ 3,
    dif_rep < -3 ~ -3,
    TRUE ~ dif_rep
  ),
  dif_growth_adjust = case_when(
    dif_growth > 10 ~ 10,
    dif_growth < -10 ~ -10,
    TRUE ~ dif_growth
  ))

# Mean lines for reproduction
mean_rep_df <- hist_figure_df %>%
  group_by(Temp_Category) %>%
  summarise(mean_val = median(dif_rep_adjust, na.rm = TRUE))

# Mean lines for growth
mean_growth_df <- hist_figure_df %>%
  group_by(Temp_Category) %>%
  summarise(mean_val = median(dif_growth_adjust, na.rm = TRUE))


# Generate the histogram
g_result3_hist_rep <- ggplot(data = hist_figure_df, aes(x = dif_rep_adjust)) +
  geom_histogram(aes(fill = pmin(..x.., 100)), alpha = 1, bins = 30) +
  scale_x_continuous(
    limits = c(-3.2, 3.2),
    breaks = c(-3, -1.5, 0, 1.5, 3),
    labels = c("-3>", "-1.5", "0", "1.5", "3<")
  ) +
  scale_fill_viridis_c(
    option = "viridis",
    #option = "cividis",  # Or try "cividis", "magma", or "viridis"
    limits = c(-3, 3),
    breaks = c(-3, -1.5, 0, 1.5, 3),
    labels = c("-3>", "-1.5", "0", "1.5", "3<"),
    direction = 1
  ) +
  theme_minimal() +
  theme(
    #strip.text = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none",
    legend.title = element_blank()
  ) +
  geom_vline(aes(xintercept = 0), color = "black", alpha = 0.99, size = 1, linetype = "dashed") +
  xlab("Change in preferred days for reproduction") +
  ylab("Number of lakes") +
  facet_wrap(~Temp_Category, nrow = 1) 

# Generate the histogram
g_result3_hist_growth <- ggplot(data = hist_figure_df, aes(x = dif_growth_adjust)) +
  geom_histogram(aes(fill = pmin(..x.., 100)), alpha = 1, bins = 30) +
  scale_x_continuous(
    limits = c(-10.5, 10.5),
    breaks = c(-10, -5, 0, 5, 10),
    labels = c("-10>", "-5", "0", "5", "10<")
  ) +
  scale_fill_viridis_c(
    option = "viridis",
    #option = "cividis",  # Or try "cividis", "magma", or "viridis"
    limits = c(-10, 10),
    breaks = c(-10, -5, 0, 5, 10),
    labels = c("-10>", "-5", "0", "5", "10<"),
    direction = 1
  ) +
  theme_minimal() +
  theme(
    # strip.text = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none",
    legend.title = element_blank()
  ) +
  geom_vline(aes(xintercept = 0), color = "black", alpha = 0.99, size = 1, linetype = "dashed") +
  xlab("Change in preferred days for growth") +
  ylab("Number of lakes") +
  facet_wrap(~Temp_Category, nrow = 1) 

g_result3_hist_rep  <- g_result3_hist_rep  + ggtitle("Reproduction")
g_result3_hist_growth <- g_result3_hist_growth + ggtitle("Growth")

max_y <- max(
  ggplot_build(g_result3_hist_rep)$data[[1]]$count,
  ggplot_build(g_result3_hist_growth)$data[[1]]$count
)
g_result3_hist_rep  <- g_result3_hist_rep  + coord_cartesian(ylim = c(0, max_y))
g_result3_hist_growth <- g_result3_hist_growth + coord_cartesian(ylim = c(0, max_y))

combined_plot_2 <- plot_grid(
  g_result3_hist_rep, g_result3_hist_growth,
  ncol = 1,
  labels = c("A", "B"),      # keep subplot tags
  label_size = 10,
  rel_widths = c(1, 1)
)

# save the plot
jpeg("Output/Figure/FTP/Figure_histogram_result3_1.jpeg",width=183,height=160,units = "mm",res=600)
combined_plot_2
dev.off()

# Count negative impacts by Temp_Category and Habitat
growth_neg <- mean_result_every_21_years %>%
  mutate(Direction = ifelse(dif_growth >= 0, "Positive", "Negative")) %>%
  count(Temp_Category, Habitat = "Growth", Direction)

rep_neg <- mean_result_every_21_years %>%
  mutate(Direction = ifelse(dif_rep >= 0, "Positive", "Negative")) %>%
  count(Temp_Category, Habitat = "Reproduction", Direction)

# Combine and calculate percentages
count_df <- bind_rows(growth_neg, rep_neg) %>%
  group_by(Temp_Category, Habitat) %>%
  mutate(Percentage = round(n / sum(n), 3) * 100) %>%
  filter(Direction == "Negative")  # Only keep negative impacts

# Plot
g_result1_1_compare_habitat_type = ggplot(count_df, aes(x = Habitat, y = Percentage, fill = Habitat)) +
  geom_col(width = 0.7, fill = "gray") +
  geom_text(
    aes(label = paste0(Percentage, "%")),
    vjust = -0.3,
    size = 4,
    fontface = "bold"
  ) +
  facet_wrap(~ Temp_Category) +
  labs(
    x = "Habitat type",
    y = "Percentage of populations with reduced habitat",
    title = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

jpeg("Output/Figure/g_result1_1_compare_habitat_type.jpeg",width=183,height=110,units = "mm",res=600)
g_result1_1_compare_habitat_type
dev.off()

# increase decrease and no change
growth_df <- mean_result_every_21_years %>%
  mutate(Direction = case_when(
    dif_growth > 0 ~ "Positive",
    dif_growth < 0 ~ "Negative",
    TRUE ~ "No change"
  ),
  Habitat = "Growth")

rep_df <- mean_result_every_21_years %>%
  mutate(Direction = case_when(
    dif_rep > 0 ~ "Positive",
    dif_rep < 0 ~ "Negative",
    TRUE ~ "No change"
  ),
  Habitat = "Reproduction")

# Combine and count
count_df <- bind_rows(growth_df, rep_df) %>%
  count(Temp_Category, Habitat, Direction) %>%
  group_by(Temp_Category, Habitat) %>%
  mutate(Percentage = round(n / sum(n) * 100, 1))

# Plot
# Combine and count
count_df <- bind_rows(growth_df, rep_df) %>%
  count(Temp_Category, Habitat, Direction) %>%
  group_by(Temp_Category, Habitat) %>%
  mutate(Percentage = round(n / sum(n) * 100, 1)) %>%
  ungroup()

# Reorder stacking: Negative (bottom), Positive (middle), No change (top)
count_df$Direction <- factor(count_df$Direction, levels = c("No change", "Positive", "Negative"))

g_result1_1_compare_habitat_type <- ggplot(count_df, aes(x = Habitat, y = Percentage, fill = Direction)) +
  geom_col(position = "stack", width = 0.7) +
  geom_text(
    aes(label = paste0(Percentage, "%")),
    position = position_stack(vjust = 0.5),
    size = 3,
    fontface = "bold"
  ) +
  facet_wrap(~ Temp_Category) +
  labs(
    x = "Habitat type",
    y = "Percentage of populations",
    title = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# Save plot
jpeg("Output/Figure/g_result1_1_compare_habitat_type_all_results.jpeg", width = 183, height = 110, units = "mm", res = 600)
print(g_result1_1_compare_habitat_type)
dev.off()

# Growth
growth_df <- mean_result_every_21_years %>%
  filter(dif_growth != 0) %>%   # remove no change
  mutate(Direction = ifelse(dif_growth > 0, "Positive", "Negative"),
         Habitat = "Growth")

# Reproduction
rep_df <- mean_result_every_21_years %>%
  filter(dif_rep != 0) %>%      # remove no change
  mutate(Direction = ifelse(dif_rep > 0, "Positive", "Negative"),
         Habitat = "Reproduction")

# Combine, count, and calculate percentage
count_df <- bind_rows(growth_df, rep_df) %>%
  count(Temp_Category, Habitat, Direction) %>%
  group_by(Temp_Category, Habitat) %>%
  mutate(Percentage = round(n / sum(n) * 100, 1)) %>%
  ungroup()

# Ensure order: Negative first, then Positive
count_df$Direction <- factor(count_df$Direction, levels = c("Negative", "Positive"))

# Plot
g_result1_1_compare_habitat_type <- ggplot(count_df, aes(x = Habitat, y = Percentage, fill = Direction)) +
  geom_col(position = "stack", width = 0.7) +
  geom_text(
    aes(label = paste0(Percentage, "%")),
    position = position_stack(vjust = 0.5),
    size = 3,
    fontface = "bold"
  ) +
  facet_wrap(~ Temp_Category) +
  labs(
    x = "Habitat type",
    y = "Percentage of populations"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

g_result1_1_compare_habitat_type

### the figure showing the decrease habitat for each species and each thermal guild together
# species
growth_neg <- mean_result_every_21_years %>%
  mutate(Direction = ifelse(dif_growth >= 0, "Positive", "Negative")) %>%
  count(species, Habitat = "Growth", Direction)

rep_neg <- mean_result_every_21_years %>%
  mutate(Direction = ifelse(dif_rep >= 0, "Positive", "Negative")) %>%
  count(species, Habitat = "Reproduction", Direction)

# Combine and calculate percentages
count_df <- bind_rows(growth_neg, rep_neg) %>%
  group_by(species, Habitat) %>%
  mutate(Percentage = round(n / sum(n), 3) * 100) %>%
  filter(Direction == "Negative")  # Only keep negative impacts

# Plot
# Keep only reduced habitat (Direction == "Negative")
plot_df <- count_df %>%
  filter(Direction == "Negative") %>%
  mutate(
    Habitat = factor(Habitat, levels = c("Growth", "Reproduction")),
    # Flip sign for Growth so it plots to the left
    PlotValue = ifelse(Habitat == "Growth", -Percentage, Percentage)
  ) %>%
  left_join(fish_thermal_metrics,c("species"="Scientific Name")) %>%
  mutate(Species_label = paste(species, "(", sprintf("%.1f", FTP),")", sep = ""))


# Order species by max absolute % (helps readability)
plot_df <- plot_df %>%
  group_by(species) %>%
  mutate(order_val = max(abs(PlotValue), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(species = fct_reorder(species, order_val))

g_result1_1_compare_habitat_type_species <- ggplot(
  plot_df,
  aes(x = PlotValue, y = reorder(Species_label,FTP), fill = Habitat)
) +
  geom_col(width = 0.7, position = "identity") +
  geom_text(
    aes(label = paste0(Percentage, "%")),
    hjust = ifelse(plot_df$Habitat == "Growth", 1.1, -0.1),  # left vs right
    size = 3
    #fontface = "bold"
  ) +
  scale_x_continuous(
    limits = c(-100, 100),
    labels = abs,  # show positive labels on both sides
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  labs(
    x = "Percentage of populations with reduced habitats",
    y = "Species",
    title = NULL
  ) +
  scale_fill_manual(
    values = c("Growth" = "blue", "Reproduction" = "red"),
    name = "Habitat type"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "bottom",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )+  annotate("rect", ymin = -Inf, ymax = 16.5, xmin = -Inf, xmax = Inf, alpha = 0.1, fill = "blue") +
  annotate("rect", ymin = 16.5, ymax = 39.5, xmin = -Inf, xmax = Inf, alpha = 0.1, fill = "springgreen1") +
  annotate("rect", ymin = 39.5 , ymax = Inf , xmin = -Inf, xmax = Inf, alpha = 0.1, fill = "coral") 
# geom_text(aes(x = 5, y = 20, label = "0 species"),color="blue") +
# geom_text(aes(x = 25, y = 20, label = "10 species"),color="red")
g_result1_1_compare_habitat_type_species <- ggplot(
  plot_df,
  aes(x = PlotValue, y = reorder(Species_label, FTP), fill = Habitat)
) +
  geom_col(width = 0.7, position = "identity") +
  geom_text(
    data = transform(
      subset(
        plot_df,
        species %in% c("Salvelinus namaycush", "Micropterus salmoides", "Sander vitreus")
      ),
      label_hjust = ifelse(Habitat == "Growth", 1.1, -0.1)
    ),
    aes(label = paste0(Percentage, "%"), hjust = label_hjust),
    size = 3
  ) +
  scale_x_continuous(
    limits = c(-100, 100),
    labels = abs,  # show positive labels on both sides
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  labs(
    x = "Percentage of populations with reduced habitats",
    y = "Species",
    title = NULL
  ) +
  scale_fill_manual(
    values = c("Growth" = "blue", "Reproduction" = "red"),
    name = "Habitat type"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "bottom",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  annotate("rect", ymin = -Inf, ymax = 16.5,
           xmin = -Inf, xmax = Inf, alpha = 0.1, fill = "blue") +
  annotate("rect", ymin = 16.5, ymax = 39.5,
           xmin = -Inf, xmax = Inf, alpha = 0.1, fill = "springgreen1") +
  annotate("rect", ymin =39.5, ymax = Inf,
           xmin = -Inf, xmax = Inf, alpha = 0.1, fill = "coral")

# for each thermal guild
growth_neg <- mean_result_every_21_years %>%
  mutate(Direction = ifelse(dif_growth >= 0, "Positive", "Negative")) %>%
  count(Temp_Category, Habitat = "Growth", Direction)

rep_neg <- mean_result_every_21_years %>%
  mutate(Direction = ifelse(dif_rep >= 0, "Positive", "Negative")) %>%
  count(Temp_Category, Habitat = "Reproduction", Direction)

# Combine and calculate percentages
count_df <- bind_rows(growth_neg, rep_neg) %>%
  group_by(Temp_Category, Habitat) %>%
  mutate(Percentage = round(n / sum(n), 3) * 100) %>%
  filter(Direction == "Negative")  # Only keep negative impacts

# Plot
# Keep only reduced habitat (Direction == "Negative")
plot_df <- count_df %>%
  filter(Direction == "Negative") %>%
  mutate(
    Habitat = factor(Habitat, levels = c("Growth", "Reproduction")),
    # Flip sign for Growth so it plots to the left
    PlotValue = ifelse(Habitat == "Growth", -Percentage, Percentage)
  ) 

# Order species by max absolute % (helps readability)


g_result1_1_compare_habitat_type_thermal_guild <- ggplot(
  plot_df,
  aes(x = PlotValue, y = Temp_Category, fill = Habitat)
) +
  geom_col(width = 0.7, position = "identity") +
  geom_text(
    aes(label = paste0(Percentage, "%")),
    hjust = ifelse(plot_df$Habitat == "Growth", 1.1, -0.1),  # left vs right
    size = 3
    #fontface = "bold"
  ) +
  scale_x_continuous(
    limits = c(-100, 100),
    labels = abs,  # show positive labels on both sides
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  labs(
    x = "",
    y = "Thermal \nguild",
    title = NULL
  ) +
  scale_fill_manual(
    values = c("Growth" = "blue", "Reproduction" = "red"),
    name = "Habitat type"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),   # no x-axis title
    axis.text.x  = element_blank(),   # no x-axis tick labels
    axis.ticks.x = element_blank()    # no x-axis ticks
  )+
  annotate("rect", ymin = -Inf, ymax = 1.5, xmin = -Inf, xmax = Inf, alpha = 0.1, fill = "blue") +
  annotate("rect", ymin = 1.5, ymax = 2.5, xmin = -Inf, xmax = Inf, alpha = 0.1, fill = "springgreen1") +
  annotate("rect", ymin = 2.5 , ymax = Inf , xmin = -Inf, xmax = Inf, alpha = 0.1, fill = "coral") 
# geom_text(aes(x = 5, y = 2, label = "0 species"),color="blue") +
# geom_text(aes(x = 25, y = 2, label = "10 species"),color="red")

combined_plot_reduced_habitat_percengate <- plot_grid(g_result1_1_compare_habitat_type_thermal_guild, g_result1_1_compare_habitat_type_species, ncol = 1, rel_heights = c(5,20), rel_widths = c(1, 1))

combined_plot <- plot_grid(g_result1_1_compare_habitat_type_thermal_guild
                           , g_result1_1_compare_habitat_type_species,
                           ncol = 1, align = "v", axis = "lr" , rel_heights = c(2,20)  # force left/right axis alignment
)

jpeg("Output/Figure/FTP/g_result1_1_compare_habitat_type.jpeg",width=183,height=200,units = "mm",res=600)
combined_plot
dev.off()

# plot the OGT and OE
fish_thermal_metrics <- fish_thermal_metrics %>%
  mutate(Temp_Category = cut(FTP, breaks = c(-Inf, 18.99, 24.99, Inf), labels = c("Cold-water fish", "Cool-water fish", "Warm-water fish")))

# Prepare data in long format for paired plotting
paired_data <- fish_thermal_metrics %>%
  select(`Scientific Name`, Temp_Category, OGT, OE) %>%
  pivot_longer(cols = c(OGT, OE), names_to = "Metric", values_to = "Temperature")

# Ensure proper ordering
paired_data$Metric <- factor(paired_data$Metric, levels = c("OGT", "OE"))

# Plot
ggplot(paired_data, aes(x = Metric, y = Temperature, group = `Scientific Name`)) +
  geom_point(aes(color = Metric), size = 2) +
  geom_line(color = "gray60", alpha = 0.5) +
  facet_wrap(~ Temp_Category) +
  labs(
    title = "Comparison of OGT and OE by Species",
    x = NULL,
    y = "Temperature (°C)"
  ) +
  scale_color_manual(values = c("OGT" = "steelblue", "OE" = "darkred")) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 10),
    panel.grid.major.x = element_blank()
  )

avg_diff_labels <- fish_thermal_metrics %>%
  mutate(diff = OE - OGT) %>%
  group_by(Temp_Category) %>%
  summarize(avg_diff = mean(diff, na.rm = TRUE)) %>%
  mutate(
    label = paste0(round(avg_diff, 1)),
    x = 32,  # adjust as needed
    y = 8    # adjust as needed
  )

g_oe_ogt_compare = ggplot(fish_thermal_metrics, aes(x = OGT, y = OE)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  facet_wrap(~ Temp_Category) +
  #facet_grid(rows = vars(Temp_Category)) +
  labs(
    title = "",
    x = "Optimal growth temperature (°C)",
    y = "Optimal egg development temperature (°C)",
    size=0.1
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90")
  )+
  geom_text(
    data = avg_diff_labels,
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    size = 3.5,
    hjust = 1
  ) 

jpeg("Output/Figure/FTP/g_oe_ogt_compare_alternative.jpeg", width = 183, height = 80, units = "mm", res = 600)
g_oe_ogt_compare
dev.off()

