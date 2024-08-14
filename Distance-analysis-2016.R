
# ................................
# Proximity analysis between icebreakers and narwals, Tasiujaq, Nunavut, Canada
# Distance analysis for 2016
# Author: Alexandra Mayette
# Last updated: 2024-08-14
# ................................

rm(list = ls())

# Load libraries ---------------------------------------------------------------
library(readxl)
library(tidyverse)
library(sf)
library(ggspatial)
library(rcartocolor)

ggplot2::theme_set(theme_light())

# Import data ------------------------------------------------------------------
## Land
land <- sf::read_sf("data/Canada_Greenland.shp")

land <- sf::st_make_valid(land) %>% 
  sf::st_set_crs(4326) %>%
  sf::st_transform(3160)

ggplot() +
  ggspatial::layer_spatial(land, fill = "cornsilk3", size = 0)

## Eclispe Sound
es <- sf::read_sf("data/eclispe_sound.shp")
st_crs(es)

es <- sf::st_make_valid(es) %>%
  sf::st_transform(3160)

ggplot() +
  ggspatial::layer_spatial(es, fill = "deepskyblue3", size = 0) +
  ggspatial::annotation_spatial(land, fill = "cornsilk3", size = 0)

## Narwhals
narwhals2016_reroute <- read_csv("output/narwhals2016_reroute.csv")

narwhals2016_reroute <- narwhals2016_reroute %>%
  sf::st_as_sf(coords = c("long_m","lat_m"), crs = 3160)

## Icebreakers
icebreakers2016.sf <- read_csv("output/icebreakers2016.csv")

icebreakers2016.sf <- icebreakers2016.sf %>%
  sf::st_as_sf(coords = c("long_m","lat_m"), crs = 3160)

## Time narwhals spent in Eclipse Sound
# This step was done in ArcMap 
timeES_2016 <- read_csv("output/timeES_2016.csv")

head(timeES_2016)

# Distance analysis ------------------------------------------------------------

# sf point icebreakers: icebreakers2016.sf
# sf point narwhals: narwhals2016_reroute

# Project all shapefiles to the same projection and extract coordinates
points_narwhals2016 <- narwhals2016_reroute %>%
  sf::st_transform(crs = "+proj=eqdc +lat_0=69.5d +lon_0=-74.5d +lat_1=64d +lat_2=75d +x_0=0 +y_0=0 +units=m +x_0=0 +y_0=0") %>%
  dplyr::mutate(long_m = st_coordinates(.)[,1],
                lat_m = st_coordinates(.)[,2])

points_icebreakers2016 <- icebreakers2016.sf %>%
  sf::st_transform(crs = "+proj=eqdc +lat_0=69.5d +lon_0=-74.5d +lat_1=64d +lat_2=75d +x_0=0 +y_0=0 +units=m +x_0=0 +y_0=0") %>%
  dplyr::mutate(long_m = st_coordinates(.)[,1],
                lat_m = st_coordinates(.)[,2])

es <- es %>%
  sf::st_transform(crs = "+proj=eqdc +lat_0=69.5d +lon_0=-74.5d +lat_1=64d +lat_2=75d +x_0=0 +y_0=0 +units=m +x_0=0 +y_0=0")


# Create a 100 km buffer around narwhal
buffer100km <- sf::st_buffer(points_narwhals2016, 100000)

# Make sure the CRS is the same as the points
st_crs(points_icebreakers2016) == st_crs(buffer100km)

ggplot() +
  ggspatial::layer_spatial(buffer100km, color = "grey50", size = 0.5) + # 100 km buffer
  ggspatial::annotation_spatial(land, fill = "cornsilk3", size = 0) +
  ggspatial::layer_spatial(points_narwhals2016, color = "red", size = 0.5) + # Narwhal
  ggspatial::layer_spatial(points_icebreakers2016, color = "blue", size = 0.5) + # Icebreakers
  theme_void()


# Intersection between icebreakers points and the buffer (which ship are within 100 km of the narwhal buffer)
encounter_I_N_100 <- sf::st_intersection(points_icebreakers2016, buffer100km)

# Reorganize columns to compare date and time
# I : icebreakers
# N : narwhals
encounter_I_N_100 <- encounter_I_N_100 %>%
  dplyr::select(Vessel_Name, Date, long_m, lat_m, tag_id, date, long_m.1, lat_m.1, geometry) %>%
  dplyr::rename(vessel_id = Vessel_Name,
                date_I = Date,
                date_N = date,
                long_I = long_m,
                lat_I = lat_m,
                long_N = long_m.1,
                lat_N = lat_m.1)

# Calculate the time difference between the narwhal and the ship
encounter_I_N_100 <- encounter_I_N_100 %>%
  dplyr::mutate(time_diff = abs(difftime(date_N, date_I, units = "min"))) %>%
  dplyr::filter(time_diff <= 60) # Remove time difference that is more than 1h

encounter_I_N_100 %>%
  dplyr::group_by(tag_id, vessel_id) %>%
  dplyr::count() %>%
  sf::st_drop_geometry()

# A tibble: 10 × 3
#   tag_id    vessel_id             n
#*  <chr>     <chr>             <int>
#1  16_148684 AMUNDSEN            638
#2  16_148684 CCGS HENRY LARSEN 12661
#3  16_148684 TERRY FOX          1075
#4  16_148685 AMUNDSEN            589
#5  16_148685 CCGS HENRY LARSEN  9741
#6  16_148685 TERRY FOX           687
#7  16_148686 CCGS HENRY LARSEN  1104
#8  16_148693 CCGS HENRY LARSEN    24
#9  16_148693 TERRY FOX           308
#10 16_164369 CCGS HENRY LARSEN   770

# Only based on a 1h time difference and a 100 km, we can see that three icebreakers are involves and five narwhals

# Look at the distribution of the time difference 
encounter_I_N_100 %>%
  mutate(time_class = case_when(time_diff <= 15 ~ "0-15 min",
                                time_diff > 15 & time_diff <= 30 ~ "15-30",
                                time_diff > 30 & time_diff <= 45 ~ "30-45",
                                time_diff <= 60 ~ "45-60")) %>%
  group_by(time_class) %>%
  count() %>%
  mutate(percent = n/sum(nrow(encounter_I_N_100)))

# We can see that it is well distributed between the four intervals
#1 0-15 min    8526
#2 15-30       4509                                                                                           #3 30-45       6686
#4 45-60       7876

# To calculate the distance between 2 points (icebreakers and narwhals), we create a shapefile with just the coordinate
# Icebreakers
coords_I_100 <- encounter_I_N_100 %>%
  dplyr::select(long_I, lat_I) %>%
  data.frame(.) %>%
  dplyr::select(-geometry) %>%
  sf::st_as_sf(coords = c("long_I", "lat_I")) %>%
  sf::st_set_crs("+proj=eqdc +lat_0=69.5d +lon_0=-74.5d +lat_1=64d +lat_2=75d +x_0=0 +y_0=0 +units=m +x_0=0 +y_0=0")

# Narwhals
coords_N_100 <- encounter_I_N_100 %>%
  dplyr::select(long_N, lat_N) %>%
  data.frame(.) %>%
  dplyr::select(-geometry) %>%
  sf::st_as_sf(coords = c("long_N", "lat_N")) %>%
  sf::st_set_crs("+proj=eqdc +lat_0=69.5d +lon_0=-74.5d +lat_1=64d +lat_2=75d +x_0=0 +y_0=0 +units=m +x_0=0 +y_0=0")

## Calculate distance ##########################################################
# Calculate distance between 2 points for each row
encounter_I_N_100 <- encounter_I_N_100 %>%
  dplyr::ungroup() %>%
  dplyr::mutate(distance = sf::st_distance(coords_I_100, coords_N_100, by_element = TRUE))

# Save
readr::write_csv(encounter_I_N_100, file = "output/encounter_I_N_2016.csv", col_names = TRUE)

## Filter 50 km ################################################################
# Open file that was just saved
encounter_I_N_100_2016 <- read_csv("output/encounter_I_N_2016.csv")

# Filter distance within 50km
encounter_I_N_50_2016 <- encounter_I_N_100_2016 %>%
  dplyr::mutate(distance = as.numeric(distance)/1000) %>%
  dplyr::filter(distance <= 50)

# Save
readr::write_csv(encounter_I_N_50_2016, file = "output/encounter_I_N_50_2016.csv", col_names = TRUE)

# Create segment between narwhals and icebreakers to see if they intersect land
encounter_I_N_50_2016$line_id <- 1:nrow(encounter_I_N_50_2016)

# Select coordinates for narwhals 
start_xy <- encounter_I_N_50_2016 %>% 
  dplyr::select(start_x = long_N, start_y = lat_N, tag_id, line_id) %>%
  dplyr::rename(id = tag_id) %>%
  sf::st_as_sf(coords = c("start_x", "start_y"), crs = "+proj=eqdc +lat_0=69.5d +lon_0=-74.5d +lat_1=64d +lat_2=75d +x_0=0 +y_0=0 +units=m +x_0=0 +y_0=0")


# Select coordinates for icebreakers  
end_xy <- encounter_I_N_50_2016 %>% 
  dplyr::select(start_x = long_I, start_y = lat_I, vessel_id, line_id) %>%
  dplyr::rename(id = vessel_id) %>%
  sf::st_as_sf(coords = c("start_x", "start_y"), crs = "+proj=eqdc +lat_0=69.5d +lon_0=-74.5d +lat_1=64d +lat_2=75d +x_0=0 +y_0=0 +units=m +x_0=0 +y_0=0")

# Join two points
segment_pts_2016 <- rbind(start_xy, end_xy)

# Create segments
segment_2016 <- segment_pts_2016 %>%
  dplyr::group_by(line_id) %>%
  dplyr::summarise(do_union = FALSE) %>% 
  sf::st_cast("LINESTRING")

# Visualize all the segments
ggplot() +
  ggspatial::layer_spatial(segment_2016, color = "grey", size = 1)+
  ggspatial::annotation_spatial(land, fill = "cornsilk3", size = 0) +
  ggspatial::annotation_spatial(points_narwhals2016, color = "blue", size = 0.5)+
  ggspatial::annotation_spatial(points_icebreakers2016, color = "red", size = 0.5)+
  theme_void()

# Identify interactions that crosses land
land_eqdc <- sf::st_transform(land, crs = "+proj=eqdc +lat_0=69.5d +lon_0=-74.5d +lat_1=64d +lat_2=75d +x_0=0 +y_0=0 +units=m +x_0=0 +y_0=0")
land_eqdc <- sf::st_union(land_eqdc)

st_crs(segment_2016) == st_crs(land_eqdc)

# If the geometry of the segment intersects the land, then print TRUE, otherwise print FALSE
segment_2016 <- segment_2016 %>%
  dplyr::mutate(to_remove = ifelse((sf::st_intersects(segment_2016$geometry, land_eqdc, sparse = FALSE) == TRUE), "TRUE", "FALSE"))

# Visualize only segments that cross land
ggplot() +
  ggspatial::layer_spatial(segment_2016, aes(color = to_remove), size = 1)+
  ggspatial::annotation_spatial(land, fill = "cornsilk3", size = 0) +
  ggspatial::annotation_spatial(points_narwhals2016, color = "blue", size = 0.5)+
  ggspatial::annotation_spatial(points_icebreakers2016, color = "red", size = 0.5)+
  theme_void()

# Join with segment points
segment_2016 <- data.frame(segment_2016)
segment_2016$to_remove <- as.character(segment_2016$to_remove)
segment_2016 <- segment_2016[,c(1,3)]

segment_pts_2016 <- dplyr::left_join(segment_pts_2016, segment_2016, by = "line_id")

segment_pts_2016 <- segment_pts_2016 %>%
  dplyr::mutate(long = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2]) %>%
  dplyr::group_by(line_id) %>%
  dplyr::summarise(vessel_id = last(id),
                   long_I = last(long),
                   lat_I = last(lat),
                   tag_id = first(id),
                   long_N = first(long),
                   lat_N = first(lat),
                   to_remove = first(to_remove))

sum(segment_2016$to_remove == FALSE) == sum(segment_pts_2016$to_remove == FALSE)

segment_pts_2016 <- data.frame(segment_pts_2016)
segment_pts_2016 <- segment_pts_2016 %>%
  dplyr::select(-geometry)

# Join the segments identified to remove with the single encounters
encounter_I_N_50_2016 <- dplyr::left_join(encounter_I_N_50_2016, segment_pts_2016, by = c("line_id", "vessel_id", "long_I", "lat_I", "tag_id", "long_N", "lat_N"))

readr::write_csv(encounter_I_N_50_2016, file = "output/encounter_I_N_50_2016_landintersectid.csv", col_names = TRUE)


## Minimum and average distance with ship ######################################
encounter_I_N_50_2016 <- read_csv("output/encounter_I_N_50_2016_landintersectid.csv")

# Remove interaction crossing land
encounter_I_N_50_2016 <- encounter_I_N_50_2016 %>%
  dplyr::filter(to_remove == "FALSE")

# Summary of single encounters
# Within 1h of a narwhal position, what was the closest (min), farthest (max) and average (avg) position it got to a ship
summary_encounter_50_2016 <- encounter_I_N_50_2016 %>%
  dplyr::group_by(tag_id, vessel_id, date_N) %>%
  dplyr::summarise(min = min(distance),
                   max = max(distance),
                   avg = mean(distance),
                   sd = sd(distance),
                   min_time = min(date_I),
                   max_time = max(date_I)) %>%
  dplyr::mutate(interaction = "Y")


# Get the position before and after the start and end of an encounter
to_remove_2016 <- summary_encounter_50_2016 %>% 
  dplyr::select(tag_id, vessel_id, date_N)
to_remove_2016 <- data.frame(to_remove_2016)


# From the 100km encounters, remove all rows that are in the 50km encounters, based on the tag_id, vessel_id and date_N
summary_encounter_100_2016 <- dplyr::anti_join(x = encounter_I_N_100_2016, y = to_remove_2016, by = c("tag_id", "vessel_id", "date_N"))

# Calculate the distance at 100km of those other encounter
summary_encounter_100_2016 <- summary_encounter_100_2016 %>%
  dplyr::group_by(tag_id, vessel_id, date_N) %>%
  dplyr::mutate(distance = as.numeric(distance)/1000) %>%
  dplyr::summarise(min = min(distance),
                   max = max(distance),
                   avg = mean(distance),
                   sd = sd(distance),
                   min_time = min(date_I),
                   max_time = max(date_I)) %>%
  dplyr::mutate(interaction = "N")

# Join with the 50km data
summary_encounter_2016 <- rbind(summary_encounter_50_2016, summary_encounter_100_2016)

summary_encounter_2016 <- summary_encounter_2016 %>%
  dplyr::arrange(tag_id, vessel_id, date_N)

# Are there any narwhals that never get within 50km of a ship?
summary_encounter_2016 %>%
  dplyr::ungroup() %>%
  dplyr::group_by(tag_id, interaction) %>%
  dplyr::summarise(interaction = nrow(interaction))

# A tibble: 8 × 2
# Groups:   tag_id [5]
#  tag_id    interaction
#  <chr>     <chr>      
#1 16_148684 N          
#2 16_148684 Y          
#3 16_148685 N          
#4 16_148685 Y          
#5 16_148686 N          
#6 16_148686 Y          
#7 16_148693 N    <- this narwhal      
#8 16_164369 N    <- this narwhal


# Only keep narwhals #16_1486864, #16_148685 and #16_148686
summary_encounter_2016 <- summary_encounter_2016 %>%
  dplyr::filter(tag_id == "16_148684" | tag_id == "16_148685" | tag_id == "16_148686")

# Identify start and end of interaction (entering and leaving 50km)
summary_encounter_2016 <- summary_encounter_2016 %>%
  dplyr::ungroup() %>%
  dplyr::mutate(keep = ifelse((interaction  == "N" & (interaction != dplyr::lead(interaction))), "1", "0")) %>% #put 1 if the next obs is not the same
  dplyr::mutate(keep = ifelse(interaction  == "N" & (interaction != dplyr::lag(interaction)), "1", keep)) %>% #put 1 if the last obs is not the same
  dplyr::mutate(keep = ifelse(interaction == "Y", "1", keep))

# see with only keep = 1 
summary_encounter_2016_simp <- summary_encounter_2016 %>%
  dplyr::filter(keep == "1")

# Are there any interaction identified that are not from the same icebreaker
which(summary_encounter_2016$interaction == "N" & lag(summary_encounter_2016$interaction) == "Y" & summary_encounter_2016$vessel_id != lag(summary_encounter_2016$vessel_id))

print(summary_encounter_2016[228:231,])

# Remove row between Amundsen and CCGS (not the same encounter)
summary_encounter_2016[1, 11] <- "0"
summary_encounter_2016[230, 11] <- "0"

# Are there any interaction identified as "no" between two interactions identified as "yes"
which(summary_encounter_2016$interaction == "N" & lead(summary_encounter_2016$interaction) == "Y" & lag(summary_encounter_2016$interaction) == "Y")

# Not in this case, but if it did, we just copied that row so that it identifies the end of one encounter and the start of the next one.

# Identified the time difference between two consecutive encounters to see if it's more than 6 hours
summary_encounter_2016 <- summary_encounter_2016 %>%
  dplyr::mutate(time_diff = difftime(date_N, lag(date_N), units = "hour")) %>%
  dplyr::mutate(time_diff = round(time_diff, 4),
                too_long = ifelse(as.numeric(time_diff) <= 6, "no", "yes"))

summary_encounter_2016 <- summary_encounter_2016 %>%
  dplyr::arrange(tag_id, vessel_id, date_N)

# Remove previous encounters outside of 50km (only keep the encounters before and after the interaction)
summary_encounter_2016_simp <- summary_encounter_2016 %>%
  dplyr::filter(keep == "1")


# Add a new column for encounter event ID
summary_encounter_2016$enc_ID <- NA
summary_encounter_2016_simp$enc_ID <- NA

# At this point, I opened the data frame and look at when an encounter event starts and ends. I look at one narwhal and one icebreaker at the time, starting with the first interaction == N until the next interaction == N. Also taken into account if there is more than 6 hours between two consecutive rows. Unfortunately, I did not find a better method than to do it manually and I acknowledge this might not be the best practice.

# Narwhal 16_148684
summary_encounter_2016_simp[1:9, 14] <- "A1"
summary_encounter_2016[1:13, 14] <- "A1"

summary_encounter_2016_simp[10:14, 14] <- "A2"
summary_encounter_2016[14:31, 14] <- "A2"

summary_encounter_2016_simp[15:24, 14] <- "A3"
summary_encounter_2016[32:60, 14] <- "A3"

summary_encounter_2016_simp[25, 14] <- "A4"
summary_encounter_2016[61, 14] <- "A4"

summary_encounter_2016_simp[26:32, 14] <- "A5"
summary_encounter_2016[62:68, 14] <- "A5"

summary_encounter_2016_simp[33:42, 14] <- "A6"
summary_encounter_2016[69:78, 14] <- "A6"

summary_encounter_2016_simp[43:51, 14] <- "A7"
summary_encounter_2016[79:215, 14] <- "A7"

# Narwhal 16_148685
summary_encounter_2016_simp[52:59, 14] <- "B1"
summary_encounter_2016[216:229, 14] <- "B1"

summary_encounter_2016_simp[60:63, 14] <- "B2"
summary_encounter_2016[230:297, 14] <- "B2"

summary_encounter_2016_simp[64:68, 14] <- "B3"
summary_encounter_2016[298:365, 14] <- "B3"

# Narwhal 16_148686
summary_encounter_2016_simp[69:72, 14] <- "C1"
summary_encounter_2016[366:378, 14] <- "C1"

readr::write_csv(summary_encounter_2016_simp, file = "output/summary_2016_enc_events.csv", col_names = TRUE)


summary_encounter_2016_simp %>%
  dplyr::group_by(tag_id, enc_ID) %>%
  dplyr::filter(interaction == "Y") %>%
  dplyr::count()

# A4 has only one observation, remove it

summary_encounter_2016_simp <- summary_encounter_2016_simp %>%
  dplyr::mutate(interaction = ifelse(enc_ID == "A4", "N", interaction))

summary_encounter_2016 <- summary_encounter_2016 %>%
  dplyr::mutate(interaction = ifelse(enc_ID == "A4", "N", interaction))

# Calculate duration of encounter events
duration_2016 <- summary_encounter_2016_simp %>%
  dplyr::filter(interaction == "Y") %>%
  dplyr::group_by(tag_id, vessel_id, enc_ID) %>%
  dplyr::summarise(duration = difftime(max(max_time), min(min_time), units = "hour"),
                   duration_min = difftime(max(max_time), min(min_time), units = "min"))



readr::write_csv(duration_2016, file = "output/duration_events_2016.csv", col_names = TRUE)

# Which enc are less than 15 min
duration_2016 %>%
  dplyr::filter(as.numeric(duration_min) < 1) %>%
  dplyr::select(enc_ID, duration_min)

# No encounter events under 1 min

## Calculate duration ##########################################################
duration_2016 <- summary_encounter_2016_simp %>%
  dplyr::filter(interaction == "Y") %>%
  dplyr::group_by(tag_id, vessel_id, enc_ID) %>%
  dplyr::summarise(duration = difftime(max(max_time), min(min_time), units = "hour"),
                   duration_min = difftime(max(max_time), min(min_time), units = "min"))

duration_2016 %>%
  dplyr::group_by(tag_id, vessel_id) %>%
  dplyr::summarise(n_int = n_distinct(enc_ID),
                   time = sum(duration),
                   time_min = sum(duration_min)) %>%
  dplyr::mutate(time = round(time, 1),
                time_min = round(time_min, 0))

# A tibble: 5 × 5
# Groups:   tag_id [3]
#  tag_id    vessel_id         n_int time       time_min
#  <chr>     <chr>             <int> <drtn>     <drtn>  
#1 16_148684 AMUNDSEN              1  1.7 hours 105 mins
#2 16_148684 CCGS HENRY LARSEN     5 13.5 hours 810 mins
#3 16_148685 AMUNDSEN              1  1.8 hours 107 mins
#4 16_148685 CCGS HENRY LARSEN     2  0.3 hours  18 mins
#5 16_148686 CCGS HENRY LARSEN     1  0.2 hours  11 mins

# Join the duration of encounter with the total time in Eclipse Sound
duration_2016 <- dplyr::left_join(duration_2016, timeES_2016, by = "tag_id")

duration_2016 %>%
  dplyr::group_by(tag_id, vessel_id) %>%
  dplyr::summarise(n_int = n_distinct(enc_ID),
                   time = sum(duration),
                   total_hour = first(total_hour)) %>%
  dplyr::mutate(time = round(time, 1)) %>%
  dplyr::mutate(proportion = (as.numeric(time)/total_hour)*100)

# A tibble: 5 × 6
# Groups:   tag_id [3]
#  tag_id    vessel_id         n_int time       total_hour proportion
#  <chr>     <chr>             <int> <drtn>          <dbl>      <dbl>
#1 16_148684 AMUNDSEN              1  1.7 hours      1530.     0.111 
#2 16_148684 CCGS HENRY LARSEN     5 13.5 hours      1530.     0.882 
#3 16_148685 AMUNDSEN              1  1.8 hours      1238.     0.145 
#4 16_148685 CCGS HENRY LARSEN     2  0.3 hours      1238.     0.0242
#5 16_148686 CCGS HENRY LARSEN     1  0.2 hours       170.     0.118

# Calculate the average duration of encounter for all narwhals in 2016
duration_2016 %>%
  ungroup() %>%
  summarise(mean = mean(duration),
            sd = sd(duration),
            max = max(duration))

# A tibble: 1 × 3
#  mean              sd max           
#1 1.750472 hours  1.99 6.493889 hours

## Summary of encounter events #################################################
summary_encounter_2016_simp %>%
  dplyr::group_by(tag_id, vessel_id) %>%
  dplyr::filter(interaction == "Y") %>%
  dplyr::summarise(nb_interaction = n_distinct(enc_ID),
                   min = min(min),
                   average = mean(avg),
                   sd = round(sd(avg),1))

# A tibble: 5 × 6
# Groups:   tag_id [3]
#  tag_id    vessel_id         nb_interaction   min average    sd
#  <chr>     <chr>                      <int> <dbl>   <dbl> <dbl>
#1 16_148684 AMUNDSEN                       1  23.0    29.3   3.3
#2 16_148684 CCGS HENRY LARSEN              5  11.6    30.4  10.7
#3 16_148685 AMUNDSEN                       1  20.3    27.7   5.5
#4 16_148685 CCGS HENRY LARSEN              2  27.7    39.8  10.5
#5 16_148686 CCGS HENRY LARSEN              1  25.9    31.1   5.6

# Calculate the number of encounter events for 2016, the minimum distance, average distance and SD
summary_encounter_2016_simp %>%
  ungroup() %>%
  dplyr::filter(interaction == "Y") %>%
  dplyr::summarise(nb_interaction = n_distinct(enc_ID),
                   min = min(min),
                   average = mean(avg),
                   sd = round(sd(avg),1))

# A tibble: 1 × 4
#  nb_interaction   min average    sd
#            <int> <dbl>   <dbl> <dbl>
#1             10  11.6    30.8   9.6


# Create final data frame to summarise each encounter events of narwhals in 2016
ind_enc_events_2016 <- summary_encounter_2016_simp %>%
  dplyr::ungroup() %>%
  dplyr::filter(interaction == "Y") %>%
  dplyr::group_by(enc_ID) %>%
  dplyr::summarise(tag_id = first(tag_id), 
                   vessel_id = first(vessel_id),
                   start_date = min(format(min_time, "%m-%d")),
                   end_date = max(format(max_time, "%m-%d")),
                   duration = difftime(max(max_time), min(min_time), units = "hour"),
                   duration_min = difftime(max(max_time), min(min_time), units = "min"),
                   min = min(min),
                   average = mean(avg),
                   sd = round(sd(avg),1)) %>%
  dplyr::mutate(duration = round(duration, 1),
                duration_min = round(duration_min, 0))


readr::write_csv(ind_enc_events_2016, file = "output/ind_enc_events_2016.csv", col_names = TRUE)

# Map --------------------------------------------------------------------------
# Points for narwhals : narwhals_reroute
# We only want the 3 narwhals interacting and identify when there are close to ship

narwhals2016_reroute <- read_csv("output/narwhals2016_reroute.csv")


narwhals_enc2016 <- narwhals2016_reroute %>%
  dplyr::filter(tag_id == "16_148684" | tag_id == "16_148685" | tag_id == "16_148686") %>%
  dplyr::rename(date_N = date) %>%
  dplyr::select(tag_id, date_N, long_m, lat_m)

# Add the information when narwhal are in an encounter
narwhals_enc2016 <- dplyr::left_join(narwhals_enc2016, summary_encounter_2016_simp, by = c("tag_id", "date_N"))

# All the points with NA in the vessel_id are not involved in an encounter with an icebreaker.

# Identify points not in an encounter and distance less than 50 km
narwhals_enc2016 <-  narwhals_enc2016 %>%
  dplyr::mutate(vessel_id = ifelse(is.na(vessel_id), "No", vessel_id),
                interaction = ifelse(is.na(interaction), "N", interaction)) %>%
  dplyr::mutate(vessel_id = ifelse(interaction == "N", "No", vessel_id),
                min = ifelse(is.na(min)|min > 50, 100, min),
                max = ifelse(is.na(max)|max > 50, 100, max),
                avg = ifelse(is.na(avg)|avg > 50, 100, avg)) %>%
  dplyr::mutate(day = as.Date(format(date_N, "%Y-%m-%d")))


# Points for icebreakers:
icebreakers2016 <- read_csv("output/icebreakers_2016_2018.csv")

# Only keeps the three icebreakers involved
iceb_enc2016 <- icebreakers2016 %>%
  dplyr::filter(Year == "2016") %>%
  dplyr::filter(Vessel_Name == "AMUNDSEN" | Vessel_Name == "CCGS HENRY LARSEN" | Vessel_Name == "TERRY FOX") %>%
  sf::st_as_sf(coords = c("Longitude","Latitude"), crs = 4326)%>%
  sf::st_transform(3160) %>%
  dplyr::mutate(long_m = st_coordinates(.)[,1],
                lat_m = st_coordinates(.)[,2])
  

## Map - Distance  #############################################################
map_2016 <- ggplot(data = narwhals_enc2016, aes(x = long_m, y = lat_m)) +
  
  geom_point(data = . %>% filter(interaction == "N"), color = "#E0E0E0", fill = "white", alpha = 0.3, shape = 21) +
  
  geom_point(data = iceb_enc2016, color = "black", alpha = 0.5, size = 0.5, shape = 18) +
  
  ggspatial::layer_spatial(land, fill = "#f5f8fc", color = "#9eb9ce", size = 0) +
  
  geom_point(data = . %>% filter(avg <= 50 & interaction == "Y"), aes(color = avg), shape = 16) +
  
  scale_color_gradient2(high = "#F9D639",
                        mid = "#F94144",
                        low = "#4F0224",
                        midpoint = 25,
                        limits = c(0, 50),
                        na.value = "white",
                        name = "Distance to ship (km)") +
  
  coord_sf(x = c(650000, 890000),
           y = c(7980000, 8200000)) +
  annotation_scale() +
  annotation_north_arrow(which_north = "true", height = unit(0.75, "cm"),
                         width = unit(0.75, "cm"), pad_x = unit(4.5, "cm")) +
  annotate("text", x = 880000, y = 8190000, label = "2016", color = "black") +
  theme_minimal()+
  theme(axis.title = element_blank(),
        legend.position = "none",
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "#cadcea", color = NA),
        plot.background = element_rect(fill = "white", color = NA))

map_2016

ggsave("map_2016.png", plot = map_2016, device = "png", path = "figures", height = 5, width = 6, dpi = 300)

## Map - Date  #################################################################
map_2016_time <- ggplot(data = narwhals_enc2016, aes(x = long_m, y = lat_m)) +
  
  geom_point(data = . %>% filter(interaction == "N"), color = "#E0E0E0", fill = "white", alpha = 0.3, shape = 21) +
  
  geom_point(data = iceb_enc2016, color = "black", alpha = 0.5, size = 0.5, shape = 18) +
  
  ggspatial::layer_spatial(land, fill = "#f5f8fc", color = "#9eb9ce", size = 0) +  
  
  geom_point(data = . %>% filter(avg <= 50 & interaction == "Y"), aes(color = day), shape = 16) +
  
  scale_color_gradientn(#colors = c("#2892BF", "#46AF8F","#8FCC5F","#F9D639", "#F8961E","#F94144"),
    colors = c("#F94144", "#F8961E","#F9D639","#8FCC5F", "#46AF8F", "#2892BF"),
    trans = "date",
    name = "Day") +
  
  coord_sf(x = c(650000, 890000),
           y = c(7980000, 8200000)) +
  annotation_scale() +
  annotation_north_arrow(which_north = "true", height = unit(0.75, "cm"),
                         width = unit(0.75, "cm"), pad_x = unit(4.5, "cm")) +
  annotate("text", x = 880000, y = 8190000, label = "2016", color = "black") +
  theme_minimal()+
  theme(axis.title = element_blank(),
        legend.position = "none",
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "#cadcea", color = NA),
        plot.background = element_rect(fill = "white", color = NA))

map_2016_time

ggsave("map_2016_time.png", plot = map_2016_time, device = "png", path = "figures", height = 5, width = 6, dpi = 300)
