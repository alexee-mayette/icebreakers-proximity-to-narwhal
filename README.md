# Proximity analysis between icebreakers and narwals in Tasiujaq, Nunavut, Canada
**Author**: Alexandra Mayette  
**Date**: 2024-08  
**Manuscript**: Mayette et al. 2024. Proximity analysis between icebreakers and narwals, Tasiujaq, Nunavut, Canada. Journal of Wildlife Management

This code accompanies the analysis presented in Mayette et al. 2024. The code presents the steps carried out in R for the analysis, giving the example of a narwhal (narwhal 16_148684 from the 2016 season). The analysis was repeated for all narwhals in 2016, 2017 and 2018 data, following the same steps.
## Data preparation
### Narwhal
Before the distance analysis, narwhal and ship position data were processed. First, a correlation random walk was performed for each tagging season (2016-2018), using the [aniMotum](https://github.com/ianjonsen/aniMotum) package and the fitted positions were saved as a .csv file.
```r
#install.packages("aniMotum", repos = c("https://cloud.r-project.org", "https://ianjonsen.r-universe.dev"), dependencies = TRUE)
library(aniMotum)

fit_narwhals2016 <- fit_ssm(narwhals2016_data,
                            model = "crw",
                            vmax = 10,
                            time.step = NA,
                            control = ssm_control(verbose = 1, se = FALSE))

# Save out locations
fitted_narwhals2016 <- grab(fit_narwhals2016, what = "fitted", as_sf = F)

readr::write_csv(fitted_narwhals2016, file = "output/fitted_narwhals2016.csv", col_names = TRUE)
```
The fitted positions were then rerouted to land in the water, with the package [pathroutr](https://jmlondon.github.io/pathroutr/). 
```r
# Fitted positions as sf
fitted_narwhals2016.sf <- fitted_narwhals2016 %>%
  sf::st_as_sf(coords = c("lon","lat"), crs = 4326)%>%
  sf::st_transform(3160)

ggplot() +
  ggspatial::annotation_spatial(land, fill = "cornsilk3", size = 0) +
  ggspatial::layer_spatial(fitted_narwhals2016.sf, aes(color = id), size = 0.5) +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
```
This map shows the extent of the fitted narwhal positions. We created a buffer of 5 km around the position to create the barrier polygon for the region.
![image](https://github.com/alexee-mayette/icebreakers-proximity-to-narwhal/assets/112501043/310843a9-8665-4435-8781-bf7a3907c5ec)
```r
#install.packages("pathroutr", repos = "https://jmlondon.r-universe.dev")
library(pathroutr)
library(sfnetworks)

# Limit barrier polygon to the region of interest
land_region <- sf::st_buffer(fitted_narwhals2016.sf, dist = 5000) %>% 
  sf::st_union() %>% 
  sf::st_convex_hull() %>% 
  sf::st_intersection(land) %>% 
  sf::st_collection_extract("POLYGON") %>% 
  sf::st_sf()

# Create a visibility graph
vis_graph <- prt_visgraph(land_region)
vis_graph.sf <- sfnetworks::activate(vis_graph, "edges") %>% 
  sf::st_as_sf()

ggplot() +
  ggspatial::annotation_spatial(land_region, fill = "cornsilk3", size = 0) +
  ggspatial::layer_spatial(vis_graph.sf, size = 0.5) +
  theme_void()
```
![image](https://github.com/alexee-mayette/icebreakers-proximity-to-narwhal/assets/112501043/e97df861-0e60-4dd8-9284-5cd9aa1fc650)
```r
# Filter one narwhal
n16_148684 <- fitted_narwhals2016.sf %>%
  dplyr::filter(id == "16_148684") %>%
  dplyr::filter(!sf::st_is_empty(.))

# Reroute track around land
n16_148684_fix <- prt_reroute(n16_148684, land_region, vis_graph, blend = TRUE)

# Update points
n16_148684_fix <- prt_update_points(n16_148684_fix, n16_148684)

# Repeat for individual narwhal

# Save new points
narwhals2016_reroute <- rbind(n16_148684_fix, n16_148685_fix, n16_148686_fix, n16_148693_fix, n16_164369_fix)

narwhals2016_reroute <- narwhals2016_reroute %>%
  dplyr::rename(tag_id = id) %>%
  dplyr::mutate(long_m = st_coordinates(.)[,1],
                lat_m = st_coordinates(.)[,2])

# Save as .csv
readr::write_csv(narwhals2016_reroute, file = "output/narwhals2016_reroute.csv", col_names = TRUE)
# Save as shapefile
sf::st_write(narwhals2016_reroute, "output/narwhals2016_reroute.shp")
```
### Icebreakers
Second, icebreaker vessel positions were obtained through AIS data. The seven icebreakers known to be in the region were filtered and dates were filtered around the dates of transmission of the narwhal's tags each year.
```r
# Filter date with duration of narwhal tags
ib2016 <- icebreakers_2016_2018 %>% # Between Aug-18 and Nov-17, 2016
  dplyr::filter(Date > "2016-08-17 00:00:00") %>%
  dplyr::filter(Date < "2016-11-18 00:00:00") %>%
  dplyr::mutate(Year = "2016")


ib2017 <- icebreakers_2016_2018 %>% # Between July-31, 2017 and Mar-01, 2018
  dplyr::filter(Date > "2017-07-30 00:00:00") %>%
  dplyr::filter(Date < "2018-03-02 00:00:00") %>%
  dplyr::mutate(Year = "2017")

ib2018 <- icebreakers_2016_2018 %>% # Between Aug-17 and Nov-03, 2018
  dplyr::filter(Date > "2018-08-16 00:00:00") %>%
  dplyr::filter(Date < "2018-11-04 00:00:00") %>%
  dplyr::mutate(Year = "2018")

icebreakers_2016_2018 <- rbind(ib2016, ib2017, ib2018)

readr::write_csv(icebreakers_2016_2018, file = "output/icebreakers_2016_2018.csv", col_names = TRUE)
```
Then, positions were intersected with the study area, to only keep AIS transmission inside Tasiujaq (Eclipse Sound).
![image](https://github.com/alexee-mayette/icebreakers-proximity-to-narwhal/assets/112501043/34811660-7bac-44f9-bcc0-170346fd3629)
```r
icebreakers2016.sf <- icebreakers2016 %>%
  sf::st_as_sf(coords = c("Longitude","Latitude"), crs = 4326)%>%
  sf::st_transform(3160)

# Intersect with Tasiujaq boundary
icebreakers2016.sf <- sf::st_intersection(icebreakers2016.sf, es)

ggplot() +
  ggspatial::annotation_spatial(es, fill = "deepskyblue3", size = 0) +
  ggspatial::annotation_spatial(land, fill = "cornsilk3", size = 0) +
  ggspatial::layer_spatial(icebreakers2016.sf, aes(color = Vessel_Name), size = 0.5)+
  scale_color_brewer(palette = "Pastel2") +
  theme_void() +
  theme(legend.position = "none")
```
![image](https://github.com/alexee-mayette/icebreakers-proximity-to-narwhal/assets/112501043/1d97e147-c176-41a2-b244-3e3c2be8c703)
```r
# Save as .csv file
readr::write_csv(icebreakers2016.sf, file = "output/icebreakers2016.csv", col_names = TRUE)

# Save as shapefile
sf::st_write(icebreakers2016.sf, "output/icebreakers2016.shp")

icebreakers2016.sf <- icebreakers2016.sf %>%
  dplyr::mutate(long_m = st_coordinates(.)[,1],
                lat_m = st_coordinates(.)[,2])
```
The `narwhals2016_reroute` and `icebreakers2016.sf` files will be used for the distance analysis. 
See the R file for the distance analysis and maps created for the manuscript.
