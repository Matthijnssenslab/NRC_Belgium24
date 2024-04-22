###Figure 1.A###
library(tidyverse) #Collection of packages in the tidyverse (see https://www.tidyverse.org/)
library(lubridate)
library(sf)

#load the data
csv <- "/Users/mustafakaratas/OneDrive - KU Leuven/Attachments/all-in-one page.csv"

#exclude rows without lat and longitude data
csv_data <- csv[!is.na(csv$lat) & !is.na(csv$long), ]

#extract the date correctly for each row
csv$`Date-of-sample-collection` <- dmy(csv$`Date-of-sample-collection`)

#load the shapefile downloaded from geo.be
belgium_shp <- st_read("/Users/mustafakaratas/Downloads/AU-RefSit_20230101_shp_3812_01000/Apn_AdMu.shp")

#correct for long-lat value columns, crs code and transform csv file values accordingly to shp file
csv_sf <- st_as_sf(csv_data, coords = c("long", "lat"), crs = st_crs(belgium_shp))
csv_sf_swapped <- st_as_sf(csv_data, coords = c("lat", "long"), crs = 4326)
csv_sf_filtered <- csv_sf_swapped[!((st_coordinates(csv_sf_swapped)[, "X"] == 0) & (st_coordinates(csv_sf_swapped)[, "Y"] == 0)), ]  #filter 0.0
csv_sf_transformed <- st_transform(csv_sf_filtered, st_crs(belgium_shp)) 

#you can clean unnecessary data frames
# remove(csv_sf_filtered)
# remove(csv)
# remove(csv_sf_swapped)
# remove(csv_data)
# remove(csv_sf)


aggregated_data_by_season <- belgium_shp %>%
  st_join(csv_sf_transformed, join = st_intersects) %>%
  group_by(localId, season) %>%  # Group by polygon ID and season
  summarise(sample_count = n())

# Aggregate the sample counts for each localId across all seasons
overall_data <- aggregated_data_by_season %>%
  group_by(localId) %>%
  summarise(sample_count = sum(sample_count, na.rm = TRUE)) %>%
  mutate(sample_count = pmin(sample_count, 50)) %>%  # Cap values at 50
  st_as_sf()

# Define custom color scale
color_scale <- c("white", viridis(5, direction = -1))

# Plot
p <- ggplot() +
  geom_sf(data = belgium_shp, fill = "#FFFFFF") +  # Base layer with light yellow color
  geom_sf(data = overall_data, aes(fill = sample_count)) +   # Overlay the overall data
  scale_fill_gradientn(
    colors = color_scale,
    breaks = c(0, 10, 20, 30, 40, 50),
    labels = c("0", "10", "20", "30", "40", "50+"),
    limits = c(0, 50)
  ) +
  theme_minimal() +
  labs(title = "Overall Sample Distribution in Belgium", fill = "Number of Samples")

# Display the plot
print(p)

# Save the plot
ggsave(filename = "map_overall.png", plot = p, width = 10, height = 7)
