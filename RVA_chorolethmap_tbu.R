#overall sample plotting map - by years and overall

#yearly sample counts from each municipality.

aggregated_data_by_season <- belgium_shp %>%
  st_join(csv_sf_transformed, join = st_intersects) %>%
  group_by(localId, season) %>%  # Group by polygon ID and season
  summarise(sample_count = n())

seasons <- unique(aggregated_data_by_season$season)

aggregated_data_by_season <- filter(aggregated_data_by_season, !is.na(season))


#for combined map in 
p2 <- ggplot() +
  geom_sf(data = belgium_shp, fill = "#FFFFFF", color = "gray90", lwd = 0.2) +  # Base layer with white color, light gray borders, and thin line width
  geom_sf(data = aggregated_data_by_season, aes(fill = sample_count), color = NA) +   # Overlay the data for all seasons
  geom_sf(data = st_union(belgium_shp), fill = NA, color = "black") +  # Outer boundary of Belgium
  scale_fill_viridis_c(
    guide = "colorbar", 
    direction = -1, 
    breaks = c(0, 10, 20, 30, 40, 50),
    labels = c("0", "10", "20", "30", "40", "50+"),
    limits = c(0, 50)
  ) +
  theme_minimal() +
  labs(title = "Sample Distribution in Belgium", fill = "Number of Samples") +
  facet_wrap(~ season, ncol = 5) +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    strip.background = element_blank()
  )

# Display the plot
print(p2)

# Save the plot
ggsave(filename = "map_combined.svg", plot = p2, width = 20, height = 30)



###overall, all years on one figure
# Aggregate the sample counts for each localId across all seasons
overall_data <- aggregated_data_by_season %>%
  group_by(localId) %>%
  summarise(sample_count = sum(sample_count, na.rm = TRUE)) %>%
  mutate(sample_count = pmin(sample_count, 50)) %>%  # Cap values at 50
  st_as_sf()

# Define custom color scale
color_scale <- c("white", viridis(4, direction = -1))

# Plot
p <- ggplot() +
  geom_sf(data = belgium_shp, fill = "#FFFFFF") +  # Base layer with light yellow color
  geom_sf(data = overall_data, aes(fill = sample_count),  color = NA) +   # Overlay the overall data
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

