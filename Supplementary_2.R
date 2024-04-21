# Supplementary info2


csv <- "/Users/mustafakaratas/OneDrive - KU Leuven/Attachments/all-in-one page.csv"
csv <- read_csv(csv)
# Convert the 'Date-of-sample-collection' column to a date format
csv$`Date-of-sample-collection` <- lubridate::dmy(csv$`Date-of-sample-collection`)

# Now extract the year
csv$year <- lubridate::year(csv$`Date-of-sample-collection`)

# Extract year and month from the Date-of-sample-collection column
csv$year <- lubridate::year(csv$`Date-of-sample-collection`)
csv$month <- lubridate::month(csv$`Date-of-sample-collection`, label = TRUE)


# Group by year and month to count the cases
monthly_counts <-csv %>%
  group_by(season, month) %>%
  summarise(case_count = n())

# Group by season and month to calculate the percentage
seasonal_counts <- csv %>%
  filter(!is.na(season), !is.na(month)) %>%  # Exclude NA values from season and month
  group_by(season, month) %>%
  summarise(case_count = n()) %>%
  mutate(percentage = (case_count / sum(case_count)) * 100) %>%
  mutate(percentage_capped = pmin(percentage, 40))  # Cap values higher than 40% to 40%

# Order months starting from August
month_levels <- c("Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul")
seasonal_counts$month <- factor(seasonal_counts$month, levels = month_levels)

# Calculate total counts per season
season_totals <- csv %>%
  group_by(season) %>%
  summarise(total_count = n())


# Group by season and month to calculate the percentage
seasonal_counts <- csv %>%
  filter(!is.na(season), !is.na(month)) %>%  # Exclude NA values from season and month
  group_by(season, month) %>%
  summarise(case_count = n()) %>%
  mutate(percentage = (case_count / sum(case_count)) * 100) %>%
  mutate(percentage_capped = pmin(percentage, 40))  # Cap values higher than 40% to 40%

# Identify the month with the highest percentage for each season
highest_percentage_per_season <- seasonal_counts %>%
  group_by(season) %>%
  filter(percentage == max(percentage)) %>%
  ungroup()
# Ensure that highest_percentage_per_season has the percentage_capped column
highest_percentage_per_season <- highest_percentage_per_season %>%
  left_join(seasonal_counts[, c("season", "month", "percentage_capped")], by = c("season", "month"))


# Group by year and month to count the cases
monthly_counts <- joined_data_all_df %>%
  group_by(year, month) %>%
  summarise(case_count = n())


# Order months starting from August
month_levels <- c("Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul")
seasonal_counts$month <- factor(seasonal_counts$month, levels = month_levels)

# Calculate the highest percentage for each season
highest_percentage_per_season <- seasonal_counts %>%
  group_by(season) %>%
  filter(percentage == max(percentage)) %>%
  ungroup()

# Create the heatmap with indications for the highest percentage for each season
heatmap_plot <- ggplot(seasonal_counts, aes(x = month, y = factor(season, levels = unique(season)), fill = percentage_capped)) +
  geom_tile(color = "white") +  # Tiles with capped percentage as fill
  scale_fill_gradient(low = "gray95", high = "darkblue", name = "Percentage", limits = c(0, 40)) +  # Adjusted color gradient to dark red
  labs(title = "Monthly Cases by Season (as Percentage)", x = "Month", y = "Season") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for better visibility
  geom_point(data = highest_percentage_per_season, aes(x = month, y = as.character(season)), color = "red", size = 2) +  # Highlight the highest percentages
  geom_text(data = highest_percentage_per_season, aes(x = month, y = as.character(season), label = sprintf("%.1f%%", percentage), 
                                                      color = ifelse(percentage > 20, "white", "black")), vjust = -1, hjust = 1, size = 3.0) +  # Annotate with percentages and adjust color conditionally
  scale_color_identity()  # To ensure the color mapping in geom_text is used as-is

# Print the heatmap
print(heatmap_plot)

# Save the heatmap to a file
ggsave(filename = "heatmap_plot_monthly.png", plot = heatmap_plot, width = 15, height = 9, dpi = 300)
