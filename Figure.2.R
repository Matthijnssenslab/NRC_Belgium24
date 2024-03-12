#Figure 2.

library(lubridate)
library(tidyverse) #Collection of packages in the tidyverse (see https://www.tidyverse.org/)

csv <- "/Users/mustafakaratas/OneDrive - KU Leuven/Attachments/all-in-one page.csv"
csv <- read_csv(csv)


csv$temp_date <- dmy(csv$`Date-of-sample-collection`)
start_date <- dmy("01.08.2009")
end_date <- dmy("31.07.2023")

#cleaning the data using date columns (date of birth and collection.) and reformatting
filtered_data3 <- csv %>% 
  filter(temp_date >= start_date & temp_date <= end_date)
#reformatting
# Convert the 'Date-of-sample-collection' column to Date format
filtered_data3$Date <- as.Date(filtered_data3$`Date-of-sample-collection`, format = "%d.%m.%Y")

# Filter out rows with missing or non-finite 'Date-of-sample-collection' values
csv_filtered <- filtered_data3 %>%
  filter(!is.na(Date) & !is.infinite(Date))
# Create a sequence of weeks from the start to the end of your dataset
start_date <- min(csv_filtered$Date)
end_date <- max(csv_filtered$Date)
all_weeks <- data.frame(week = seq(floor_date(start_date, "week"), 
                                   floor_date(end_date, "week"), 
                                   by = "week"))

# Group by week and summarize sample counts, NA values per week (when there is no sample) should be included to dataframe and shown as 0 (zero)
samples_per_week <- csv_filtered %>%
  mutate(week = floor_date(Date, "week")) %>%
  group_by(week) %>%
  summarise(sample_count = n(), .groups = 'drop') %>%
  right_join(all_weeks, by = "week") %>%   # Join with the sequence of all weeks
  replace_na(list(sample_count = 0))    


# Filter the data to include only dates after December 2019 and before August 2023
samples_per_week_filtered <- samples_per_week %>%
  filter(week >= as.Date("2018-12-01") & week <= as.Date("2023-08-31"))

# Now, create the area plot for weekly data
time_series_plot_week <- ggplot(samples_per_week_filtered, aes(x = week, y = sample_count)) +
  geom_area(fill = "steelblue", alpha = 0.6) +
  labs(title = "Number of Samples Collected Over Weeks",
       x = "Time (Weeks)",
       y = "Number of Samples") +
  theme_light(base_size = 14) +
  theme(axis.text.x = element_text(angle = 65, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        plot.margin = margin(10, 10, 10, 10)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y")

# Print the plot
print(time_series_plot_week)

# Save the plot
ggsave("samples_over_weeks_area_filtered.png", plot = time_series_plot_week, width = 15, height = 8)

#rest of the annotations of figure was done on Adobe Illustrator.
