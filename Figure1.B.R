####Figure 1.B####

csv <- "/Users/mustafakaratas/OneDrive - KU Leuven/Attachments/all-in-one page.csv"
csv <- read_csv(csv)

# Group by season and count the number of samples for each season
samples_per_season <- csv %>%
  group_by(season) %>%
  summarise(sample_count = n())



# Categorizing seasons into 'Pre-Pandemic', 'Pandemic', and 'Post-Pandemic'
samples_per_season <- samples_per_season %>%
  mutate(period = case_when(
    season %in% c("2019-2020", "2020-2021") ~ "Pandemic",
    season %in% c("2009-2010", "2010-2011", "2011-2012", "2012-2013", "2013-2014", "2014-2015", "2015-2016", "2016-2017", "2017-2018", "2018-2019") ~ "Pre-Pandemic",
    TRUE ~ "Post-Pandemic"
  ))
# Inverting the order of the Y-axis by reordering the factor levels in reverse
samples_per_season$season <- factor(samples_per_season$season, levels = rev(unique(samples_per_season$season)))

# Plotting
transposed_bar_plot <- ggplot(samples_per_season, aes(x = season, y = sample_count, fill = period)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Pre-Pandemic" = "darkblue", "Pandemic" = "lightblue", "Post-Pandemic" = "darkgreen")) +
  labs(title = "Number of Samples Collected Over the Seasons",
       x = "Season",
       y = "Number of Samples") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, color = "#2c3e50"),
    axis.title = element_text(color = "#2c3e50"),
    plot.title = element_text(hjust = 0.5, color = "#2c3e50"),
    legend.title = element_text(color = "#2c3e50"),
    legend.text = element_text(color = "#2c3e50")
  ) +
  coord_flip()  # To transpose the axes

# Print the plot
print(transposed_bar_plot)
