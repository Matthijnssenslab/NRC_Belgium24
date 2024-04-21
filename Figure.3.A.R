#Figure 3.A

library(tidyverse) #Collection of packages in the tidyverse (see https://www.tidyverse.org/)
library(RColorBrewer)

csv <- "/Users/mustafakaratas/OneDrive - KU Leuven/Attachments/all-in-one page.csv"
csv <- read_csv(csv)

# Convert the date columns to Date class
csv$`Date-of-sample-collection` <- as.Date(csv$`Date-of-sample-collection`, format="%d.%m.%Y")
csv$`Date of Birth` <- as.Date(csv$`Date of Birth`, format="%d.%m.%Y")

# Calculate age
csv$Calculated_Age <- as.numeric(difftime(csv$`Date-of-sample-collection`, csv$`Date of Birth`, units = "weeks") / 52.25)



# Create age groups
csv$Age_Group <- cut(csv$Calculated_Age, 
                     breaks = c(0, 2, 5, 18, 60, Inf), 
                     labels = c("0-2", "2-5", "5-18", "18-60", "60+"), 
                     right = FALSE, 
                     include.lowest = TRUE)

# Filter out rows where Age or Age Group is NA
csv <- csv[!is.na(csv$Calculated_Age) & !is.na(csv$Age_Group), ]


# Group by season and age group, then summarize to get counts
age_group_summary <- csv %>%
  group_by(season, Age_Group) %>%
  summarise(count = n(), .groups = 'drop')
# Calculate the total counts per season for normalization
totals_per_season <- age_group_summary %>%
  group_by(season) %>%
  summarise(total = sum(count), .groups = 'drop')

# Join to get the total per season in each row, and calculate percentages
age_group_summary <- age_group_summary %>%
  left_join(totals_per_season, by = "season") %>%
  mutate(percentage = (count / total) * 100)

# Define a color palette
color_palette <- brewer.pal("Dark2", n = length(unique(age_group_summary$Age_Group)))

# Add total counts for each season to the age_group_summary for labeling
age_group_summary <- age_group_summary %>%
  group_by(season) %>%
  mutate(season_total = sum(count)) %>%
  ungroup()

# Plot with the new color palette
final_plot <- ggplot(age_group_summary, aes(x = season, y = percentage, fill = Age_Group)) +
  geom_bar(stat = "identity", show.legend = TRUE) +
  scale_fill_manual(values = color_palette) +
  geom_text(aes(label = ifelse(percentage > 5, paste0(sprintf("%.2f", percentage), "%"), "")),
            position = position_stack(vjust = 0.5), size = 3.5, color = "white") +
  geom_text(data = age_group_summary %>% distinct(season, .keep_all = TRUE),
            aes(x = season, y = 100, label = paste("n =", season_total)),
            vjust = -0.5, size = 3.5, fontface = "bold", color = "black") +
  labs(x = "Season", y = "Percentage", fill = "Age Group") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "bottom")

# Print the plot
print(final_plot)

# Use ggsave to save the plot to a file
ggsave("final_agegroups.png", final_plot, width = 12, height = 6, units = "in", dpi = 300)


#colors were changed by using Adobe Illustrator 2024
#different panels were combined with Adobe Illustrator 2024
