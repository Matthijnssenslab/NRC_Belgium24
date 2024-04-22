#Figure 5.A
library(dplyr)
library(readr)
library(ggplot2)
#combine results
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


# Filter data for the desired age groups and exclude "UNKNOWN" vaccination status
filtered_data <- csv %>%
  filter(Age_Group %in% c("0-2") & `Rotarix-vaccination-status` != "UNKNOWN") %>%
  filter(`RotaTeq-vaccination-status` != "Y")


# Group by season, age group, and vaccination status
grouped_data <- filtered_data %>%
  group_by(season, Age_Group, `Rotarix-vaccination-status`) %>%
  summarise(Count = n())

# Calculate the percentage of vaccinated individuals for each group
grouped_data <- grouped_data %>%
  group_by(season, Age_Group) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)

# Create a data frame for the vaccination coverage
vax_coverage <- data.frame(
  season = c("2011-2012", "2012-2013", "2013-2014", "2014-2015", "2015-2016", "2016-2017", "2017-2018", "2018-2019", "2019-2020", "2020-2021", "2021-2022", "2022-2023"),
  coverage = c(86, 86, 86, 88, 87, 87, 87, 84, 86, 86, 86, 86)
)

# Create the plot with improved colors and single-line text
plot <- ggplot() +
  geom_bar(data = grouped_data, aes(x = season, y = Percentage, fill = `Rotarix-vaccination-status`), stat = "identity", position = "stack") +
  geom_text(data = subset(grouped_data, `Rotarix-vaccination-status` == "Y"),
            aes(x = season, y = Percentage, label = paste(round(Percentage, 1), "% (n=", Count, ")", sep = "")),
            position = position_stack(vjust = 0.5),
            size = 3, angle = 90, hjust = 0.5, color = "white") +  # Set text color to white
  geom_line(data = vax_coverage, aes(x = season, y = coverage, group = 1, color = "Coverage"), size = 1) +
  scale_fill_manual(values = c("Y" = "#1E88E5", "N" = "orange2")) +
  scale_color_manual(values = c("Coverage" = "black")) +  # Adding color and legend for coverage
  facet_wrap(~ Age_Group, scales = "free_y") +
  labs(title = "Percentage of Vaccinated Individuals by Age Group and Season",
       x = "Season",
       y = "Percentage",
       fill = "Vaccination Status") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.text = element_text(color = "black"))  # Set legend text color to black

# Print the plot
print(plot)


# Save the plot
ggsave("vaccination_percentage_by_age_group.png", plot = plot, width = 16, height = 8)
