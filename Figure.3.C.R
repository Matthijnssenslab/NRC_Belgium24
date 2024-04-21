#Figure.3.C

# Load necessary libraries
library(dplyr)
library(ggplot2)

#load the data
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

# Subset the data to only include the 60+ age group
age_group_60_plus <- age_group_summary %>%
  filter(Age_Group == "60+")

# Calculate observed counts for all seasons
observed_counts <- age_group_60_plus %>%
  group_by(season) %>%
  summarise(observed_count = sum(count), total = sum(total))

# Calculate the expected proportion excluding 2016-2017 and 2017-2018 seasons
expected_proportion <- age_group_60_plus %>%
  filter(!season %in% c("2016-2017", "2017-2018")) %>%
  summarise(expected_count = sum(count), total = sum(total)) %>%
  mutate(expected_proportion = expected_count / total) %>%
  pull(expected_proportion)

# Calculate expected counts for each season based on the expected proportion
observed_counts <- observed_counts %>%
  mutate(expected_count = total * expected_proportion)

# Function to perform Z-test for proportion
perform_proportion_z_test <- function(observed_count, total, expected_proportion) {
  observed_prop <- observed_count / total
  p <- (observed_count + (expected_proportion * total)) / (total + total)
  z <- (observed_prop - expected_proportion) / sqrt(p * (1 - p) * (1/total + 1/total))
  p_value <- 2 * pnorm(-abs(z))
  return(p_value)
}

# Apply the Z-test for proportion for each season
z_test_results <- observed_counts %>%
  rowwise() %>%
  mutate(p_value = perform_proportion_z_test(observed_count, total, expected_proportion))

# Number of comparisons (number of seasons)
num_comparisons <- nrow(z_test_results)

# Apply Bonferroni correction
z_test_results <- z_test_results %>%
  mutate(bonferroni_p_value = pmin(p_value * num_comparisons, 1))  # Corrected p-values, capped at 1

# Print the results
print(z_test_results)

# Create a data frame with "2011-2012" season and zero observations, otherwise your plot will miss the year with 0 observations.
extra_season <- data.frame(
  season = "2011-2012",
  observed_count = 0,
  total = 0,
  expected_count = 0,
  p_value = 1,
  bonferroni_p_value = 1
  
)

# Combine the original data with the extra_season data
z_test_results_with_extra <- rbind(z_test_results, extra_season)

# Plotting the results: Observed Counts vs. Expected Counts with "Expected" bars fading
ggplot(z_test_results_with_extra, aes(x = season)) +
  geom_bar(aes(y = observed_count, fill = "Observed"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = expected_count, fill = "Expected"), stat = "identity", position = "dodge", alpha = 0.5) + # Adjust alpha for fading effect
  theme_minimal() +
  labs(title = "Observed vs Expected Counts Across All Seasons",
       x = "Season",
       y = "Count") +
  scale_fill_manual(values = c("Observed" = "blue", "Expected" = "red"))

#colors and style was changed using Adobe Illustrator 