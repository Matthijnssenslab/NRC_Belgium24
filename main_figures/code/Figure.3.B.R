#Figure 3.B
#0-2 year old comparison over the years, pairwise composition comparison and comparison with average of other seasons is available below.

# Load necessary library
if (!require(dplyr)) install.packages("dplyr")
library(dplyr)

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

# Subset the data to only include the 2-5 age group
age_group_2_5 <- age_group_summary %>%
  filter(Age_Group == "2-5")

# Select the 2021-2022 season for comparison with others
ref_season <- age_group_2_5 %>%
  filter(season == "2021-2022")

# Function to perform Z-test
perform_z_test <- function(count1, n1, count2, n2) {
  p1 <- count1 / n1
  p2 <- count2 / n2
  p <- (count1 + count2) / (n1 + n2)
  z <- (p1 - p2) / sqrt(p * (1 - p) * (1/n1 + 1/n2))
  p_value <- 2 * pnorm(-abs(z))
  return(p_value)
}

# Apply the Z-test for each season against the 2021-2022 season
results <- lapply(age_group_2_5$season, function(season) {
  if (season != "2021-2022") {
    season_data <- age_group_2_5 %>%
      filter(season == season)
    p_value <- perform_z_test(ref_season$count, ref_season$total, season_data$count, season_data$total)
    return(data.frame(season1 = "2021-2022", season2 = season, p_value = p_value))
  }
})
# Calculate the mean proportion for all seasons except 2021-2022
mean_proportion_rest <- age_group_2_5 %>%
  filter(season != "2021-2022") %>%
  summarize(mean_count = mean(count), mean_total = mean(total)) %>%
  mutate(mean_proportion = mean_count / mean_total) %>%
  pull(mean_proportion)

# Proportion for 2021-2022 season
proportion_2021_2022 <- age_group_2_5 %>%
  filter(season == "2021-2022") %>%
  mutate(proportion = count / total) %>%
  pull(proportion)

# Perform Z-test between 2021-2022 season and the mean of the rest
p_value <- perform_z_test(ref_season$count, ref_season$total, mean_proportion_rest * ref_season$total, ref_season$total)

# Print the result
print(paste("P-value for comparison with 2021-2022: ", p_value))


# Create a data frame for plotting
plot_data <- data.frame(
  Season = c("2021-2022", "Average of Other Seasons"),
  Proportion = c(proportion_2021_2022, mean_proportion_rest)
)

# Plot
ggplot(plot_data, aes(x = Season, y = Proportion, fill = Season)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Comparison of 2-5 Age Group Proportion",
       x = "Season",
       y = "Proportion") +
  scale_fill_brewer(palette = "Set1")

#given that 2016-17 and 17-18 has proportionally less 2-5 years old children (due to outbreak in Mechelen, retirement home), it might appear 2-5 year age group is underrepresented. 
#however, for calculation purposes, I did not remove those two years. Removing those two years did not change the result of our comparison, so I left them included.

#above, we have conducted z test for 2 proportions: 2021-2022 and rest. Below, you can find pairwise calculations.

# Load necessary library
if (!require(dplyr)) install.packages("dplyr")
library(dplyr)

# Subset the data to only include the 2-5 age group
age_group_2_5 <- age_group_summary %>%
  filter(Age_Group == "2-5")

# Proportion for 2021-2022 season
proportion_2021_2022 <- age_group_2_5 %>%
  filter(season == "2021-2022") %>%
  summarise(proportion = sum(count) / sum(total)) %>%
  pull(proportion)

# Function to perform Z-test
perform_z_test <- function(count1, n1, count2, n2) {
  p1 <- count1 / n1
  p2 <- count2 / n2
  p <- (count1 + count2) / (n1 + n2)
  z <- (p1 - p2) / sqrt(p * (1 - p) * (1/n1 + 1/n2))
  p_value <- 2 * pnorm(-abs(z))
  return(p_value)
}

# Apply the Z-test for each season against the 2021-2022 season
comparison_results <- age_group_2_5 %>%
  filter(season != "2021-2022") %>%
  group_by(season) %>%
  summarise(p_value = perform_z_test(sum(count), sum(total), proportion_2021_2022 * sum(total), sum(total)))

# Apply Bonferroni correction
comparison_results$adjusted_p_value <- p.adjust(comparison_results$p_value, method = "bonferroni")

# Print the results
print(comparison_results)

# Formatting the p-values
comparison_results$formatted_p_value <- format(comparison_results$p_value, digits = 4, scientific = FALSE)
comparison_results$formatted_adjusted_p_value <- format(comparison_results$adjusted_p_value, digits = 4, scientific = FALSE)

# Print the formatted results
print(comparison_results)


#####plotting expected and observed counts for 0-2 years#####

# Subset the data to only include the 2-5 age group
age_group_2_5 <- age_group_summary %>%
  filter(Age_Group == "2-5")

# Calculate the average expected percentage excluding the 2021-2022 season
average_percentage_excluding_ref <- age_group_2_5 %>%
  filter(season != "2021-2022") %>%
  summarise(avg_percentage = sum(count) / sum(total))

# Calculate expected counts for each season using the average percentage
expected_counts <- age_group_2_5 %>%
  group_by(season) %>%
  summarise(total = sum(total)) %>%
  mutate(expected = total * average_percentage_excluding_ref$avg_percentage)

# Calculate observed counts per season
observed_counts <- age_group_2_5 %>%
  group_by(season) %>%
  summarise(observed = sum(count))

# Combine observed and expected counts into a single data frame for plotting
counts_data <- observed_counts %>%
  left_join(expected_counts, by = "season") %>%
  select(season, observed, expected) %>%
  pivot_longer(cols = c("observed", "expected"), names_to = "type", values_to = "count")

# Plotting with overlapping bars for observed and expected counts
ggplot(counts_data) +
  geom_bar(data = subset(counts_data, type == "observed"), aes(x = season, y = count, fill = "Observed"), stat = "identity", width = 0.7) +
  geom_bar(data = subset(counts_data, type == "expected"), aes(x = season, y = count, fill = "Expected"), stat = "identity", width = 0.7, alpha = 0.5) +
  scale_fill_manual(values = c("Observed" = "blue", "Expected" = "red")) +
  theme_minimal() +
  labs(title = "Observed vs Expected Counts by Season (Excluding 2021-2022)",
       x = "Season",
       y = "Count") +
  theme(legend.title = element_blank())


#colors and annotations were improved using Adobe Illustrator 2024.
#different panels were combined with Adobe Illustrator 2024
