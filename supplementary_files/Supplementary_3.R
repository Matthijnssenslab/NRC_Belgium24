#lets make a table of all data

# Load Libraries
library(dplyr)
library(tidyr)


csv <- "/Users/mustafakaratas/OneDrive - KU Leuven/Attachments/all-in-one page.csv"
csv <- read_csv(csv)

# Load necessary libraries
library(dplyr)
library(tidyr)
library(readr)  # Ensure this is loaded for read_csv

#group not typed ones together. 
#`No rotavirus detected`, Notapplicable, `Sample missing`, `not tested`
# Replace specified genotypes with 'Not Typed'
csv$Genotype <- as.factor(ifelse(csv$Genotype %in% c("No rotavirus detected", "NotApplicable", "Sample Missing", "not tested", NA), "Not Typed", as.character(csv$Genotype)))
csv$Genotype <- as.factor(ifelse(csv$Genotype %in% c("G1P[8]vaccineDerived","G1[8]vaccineDerived" ), "Vaccinederived_G1P[8]", as.character(csv$Genotype)))

# Group by season and Genotype, then summarise to count occurrences
genotype_summary <- csv %>%
  group_by(season, Genotype) %>%
  summarise(n = n(), .groups = 'drop')

# Pivot the summarised data to create a crosstab
crosstab_result <- genotype_summary %>%
  pivot_wider(names_from = Genotype, values_from = n, values_fill = list(n = 0)) %>%
  rowwise() %>%
  mutate(Total = sum(c_across(-season), na.rm = TRUE)) %>%
  ungroup()

print(crosstab_result)

# Add a row at the bottom with totals for all columns except 'season'
totals <- crosstab_result %>% 
  summarise(across(-season, sum, na.rm = TRUE)) %>%
  mutate(season = "Total")

# Append the totals row to the original crosstab_result
crosstab_result <- bind_rows(crosstab_result, totals)

# View the updated crosstab result with totals for each column and row
print(crosstab_result)

write_csv2(x = crosstab_result,file = "crosstab_results_genotype.csv")


####create other####
library(dplyr)

# Exclude the 'season' column from the check
columns_to_aggregate <- names(crosstab_result)[-1][sapply(crosstab_result[-1], function(x) sum(x) < 100)]

# Aggregate those columns into a new column called "Other"
crosstab_result2 <- crosstab_result %>%
  mutate(Other = rowSums(select(., all_of(columns_to_aggregate)))) %>%
  select(-one_of(columns_to_aggregate))

# If needed, reorder columns with "Other" at the end
crosstab_result2 <- crosstab_result2 %>%
  select(season, everything(), Other)

# Print the updated dataframe
print(crosstab_result2)



write_csv2(x = crosstab_result2,file = "crosstab_results_genotype_other.csv")


library(dplyr)
library(tidyr)
library(readr)
# read your data from the csv file
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


# Create a cross-tabulation for age group and season
cross_tab_age_season <- csv %>%
  group_by(season, Age_Group) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = Age_Group, values_from = count, values_fill = 0) %>%
  ungroup()

# Print the cross-tabulation
print(cross_tab_age_season)

write_csv2(cross_tab_age_season, file = "cross_tab_age_season.csv")




