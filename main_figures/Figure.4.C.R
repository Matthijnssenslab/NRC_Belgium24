#Figure 4.C
#genotype DS-1 like backbone and WA-like backbone

csv <- "/Users/mustafakaratas/OneDrive - KU Leuven/Attachments/all-in-one page.csv"
csv <- read_csv(csv)
csv2 <- read_csv("/Users/mustafakaratas/Desktop/Desktop - MustafaKaratas’s MacBook Pro/work/sequences_rota/sequences_chosen/processed_blast_results2.csv")
#csv is your first dataframe and csv2 is your second dataframe
together <- left_join(csv, csv2, by = c("Sample-ID" = "SampleName"))

#group not typed ones together. 
#`No rotavirus detected`, Notapplicable, `Sample missing`, `not tested`

# Add a new column "New Genotypes" based on "ReferenceType"
together <- together %>%
  mutate(New_Genotypes = ifelse(Genotype == "G3P[8]", ReferenceType, Genotype))

# Replace specified genotypes with 'Not Typed'
together$New_Genotypes <- as.factor(ifelse(together$New_Genotypes %in% c("No rotavirus detected", "NotApplicable", "Sample Missing", "not tested", NA), "Not Typed", as.character(together$New_Genotypes)))
together$New_Genotypes <- as.factor(ifelse(together$New_Genotypes %in% c("G1P[8]vaccineDerived","G1[8]vaccineDerived" ), "Vaccinederived_G1P[8]", as.character(together$New_Genotypes)))

# Group by season and Genotype, then summarise to count occurrences
genotype_summary <- together %>%
  group_by(season, New_Genotypes) %>%
  summarise(n = n(), .groups = 'drop')

# Pivot the summarised data to create a crosstab
crosstab_result <- genotype_summary %>%
  pivot_wider(names_from = New_Genotypes, values_from = n, values_fill = list(n = 0)) %>%
  rowwise() %>%
  mutate(Total = sum(c_across(-season), na.rm = TRUE)) %>%
  ungroup()

# Selecting columns to keep
columns_to_keep <- c("season", "G12P[8]", "G1P[8]", "G2P[4]", "'MF469162.1_human'", "G4P[8]", "G9P[4]", "G9P[8]", "'MW280957.1_eqlike'")

# Subsetting the dataframe to keep only selected columns
crosstab_result <- crosstab_result %>%
  select(all_of(columns_to_keep))

# Print the modified dataframe
print(crosstab_result)

#G2P[4], 'MW280957.1_eqlike', G9P[4] should be DS_1_like and rest should be WA_like

library(dplyr)

# Define the columns to be renamed
columns_to_rename <- c("G2P[4]", "'MW280957.1_eqlike'", "G9P[4]")

# Copy the original dataframe
renamed_crosstab_result <- crosstab_result

# Rename the selected columns
colnames(renamed_crosstab_result)[colnames(renamed_crosstab_result) %in% columns_to_rename] <- "DS_1_like"
# Rename the selected columns to 'WA_like'
colnames(renamed_crosstab_result)[colnames(renamed_crosstab_result) %in% c("G12P[8]", "G1P[8]", "'MF469162.1_human'", "G4P[8]", "G9P[8]")] <- "WA_like"

# Reshape the data for counting
count_data <- renamed_crosstab_result %>%
  pivot_longer(cols = c(DS_1_like, WA_like), names_to = "Genotype_Like", values_to = "Count") %>%
  mutate(Genotype_Like = ifelse(Genotype_Like == "DS_1_like", "DS_1_like", "WA_like"))

# Count the number of samples for each season under DS_1_like and WA_like
count_per_season <- count_data %>%
  group_by(season, Genotype_Like) %>%
  summarise(Sample_Count = sum(Count))

# Print the new dataframe
print(count_per_season)


# Print the new dataframe
print(count_per_season)
library(dplyr)
library(ggplot2)

# Calculate total counts for each season
total_counts <- count_per_season %>%
  group_by(season) %>%
  summarise(total_count = sum(Sample_Count))

# Calculate percentages for each season
percentage_per_season <- count_per_season %>%
  left_join(total_counts, by = "season") %>%
  mutate(percentage = (Sample_Count / total_count) * 100)

# Plotting
ggplot(percentage_per_season, aes(x = season, y = percentage, fill = Genotype_Like)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Percentage of Samples by Genotype_Like per Season",
       x = "Season",
       y = "Percentage") +
  scale_fill_manual(values = c("DS_1_like" = "gray25", "WA_like" = "gray70")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

