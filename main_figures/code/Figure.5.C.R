# Figure 5.C
#you can find pairwise comparison and plotting of
# Load necessary libraries
library(dplyr)
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


csv2 <- read_csv("/Users/mustafakaratas/Desktop/Desktop - MustafaKaratas’s MacBook Pro/work/sequences_rota/sequences_chosen/processed_blast_results2.csv")
#csv is your first dataframe and csv2 is your second dataframe
together <- left_join(csv, csv2, by = c("Sample-ID" = "SampleName"))

# Add a new column "New Genotypes" based on "ReferenceType"
together <- together %>%
  mutate(New_Genotypes = ifelse(Genotype == "G3P[8]", ReferenceType, Genotype))

# Filter the data for the specific genotype and exclude "UNKNOWN" status
filtered_data <- together %>%
  filter(New_Genotypes == "'MW280957.1_eqlike'" & `Rotarix-vaccination-status` != "UNKNOWN")

# Filter the data frame to include relevant genotypes and exclude "UNKNOWN" status
filtered_data <- together %>%
  filter(New_Genotypes %in% c("G2P[4]", "'MF469162.1_human'", "'MW280957.1_eqlike'", "G9P[8]", "G1P[8]", "G12P[8]", "G4P[8]", "G9P[4]") & `Rotarix-vaccination-status` != "UNKNOWN" & `RotaTeq-vaccination-status` != "Y" & Age_Group == "0-2" )

# Convert 'Rotarix-vaccination-status' to a binary indicator (1 for vaccinated, 0 for not vaccinated)
filtered_data2 <- filtered_data %>%
  mutate(Vaccinated = ifelse(`Rotarix-vaccination-status` == "Y", 1, 0))

# Calculate proportions for each genotype
genotype_proportions <- filtered_data2 %>%
  group_by(New_Genotypes) %>%
  summarise(
    count_vaccinated = sum(Vaccinated),
    total = n(),
    proportion_vaccinated = count_vaccinated / total
  ) %>%
  ungroup()

# Overall vaccination rate
overall_vaccination_rate <- sum(genotype_proportions$count_vaccinated) / sum(genotype_proportions$total)

# Expected counts based on overall vaccination rate
genotype_proportions <- genotype_proportions %>%
  mutate(expected_vaccinated = total * overall_vaccination_rate)

# Define the Z-test function for your dataset
perform_z_test <- function(count1, total1, count2, total2) {
  # Calculate the proportions for each group
  proportion1 <- count1 / total1
  proportion2 <- count2 / total2
  
  # Calculate the pooled standard error
  pooled_se <- sqrt((proportion1 * (1 - proportion1) / total1) + (proportion2 * (1 - proportion2) / total2))
  
  # Calculate the Z-statistic
  z_stat <- (proportion1 - proportion2) / pooled_se
  
  # Calculate the p-value (two-tailed test)
  p_value <- 2 * pnorm(-abs(z_stat))
  
  return(p_value)
}

# All pairs of genotypes
genotype_pairs <- combn(genotype_proportions$New_Genotypes, 2, simplify = FALSE)

# Perform pairwise Z-tests
pairwise_results <- map(genotype_pairs, function(pair) {
  genotype1 <- filter(genotype_proportions, New_Genotypes == pair[1])
  genotype2 <- filter(genotype_proportions, New_Genotypes == pair[2])
  
  p_value <- perform_z_test(
    genotype1$count_vaccinated, genotype1$total, 
    genotype2$count_vaccinated, genotype2$total
  )
  
  data.frame(
    genotype1 = pair[1], 
    genotype2 = pair[2], 
    p_value = p_value
  )
}) %>% bind_rows()

# Adjust for multiple comparisons
pairwise_results$adjusted_p_value <- p.adjust(pairwise_results$p_value, method = "bonferroni")

# Formatting the p-values
pairwise_results$formatted_p_value <- format(pairwise_results$p_value, digits = 4, scientific = FALSE)
pairwise_results$formatted_adjusted_p_value <- format(pairwise_results$adjusted_p_value, digits = 4, scientific = FALSE)

# Print the formatted results
print(pairwise_results)

# Create a data frame for plotting
plot_data <- genotype_proportions %>%
  select(New_Genotypes, observed = count_vaccinated, expected = expected_vaccinated)
# Rename values in the New_Genotypes column
plot_data <- plot_data %>%
  mutate(New_Genotypes = case_when(
    New_Genotypes == "'MF469162.1_human'" ~ " Typical human VP7 G3P[8]",
    New_Genotypes == "'MW280957.1_eqlike'" ~ " Equine-like VP7 G3P[8]",
    TRUE ~ New_Genotypes
  ))

# Plot the observed vs expected counts for each genotype
ggplot(plot_data, aes(x = New_Genotypes)) +
  geom_bar(aes(y = observed, fill = "Observed"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = expected, fill = "Expected"), stat = "identity", position = "dodge", color = "black") +
  theme_minimal() +
  labs(title = "Observed vs Expected Counts of Vaccinated Individuals by Genotype",
       x = "Genotype",
       y = "Count") +
  scale_fill_manual(values = c("Observed" = "#1E88E5", "Expected" = "transparent")) +
  scale_color_manual(values = c("Expected" = "black"))
