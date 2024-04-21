#Figure 5.D

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
# csv is your first dataframe and csv2 is your second dataframe
together <- left_join(csv, csv2, by = c("Sample-ID" = "SampleName"))


# Add a new column "New Genotypes" based on "ReferenceType"
together <- together %>%
  mutate(New_Genotypes = ifelse(Genotype == "G3P[8]", ReferenceType, Genotype))


# Filter the data frame to include relevant genotypes and exclude "UNKNOWN" status, look at only 2021-2022
filtered_data <- together %>%
  filter(New_Genotypes %in% c("'MF469162.1_human'", "'MW280957.1_eqlike'") & `Rotarix-vaccination-status` != "UNKNOWN" & (season == "2021-2022") & Age_Group == "0-2"  )

# Calculate the percentage of vaccinated and not vaccinated for each genotype
vaccination_data <- filtered_data %>%
  group_by(New_Genotypes, `Rotarix-vaccination-status`) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)

# Rename values in the New_Genotypes column
vaccination_data <- vaccination_data %>%
  mutate(New_Genotypes = case_when(
    New_Genotypes == "'MF469162.1_human'" ~ " Typical human VP7 G3P[8]",
    New_Genotypes == "'MW280957.1_eqlike'" ~ " Equine-like VP7 G3P[8]",
    TRUE ~ New_Genotypes
  ))

# Create a stacked bar plot with all genotypes
plot <- ggplot(data = vaccination_data, aes(x = New_Genotypes, y = Percentage, fill = `Rotarix-vaccination-status`)) +
  geom_bar(stat = "identity") +
  labs(title = "Rotarix Vaccination Status for Multiple Genotypes",
       x = "Genotype",
       y = "Percentage (%)") +
  theme_minimal() +
  scale_fill_manual(values = c("Y" = "#1E88E5", "N" = "orange2")) +
  guides(fill = guide_legend(title = "Vaccination Status")) +
  geom_text(aes(label = paste( "n =", Count,"(", round(Percentage, 1), "%)")),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white",
            angle = 90, hjust = 0.5, vjust = 0.5)  # Rotate text vertically

# Display the stacked bar plot
plot




