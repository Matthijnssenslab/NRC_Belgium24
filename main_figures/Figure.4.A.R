#Figure 4.A
library("dplyr")
library("ggplot2")

#your dataframe is named csv
csv <- "/Users/mustafakaratas/OneDrive - KU Leuven/Attachments/all-in-one page.csv"
csv <- read_csv(csv)

# Clean and summarize the data (filtered things below all mean genotyping was not successful for mentioned sample therefore excluded)
cleaned_data <- csv %>%
  filter(!is.na(Genotype), !is.na(season)) %>%
  filter(!Genotype %in% c("NotApplicable", "No rotavirus detected", "not tested", "Sample Missing")) 

# Identify genotypes to be labeled as "Other" based on their overall counts, vaccine derived cases are also included under this category since they cannot be thought as "wild type" G1P[8]
other_genotypes <- cleaned_data %>%
  group_by(Genotype) %>%
  summarise(total_count = n()) %>%
  filter(total_count < 40) %>%
  pull(Genotype)

# Group the data by 'Genotype' and count the number of samples
genotype_counts <- cleaned_data %>%
  group_by(Genotype) %>%
  summarise(count = n())

# Print the result
print(genotype_counts)

# Replace these genotypes with "Other" in the cleaned data
cleaned_data$Genotype[cleaned_data$Genotype %in% other_genotypes] <- 'Other'

# Summarize data by season and Genotype
summary_table <- cleaned_data %>%
  group_by(season, Genotype) %>%
  summarise(Count = n(), .groups = 'drop')

# Calculate percentage for each genotype per season such that it sums up to 100% for each season
summary_table <- summary_table %>%
  group_by(season) %>%
  mutate(Total_Count = sum(Count),
         Percentage = (Count / Total_Count) * 100)

# Print the updated summary table
print(summary_table)
# Get total counts for each season
total_counts <- summary_table %>%
  group_by(season) %>%
  summarise(Total_Count = sum(Count), .groups = 'drop')

# Create the bar plot
p <- ggplot(summary_table, aes(x = season, y = Percentage, fill = Genotype)) +
  geom_bar(stat = "identity") +
  geom_text(data = total_counts, aes(x = season, y = 100, label = sprintf("n=%d", Total_Count)), 
            inherit.aes = FALSE, vjust = -0.5, size = 4, color = "black", 
            fontface = "bold") +
  geom_text(aes(label = ifelse(Genotype == "G3P[8]", paste0(sprintf("%.2f", Percentage), "%"), "")),
            position = position_stack(vjust = 0.5), size = 3.0, color = "white")+
  labs(title = "Relative Abundance of Genotypes per Season",
       x = "Season",
       y = "Percentage") +
  theme(axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 14, face = "bold"),
        legend.title = element_text("Genotype"),
        legend.position = "bottom") +
  scale_fill_brewer(palette = "Paired")


# Display and save the plot
print(p)
ggsave("rota_genotyperelative_abundance_plot.png", p, width = 10, height = 8)
