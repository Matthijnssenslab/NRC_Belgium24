#Figure 4.B
library(dplyr)
library(ggplot2)
csv <- "/Users/mustafakaratas/OneDrive - KU Leuven/Attachments/all-in-one page.csv"
csv <- read_csv(csv)
csv2 <- read_csv("/Users/mustafakaratas/Desktop/Desktop - MustafaKaratas’s MacBook Pro/work/sequences_rota/sequences_chosen/processed_blast_results2.csv")
#csv is your first dataframe and csv2 is your second dataframe
together <- left_join(csv, csv2, by = c("Sample-ID" = "SampleName"))


#our data frame is called together
# Step 1: Filter for G3P[8] sequences
g3p8_data <- together %>%
  filter(Genotype == 'G3P[8]')


# Filter out rows with NA or AlignmentLength less than 500, and identity is higher than 80. 
#Please keep in mind that Sanger sequencing data from different years have different quality measurements. 
filtered_data <- g3p8_data %>%
  filter(!is.na(AlignmentLength), AlignmentLength >= 500, Identity >= 80)

# Manually calculate the counts for each ReferenceType per season
counts_manual <- filtered_data %>%
  group_by(season, ReferenceType) %>%
  summarise(count = n())

# Calculate percentages
percentages_manual <- counts_manual %>%
  group_by(season) %>%
  mutate(perc = count / sum(count) * 100)

# Plotting the percentage stacked bar plot
ggplot(percentages_manual, aes(fill=ReferenceType, y=perc, x=season)) +
  geom_bar(stat="identity", position="fill") +
  scale_fill_manual(values=c("darkgreen", "lightgreen")) +
  coord_flip() +
  labs(y = "Percentage", x = "Season", title = "Percentage of Each Reference Type per Season")

# Plotting the count stacked bar plot
ggplot(counts_manual, aes(fill=ReferenceType, y=count, x=season)) +
  geom_bar(stat="identity", position="stack") +
  scale_fill_manual(values=c("darkgreen", "lightgreen")) +
  coord_flip() +
  labs(y = "Count", x = "Season", title = "Count of Each Reference Type per Season")


#colors were changed and different panels were combined in Adobe Illustrator 2024.
