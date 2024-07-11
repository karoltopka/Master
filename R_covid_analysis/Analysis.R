library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
#1.1
# Read the CSV file into a variable called 'inf_dis'
inf_dis <- read_csv("disease.csv", 
                    col_types = cols_only(
                      Disease = col_factor(),
                      County = col_factor(),
                      Year = col_double(),
                      Sex = col_factor(),
                      Cases = col_double(),
                      Population = col_double(),
                      Rate = col_double(),
                      Lower_95__CI = col_double(),
                      Upper_95__CI = col_double()
                    ))
print(inf_dis)

#1.2
# Filter the data set
inf_dis <- inf_dis %>%
  drop_na(Population) %>%  # Remove rows with NA in Population
  drop_na(Cases) %>%  # Remove rows with NA in Cases
  filter(Cases != 0)  # Remove rows with Cases equal to 0

# Save the filtered data set
inf_dis <- inf_dis
print(inf_dis)
# 1.3 Create a variable for the sum of Cases for each Disease
cases_sum <- inf_dis %>%
  group_by(Disease) %>%
  summarize(Sum_Cases = sum(Cases))

unique_diseases <- cases_sum %>% 
  summarise(unique_diseases = n_distinct(Disease))
print(unique_diseases)

# Create a ggplot with flipped coordinates and clean x-axis labels
ggplot(cases_sum, aes(x = reorder(Disease, Sum_Cases), y = Sum_Cases)) +
  geom_bar(stat = "identity")+
  xlab("Disease") +
  ylab("Sum of Cases") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_y_continuous(labels = label_number(accuracy = 1))

# Second plot 
ggplot(cases_sum, aes(x = reorder(Disease, Sum_Cases), y = Sum_Cases)) +
  geom_bar(stat = "identity")+
  xlab("Disease") +
  ylab("Sum of Cases") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))+
  scale_y_continuous(labels = label_number(accuracy = 1))

#Third plot
ggplot(cases_sum, aes(x = reorder(Disease, Sum_Cases), y = Sum_Cases)) +
  geom_bar(stat = "identity")+
  xlab("Disease") +
  ylab("Sum of Cases") +
  theme(axis.text.x = element_blank(),
  axis.ticks.x = element_blank()) + 
  scale_y_continuous(labels = label_number(accuracy = 1))

# Fourth plot filter dataset
threshold <- 1000
significant_cases <- cases_sum %>%
  filter(Sum_Cases > threshold)

ggplot(significant_cases, aes(x = reorder(Disease, Sum_Cases), y = Sum_Cases)) +
  geom_bar(stat = "identity")+
  xlab("Disease") +
  ylab("Sum of Cases") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_y_continuous(labels = label_number(accuracy = 1)) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))

#I've made this like that coz i didn't really know what You expected by "clean the X axis", so yeah i've made couple approaches.
# Identify 7 outliers (top 7 highest sum values)
outliers <- cases_sum %>%
  arrange(desc(Sum_Cases)) %>%
  head(7)
print(outliers)

# Create a graph with y-axis limited to the lowest of the top 7 sum values
lowest_top7 <- min(outliers$Sum_Cases) - 1
cases_sum_filtered <- cases_sum %>%
  arrange(desc(Sum_Cases)) %>%
  slice(8:n())
ggplot(cases_sum_filtered, aes(x = reorder(Disease, Sum_Cases), y = Sum_Cases)) +
  geom_bar(stat = "identity") +
  xlab("Disease") +
  ylab("Sum of Cases") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  ylim(0, lowest_top7)


# 1.4 Create a subset 'top_data' with the top 10 diseases by summarized number of cases
top_diseases <- cases_sum %>%
  arrange(desc(Sum_Cases)) %>%
  head(10) %>%
  pull(Disease)

top_data <- inf_dis %>%
  filter(Disease %in% top_diseases) %>%
  mutate(Disease = factor(Disease, levels = top_diseases))

# Line graph with layer mapping for the top 10 diseases
top_diseases <- cases_sum %>%
  arrange(desc(Sum_Cases)) %>%
  head(10) %>%
  pull(Disease)

top_data <- inf_dis %>%
  filter(Disease %in% top_diseases) %>%
  mutate(Disease = factor(Disease, levels = top_diseases))

# Aggregatting
top_data_aggregated <- top_data %>%
  group_by(Disease, Year) %>%
  summarise(Total_Cases = sum(Cases), .groups = 'drop')

# Line graph with aggregated data
ggplot(top_data_aggregated, aes(x = Year, y = Total_Cases, color = Disease)) +
  geom_line() +
  scale_y_continuous(limits = c(0, 2000)) +
  labs(title = "No. Cases recorded for top 10 infectious diseases",
       subtitle = "Trends over years",
       x = "Year",
       y = "No. Cases",
       color = "Disease") +
  theme_dark()

# 1.5 Create a summarized data frame for the top 10 diseases
top_cases_sum <- top_data %>%
  group_by(Disease) %>%
  summarize(Sum_Cases = sum(Cases))

# Create a bar plot for the summarized cases
ggplot(top_cases_sum, aes(x = reorder(Disease, Sum_Cases), y = Sum_Cases)) +
  geom_col(fill = "steelblue") +
  xlab("Disease") +
  ylab("Sum of Cases") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Create a line graph with facet_wrap for the top 10 diseases
ggplot(top_data, aes(x = Year, y = Cases)) +
  geom_line() +
  facet_wrap(~ Disease, ncol = 2) +
  labs(title = "No. Cases over the years for top 10 infectious diseases",
       x = "Year",
       y = "No. Cases")

# Top 3 diseases by summarized number of cases
top3_cases_sum <- top_cases_sum %>%
  arrange(desc(Sum_Cases)) %>%
  head(3)
print(top3_cases_sum)

# 1.6 Choose one disease from the top 3 and create a subset
chosen_disease <- top3_cases_sum$Disease[1]
subset_data <- top_data %>%
  filter(Disease == chosen_disease) %>%
  select(-Disease, -Lower_95__CI, -Upper_95__CI)

# 1.7 Summarize the number of cases by sex and check for gender prevalence
cases_by_sex <- subset_data %>%
  group_by(Sex) %>%
  summarize(Sum_Cases = sum(Cases))

print(cases_by_sex)

# Males have a higher prevalence towards this infectious disease

# 1.8 Create a facet_grid line graph for cases by Year and Sex
colors <- c("Female" = "magenta", "Male" = "blue", "Total" = "green")
ggplot(subset_data, aes(x = Year, y = Cases, fill = Sex)) +
  geom_col() +
  scale_fill_manual(values = colors) +
  facet_grid(Sex ~ .) +
  labs(title = paste0("No. Cases by Year and Sex for ", chosen_disease),
       x = "Year",
       y = "No. Cases",
       fill = "Sex")

# Create a line graph for cases by Year and Sex with layer mapping
# Aggregate the data by Year and Sex
subset_data_aggregated <- subset_data %>%
  group_by(Year, Sex) %>%
  summarise(Total_Cases = sum(Cases), .groups = 'drop')

# Create the plot with aggregated data
ggplot(subset_data_aggregated, aes(x = Year, y = Total_Cases, color = Sex, group = Sex)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = colors) +
  labs(title = paste0("No. Cases by Year and Sex for ", chosen_disease),
       x = "Year",
       y = "No. Cases",
       color = "Sex") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
# Males and females do tend to get sick the most in the same years for campylobacteriosis.

# 1.9 Plot the population for 3 counties over the years
counties <- c("Santa Clara", "Alameda", "Alpine")
county_pop <- inf_dis %>%
  filter(County %in% counties) %>%
  select(County, Year, Population) %>%
  pivot_wider(names_from = County, values_from = Population, values_fn = mean)

ggplot(county_pop, aes(x = Year)) +
  geom_line(aes(y = `Santa Clara`), color = "red") +
  geom_line(aes(y = Alameda), color = "blue") +
  geom_line(aes(y = Alpine), color = "green") +
  labs(title = "Population over the years for selected counties",
       x = "Year",
       y = "Population")



#################################################################################################
#Exercise 2
#################################################################################################
# 2.1
herb_gdansk <- read.csv("/Users/Karez/Library/Mobile Documents/com~apple~CloudDocs/2_semestr_chemia/R/R_wtorek/Zadania zaliczenie/herb_gdansk_set.csv", sep = ";", header = TRUE)

herb_pomeranian <- read.csv("/Users/Karez/Library/Mobile Documents/com~apple~CloudDocs/2_semestr_chemia/R/R_wtorek/Zadania zaliczenie/herb_pomeranian_set.csv", sep = ";", header = TRUE)

#2.2
# Columns with NA in herb_gdansk
any(is.na(herb_gdansk))
# Columns with NA in herb_pomeranian
any(is.na(herb_pomeranian))
# Summarizing NA in herb_gdansk
sum(is.na(herb_gdansk))
# Summarizing NA in herb_pomeranian
sum(is.na(herb_pomeranian))

#2.3
# Finding columns with NA in herb_gdansk
colnames(herb_gdansk)[which(colSums(is.na(herb_gdansk)) > 0)]
# Finding columns with NA in herb_pomeranian
colnames(herb_pomeranian)[which(colSums(is.na(herb_pomeranian)) > 0)]
# Removing columns with NA from herb_gdansk
herb_gdansk <- herb_gdansk[, colSums(is.na(herb_gdansk)) == 0]
# Removing columns with NA from herb_pomeranian
herb_pomeranian <- herb_pomeranian[, colSums(is.na(herb_pomeranian)) == 0]
# Columns with NA in herb_gdansk after removal
colnames(herb_gdansk)[which(colSums(is.na(herb_gdansk)) > 0)]
# Columns with NA in herb_pomeranian after removal
colnames(herb_pomeranian)[which(colSums(is.na(herb_pomeranian)) > 0)]

#2.4
# Converting columns to factors in herb_gdansk
herb_gdansk <- herb_gdansk %>%
  mutate(across(c(Scientific.name, Kingdom, Phylum, Class, Order, Species), factor))
# Converting columns to factors in herb_pomeranian
herb_pomeranian <- herb_pomeranian %>%
  mutate(across(c(Scientific.name, Kingdom, Phylum, Class, Order, Species), factor))

length(levels(herb_gdansk$Scientific.name))
length(levels(herb_pomeranian$Scientific.name))

# Number of different scientific names in herb_gdansk: 9
# Number of different scientific names in herb_pomeranian: 402

#2.5
# Count occurrences of Scientific.name in herb_gdansk
herb_gdansk_counts <- herb_gdansk %>%
  count(Scientific.name, name = "count") %>%
  arrange(desc(count)) %>%
  head(6)
print(herb_gdansk_counts)
# Count occurrences of Scientific.name in herb_pomeranian
herb_pomeranian_counts <- herb_pomeranian %>%
  count(Scientific.name, name = "count") %>%
  arrange(desc(count)) %>%
  head(6)
print(herb_pomeranian_counts)

#2.6
# Filter Scientific.names starting with "Ph" in herb_gdansk
ph_count_gdansk <- herb_gdansk %>%
  count(Scientific.name, name = "count") %>%
  filter(grepl("^Ph", Scientific.name)) %>%
  arrange(Scientific.name)
# Filter Scientific.names starting with "Ph" in herb_pomeranian
ph_count_pomeranian <- herb_pomeranian %>%
  count(Scientific.name, name = "count") %>%
  filter(grepl("^Ph", Scientific.name)) %>%
  arrange(Scientific.name)

print(ph_count_gdansk)
print(ph_count_pomeranian)

#2.7
# Subset data for 'Lecanora' in herb_gdansk
lecanora_gdansk <- herb_gdansk %>%
  filter(grepl("Lecanora", Scientific.name))
# Subset data for 'Lecanora' in herb_pomeranian
lecanora_pomeranian <- herb_pomeranian %>%
  filter(grepl("Lecanora", Scientific.name))
# Convert Scientific.name to factor
lecanora_gdansk$Scientific.name <- factor(lecanora_gdansk$Scientific.name)
lecanora_pomeranian$Scientific.name <- factor(lecanora_pomeranian$Scientific.name)
# Change longitude to numeric
lecanora_gdansk$longitude <- as.numeric(lecanora_gdansk$longitude)
lecanora_pomeranian$longitude <- as.numeric(lecanora_pomeranian$longitude)
# Check number of levels in Scientific.name
length(levels(lecanora_gdansk$Scientific.name))
length(levels(lecanora_pomeranian$Scientific.name))
# lecanora_gdansk: 0
# lecanora_pomeranian: 7
#Longitude was removed before coz consists NA's
#Re-do with NA drop from rows not columns
herb_pomeranian <- read.csv("/Users/Karez/Library/Mobile Documents/com~apple~CloudDocs/2_semestr_chemia/R/R_wtorek/Zadania zaliczenie/herb_pomeranian_set.csv", sep = ";", header = TRUE)
herb_pomeranian <- herb_pomeranian[complete.cases(herb_pomeranian[, c("Latitude", "Longitude")]), ]
herb_pomeranian <- herb_pomeranian[, colSums(is.na(herb_pomeranian)) == 0]
# Subset data for 'Lecanora' in herb_pomeranian
lecanora_pomeranian <- herb_pomeranian %>%
  filter(grepl("Lecanora", Scientific.name))
# Convert Scientific.name to factor
lecanora_pomeranian$Scientific.name <- factor(lecanora_pomeranian$Scientific.name)
# Change longitude to numeric
lecanora_pomeranian$Longitude <- as.numeric(lecanora_pomeranian$Longitude)
# Plot
ggplot(lecanora_pomeranian, aes(x = Scientific.name, y = Longitude)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = NULL, y = "Longitude", title = "Longitude of Lecanora Species") +
  theme(axis.title.x = element_blank())
#2.8
# Create mean_long data frame
mean_long <- herb_pomeranian %>%
  group_by(Order) %>%
  summarize(mean_Longitude = mean(Longitude, na.rm = TRUE))
# Rename Orders with min and max mean_Longitude
mean_long <- mean_long %>%
  mutate(Order = case_when(
    mean_Longitude == min(mean_Longitude) ~ paste0(Order, "_min"),
    mean_Longitude == max(mean_Longitude) ~ paste0(Order, "_max"),
    TRUE ~ Order
  ))
# Visualize mean_long with point plot
ggplot(mean_long, aes(x = Order, y = mean_Longitude)) +
  geom_point(shape = 0, size = 3) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Order", y = "Mean Longitude") +
  ylim(min(mean_long$mean_Longitude), max(mean_long$mean_Longitude))
# Calculate overall mean Longitude
overall_mean_Longitude <- mean(herb_pomeranian$Longitude, na.rm = TRUE)
cat("The mean Longitude of all analyzed Order types is:", overall_mean_Longitude)
ggsave("mean_Longitude_graph.jpg", width = 10, height = 6)

#2.10
# Top 3 species in Pomeranian area
top_species <- herb_pomeranian %>%
  count(Scientific.name, sort = TRUE) %>%
  head(3)
print(top_species)
# Conclude the Lecanora plot
min_longitude <- min(herb_pomeranian$Longitude)
print(paste("Minimum longitude in Pomeranian area:", min_longitude))
max_longitude <- max(herb_pomeranian$Longitude)
print(paste("Maximum longitude in Pomeranian area:", max_longitude))
lecanora_min_longitude <- min(lecanora_pomeranian$Longitude)
print(paste("Minimum longitude for Lecanora species:", lecanora_min_longitude))
lecanora_max_longitude <- max(lecanora_pomeranian$Longitude)
print(paste("Maximum longitude for Lecanora species:", lecanora_max_longitude))
lecanora_min_diff <- lecanora_min_longitude - min_longitude
print(paste("Difference between Lecanora minimum longitude and overall minimum longitude:", lecanora_min_diff))
lecanora_max_diff <- lecanora_max_longitude - max_longitude
print(paste("Difference between Lecanora maximum longitude and overall maximum longitude:", lecanora_max_diff))

# Overall mean Longitude
overall_mean_Longitude <- mean(herb_pomeranian$Longitude, na.rm = TRUE)
print(paste("Overall mean Longitude:", overall_mean_Longitude))
min_order_mean <- min(mean_long$mean_Longitude)
print(paste("Minimum mean Longitude among Orders:", min_order_mean))
max_order_mean <- max(mean_long$mean_Longitude)
print(paste("Maximum mean Longitude among Orders:", max_order_mean))
mean_min_diff <- overall_mean_Longitude - min_order_mean
print(paste("Difference between overall mean and minimum Order mean:", mean_min_diff))
mean_max_diff <- overall_mean_Longitude - max_order_mean
print(paste("Difference between overall mean and maximum Order mean:", mean_max_diff))

# Plot Dimensionality (Longitude x Latitude) by Class
ggplot(herb_pomeranian, aes(x = Longitude, y = Latitude, color = Class)) +
  geom_point() +
  theme_classic() +
  labs(x = "Longitude", y = "Latitude", color = "Class") +
  ggtitle("Dimensionality by Class")

ggsave("dimensionality_plot.jpg", width = 10, height = 6)

