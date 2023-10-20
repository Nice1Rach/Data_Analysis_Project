# Project Title: Global Salary Data Analysis By Rachel Heke
# Description: This script analyzes a dataset containing 
# salary data from various countries and continents 
# and determines statistical patterns across them.

# Load required libraries
library(tidyverse)
library(plotly)
library(lubridate)
library(scales)
library(RColorBrewer)
library(stats)

# Read in salary data from CSV
salary_data <- read.csv("C:/Users/Rachel Heke/Desktop/salary_data.csv")

# Data Exploration and Preprocessing
# Display the first and last few rows of the data
head(salary_data)
tail(salary_data)
summary(salary_data)

# Modify the 'continent_name' column to consolidate some categories
salary_data <- salary_data %>%
  mutate(continent_name = case_when(
    continent_name %in% c("Central America", "Northern America") ~ "North America",
    TRUE ~ continent_name
  ))

# Filter out data for the Caribbean
salary_data <- salary_data %>%
  filter(continent_name != "Caribbean")

# Check for missing values in salary columns
sum(is.na(salary_data$average_salary))
sum(is.na(salary_data$lowest_salary))

# Display the first and last few rows of the modified data
head(salary_data)
tail(salary_data)

# Display the structure of the 'salary_data' dataframe 
str(salary_data)

# Display column names of the 'data' dataframe
colnames(salary_data)

# Remove duplicate rows
duplicated_rows <- duplicated(salary_data)
salary_data_no_duplicates <- salary_data[!duplicated_rows, ]
sapply(salary_data, function(col) sum(is.na(col)))

# Group data by 'country_name' and compute salary statistics
salary_stats <- data %>%
  group_by(country_name) %>%
  summarise(
    Mean_Salary = mean(median_salary, na.rm = TRUE),      # Calculate the mean of 'median_salary'
    Median_Salary = median(median_salary, na.rm = TRUE),  # Calculate the median of 'median_salary'
    Max_Salary = max(median_salary, na.rm = TRUE),        # Find the maximum value of 'median_salary'
    Min_Salary = min(median_salary, na.rm = TRUE)         # Find the minimum value of 'median_salary'
  ) %>%
  arrange(desc(Mean_Salary))   # Arrange the summarized data in descending order of Mean_Salary

# Print the summarized salary statistics
print(salary_stats)

# Filter the data to only include rows where 
# 'average_salary' is greater than or equal to 1
filtered_data <- subset(data, average_salary >= 1)

# Start of Analysis
# -------------------------------------------
# Question 1: What is the distribution of 
# median, average, lowest, and highest 
# salaries across different countries and 
# continents?
# -------------------------------------------

# Visualize 'average_salary' distribution
ggplot(salary_data, aes(x=average_salary)) + geom_histogram()

# Get summary statistics for individual salary columns
summary(salary_data$average_salary)
summary(salary_data$median_salary)
summary(salary_data$lowest_salary)
summary(salary_data$highest_salary)

# Boxplot: Salary distribution by country
plot_ly(data = salary_data, x = ~country_name, y = ~average_salary, type = 'box', 
        color = ~country_name, colors = color_mapping)

# Create a boxplot showing the distribution of 
# average salaries across continents, using custom colors
ggplot(salary_data, aes(x = continent_name, y = average_salary, fill = continent_name)) +
  geom_boxplot() +
  scale_fill_manual(values = color_palette) +
  labs(title = "Distribution of Average Salaries across Continents")

# Bar Chart for Median Salaries across Countries:
plot_ly(data = salary_data, x = ~country_name, y = ~median_salary, type = 'bar') %>%
  layout(title = "Median Salaries across Countries")

# Bar Chart for Median Salaries across Continents:
plot_ly(data = salary_data, x = ~continent_name, y = ~median_salary, type = 'bar') %>%
  layout(title = "Median Salaries across Continents")

# Bar charts: Lowest and highest salaries by country
plot_ly(data = salary_data, x = ~country_name, y = ~lowest_salary, type = 'bar')
plot_ly(data = salary_data, x = ~country_name, y = ~highest_salary, type = 'bar')

# Bar Chart for Highest Salaries across Countries:
plot_ly(data = salary_data, x = ~country_name, y = ~highest_salary, type = 'bar', name = 'Highest Salary') %>%
  layout(title = "Highest Salaries across Countries")

# Grouped bar chart: All salary metrics by country
salary_data_long <- tidyr::gather(salary_data, "Salary_Type", "Amount", 
                                 c(median_salary, average_salary, lowest_salary, highest_salary))
plot_ly(data = salary_data_long, x = ~country_name, y = ~Amount, color = ~Salary_Type, type = 'bar', barmode = 'group')

# Correlation matrix for salary types
cor_matrix <- cor(salary_data[, c("median_salary", "average_salary", "lowest_salary", "highest_salary")])
print(cor_matrix)

# Comparative bar chart: All salary metrics by country
ggplot(salary_data_long, aes(x = country_name, y = Amount, fill = Salary_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# One-way ANOVA for salaries by continent and country
anova_result_continent <- aov(average_salary ~ continent_name, data = salary_data)
anova_result_country <- aov(average_salary ~ country_name, data = salary_data)
summary(anova_result_continent)
summary(anova_result_country)

# Post-hoc tests for continents and countries
tukey_continent <- TukeyHSD(anova_result_continent)
tukey_country <- TukeyHSD(anova_result_country)
print(tukey_continent)
print(tukey_country)

# Conclusion:
# - Salaries show distinct patterns across continents with 
# statistically significant differences.
# - High correlation observed between median, average, 
# lowest, and highest salaries.
# - Issues noted in country-level post-hoc analysis, 
# particularly involving Mauritania.
# - Mauritania's data didn't have missing values, but 
# post-hoc results were inconclusive.
# - Further in-depth analysis is recommended for a 
# comprehensive understanding.

# -------------------------------------------
# Question 2: How do salary statistics vary 
# from one country to another? Which 
# countries have the highest and lowest 
# salary levels?
# -------------------------------------------

# Start of Analysis

# Identify countries with the highest median salaries
highest_salary_country <- salary_data %>% 
  arrange(desc(median_salary)) %>%
  head(10)

# Identify countries with the lowest median salaries
lowest_salary_country <- salary_data %>% 
  arrange(median_salary) %>%
  head(10)

# Visualize median salaries for all countries 
# in descending order
sorted_data <- salary_data %>% 
  arrange(desc(median_salary))

plot_ly(data = sorted_data, x = ~country_name, y = ~median_salary, type = "bar", 
        name = "Median Salary") %>%
  layout(title = "Median Salary by Country")

# Boxplot visualization of median salaries by continent
plot_ly(data = salary_data, x = ~continent_name, y = ~median_salary, type = "box", 
        name = "Median Salary by Continent", boxpoints = "all", jitter = 0.3, pointpos = -1.8) %>%
  layout(title = "Median Salary Distribution by Continent")

# Calculate wage disparity for each country and visualize it
salary_data <- salary_data %>% 
  mutate(wage_disparity = highest_salary - lowest_salary)

disparity_sorted <- salary_data %>% 
  arrange(desc(wage_disparity))

plot_ly(data = disparity_sorted, x = ~country_name, y = ~wage_disparity, type = "bar", 
        name = "Wage Disparity") %>%
  layout(title = "Wage Disparity by Country")

# Identify countries with highest and lowest average salaries
top_countries <- salary_data %>% 
  arrange(desc(average_salary)) %>% 
  head(10)

bottom_countries <- salary_data %>% 
  arrange(average_salary) %>% 
  head(10)

# Side-by-side bar plots showing median salaries 
# by country within each continent
continent_summary <- salary_data %>%
  group_by(continent_name, country_name) %>%
  summarize(median_salary = median(median_salary, na.rm = TRUE))

plot_ly(data = continent_summary, x = ~country_name, y = ~median_salary, color = ~continent_name, 
        type = "bar", split = ~continent_name) %>%
  layout(title = "Median Salary by Country within Each Continent", 
         barmode = "group",
         yaxis = list(title = "Median Salary"),
         xaxis = list(title = "Country"))

# Boxplot visualization for median salaries within 
# each continent (repeated from #4 but with added color distinction)
plot_ly(data = salary_data, x = ~continent_name, y = ~median_salary, color = ~continent_name, 
        type = "box", boxpoints = "all", jitter = 0.3, pointpos = -1.8) %>%
  layout(title = "Median Salary Distribution by Continent")

# Identify countries leading and lagging based on 
# average salary within each continent
top_average_countries <- salary_data %>%
  group_by(continent_name) %>%
  top_n(1, wt = average_salary)

bottom_average_countries <- salary_data %>%
  group_by(continent_name) %>%
  top_n(-1, wt = average_salary)

print(top_average_countries)
print(bottom_average_countries)

# Conclusion:
# Salary statistics vary significantly across countries. Some 
# countries consistently rank at the top in terms of median 
# and average salaries, showcasing strong economic 
# performance and high living standards. On the other hand, 
# countries that consistently rank at the bottom 
# might face economic challenges

# -------------------------------------------
# Question 3: Is there a significant income 
# inequality within individual countries, and
# if so, what factors might contribute to it?
# -------------------------------------------

# Start of Analysis

# Identify countries with the highest median salaries
top_median_salary_countries <- salary_data %>% 
  arrange(desc(median_salary)) %>%
  head(10)

# Identify countries with the lowest median salaries
bottom_median_salary_countries <- salary_data %>% 
  arrange(median_salary) %>%
  head(10)

# Visualize median salaries for all countries 
# in descending order
sorted_data <- salary_data %>% 
  arrange(desc(median_salary))

plot_ly(data = sorted_data, x = ~country_name, y = ~median_salary, type = "bar", 
        name = "Median Salary") %>%
  layout(title = "Median Salary by Country")

# Boxplot visualization for median salaries within 
# each continent 
plot_ly(data = salary_data, x = ~continent_name, y = ~median_salary, color = ~continent_name, 
        type = "box", boxpoints = "all", jitter = 0.3, pointpos = -1.8) %>%
  layout(title = "Median Salary Distribution by Continent")

# Calculate wage disparity for each country and visualize it
salary_data <- salary_data %>% 
  mutate(wage_disparity = highest_salary - lowest_salary)

disparity_sorted <- salary_data %>% 
  arrange(desc(wage_disparity))

plot_ly(data = disparity_sorted, x = ~country_name, y = ~wage_disparity, type = "bar", 
        name = "Wage Disparity") %>%
  layout(title = "Wage Disparity by Country")

# Identify countries with highest and lowest average salaries
top_countries <- salary_data %>% 
  arrange(desc(average_salary)) %>% 
  head(10)

bottom_countries <- salary_data %>% 
  arrange(average_salary) %>% 
  head(10)

# Side-by-side bar plots showing median salaries by 
# country within each continent
continent_summary <- salary_data %>%
  group_by(continent_name, country_name) %>%
  summarize(median_salary = median(median_salary, na.rm = TRUE))

plot_ly(data = continent_summary, x = ~country_name, y = ~median_salary, color = ~continent_name, 
        type = "bar", split = ~continent_name) %>%
  layout(title = "Median Salary by Country within Each Continent", 
         barmode = "group",
         yaxis = list(title = "Median Salary"),
         xaxis = list(title = "Country"))

# Display data frames for leading and lagging countries 
# (assuming these data frames have been previously defined)
print(leading_countries)
print(lagging_countries)

# Identify countries leading and lagging based on 
# average salary within each continent
top_average_countries <- salary_data %>%
  group_by(continent_name) %>%
  top_n(1, wt = average_salary)

bottom_average_countries <- salary_data %>%
  group_by(continent_name) %>%
  top_n(-1, wt = average_salary)

# Number of countries you want to consider as "leading" and "lagging"
n <- 10

# Leading countries based on average salary
leading_countries <- salary_data %>%
  arrange(desc(average_salary)) %>%
  head(n)

# Lagging countries based on average salary
lagging_countries <- salary_data %>%
  arrange(average_salary) %>%
  head(n)

# Display the data frames
print(leading_countries)
print(lagging_countries)

print(top_average_countries)
print(bottom_average_countries)

# Conclusion:
# Our analysis explored salary variations across countries. 
# Countries with the highest median salaries typically 
# have robust economies and high living standards, while 
# those with the lowest may face economic challenges. 
# Wage disparity metrics further highlighted potential 
# income inequalities within nations. Overall, understanding
# the economic and sociopolitical context is crucial when 
#interpreting these salary differences across countries.

# -------------------------------------------
# Question 4: Can you compare salary 
# statistics between specific pairs of 
# countries or regions, even if you don't 
# have a full regional breakdown?
# -------------------------------------------

# Start of Analysis

# Data Preparation:
# Select relevant countries for analysis
chosen_countries <- c("United States", "Switzerland", "Norway", "Denmark", "Luxembourg", "Singapore", "Australia", "Canada")
subset_data <- salary_data[salary_data$country_name %in% chosen_countries, ]

# Descriptive Statistics:
# Calculate summary statistics for each of the selected countries
summary_stats <- subset_data %>%
  group_by(country_name) %>%
  summarise(
    Median = median(median_salary),          # Calculate median salary
    Mean = mean(average_salary),             # Calculate mean salary
    Lowest = min(lowest_salary),             # Identify the minimum salary
    Highest = max(highest_salary)            # Identify the maximum salary
  )
print(summary_stats)

# Visual Analysis:
# Generate bar plot to compare median salaries of chosen countries
plot_ly(data = subset_data, x = ~country_name, y = ~median_salary, type = 'bar', color = ~country_name) %>%
  layout(title = "Comparison of Median Salaries")

# Extract specific metrics for USA and Switzerland
usa_median <- usa_data$median_salary
switzerland_median <- switzerland_data$median_salary

usa_average <- usa_data$average_salary
switzerland_average <- switzerland_data$average_salary

usa_lowest <- usa_data$lowest_salary
switzerland_lowest <- switzerland_data$lowest_salary

usa_highest <- usa_data$highest_salary
switzerland_highest <- switzerland_data$highest_salary

# Create visual comparison of salary metrics 
# between USA and Switzerland
# Compare median salaries
plot_ly(data = salary_data %>% filter(country_name %in% c("United States", "Switzerland")),
        x = ~country_name, y = ~median_salary, type = 'bar', color = ~country_name) %>%
  layout(title = "Comparison of Median Salaries between United States and Switzerland")

# Compare average salaries
plot_ly(data = salary_data %>% filter(country_name %in% c("United States", "Switzerland")),
        x = ~country_name, y = ~average_salary, type = 'bar', color = ~country_name, colors = c("United States" = "blue", "Switzerland" = "red")) %>%
  layout(title = "Comparison of Average Salaries between United States and Switzerland")

# Compare lowest salaries
plot_ly(data = salary_data %>% filter(country_name %in% c("United States", "Switzerland")),
        x = ~country_name, y = ~lowest_salary, type = 'bar', color = ~country_name, colors = c("United States" = "green", "Switzerland" = "purple")) %>%
  layout(title = "Comparison of Lowest Salaries between United States and Switzerland")

# Compare highest salaries
plot_ly(data = salary_data %>% filter(country_name %in% c("United States", "Switzerland")),
        x = ~country_name, y = ~highest_salary, type = 'bar', color = ~country_name, colors = c("United States" = "yellow", "Switzerland" = "cyan")) %>%
  layout(title = "Comparison of Highest Salaries between United States and Switzerland")

# Calculate the mean (average) salary for USA and Switzerland
# Using the `average_salary` column and filtering by `country_name`
# Mean Salary for the United States
mean_usa <- mean(salary_data$average_salary[salary_data$country_name == "United States"])
# Mean Salary for Switzerland
mean_switzerland <- mean(salary_data$average_salary[salary_data$country_name == "Switzerland"])

# Calculate the median salary for USA and Switzerland
# Using the `median_salary` column and filtering by `country_name`
# Median Salary for the United States
median_usa <- median(salary_data$median_salary[salary_data$country_name == "United States"])
# Median Salary for Switzerland
median_switzerland <- median(salary_data$median_salary[salary_data$country_name == "Switzerland"])

# Calculate the range (difference between highest and lowest values) for USA and Switzerland
# Using the `highest_salary` and `lowest_salary` columns and filtering by `country_name`
# Range of Salary for the United States
range_usa <- max(salary_data$highest_salary[salary_data$country_name == "United States"]) - min(salary_data$lowest_salary[salary_data$country_name == "United States"])
# Range of Salary for Switzerland
range_switzerland <- max(salary_data$highest_salary[salary_data$country_name == "Switzerland"]) - min(salary_data$lowest_salary[salary_data$country_name == "Switzerland"])

# Calculate the difference and ratio between the mean 
# and median salaries of USA and Switzerland
# This helps in comparing the salaries of both countries
# Difference and Ratio for Mean Salaries
difference_mean <- mean_switzerland - mean_usa
ratio_mean <- mean_switzerland / mean_usa
# Difference and Ratio for Median Salaries
difference_median <- median_switzerland - median_usa
ratio_median <- median_switzerland / median_usa

# Conclusion:
# My analysis revealed that countries with top median 
# salaries often possess strong economies and elevated 
# living standards. In contrast, nations with lower 
# median salaries might grapple with economic hurdles. 
# Wage disparity metrics shed light on potential income 
# inequalities internally. Hence, a deeper grasp of each 
# country's economic and sociopolitical backdrop is 
# essential to fully comprehend these international salary contrasts.

