# Part I-B
# Removing everything in my environment and setting up working directory.
setwd("~/Downloads")
rm(list = ls())

# Downloading required packages.
if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(
  tidyverse, 
  kableExtra,
  flextable, 
  skimr) 

# Importing the vaccine data in .csv format and calling it "data". 
data <- read.csv("vaccine_2017.csv", header = TRUE)

# Renaming columns
library(dplyr)

data <- data %>%
  rename("BCG_immunity" = "BCG.immunization.coverage.among.1.year.olds..WHO.2017.",
         "HepB3_immunity" = "Hepatitis.B..HepB3..immunization.coverage.among.1.year.olds..WHO.2017.",
         "DTP3_immunity" = "DTP3.immunization.coverage.among.1.year.olds..WHO.2017.",
         "Polio_immunity" = "Polio..Pol3..immunization.coverage.among.1.year.olds..WHO.2017.",
         "Measles_immunity" = "Measles..MCV..immunization.coverage.among.1.year.olds..WHO.2017.",
         "Tetanus_cases" = "Number.of.confirmed.tetanus.cases..WHO.2017.",
         "Polio_cases" = "Number.confirmed.polio.cases..WHO.2017.",
         "Pertussis_cases" = "Number.of.confirmed.pertussis.cases..WHO.2017.",
         "Measles_cases" = "Number.of.confirmed.measles.cases..WHO.2017.",
         "Diphtheria_cases" = "Number.of.confirmed.diphtheria.cases..WHO.2017.",
         "Deaths_tuberculosisper100,000" = "Estimated.deaths.due.to.tuberculosis.per.100.000.population..excluding.HIV..WHO.2017.",
         "Deaths_tuberculosis" = "Estimated.number.of.deaths.due.to.tuberculosis..excluding.HIV..WHO.2017.",
         "Country" = "Entity"
  )
 
# Selecting variables - Deleting unnecessary empty columns at the far right of "data" and calling this new dataset "data_clean"
cols_to_remove <- c("X", "X.1", "X.2", "X.3", "X.4", "X.5")
data_clean <- data %>% 
  select(-all_of(cols_to_remove))

# Tidying column names
library(dplyr)
library(stringr)
library(tidyr)

colnames(data_clean) <- colnames(data_clean) %>%
  str_squish() %>%  # Using str_squish() to make sure that there are no unnecessary spaces.
  str_to_lower() # Using str_to_lower() to make all characters lowercase and thus make my text consistent. 

colnames(data_clean) # Checking to see newly cleaned column names.

# Reshaping data frame: Pivoting the "data_clean" longer, creating new variables ("disease" and "measure"), and removing NA. 
data_long <- data_clean %>%
  pivot_longer(
    cols = -c(country, year),          
    names_to = c("disease", "measure"),  
    names_sep = "_",          # Separates disease from the measure (immunisation coverage or number of cases)         
    values_to = "value",
    values_drop_na = TRUE
  )

# Part II-A
# Visualising data using ggplot.
install.packages("tidyverse")

# Creating a new table with the average value 
library(dplyr)

averages <- diff_measure_overtime %>%
  group_by(country, measure) %>%      # group by country AND variable type
  summarise(mean_value = mean(value, na.rm = TRUE)) %>% 
  ungroup()

# Grouping countries by continent
library(dplyr)
library(countrycode)

continent <- averages %>%
  filter(!country %in% c("World", "Africa", "Americas", "Eastern Mediterranean", 
                         "Europe", "Micronesia (country)", "South-East Asia", "Western Pacific")) %>%
  mutate(continent = countrycode(country, "country.name", "continent"))

continent <- averages %>%
  filter(!country %in% c("World", "Africa", "Americas", "Eastern Mediterranean", 
                         "Europe", "Micronesia (country)", "South-East Asia", "Western Pacific")) %>%
  mutate(continent = countrycode(country, "country.name", "continent",
                                 custom_match = c("Timor" = "Asia")))

continent_means <- continent %>%
  group_by(continent, measure) %>%
  summarise(mean_value = mean(mean_value, na.rm = TRUE))

# Creating heatmap
ggplot(continent_means, aes(x = continent, 
                            y = measure, 
                            fill = mean_value)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "plasma") +
  theme_minimal() +
  labs(title = "Heatmap of Average Measure Values by Continent", x = "Continent", y = "Measure", fill = "Mean Value")

# Plotting a bar chart
ggplot(continent_means, aes(x = continent, 
                            y = mean_value, 
                            fill = measure)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(title = "Average Values by Continent", x = "Continent", y = "Mean Value")

# Applying a logscale because immunity and tuberculosisper100,000 aren't visible (values are too small)
ggplot(continent_means, aes(x = continent, 
                            y = mean_value, 
                            fill = measure)) +
  geom_col(position = "dodge") +
  scale_y_log10() + 
  labs(
    title = "Average Values of different measures by Continent",
    x = "Continent",
    y = "Measure value (log scale)"
  ) +
  theme_minimal()

# Creating a dot plot
library(ggplot2)
library(dplyr)

ggplot(continent_means, aes(x = continent, y = mean_value, color = continent)) +
  geom_point(size = 4, alpha = 0.8) +
  scale_y_log10() +   # <-- log scale applied here. Makes either extremely small/large values comparable. 
  facet_wrap(~ measure, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Mean Values Across Continents",
    x = "Continent",
    y = "Mean Value (log scale)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "none"
  )
