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

averages <- data_long %>%
  group_by(country, measure) %>%      # group by country AND variable type
  summarise(mean_value = mean(value, na.rm = TRUE)) %>% 
  ungroup()

# Grouping countries by continent
library(dplyr)
library(countrycode)

continent <- averages %>%
  filter(!country %in% c("World", "Africa", "Americas", "Eastern Mediterranean", 
                         "Europe", "Micronesia (country)", "South-East Asia", "Western Pacific")) %>%
  mutate(continent = countrycode(country, "country.name", "continent",
custom_match = c("Timor" = "Asia"))) # The countrycode() function could not match "Timor" to "Asia", so I had to manually code this, as a potential risk might have been that the code would return "Timor" as an NA. 

continent <- averages %>%
  filter(!country %in% c("World", "Africa", "Americas", "Eastern Mediterranean", 
                         "Europe", "Micronesia (country)", "South-East Asia", "Western Pacific")) %>%
  mutate(continent = countrycode(country, "country.name", "continent", # The countrycode() function matches the country to the continent. 
                                 custom_match = c("Timor" = "Asia"))) 

continent_means <- continent %>%
  group_by(continent, measure) %>%
  summarise(mean_value = mean(mean_value, na.rm = TRUE))

# Creating heatmap
ggplot(continent_means, aes(x = continent, # The x axis is the different continents
                            y = measure, # The y axis is the mean value of different measures
                            fill = mean_value)) + # The color of the heatmap depends on the mean value in the y axis. 
  geom_tile(color = "white") + # This function allows the separation of each tile in the heatmap by a white border. 
  scale_fill_viridis_c(option = "plasma") + # This function allows a colour gradient to be applied, which changes based on the mean value (y axis value).
  theme_minimal() +
  labs(title = "Heatmap of Average Measure Values by Continent", x = "Continent", y = "Measure", fill = "Mean Value")

# Plotting a bar chart
ggplot(continent_means, aes(x = continent, # The x axis is the different continents
                            y = mean_value, # The y axis is the mean value of different measures. 
                            fill = measure)) + # Each measure will have a separate color. 
  geom_col(position = "dodge") + # This function allows the bars to be stacked side by side, and not one on top of the other. 
  theme_minimal() +
  labs(title = "Average Values of Different Measures by Continent", x = "Continent", y = "Mean Value")

# Applying a logscale because immunity and tuberculosisper100,000 aren't visible (values are too small)
ggplot(continent_means, aes(x = continent, 
                            y = mean_value, 
                            fill = measure)) +
  geom_col(position = "dodge") +
  scale_y_log10() + 
  labs(
    title = "Average Values of Different Measures by Continent",
    x = "Continent",
    y = "Measure value (log scale)"
  ) +
  theme_minimal()

# Creating a dot plot
library(ggplot2)
library(dplyr)

ggplot(continent_means, aes(x = continent, y = mean_value, color = continent)) +
  geom_point(size = 4, alpha = 0.8) + # This line of code allows the dots sizes to be relatively large so that it is easy to see, and also slightly transparent just in case they overlap. 
  scale_y_log10() +   # log scale applied here to the y-axis. Makes either extremely small/large values comparable.
  facet_wrap(~ measure, scales = "free_y") + # This is the function that creates 4 separate graphs for each measure. "scales = free_y" makes each of the 4 graphs to have its own y-axis. 
  theme_minimal() +
  labs(
    title = "Mean Values of Different Measures Across Continents",
    x = "Continent",
    y = "Mean Value (log scale)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "none"
  )

# Automating the dot plot and bar chart. (So that I don't have to manually generate different plots for each 4 measures.)
library(tidyverse)
library(glue)

# Retrieving all the different measures
measure_list <- unique(continent_means$measure)

# Creating a list of bar plots – one for each measure
bar_plots <- map(measure_list, function(m) { # The map() function runs makes sure to run the code for each measure without me having to manually do it one by one. 
  
  ggplot(
    data = filter(continent_means, measure == m),
    aes(x = continent, 
        y = mean_value, 
        fill = continent)
  ) +
    geom_col() +
    scale_y_log10() +
    theme_minimal() +
    labs(
      title = glue("Average value of {m} by continent"),
      x = "Continent",
      y = glue("{m} (log scale)")
    )
})

# Displaying the first plot
bar_plots[[1]]

# Displaying the second plot
bar_plots[[2]]

# Displaying the third plot
bar_plots[[3]]

# Displaying the fourth plot
bar_plots[[4]]

# Critical engagement of copilot : Although it suggested to automate the task and make separate plots for each measure, 
# I feel like when I had manually done it with all measures showing at once was more insightful and comparable. 
