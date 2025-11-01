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
 
# Deleting unnecessary empty columns at the far right of "data" and calling this new dataset "data_clean"
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

colnames(data_clean) # Cheking to see newly cleaned column names.

# Pivoting the "data_clean" longer and removing NA. 
data_long <- data_clean %>%
  pivot_longer(
    cols = -c(country, year),          
    names_to = c("disease", "measure"),  
    names_sep = "_",                    
    values_to = "value",
    values_drop_na = TRUE
  )

