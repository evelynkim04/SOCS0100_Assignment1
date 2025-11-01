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

# Importing the vaccine data in .csv format.
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
         "Deaths_tuberculosis_per100,000" = "Estimated.deaths.due.to.tuberculosis.per.100.000.population..excluding.HIV..WHO.2017.",
         "Deaths_tuberculosis" = "Estimated.number.of.deaths.due.to.tuberculosis..excluding.HIV..WHO.2017."
  )
