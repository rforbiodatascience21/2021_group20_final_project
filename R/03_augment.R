# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
#source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
stjude_clean <- read_tsv(file = "data/02_stjude_clean.tsv.gz")


# Wrangle data ------------------------------------------------------------
stjude_clean <- stjude_clean %>%
  separate(id, "-", into = "id_start")

if (all(stjude_clean %>% pluck("value") == stjude_clean %>% pluck("id_start"))) {
  print("everything ok")
} else {
  print("something wrong")
}

stjude_clean <- stjude_clean %>% 
  select(-id_start)

# Write data --------------------------------------------------------------
write_tsv(x = stjude_clean,
          file = "data/03_stjude_clean_aug.tsv.gz")