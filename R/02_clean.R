# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
#source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
x_clean <- read_tsv(file = "./data/01_stjude_x.tsv.gz")
y_clean <- read_tsv(file = "./data/01_stjude_y.tsv.gz")

# Wrangle data ------------------------------------------------------------
stjude_clean <- as_tibble(cbind(y_clean,x_clean), rownames = NA)


# Write data --------------------------------------------------------------
write_tsv(x = stjude_clean,
         file = "data/02_stjude_clean.tsv.gz")