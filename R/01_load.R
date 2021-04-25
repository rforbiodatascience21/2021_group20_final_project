# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library(stjudem)


# Define functions --------------------------------------------------------
# source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
data(stjude)
summary(stjude)

# Wrangle data ------------------------------------------------------------
x <- stjude %>% 
  pluck("expr") %>% 
  as_tibble(rownames = NA)
  
y <- stjude %>% 
  pluck("labels") %>% 
  as_tibble()


# Write data --------------------------------------------------------------
write_tsv(x = x, file = "data/01_stjude_x.tsv")
write_tsv(x = y, file = "data/01_stjude_y.tsv")
