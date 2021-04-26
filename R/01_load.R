# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
# source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
x <- read_tsv(file = "_raw/raw_stjude_x.tsv.gz")
xrows <- read_tsv(file = "_raw/raw_stjude_xrows.tsv.gz")
y <- read_tsv(file = "_raw/raw_stjude_y.tsv.gz")

# Wrangle data ------------------------------------------------------------
x <- t(x) %>% 
  as_tibble(rownames = NA)
colnames(x) = t(xrows)
x <- x %>% 
  mutate(id = rownames(x)) %>% 
  relocate(id)
# Write data --------------------------------------------------------------
write_tsv(x = x, file = "data/01_stjude_x.tsv.gz")
write_tsv(x = y, file = "data/01_stjude_y.tsv.gz")
