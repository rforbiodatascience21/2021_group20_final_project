# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("readxl")


# Define functions --------------------------------------------------------
# source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
x <- read_tsv(file = "_raw/raw_stjude_x.tsv.gz")
xrows <- read_tsv(file = "_raw/raw_stjude_xrows.tsv.gz")

genes <- read_excel(path = "_raw/raw_bcr.xls")

# Wrangle data ------------------------------------------------------------
x <- t(x) %>% 
  as_tibble(rownames = NA)
colnames(x) = t(xrows)

x <- x %>% 
  mutate(id = rownames(x)) %>% 
  relocate(id) 

genes <- as_tibble(genes) %>%
  select(matches("Probe|Description")) %>%
  filter(!(is.na(`Probe set`) & is.na(Descriptions)))





# Write data --------------------------------------------------------------
write_tsv(x = x, file = "data/01_stjude_x.tsv.gz")
write_tsv(x = genes, file = "data/01_stjude_genes.tsv.gz")
