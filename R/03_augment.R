# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
#source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
x_clean <- read_tsv(file = "data/02_x_clean.tsv.gz")

genes_clean <- read_tsv(file = "data/02_genes_clean.tsv.gz")




# Wrangle data ------------------------------------------------------------

stjude_aug <- x_clean %>%
  filter(leukemia %in% c("BCR", "E2A", "Hyperdip", "MLL", "T", "TEL")) %>%
  rename(!!! set_names(genes_clean$`Probe set`, genes_clean$Gene_name))


# Write data --------------------------------------------------------------
write_tsv(x = stjude_aug,
          file = "data/03_stjude_clean_aug.tsv.gz")