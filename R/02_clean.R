# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
#source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
x_clean <- read_tsv(file = "./data/01_stjude_x.tsv.gz")
genes_clean <- read_tsv(file = "./data/01_stjude_genes.tsv.gz")


# Wrangle data ------------------------------------------------------------
x_clean <- x_clean %>% 
  separate(col = "id", 
           into = c("leukemia", NA, NA, NA), 
           sep = "-") %>%
  mutate(sampleID = row_number()) %>%
  relocate(sampleID)

number_nas <- x_clean %>% 
  summarise(NAs = sum(is.na(.)), .groups = "rowwise") %>% 
  sum()

paste("The clean dataset has", number_nas, "NAs")

gene_pattern <- "\\w{1,2}\\d{5,6}"
genes_clean <- genes_clean %>%
  mutate(Gene_name = str_extract(Descriptions,gene_pattern))


# Write data --------------------------------------------------------------
write_tsv(x = x_clean,
         file = "data/02_stjude_clean.tsv.gz")