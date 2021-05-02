# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
# source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
st_jude <- read_tsv(file = "data/03_stjude_clean_aug.tsv.gz")


# Wrangle data ------------------------------------------------------------
st_jude <- st_jude %>%
  filter(leukemia %in% c("BCR", "E2A", "Hyperdip", "MLL", "T", "TEL"))

# Log transforming before t-test
st_jude_log <- st_jude %>%
  mutate_if(is.numeric, log)

st_jude_long_nested <- st_jude_log %>% 
  pivot_longer(cols = c(-sampleID,-leukemia),
               names_to = "gene",
               values_to = "expr_level") %>% 
  group_by(gene) %>% 
  nest() %>% 
  ungroup()

# Idea that does not work for now
st_jude_long_nested <- st_jude_long_nested %>% 
  mutate(ttest = map(data, ~ apply_t_test(.x)))

# Model data
my_data_clean_aug %>% ...


# Visualise data ----------------------------------------------------------
my_data_clean_aug %>% ...


# Write data --------------------------------------------------------------
write_tsv(...)
ggsave(...)