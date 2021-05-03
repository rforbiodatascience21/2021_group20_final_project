# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
st_jude <- read_tsv(file = "data/03_stjude_clean_aug.tsv.gz")


# Wrangle data ------------------------------------------------------------
cancer_types <- c("BCR", "E2A", "Hyperdip", "MLL", "T", "TEL")

st_jude <- st_jude %>%
  filter(leukemia %in% cancer_types)

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

st_jude_long_nested <- st_jude_long_nested %>% 
  mutate(ttest = map(data, ~ apply_t_test(.x))) %>% 
  unnest_wider(ttest) %>% 
  select(-data)

# Not working
# map(c(BCR, E2A, Hyperdip, MLL, T, TEL),
#     ~ top_n(st_jude_long_nested, n = 40, wt = .x) %>% 
#       pull(genes))
    
# Model data
my_data_clean_aug %>% ...


# Visualise data ----------------------------------------------------------
my_data_clean_aug %>% ...


# Write data --------------------------------------------------------------
write_tsv(...)
ggsave(...)