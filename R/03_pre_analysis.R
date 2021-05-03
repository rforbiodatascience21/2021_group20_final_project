# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
st_jude <- read_tsv(file = "data/02_x_clean.tsv.gz")


# Wrangle data ------------------------------------------------------------
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

selected_genes <- st_jude_long_nested %>% 
  pivot_longer(cols = -gene,
               names_to = "leukemia",
               values_to = "ttest") %>% 
  group_by(leukemia) %>% 
  top_n(n = 40, wt = abs(ttest)) %>%
  pull(gene)


st_jude_downsized <- st_jude %>% 
  select(sampleID, leukemia, all_of(st_jude_long_nested2))

# Write data --------------------------------------------------------------
write_tsv(x = st_jude_downsized,
          file = "data/03_x_downsized.tsv.gz")