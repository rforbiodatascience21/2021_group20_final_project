# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
st_jude <- read_tsv(file = "data/02_stjude_clean.tsv.gz")


# Wrangle data ------------------------------------------------------------

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

top_40_genes <- st_jude_long_nested %>% 
  pivot_longer(cols = -gene,
               names_to = "leukemia",
               values_to = "ttest") %>% 
  group_by(leukemia) %>% 
  top_n(n = 40, wt = abs(ttest))
  
selected_genes <- top_40_genes %>%
  pull(gene)


st_jude_downsized <- st_jude %>% 
  select(sampleID, leukemia, all_of(selected_genes))

# Write data --------------------------------------------------------------
write_tsv(x = st_jude_downsized,
          file = "data/03_stjude_downsized.tsv.gz")

write_tsv(x = top_40_genes,
          file = "data/03_top_40_genes.tsv.gz")