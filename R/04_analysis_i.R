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
  filter(value %in% c("BCR", "E2A", "Hyperdip", "MLL", "T", "TEL"))

# Log transforming before t-test
st_jude_log <- st_jude %>%
  mutate_if(is.numeric, log)

st_jude_long_nested <- st_jude_log %>% 
  pivot_longer(cols = -value,
               names_to = "gene",
               values_to = "expr_level") %>% 
  group_by(gene) %>% 
  nest() %>% 
  ungroup()

# Idea that does not work for now
st_jude_long_nested[[2]][[350]] %>%
  group_by(value) %>% 
  group_map(~ t.test(x = .x %>% select(expr_level), 
                     y = st_jude_long_nested[[2]][[350]] %>% 
                       filter(value == "BCR") %>% 
                       select(expr_level)) %>% 
              tidy() %>% 
              select(statistic))

# Model data
my_data_clean_aug %>% ...


# Visualise data ----------------------------------------------------------
my_data_clean_aug %>% ...


# Write data --------------------------------------------------------------
write_tsv(...)
ggsave(...)