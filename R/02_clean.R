# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
x_clean <- read_tsv(file = "./data/01_stjude_x.tsv.gz")
genes_clean <- read_tsv(file = "./data/01_stjude_genes.tsv.gz")


# Wrangle data ------------------------------------------------------------

# Clean gene names
gene_pattern <- "\\w{1,2}\\d{5,6}"

genes_clean <- genes_clean %>% 
  rename("Probe_set" = `Probe set`) %>%
  mutate(Gene_name = str_extract(Descriptions, gene_pattern),
         Gene_name = case_when(
           !is.na(Gene_name) ~ Gene_name,
           is.na(Gene_name) & !is.na(Descriptions) ~ Descriptions,
           is.na(Gene_name) & is.na(Descriptions) ~ str_c("Probe_", Probe_set))) %>%
  group_by(Gene_name) %>%
  mutate(count = n(),
        count = case_when(
           count != 1 ~ row_number()),
        Gene_name = case_when(
           is.na(count) ~ Gene_name,
           !is.na(count) ~ str_c(Gene_name, "_", count, sep = ""))) %>%
  ungroup() %>%
  select(-count, -Descriptions) %>%
  pivot_wider(names_from = Gene_name, 
              values_from = Probe_set) %>%
  as_tibble(.name_repair = "universal") %>%
  pivot_longer(cols = everything(),
              names_to = "Gene_name",
              values_to = "Probe_set") %>%
  unnest(cols = Probe_set)


# Clean x
x_clean <- x_clean %>% 
  separate(col = "id", 
           into = c("leukemia"), 
           sep = "-") %>%
  mutate(sampleID = str_c("s_", row_number())) %>%
  relocate(sampleID) %>%
  filter(leukemia %in% c("BCR", "E2A", "Hyperdip", "MLL", "T", "TEL"))
  
x_clean <- x_clean %>%
  select(sort(current_vars())) %>% 
  rename_with(~ genes_clean %>% 
                arrange(Probe_set) %>% 
                pull(Gene_name),
              .cols = c(-sampleID, -leukemia)) %>% 
  relocate(c(sampleID, leukemia))

number_nas <- x_clean %>% 
  summarise(NAs = sum(is.na(.)), .groups = "rowwise") %>% 
  sum()

print(str_c("The clean dataset has", number_nas, "NAs", sep = " "))


# Write data --------------------------------------------------------------
write_tsv(x = x_clean,
          file = "data/02_stjude_clean.tsv.gz")