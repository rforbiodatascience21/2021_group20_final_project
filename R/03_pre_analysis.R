# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
st_jude <- read_tsv(file = "data/02_stjude_clean.tsv.gz")


# Wrangle data ------------------------------------------------------------

# Top 40 genes

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
  mutate(ttest = map(data, 
                     ~ apply_t_test(.x))) %>% 
  unnest_wider(ttest) %>% 
  select(-data)

top40_genes <- st_jude_long_nested %>% 
  pivot_longer(cols = -gene,
               names_to = "leukemia",
               values_to = "ttest") %>% 
  group_by(leukemia) %>% 
  top_n(n = 40, 
        wt = abs(ttest))
  
selected_genes <- top40_genes %>%
  pull(gene)


st_jude_top40 <- st_jude %>% 
  select(sampleID, leukemia, all_of(selected_genes))


# Top 10 genes

top10_genes <- top40_genes %>%
  group_by(leukemia) %>% 
  top_n(n = 10, wt = ttest) %>% 
  ungroup() %>%
  arrange(leukemia, desc(ttest)) %>%
  mutate(rank = case_when(ttest > 0 ~ str_c(leukemia, "_top10"),
                          ttest < 0 ~ str_c("neg_corr_", leukemia)),
         rank = fct_inorder(rank))%>%
  select(gene, rank)


gene_names <- top10_genes %>%
  pull(gene)

st_jude_scaled <- st_jude %>%
  mutate(sampleID = fct_inorder(sampleID),
         leukemia = fct_inorder(leukemia),
         across(c(-sampleID, -leukemia),
                scale))

st_jude_top10 <- st_jude_scaled %>%
  select(sampleID, leukemia, all_of(gene_names)) %>%
  pivot_longer(cols = c(-sampleID,-leukemia),
               names_to = "gene",
               values_to = "expr_level") %>%
  left_join(top10_genes,
            by = c("gene"))


# Write data --------------------------------------------------------------

write_tsv(x = st_jude_top40,
          file = "data/03_stjude_top40.tsv.gz")

write_tsv(x = st_jude_top40,
          file = "shiny_final/data/03_stjude_top40.tsv.gz")

write_tsv(x = top40_genes,
          file = "data/03_top40_genes.tsv.gz")

write_tsv(x = st_jude_top10,
          file = "data/03_stjude_top10.tsv.gz")
