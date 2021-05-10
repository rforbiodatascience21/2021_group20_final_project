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

top_40_genes <- st_jude_long_nested %>% 
  pivot_longer(cols = -gene,
               names_to = "leukemia",
               values_to = "ttest") %>% 
  group_by(leukemia) %>% 
  top_n(n = 40, 
        wt = abs(ttest))
  
selected_genes <- top_40_genes %>%
  pull(gene)


st_jude_downsized <- st_jude %>% 
  select(sampleID, leukemia, all_of(selected_genes))


#Top10 genes

top_10_genes <- top_40_genes %>%
  group_by(leukemia) %>% 
  top_n(n = 10, wt = ttest) %>% 
  ungroup() %>%
  arrange(leukemia, desc(ttest)) %>%
  mutate(rank = case_when(ttest > 0 ~ str_c(leukemia, "_top10"),
                          ttest < 0 ~ str_c("neg_corr_", leukemia)),
         rank = fct_inorder(rank))%>%
  select(gene, rank)


gene_names <- top_10_genes %>%
  pull(gene)

st_jude_scaled <- st_jude %>%
  mutate(sampleID = fct_inorder(sampleID),
         leukemia = fct_inorder(leukemia),
         across(c(-sampleID, -leukemia),
                scale))

st_jude_top_10 <- st_jude_scaled %>%
  select(sampleID, leukemia, all_of(gene_names)) %>%
  pivot_longer(cols = c(-sampleID,-leukemia),
               names_to = "gene",
               values_to = "expr_level") %>%
  left_join(top_10_genes,
            by = c("gene"))

# Write data --------------------------------------------------------------

write_tsv(x = st_jude_downsized,
          file = "/03_stjude_downsized.tsv.gz")

write_tsv(x = st_jude_downsized,
          file = "shiny_final/data/03_stjude_downsized.tsv.gz")

write_tsv(x = top_40_genes,
          file = "data/03_top_40_genes.tsv.gz")

write_tsv(x = st_jude_top_10,
          file = "data/03_st_jude_top_10.tsv.gz")
