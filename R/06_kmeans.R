# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("broom")
library("ggplot2")
library("cowplot")


# Load data ---------------------------------------------------------------
top_40_genes <- read_tsv(file = "./data/03_stjude_downsized.tsv.gz")
all_genes <- read_tsv(file = "./data/02_stjude_clean.tsv.gz")


kclust_top40 <- top_40_genes %>%
  as_tibble() %>% 
  select(c(-sampleID, -leukemia)) %>% 
  kmeans(centers = 6)

clusters_top40 <- augment(kclust_top40, top_40_genes) %>% 
  select(leukemia, .cluster) %>% 
  arrange(.cluster)

clusters_count_top40 <- clusters_top40 %>%
  group_by(.cluster, leukemia) %>% 
  summarize(n = n()) %>% 
  filter(n > 5)


kclust_all <- all_genes %>%
  as_tibble() %>% 
  select(c(-sampleID, -leukemia)) %>% 
  kmeans(centers = 6)

clusters_all <- augment(kclust_all, all_genes) %>% 
  select(leukemia, .cluster) %>% 
  arrange(.cluster)

clusters_count_all <- clusters_all %>%
  group_by(.cluster, leukemia) %>% 
  summarize(n = n()) %>% 
  filter(n > 5)

