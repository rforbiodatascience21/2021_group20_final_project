# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("broom")
library("ggplot2")


# Load data ---------------------------------------------------------------
top_40_genes <- read_tsv(file = "./data/03_stjude_downsized.tsv.gz")
all_genes <- read_tsv(file = "./data/02_stjude_clean.tsv.gz")


# Model data --------------------------------------------------------------
set.seed(20)
kclust_top40 <- top_40_genes %>%
  as_tibble() %>% 
  select(c(-sampleID, -leukemia)) %>% 
  kmeans(centers = 6)

clusters_top40 <- kclust_top40 %>% 
  augment(top_40_genes) %>% 
  select(.cluster, leukemia) %>% 
  group_by(.cluster, leukemia) %>% 
  count()

set.seed(20)
kclust_all <- all_genes %>%
  as_tibble() %>% 
  select(c(-sampleID, -leukemia)) %>% 
  kmeans(centers = 6)

clusters_all <- kclust_all %>% 
  augment(top_40_genes) %>% 
  select(.cluster, leukemia) %>% 
  group_by(.cluster, leukemia) %>% 
  count()


# Plot models -------------------------------------------------------------
clusters_top40_plot <- clusters_top40 %>% 
  ggplot(aes(.cluster, n, fill = leukemia)) +
  geom_bar(stat = "identity") +
  labs(x = 'cluster', y = 'count')

clusters_all_plot <- clusters_all %>% 
  ggplot(aes(.cluster, n, fill = leukemia)) +
  geom_bar(stat = "identity") +
  labs(x = 'cluster', y = 'count')
 

# Save plots --------------------------------------------------------------
ggsave(plot = clusters_top40_plot,
       filename = "results/06_top40_clusters.png")

ggsave(plot = clusters_all_plot,
       filename = "results/06_all_clusters.png")
