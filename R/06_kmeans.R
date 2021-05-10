# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("broom")
library("ggplot2")


# Load data ---------------------------------------------------------------
st_jude_top40 <- read_tsv(file = "./data/03_stjude_top40.tsv.gz")
st_jude_all <- read_tsv(file = "./data/02_stjude_clean.tsv.gz")


# Model data --------------------------------------------------------------
set.seed(20)
kclust_top40 <- st_jude_top40 %>%
  select(c(-sampleID, -leukemia)) %>% 
  kmeans(centers = 6)

clusters_top40 <- kclust_top40 %>% 
  augment(st_jude_top40) %>% 
  select(.cluster, leukemia) %>% 
  group_by(.cluster, leukemia) %>% 
  count %>% 
  rename(cluster = .cluster, counts = n)

set.seed(20)
kclust_all <- st_jude_all %>%
  select(c(-sampleID, -leukemia)) %>% 
  kmeans(centers = 6)

clusters_all <- kclust_all %>% 
  augment(st_jude_all) %>% 
  select(.cluster, leukemia) %>% 
  group_by(.cluster, leukemia) %>% 
  count %>% 
  rename(cluster = .cluster, counts = n)


# Plot models -------------------------------------------------------------
kmeans_top40 <- clusters_top40 %>% 
  ggplot(mapping = aes(x = cluster, 
                       y = counts, 
                       fill = leukemia)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(x = "Cluster ID", 
       y = "Number of samples",
       title = "Samples assigned to each cluster (top 40)",
       fill = "Leukemia type")

+

  
  
  
    theme(plot.title = element_text(size = 14,
                                  hjust = 0.5,
                                  vjust = 2),
        legend.text = element_text(size = 7))

kmeans_top40

kmeans_all <- clusters_all %>% 
  ggplot(mapping = aes(x = cluster, 
                       y = counts, 
                       fill = leukemia)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(x = "Cluster ID", 
       y = "Number of samples",
       title = "Samples assigned to each cluster (all)",
       fill = "Leukemia type")
 

# Save plots and tables ---------------------------------------------------
ggsave(plot = kmeans_top40,
       filename = "results/06_kmeans_barplot_top40.png")


,
       width = 18,
       units = "cm")

write_tsv(x = clusters_top40, 
          file = "results/06_kmeans_table_top40.tsv")

ggsave(plot = kmeans_all,
       filename = "results/06_kmeans_barplot_all.png")

write_tsv(x = clusters_all, 
          file = "results/06_kmeans_table_all.tsv")
