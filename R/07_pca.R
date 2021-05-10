# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("broom")
library("ggplot2")
library("scales")


# Load data ---------------------------------------------------------------
st_jude_top40 <- read_tsv(file = "./data/03_stjude_top40.tsv.gz")
st_jude_all <- read_tsv(file = "./data/02_stjude_clean.tsv.gz")


# Model and plot data -----------------------------------------------------
# Top 40 genes
pca_fit_top40 <- st_jude_top40 %>% 
  select(where(is.numeric)) %>%
  scale %>% 
  prcomp

pca_plot_top40 <- pca_fit_top40 %>%
  augment(st_jude_top40) %>% 
  ggplot(mapping = aes(x = .fittedPC1, 
                       y = .fittedPC2, 
                       color = leukemia, 
                       shape = leukemia)) + 
  geom_point() +
  labs(x = 'PC1',
       y = 'PC2',
       title = 'PCA of top 40 genes',
       color = 'Leukemia type',
       shape = 'Leukemia type') +
  theme_classic() +
  theme(plot.title = element_text(size = 14,
                                  hjust = 0.5))

var_plot_top40 <- pca_fit_top40 %>%
  tidy(matrix = "eigenvalues") %>%
  filter(PC < 11) %>%
  ggplot(mapping = aes(x = PC, y = percent)) +
  geom_col(fill = "aquamarine3", alpha = 0.8) +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(labels = percent_format(accuracy = 1L)) +
  labs(title = 'Variance explained by the first 10 PC (top 40 genes)',
       y = 'Variance explained') +
  theme_classic() +
  theme(plot.title = element_text(size = 14,
                                  hjust = 0.5))

# All genes
pca_fit_all <- st_jude_all %>% 
  select(where(is.numeric)) %>%
  scale %>% 
  prcomp

pca_plot_all <- pca_fit_all %>%
  augment(st_jude_all) %>% 
  ggplot(mapping = aes(x = .fittedPC1, 
                       y = .fittedPC2, 
                       color = leukemia, 
                       shape = leukemia)) + 
  geom_point() +
  labs(x = 'PC1',
       y = 'PC2',
       title = 'PCA of all genes',
       color = 'Leukemia type',
       shape = 'Leukemia type') +
  theme_classic() +
  theme(plot.title = element_text(size = 14,
                                  hjust = 0.5))


var_plot_all <- pca_fit_all %>%
  tidy(matrix = "eigenvalues") %>%
  filter(PC < 11) %>%
  ggplot(mapping = aes(x = PC, y = percent)) +
  geom_col(fill = "aquamarine3", alpha = 0.8) +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(labels = percent_format(accuracy = 1L)) +
  labs(title = 'Variance explained by the first 10 PC (all genes)',
       y = 'Variance explained') +
  theme_classic() +
  theme(plot.title = element_text(size = 14,
                                  hjust = 0.5))



# Save plots --------------------------------------------------------------
ggsave(plot = pca_plot_top40,
       filename = "results/07_pca_top40.png")

ggsave(plot = var_plot_top40,
       filename = "results/07_pca_var_top40.png")

ggsave(plot = pca_plot_all,
       filename = "results/07_pca_all.png")

ggsave(plot = var_plot_all,
       filename = "results/07_pca_var_all.png")
