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


# Model data --------------------------------------------------------------
pca_fit_top40 <- st_jude_top40 %>% 
  select(where(is.numeric)) %>%
  scale %>% 
  prcomp

pca_plot_top40 <- pca_fit_top40 %>%
  augment(stjude_top40) %>% 
  ggplot(mapping = aes(x = .fittedPC1, 
                       y = .fittedPC2, 
                       color = leukemia, 
                       shape = leukemia)) + 
  geom_point() +
  labs(x = 'PC1',
       y = 'PC2',
       title = 'PCA of top 40 genes of each leukemia') +
  guides(color=guide_legend(title="Leukemia type")) +
  theme_classic()

var_plot_top40 <- pca_fit_top40 %>%
  tidy(matrix = "eigenvalues") %>%
  filter(PC < 11) %>%
  ggplot(mapping = aes(x = PC, y = percent)) +
  geom_col(fill = "aquamarine3", alpha = 0.8) +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = 'Variance explained by the first 10 PC'.
       subtitle = 'Top 40 genes of each leukemia') +
  theme_classic()


pca_fit_top40 <- genes_top40 %>% 
  select(where(is.numeric)) %>%
  scale %>% 
  prcomp

pca_plot_top40 <- pca_fit_top40 %>%
  augment(genes_top40) %>% 
  ggplot(mapping = aes(x = .fittedPC1, 
                       y = .fittedPC2, 
                       color = leukemia, 
                       shape = leukemia)) + 
  geom_point() +
  labs(x = 'PC1',
       y = 'PC2',
       title = 'PCA of top 40 genes of each leukemia') +
  theme_classic()

variance_top40 <- pca_fit_top40 %>%
  tidy(matrix = "eigenvalues") %>%
  filter(PC < 11) %>%
  ggplot(mapping = aes(x = PC, y = percent)) +
  geom_col(fill = "aquamarine3", alpha = 0.8) +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = 'Variance explained by the first 10 PC (Top 40 genes)') +
  theme_classic()


# Save plots --------------------------------------------------------------

ggsave(plot = top40_projection,
       filename = "results/07_top40_projection.png")

ggsave(plot = top40_variance,
       filename = "results/07_top40_variance.png")

ggsave(plot = all_projection,
       filename = "results/07_all_projection.png")

ggsave(plot = all_variance,
       filename = "results/07_all_variance.png")