# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("broom")
library("ggplot2")


# Load data ---------------------------------------------------------------

top_40_genes <- read_tsv(file = "./data/03_stjude_downsized.tsv.gz")
all_genes <- read_tsv(file = "./data/02_stjude_clean.tsv.gz")

pca_fit_top_40 <- top_40_genes %>% 
  select(where(is.numeric)) %>%
  prcomp(scale = TRUE)

top40_projection <- pca_fit_top_40 %>%
  augment(top_40_genes) %>% 
  ggplot(aes(.fittedPC1, .fittedPC2, color = leukemia)) + 
  geom_point()

top40_variance <- pca_fit_top_40 %>%
  tidy(matrix = "eigenvalues") %>%
  as_tibble() %>% 
  filter(PC < 11) %>%
  ggplot(aes(PC, percent)) +
  geom_col(fill = "#56B4E9", alpha = 0.8) +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(
    labels = scales::percent_format(),
    expand = expansion(mult = c(0, 0.01))
  ) +
  theme_minimal_hgrid(12)


pca_fit_all <- all_genes %>% 
  select(where(is.numeric)) %>%
  prcomp(scale = TRUE)

all_projection <- pca_fit_all %>%
  augment(all_genes) %>% 
  ggplot(aes(.fittedPC1, .fittedPC2, color = leukemia)) + 
  geom_point()

all_variance <- pca_fit_all %>%
  tidy(matrix = "eigenvalues") %>%
  as_tibble() %>% 
  filter(PC < 11) %>%
  ggplot(aes(PC, percent)) +
  geom_col(fill = "#56B4E9", alpha = 0.8) +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(
    labels = scales::percent_format(),
    expand = expansion(mult = c(0, 0.01))
  ) +
  theme_minimal_hgrid(12)


# Save plots --------------------------------------------------------------

ggsave(plot = top40_projection,
       filename = "results/07_top40_projection.png")

ggsave(plot = top40_variance,
       filename = "results/07_top40_variance.png")

ggsave(plot = all_projection,
       filename = "results/07_all_projection.png")

ggsave(plot = all_variance,
       filename = "results/07_all_variance.png")