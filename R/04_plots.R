# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("ggplot2")


# Define functions --------------------------------------------------------
#source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
st_jude_dwsz <- read_tsv(file = "data/03_stjude_downsized.tsv.gz")
st_jude_top_10 <- read_tsv(file = "data/03_st_jude_top_10.tsv.gz")

# Plot trials -------------------------------------------------------------

#Plot1
boxplot_top10 <- st_jude_top_10 %>% 
  #filter(leukemia == 'BCR') %>% 
  ggplot(aes(x = gene, 
             y = expr_level, 
             colour = leukemia)) +
  #geom_point(colour = 'cyan4') +
  geom_point() +
  #scale_y_log10() +
  xlab("Genes") +
  ylab("Expression") +
  ggtitle("Title") +
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1)) +
  theme(plot.title=element_text(hjust=0.5)) 


#Plot2 (trial with 6 plots together)
point_top10_6 <- st_jude_top_10 %>% 
  ggplot(aes(x = fct_reorder2(gene, desc(expr_level), rank), 
             y = expr_level, 
             colour = ifelse(expr_level < 1,'red','green'))) +
  #geom_point(colour = 'cyan4') +
  geom_point() +
  facet_wrap(. ~ leukemia, ncol=1) +
  #scale_y_log10() +
  xlab("Genes") +
  ylab("Expression") +
  ggtitle("Title") +
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1)) +
  theme(plot.title=element_text(hjust=0.5))


#Plot3
density_all <- st_jude_dwsz_long %>% ggplot(mapping = aes(x = expression , 
                                           color = leukemia)) +
  geom_density() 


# Write data --------------------------------------------------------------
ggsave(plot = boxplot_top10,
       filename = "results/04_boxplot_top10.png")

ggsave(plot = point_top10_6,
       width = 30, 
       height = 30, 
       units = "cm",
       filename = "results/04_point_top10_6.png")

ggsave(plot = density_all,
       filename = "results/04_density_all.png")

