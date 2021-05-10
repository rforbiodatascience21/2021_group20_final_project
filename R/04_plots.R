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

# Wrangle data ------------------------------------------------------------
st_jude_dwsz_long <- st_jude_dwsz %>% 
  pivot_longer(cols = c(-sampleID,-leukemia),
               names_to = "gene",
               values_to = "expression")

# Plot trials -------------------------------------------------------------

#Plot1
boxplot_top10 <- st_jude_top_10 %>% 
  #filter(leukemia == 'BCR') %>% 
  ggplot(aes(x = gene, 
             y = expr_level, 
             color = leukemia)) +
  geom_point() +
  labs(title = "Boxplot of top 10 genes for each leukemia",
       x = "Genes",
       y = "Expression")+
  guides(color=guide_legend(title="Leukemia type"))+
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1)) +
  theme(plot.title=element_text(hjust=0.5)) 

#Plot2 (trial with 6 plots together)
point_top10_6 <- st_jude_top_10 %>% 
  ggplot(aes(x = fct_reorder2(gene, desc(expr_level), rank), 
             y = expr_level, 
             colour = ifelse(expr_level < 1, "red", "green"))) +
  geom_point() +
  facet_wrap(. ~ leukemia, ncol=1) +
  labs(title = "",
      x = "Genes",
      y = "Expression",
      caption = "Data source: Yeoh et al. (2002)")+
  scale_color_manual(name="Expression",values=c("red","green"),labels=c("Expression < 1","Expression > 1"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1)) +
  theme(plot.title=element_text(hjust=0.5))
point_top10_6
  
#Plot3
density_all <- st_jude_dwsz_long %>% ggplot(mapping = aes(x = expression, 
                                           color = leukemia)) +
  geom_density() +
  labs(title = "Densitogram of top 40 genes for each leukemia type",
       x = "Expression",
       y = "Density") +
  guides(color=guide_legend(title="Leukemia type"))+
  theme_classic()+
  theme(plot.title=element_text(hjust=0.5))


# Write data --------------------------------------------------------------
ggsave(plot = boxplot_top10,
       width = 30, 
       height = 20, 
       units = "cm",
       filename = "results/04_boxplot_top10.png")

ggsave(plot = point_top10_6,
       width = 30, 
       height = 30, 
       units = "cm",
       filename = "results/04_point_top10_6.png")

ggsave(plot = density_all,
       filename = "results/04_density_all.png")

