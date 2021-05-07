# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("ggplot2")

# Define functions --------------------------------------------------------
#source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
st_jude_dwsz <- read_tsv(file = "data/03_stjude_downsized.tsv.gz")
top_40_genes <- read_tsv(file = "data/03_top_40_genes.tsv.gz")

# Wrangle data ------------------------------------------------------------

#Just pivot longer all genes
st_jude_dwsz_long <- st_jude_dwsz %>% 
  pivot_longer(cols = c(-sampleID,-leukemia),
               names_to = "gene",
               values_to = "expression")

# Got this code from 05 to select top 10
top_10_genes <- top_40_genes %>%
  group_by(leukemia) %>% 
  top_n(n = 10, wt = ttest) %>% 
  ungroup() %>%
  mutate(rank = case_when(ttest != is.na(ttest) & ttest > 0 ~ str_c(leukemia, "_top10"))) %>%
  arrange(rank) %>%
  mutate(rank = case_when(rank != is.na(rank) ~ rank,
                          rank = is.na(rank) ~ str_c("neg_corr")),
         rank = fct_inorder(rank)) %>%
  select(gene, rank)

gene_names <- top_10_genes %>%
  pull(gene)

st_jude_scaled <- st_jude_dwsz %>%
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


#Find p value to do a volcano plot



# Plot trials -------------------------------------------------------------

#Plot1
st_jude_top_10 %>% 
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
st_jude_top_10 %>% 
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
ggsave("plot2.png", width = 30, height = 30, units = "cm")


#Plot3
st_jude_dwsz_long %>% ggplot(mapping = aes(x = expression , 
                                           color = leukemia)) +
  geom_density() 


#Plot4 - Choose one gene of interest (e.g: U31556_2) - can add this to shiny app
st_jude_dwsz %>% ggplot(mapping = aes(x = U31556_2, 
                                      color = leukemia)) +
  geom_density()+
  labs(title = 'Densitogram of U31556_2, color=leukemia')

