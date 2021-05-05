# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")

# Define functions --------------------------------------------------------
#source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
st_jude_dwsz <- read_tsv(file = "data/03_stjude_downsized.tsv.gz")

# Wrangle data ------------------------------------------------------------

st_jude_dwsz_long <- st_jude %>% 
  pivot_longer(cols = c(-sampleID,-leukemia),
               names_to = "gene",
               values_to = "expression")

# Plot trials -------------------------------------------------------------

# Trials (too many genes...)
st_jude_dwsz_long %>% 
  filter(leukemia == 'BCR') %>% 
  ggplot(aes(x = gene, y = expression)) +
  geom_point(colour = 'cyan4') +
  #scale_y_log10() +
  xlab("Genes") +
  ylab("Expression") +
  ggtitle("Title") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title=element_text(hjust=0.5)) 

#Does that plot give any info that makes sense?
st_jude_dwsz_long %>% ggplot(mapping = aes(x = expression , color = leukemia)) +
  geom_density() 

#Choose one gene of interest (e.g: U31556_2) - we did that in the Labs
st_jude_dwsz %>% ggplot(mapping = aes(x = U31556_2, color = leukemia)) +
  geom_density()+
  labs(title = 'Densitogram of U31556_2, color=leukemia')

ggplot(data = st_jude_dwsz,
       mapping = aes(x = leukemia, y = U31556_2, fill = leukemia)) +
  geom_boxplot(alpha = 0.5)


# Write data --------------------------------------------------------------