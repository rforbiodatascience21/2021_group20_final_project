# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
#source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
st_jude <- read_tsv(file = "data/03_stjude_downsized.tsv.gz")

top_40_genes <- read_tsv(file = "data/03_top_40_genes.tsv.gz")


# Wrangle data ------------------------------------------------------------

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

st_jude_scaled <- st_jude %>%
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



p1 <- st_jude_top_10 %>% 
   filter(leukemia %in% c("E2A", "MLL", "TEL"), rank %in% c("E2A_top10", "MLL_top10", "TEL_top10"))%>%
   ggplot(mapping = aes(x = sampleID,
                        y = fct_reorder2(gene, desc(expr_level), rank),
                        fill = expr_level)) + 
   geom_tile() +
   scale_fill_gradient2(low = "green", mid = "black", high = "red", midpoint = 0) +
   theme(axis.text.x = element_text(angle = 45, hjust=1, size = 5))
      
p2 <- st_jude_top_10 %>% 
   filter(leukemia %in% c("BCR", "Hyperdip", "T"), rank %in% c("BCR_top10", "Hyperdip_top10", "T_top10"))%>%
   ggplot(mapping = aes(x = sampleID,
                        y = fct_reorder2(gene, desc(expr_level), rank),
                        fill = expr_level)) + 
   geom_tile() +
   scale_fill_gradient2(low = "green", mid = "black", high = "red", midpoint = 0) +
   theme(axis.text.x = element_text(angle = 45, hjust=1, size = 5))




# Write data --------------------------------------------------------------