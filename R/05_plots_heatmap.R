# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("patchwork")


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
   arrange(leukemia, desc(ttest)) %>%
   mutate(rank = case_when(ttest > 0 ~ str_c(leukemia, "_top10"),
                           ttest < 0 ~ str_c("neg_corr_", leukemia)),
          rank = fct_inorder(rank))%>%
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


## Plots positively-correlated genes

p1 <- st_jude_top_10 %>% 
   filter(leukemia %in% c("E2A", "MLL", "TEL"), 
          rank %in% c("E2A_top10", "MLL_top10", "TEL_top10")) %>%
   ggplot(mapping = aes(x = sampleID,
                        y = fct_reorder2(gene, desc(expr_level), rank),
                        fill = expr_level)) + 
   geom_tile() +
   scale_fill_gradient2(low = "green", 
                        mid = "black", 
                        high = "red", 
                        midpoint = 0,
                        limits = c(-4, 6)) +
   scale_y_discrete(position = "right")+
   theme(axis.text.x = element_blank(),
         axis.title.y  = element_blank(),
         axis.ticks = element_blank(),
         axis.line.x.bottom = element_line(),
         legend.position = "bottom",
         legend.direction = "horizontal")



      
p2 <- st_jude_top_10 %>% 
   filter(leukemia %in% c("BCR", "Hyperdip", "T"), 
          rank %in% c("BCR_top10", "Hyperdip_top10", "T_top10"))%>%
   ggplot(mapping = aes(x = sampleID,
                        y = fct_reorder2(gene, desc(expr_level), rank),
                        fill = expr_level)) + 
   geom_tile() +
   scale_fill_gradient2(low = "green", 
                        mid = "black", 
                        high = "red", 
                        midpoint = 0,
                        limits = c (-4, 6.9)) +
   scale_y_discrete(position = "right")+
   theme(axis.text.x = element_blank(),
         axis.title.y  = element_blank(),
         axis.ticks = element_blank(),
         axis.line.x.bottom = element_line(),
         legend.position = "none")

p3 <- st_jude_top_10 %>%   
   filter(str_detect(rank, "top10"))%>%
   ggplot(mapping = aes(x = sampleID,
                        y = fct_reorder2(gene, desc(expr_level), rank),
                        fill = expr_level)) + 
   geom_tile() +
   scale_fill_gradient2(low = "deepskyblue1", 
                        mid = "black", 
                        high = "orange", 
                        midpoint = 0,
                        limits = c (-4, 6.9)) +
   scale_y_discrete(position = "right")+
   theme(axis.text.x = element_blank(),
         axis.title.y  = element_blank(),
         axis.ticks = element_blank(),
         axis.line.x.bottom = element_line(),
         legend.position = "bottom",
         legend.direction = "horizontal")


#### Plots all top 10 genes

p1b <- st_jude_top_10 %>% 
   filter(leukemia %in% c("E2A", "MLL", "TEL"), 
         str_detect(rank, "E2A|MLL|TEL"))%>%
   ggplot(mapping = aes(x = sampleID,
                        y = fct_reorder2(gene, desc(expr_level), rank),
                        fill = expr_level)) + 
   geom_tile() +
   scale_fill_gradient2(low = "green", 
                        mid = "black", 
                        high = "red", 
                        midpoint = 0,
                        limits = c(-4, 6.9)) +
   scale_y_discrete(position = "right")+
   theme(axis.text.x = element_blank(),
         axis.title.y  = element_blank(),
         axis.ticks = element_blank(),
         axis.line.x.bottom = element_line(),
         legend.position = "bottom",
         legend.direction = "horizontal")


p2b <- st_jude_top_10 %>% 
   filter(leukemia %in% c("BCR", "Hyperdip", "T"), 
          str_detect(rank, "BCR|Hyperdip|_T|T_"))%>%
   ggplot(mapping = aes(x = sampleID,
                        y = fct_reorder2(gene, desc(expr_level), rank),
                        fill = expr_level)) + 
   geom_tile() +
   scale_fill_gradient2(low = "green", 
                        mid = "black", 
                        high = "red", 
                        midpoint = 0,
                        limits = c (-4, 6.9)) +
   scale_y_discrete(position = "right")+
   theme(axis.text.x = element_blank(),
         axis.title.y  = element_blank(),
         axis.ticks = element_blank(),
         axis.line.x.bottom = element_line(),
         legend.position = "none")


p3b <- st_jude_top_10 %>% 
   ggplot(mapping = aes(x = sampleID,
                        y = fct_reorder2(gene, desc(expr_level), rank),
                        fill = expr_level)) + 
   geom_tile() +
   scale_fill_gradient2(low = "deepskyblue1", 
                        mid = "black", 
                        high = "orange", 
                        midpoint = 0,
                        limits = c (-4, 6.9)) +
   scale_y_discrete(position = "right")+
   theme(axis.text.x = element_blank(),
         axis.title.y  = element_blank(),
         axis.ticks = element_blank(),
         axis.line.x.bottom = element_line(),
         legend.position = "bottom",
         legend.direction = "horizontal")




# Write data --------------------------------------------------------------