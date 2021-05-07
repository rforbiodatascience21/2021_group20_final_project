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
                        y = fct_reorder2(gene, 
                                         desc(expr_level), 
                                         rank),
                        fill = expr_level)) + 
   geom_tile() +
   scale_fill_gradientn(colors = c("green1", "green3", "black", "red3", "red1"),
                        limits = c(-7, 7),
                        name = "Expression level")  +
   facet_grid( ~ leukemia, 
               space="free", 
               scales="free_x") +
   scale_y_discrete(position = "right") +
   theme_grey(base_size = 10) +
   theme(axis.text.x = element_blank(),
         axis.title  = element_blank(),
         axis.ticks = element_blank(),
         panel.border = element_blank(),
         panel.spacing = unit(0, "points"),
         panel.grid = element_blank(),
         strip.background = element_rect(color = "white"),
         strip.text = element_text(size = 10,
                                   face = "bold"),
         legend.position = "bottom",
         legend.direction = "horizontal") +
   guides(fill = guide_colorbar(title.position = "bottom",
                                title.hjust = 0.5))
  



      
p2 <- st_jude_top_10 %>% 
   filter(leukemia %in% c("BCR", "Hyperdip", "T"), 
          rank %in% c("BCR_top10", "Hyperdip_top10", "T_top10"))%>%
   ggplot(mapping = aes(x = sampleID,
                        y = fct_reorder2(gene, 
                                         desc(expr_level), 
                                         rank),
                        fill = expr_level)) + 
   geom_tile() +
   scale_fill_gradientn(colors = c("green1", "green3", "black", "red3", "red1"),
                        limits = c(-7, 7),
                        name = "Expression level")  +
   facet_grid( ~ leukemia, 
               space="free", 
               scales="free_x") +
   scale_y_discrete(position = "right") +
   theme_grey(base_size = 10) +
   theme(axis.text.x = element_blank(),
         axis.title  = element_blank(),
         axis.ticks = element_blank(),
         panel.border = element_blank(),
         panel.spacing = unit(0, "points"),
         panel.grid = element_blank(),
         strip.background = element_rect(color = "white"),
         strip.text = element_text(size = 10,
                                   face = "bold"),
         legend.position = "none") 

heatmap_1 <- p1+p2  
   
  

p3 <- st_jude_top_10 %>%   
   filter(str_detect(rank, "top10"))%>%
   ggplot(mapping = aes(x = sampleID,
                        y = fct_reorder2(gene, 
                                         (expr_level), 
                                         rank),
                        fill = expr_level)) + 
   geom_tile() +
   scale_fill_gradientn(colors = c("deepskyblue1", "black", "orange", "yellow"), 
                        limits = c(-3.7, 6.9),
                        name = "Expression level") +
   facet_grid( ~ leukemia, 
               space="free", 
               scales="free_x") +
   scale_y_discrete(position = "right") +
   theme_grey(base_size = 9) + 
   guides(fill = guide_colorbar(title.position = "top",
                                title.hjust = 0.5,
                                title.theme = element_text(size = 10))) +
   theme(axis.text.x = element_blank(),
         axis.title  = element_blank(),
         axis.ticks = element_blank(),
         panel.border = element_blank(),
         panel.spacing = unit(0, "points"),
         panel.grid = element_blank(),
         strip.background = element_rect(color = "white"),
         strip.text = element_text(size = 10,
                                   face = "bold"),
         legend.position = "bottom",
         legend.direction = "horizontal")
   



#### Plots all top 10 genes

p1b <- st_jude_top_10 %>% 
   filter(leukemia %in% c("E2A", "MLL", "TEL"), 
         str_detect(rank, "E2A|MLL|TEL"))%>%
   ggplot(mapping = aes(x = sampleID,
                        y = fct_reorder2(gene, 
                                         desc(expr_level), 
                                         rank),
                        fill = expr_level)) + 
   geom_tile() +
   scale_fill_gradientn(colors = c("green1", "green3", "black", "red3", "red1"),
                        limits = c(-7, 7),
                        name = "Expression level")  +
   facet_grid( ~ leukemia, 
               space="free", 
               scales="free_x") +
   scale_y_discrete(position = "right") +
   theme_grey(base_size = 10) +
   theme(axis.text.x = element_blank(),
         axis.title  = element_blank(),
         axis.ticks = element_blank(),
         panel.border = element_blank(),
         panel.spacing = unit(0, "points"),
         panel.grid = element_blank(),
         strip.background = element_rect(color = "white"),
         strip.text = element_text(size = 10,
                                   face = "bold"),
         legend.position = "bottom",
         legend.direction = "horizontal") +
   guides(fill = guide_colorbar(title.position = "bottom",
                                title.hjust = 0.5))


p2b <- st_jude_top_10 %>% 
   filter(leukemia %in% c("BCR", "Hyperdip", "T"), 
          str_detect(rank, "BCR|Hyperdip|_T|T_"))%>%
   ggplot(mapping = aes(x = sampleID,
                        y = fct_reorder2(gene, 
                                         desc(expr_level), 
                                         rank),
                        fill = expr_level)) + 
   geom_tile() +
   scale_fill_gradientn(colors = c("green1", "green3", "black", "red3", "red1"),
                        limits = c(-7, 7),
                        name = "Expression level")  +
   facet_grid( ~ leukemia, 
               space="free", 
               scales="free_x") +
   scale_y_discrete(position = "right") +
   theme_grey(base_size = 10) +
   theme(axis.text.x = element_blank(),
         axis.title  = element_blank(),
         axis.ticks = element_blank(),
         panel.border = element_blank(),
         panel.spacing = unit(0, "points"),
         panel.grid = element_blank(),
         strip.background = element_rect(color = "white"),
         strip.text = element_text(size = 10,
                                   face = "bold"),
         legend.position = "none") 

heatmap_2<- p1b+p2b 


p3b <- st_jude_top_10 %>% 
   ggplot(mapping = aes(x = sampleID,
                        y = fct_reorder2(gene, desc(expr_level), rank),
                        fill = expr_level)) + 
   geom_tile() +
   scale_fill_gradientn(colors = c("green1", "green3", "black", "red3", "red1"),
                        limits = c(-7, 7),
                        name = "Expression level")  +
   facet_grid( ~ leukemia, space="free", scales="free_x") +
   scale_y_discrete(position = "right")+
   theme(axis.text.x = element_blank(),
         axis.title  = element_blank(),
         axis.ticks = element_blank(),
         panel.border = element_blank(),
         panel.spacing = unit(0, "points"),
         panel.grid = element_blank(),
         strip.background = element_rect(color = "white"),
         legend.position = "bottom",
         legend.direction = "horizontal") +
   guides(fill = guide_colorbar(title.position = "bottom"))




# Write data --------------------------------------------------------------

ggsave(plot = heatmap_1,
      filename = "results/05_heatmap_1_pos.png")

ggsave(plot = heatmap_2,
       filename = "results/05_heatmap_2_top10.png")
