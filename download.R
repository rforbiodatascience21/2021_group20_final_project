rm(list = ls())
library(stjudem)
library(tidyverse)
data(stjude)
x <- stjude %>% 
  pluck("expr") %>% 
  as_tibble(rownames = NA)
rows <- rownames(x) %>% 
  as_tibble()

write_tsv(x = x, 
          file = "_raw/raw_stjude_x.tsv.gz")
write_tsv(x = rows, 
          file = "_raw/raw_stjude_xrows.tsv.gz")