library(tidyverse)
library(broom)

# Not used, that was a try for renaming colummns
translate_gene = function(element, table, col1, col2){
  print(element)
  res <- table %>% 
    filter(get(col1) == element) %>% 
    pull(get(col2))
  return(res)
}

apply_t_test = function(data){
  cancer_types <- c("BCR", "E2A", "Hyperdip", "MLL", "T", "TEL")
  my_list <- map(cancer_types,
                 ~ t.test(data %>%
                            filter(leukemia == .x) %>% 
                            pull(expr_level),
                          data %>%
                            filter(leukemia != .x) %>% 
                            pull(expr_level)) %>% 
                   tidy() %>% 
                   pull(statistic))
  names(my_list) <- cancer_types
  return(my_list)
}