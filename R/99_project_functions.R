library(tidyverse)
library(broom)

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