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

# ENTROPY PER LEUKEMIA GROUPS
# Count leukemia groups occurrences
t <- st_jude_log %>% select(leukemia) %>% group_by(leukemia) %>% summarise(num = n()) %>% pull()
# normalize the table (now a probability)
p <- t %>% '/'(sum(t))
# calculate entropy
entropy <- -p %>% '*'(log(p, 2))

