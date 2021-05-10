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

# ENTROPY PER GROUP PER GENE
info.gain <- function(partition.by, categories){
  # Breakdown categories by partition.by
  #partition.by <- st_jude_log %>% pull(partition.by)
  #categories <- st_jude_log %>% pull(leukemia)
  partition.table <- st_jude_log %>% 
    select(c(partition.by, categories)) %>% 
    arrange(partition.by)
  
  # Count occurrences per partition in each group
  patientsnum <- nrow(partition.table)/2
  partition.table_g1 <- partition.table %>% top_n(n = patientsnum, wt = partition.by)
  row.sums_g1 <- partition.table_g1  %>% group_by(categories) %>% summarise(num = n()) %>% pull()
  partition.table_g2 <- partition.table %>% top_n(n = patientsnum, wt = -partition.by)
  row.sums_g2 <- partition.table_g2 %>% group_by(categories) %>% summarise(num = n()) %>% pull()
  
  # Normalize rows in each group
  prob1 <- partition.table_g1 %>% table() %>% '/'(row.sums_g1)
  prob2 <- partition.table_g2 %>% table() %>% '/'(row.sums_g2)
  
  # Calculate entropy per row in each group
  ent1 <- -prob1 %>% '*'(log(prob1, 2))
  ent2 <- -prob2 %>% '*'(log(prob2, 2))
  # we will define 0 * log(0) as 0
  ent1 %>% replace_na(0)
  ent2 %>% replace_na(0)
  
  # Calculate entropy per row adding groups
  ent <- ent1 + ent2
  
  # Calculate information gain as 
  # original entropy of categories - weighted average of entropy partitioned data
  #info.gain <- entropy(categories) - sum(ent * (row.sums / sum(row.sums)))
  info.gain <- entropy - ent
  
  return(abs(info.gain))
}

info.gain('X65962', 'leukemia')





######## THE EXAMPLE OF HOW IT WORKS PER 1 GENE

# Breakdown categories by partition.by
partition.by <- st_jude_log %>% pull(X60957)
categories <- st_jude_log %>% pull(leukemia)
partition.table <- st_jude_log %>% 
  select(X60957, leukemia) %>% 
  arrange(X60957)

# Count occurrences per partition in each group
patientsnum <- nrow(partition.table)/2
partition.table_g1 <- partition.table %>% top_n(n = patientsnum, wt = X60957)
row.sums_g1 <- partition.table_g1  %>% group_by(leukemia) %>% summarise(num = n()) %>% pull()
partition.table_g2 <- partition.table %>% top_n(n = patientsnum, wt = -X60957)
row.sums_g2 <- partition.table_g2 %>% group_by(leukemia) %>% summarise(num = n()) %>% pull()

# Normalize rows in each group
prob1 <- partition.table_g1 %>% table() %>% '/'(row.sums_g1)
prob2 <- partition.table_g2 %>% table() %>% '/'(row.sums_g2)

# Calculate entropy per row in each group
ent1 <- -prob1 %>% '*'(log(prob1, 2))
ent2 <- -prob2 %>% '*'(log(prob2, 2))
# we will define 0 * log(0) as 0
ent1 <- ent1 %>% replace_na(0)
ent2 <- ent2 %>% replace_na(0)

# Calculate entropy per row adding groups
ent <- rbind(ent1,ent2)

# Calculate information gain as 
# original entropy of categories - weighted average of entropy partitioned data
#info.gain <- entropy(categories) - sum(ent * (row.sums / sum(row.sums)))
result <- entropy - ent

abs(result)


