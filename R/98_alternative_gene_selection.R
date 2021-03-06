## Entropy-based discretization method for gene selection (not further developed, since we chose using t-test)

# ENTROPY PER LEUKEMIA GROUPS
entropy <- function(input.vector){
  # Count leukemia groups occurrences
  t <- st_jude_log %>% 
    select(input.vector) %>% 
    group_by(input.vector) %>% 
    summarise(num = n()) %>% 
    pull()
  # normalize the table (now a probability)
  p <- t %>% 
    '/'(sum(t))
  # calculate entropy
  return(-p %>% 
           '*'(log(p, 2)))
}

# ENTROPY PER lEUKEMIA GROUP PER EACH GENE
info.gain <- function(partition.by, categories){
  # Breakdown categories by partition.by
  partition.table <- st_jude_log %>% 
    select(c(partition.by, categories)) %>% 
    arrange(partition.by)
  
  # Count occurrences per partition in each group
  patientsnum <- nrow(partition.table)/2
  partition.table_g1 <- partition.table %>% 
    top_n(n = patientsnum, wt = partition.by)
  row.sums_g1 <- partition.table_g1  %>% 
    group_by(categories) %>% 
    summarise(num = n()) %>% 
    pull()
  partition.table_g2 <- partition.table %>% 
    top_n(n = patientsnum, wt = -partition.by)
  row.sums_g2 <- partition.table_g2 %>% 
    group_by(categories) %>% 
    summarise(num = n()) %>% 
    pull()
  
  # Normalize rows in each group
  prob1 <- partition.table_g1 %>% 
    table() %>% 
    '/'(row.sums_g1)
  prob2 <- partition.table_g2 %>% 
    table() %>% 
    '/'(row.sums_g2)
  
  # Calculate entropy per row in each group
  ent1 <- -prob1 %>% 
    '*'(log(prob1, 2))
  ent2 <- -prob2 %>% 
    '*'(log(prob2, 2))
  # we will define 0 * log(0) as 0
  ent1 %>% 
    replace_na(0)
  ent2 %>% 
    replace_na(0)
  
  # Calculate entropy per row adding groups
  ent <- ent1 %>% 
    '+'(ent2)
  
  # Calculate information gain as 
  # original entropy of categories - weighted average of entropy partitioned data
  info.gain <- entropy %>% 
    '-'(ent)
  
  return(abs(info.gain))
}
