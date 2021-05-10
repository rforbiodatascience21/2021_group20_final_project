# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("ggplot2")
library("caret")
library("nnet")
library("broom")


# Load data ---------------------------------------------------------------
top_40_genes <- read_tsv(file = "./data/03_stjude_downsized.tsv.gz")


# Model data --------------------------------------------------------------
# Subset the data so that we don't have too many weights in the model
top_40_genes <- top_40_genes %>% 
  select(2:100) %>% 
  mutate(leukemia = as_factor(leukemia))


training.samples <- top_40_genes %>%
  pull(leukemia) %>% 
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- top_40_genes %>% 
  filter(rownames(.) %in% training.samples)
test.data <- top_40_genes %>% 
  filter(!(rownames(.) %in% training.samples))

model <- nnet::multinom(leukemia ~., data = train.data)

test.data <- test.data %>%
  mutate(predicted = model %>% predict(test.data))

mean(test.data %>% pull(leukemia) == test.data %>% pull(predicted))