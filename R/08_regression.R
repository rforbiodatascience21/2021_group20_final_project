# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("ggplot2")
library("caret")
library("nnet")
library("broom")


# Load data ---------------------------------------------------------------
st_jude_top40 <- read_tsv(file = "./data/03_stjude_top40.tsv.gz")


# Model data --------------------------------------------------------------
# Subset the data so that we don't have too many weights in the model
st_jude_top40 <- st_jude_top40 %>% 
  select(2:100) %>% 
  mutate(leukemia = as_factor(leukemia))


training.samples <- st_jude_top40 %>%
  pull(leukemia) %>% 
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- st_jude_top40 %>% 
  filter(rownames(.) %in% training.samples)
test.data <- st_jude_top40 %>% 
  filter(!(rownames(.) %in% training.samples))

model <- nnet::multinom(leukemia ~., data = train.data)

test.data <- test.data %>%
  mutate(predicted = model %>% predict(test.data))

mean(test.data %>% pull(leukemia) == test.data %>% pull(predicted))