# 7.2 Generate specifics for Table S1, S3
library(tidyverse)

dev <- read_rds("Data/Final_Data/development.rds")
full <- read_rds("Data/Final_Data/full_data.rds")
cohens_ds <- read_rds("Output/demo_cohens_ds.rds")

cohens_ds <- cohens_ds |> 
  separate(demo_group, c('demo','group')) |> 
  mutate(group = ifelse(is.na(group), demo, group)) |> 
  Ben::recode_with_csv("Analysis/Renamer.csv",demo) |> 
  Ben::recode_with_csv("Analysis/Renamer.csv",group) |> 
  Ben::recode_with_csv("Analysis/Renamer.csv",pq) |> 
  mutate(pq = fct_inorder(pq))

library(gt)

# Detailed Ds and Rhos
dev_hr = cohens_ds |> 
  unnest(r) |> 
  ungroup() |> 
  filter(dataset == 'dev', source == "hr") |> 
  select(pq, demo,group,r) |> 
  pivot_wider(names_from= pq,values_from = r) |> 
  group_by(demo) |> 
  gt() |> 
  fmt_number(3:9)
gtsave(dev_hr,"Output/demo_dev_hr.html")  
  
dev_ms <- cohens_ds |> 
  ungroup() |> 
  filter(dataset == 'dev', source == "ms") |> 
  select(pq, demo,group,effect) |> 
  pivot_wider(names_from= pq,values_from = effect) |> 
  group_by(demo) |> 
  gt() |> 
  fmt_number(3:9)
gtsave(dev_ms,"Output/demo_dev_ms.html")  


hol_ms <- cohens_ds |> 
  ungroup() |>
  filter(dataset == 'full', source == "ms") |> 
  select(pq, demo,group,effect) |> 
  pivot_wider(names_from= pq,values_from = effect) |> 
  group_by(demo) |> 
  gt() |> 
  fmt_number(3:9)
gtsave(hol_ms,"Output/demo_hol_ms.html")  

