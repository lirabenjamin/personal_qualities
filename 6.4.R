# 6.4 Impute data
if(rerun){
library(mice)
library(tidyverse)

full <- read_rds("Data/Final_Data/full_data.rds")

# full <- read_rds("full_data.rds")
qp <- quickpred(full, exclude = c("id","response")) 
qp |> write_rds("Output/imp_qp.rds")

qp <- read_rds("Output/imp_qp.rds")

# To be predicted
qp |>
  as_tibble()  |>
  mutate(var = dimnames(qp)[[1]],rowsum = rowSums(qp))|> 
  filter(rowsum != 0) |> 
  select(var, everything(), -rowsum) |> 
  pivot_longer(-var) |> 
  filter(value!=0) |> 
  pivot_wider(names_from = var, values_from = value)

imp <- parlmice(
  pred = qp,
  full,
  n.core = 5,
  n.imp.core = 5,
  cluster.seed = 524,
  cl.type = "FORK"
)

write_rds(imp, "Data/Final_Data/imp.rds")
imp_complete <- imp |> complete(include = T, action = 'long')
imp_complete |> write_rds("Data/Final_Data/imp_complete.rds")
}