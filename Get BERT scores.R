library(tidyverse)

bert <- read_csv("Data/Final_Data/predictions2.csv")
full <- read_rds("Data/Final_Data/old_full_data.rds")
# Reliability

bert |> tail()
last = bert |> filter(`...1` == "selfconcordance9-221107-0217") |> select(-p1)
first = bert |> filter(`...1` != "selfconcordance9-221107-0217") |> select(-`...1`)
colnames(last) = colnames(first)
last <- last |> mutate(l0 = as.numeric(l0))
bert <- bind_rows(first,last)

scores <- 
  bert |> 
  select(model, p1) |> 
  mutate(pq = tm::removeNumbers(model) |> str_remove_all("--"),
         fold = parse_number(model))

scores

alphas <- 
  scores |> 
  select(-model) |> 
  group_by(pq, fold) |> 
  mutate(id = 1:306463) |> 
  pivot_wider(names_from = fold, values_from = p1,  names_prefix= "f") |> 
  group_by(pq) |> 
  select(-id) |> 
  nest()

alphas <- 
  alphas |> 
  mutate(alpha = map(data, psych::alpha))

get_mcor <- function(data){data |> corrr::correlate() |> corrr::stretch() |> pull(r) |> mean(na.rm =T)}
get_alpha = function(alpha){return(alpha$total$std.alpha)}
get_scores = function(alpha){return(alpha$scores)}

alphas$data[[1]]

alphas <- 
  alphas |> 
  mutate(std_alpha = map_dbl(alpha, get_alpha),
         mcor = map_dbl(data, get_mcor),
         scores = map(alpha,get_scores))

alphas |> arrange(std_alpha)
alphas |> arrange(mcor)

bert <- alphas |> 
  select(pq, scores) |> 
  spread(pq, scores) |> 
  unnest(cols = c(goal, leadership, mastery, perseverance, prosocial, selfconcordance, 
                  teamwork)) |> 
  mutate(id = full$id, response = full$response) |> 
  select(read_rds("Data/Final_Data/bert.rds") |> colnames())

write_rds(bert, "Data/Final_Data/roberta.rds")

new_full <- full |> 
  select(-(prosocial:goal)) |> 
  left_join(bert) |> 
  select(colnames(full))

new_full |> write_rds("Data/Final_Data/full_data.rds")

# Check: Did the id merge work correctly?
bert |>
  mutate(response = full$response) |> 
  pivot_longer(prosocial:goal) |> 
  group_by(name) |> 
  slice_max(value) |> 
  gt::gt()

