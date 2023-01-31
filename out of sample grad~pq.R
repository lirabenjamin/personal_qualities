library(tidyverse)

train <- read_csv("Data/Final_Data/aws_direct_models/traindf.csv")
test <- read_csv("Data/Final_Data/aws_direct_models/testdf.csv")
full <- read_rds("Data/Final_Data/full_data.rds")

train <- 
  train |> 
  left_join(full |> select(id, prosocial:goal))

test <- 
  test |> 
  left_join(full |> select(id, prosocial:goal))

# Run glm on train
m = train |> 
  select(grad6:goal) |> 
  glm(grad6 ~ ., family = "binomial", data = _)

predictions <- predict(m, newdata = test,type = "response")
real <- test$grad6

auc <- pROC::ci.auc(real,predictions)
auc |> enframe()

