# Descriptives
library(tidyverse)
library(gt)

dev <- read_rds("Data/Final_Data/development.rds")
hol <- read_rds("Data/Final_Data/full_data.rds")

# Development -------------------------------------------------------------
desc_dev <- dev |> select(matches("ms"),stdtest,OSAn, OSAt, OSAsport, grad6) |> Ben::HARcor() |> rename(Variable= var) |> Ben::recode_with_csv("Analysis/Renamer.csv",Variable) |>  gt()
desc_dev
desc_dev |> gtsave(filename = 'Output/desc_dev.html') 

# Holdout -----------------------------------------------------------------
desc_hol <- hol |> select(all_of(v_bert),stdtest,OSAn, OSAt, OSAsport, grad6) |> Ben::HARcor() |> rename(Variable= var) |> Ben::recode_with_csv("Analysis/Renamer.csv",Variable) |>  gt()
desc_hol
desc_hol |> gtsave(filename = 'Output/desc_hol.html') 
