# Many tables for supplement (HC Correlations one table per subgroup)

# Prelims -----------------------------------------------------------------
library(tidyverse)
library(gt)

source("Analysis/Final Files/F.R")

dev = read_rds("Data/Final_Data/development.rds")

# Nest data
nested_data <-  dev |>
  select(prosocial_hr:title1hs) |>
  mutate_at(vars(female:title1hs), as.character) |>
  pivot_longer(female:title1hs) |>
  group_by(name, value) |>
  nest()

# Table 2 for each demographic subgroup -----------------------------------
library(huxtable)
make_hcctable <- function(data) {
  cor <-
    data |>
    select(prosocial_hr:goal_ms) |>
    select(seq(1, 14, 2), seq(2, 14, 2)) |>
    Ben::harcor()
  msd = cor[15, c(1, 9:15)]
  msd$` ` = c("Mean of Computer-Generated Likelihood")
  names(msd)[2:8] = as.character(1:7)
  cor$` `[15] = "Frequency of Human Rating"
  cor[8:14,1] <- cor[8:14,1] |> str_remove_all("\\d")
  cor[8:14,1] <- paste0(1:7,cor[8:14,1])
  table = rbind(cor[8:15, 1:8], msd) |>
    separate(` `, c('Personal Quality', "Source"), sep = "_") |>
    mutate(
      Source = ifelse(
        Source == "ms",
        "Computer-Generated Likelihoods",
        "Descriptive Statistics"
      )
    ) |>
    mutate(Source = ifelse(is.na(Source), "Descriptive Statistics", Source)) |>
    group_by(Source) |>
    Ben::recode_with_csv("Analysis/Renamer.csv", `Personal Quality`) 
  table <-  table |> mutate_all(~str_remove_all(.,"†"))
  
  # table <- table |> gt::gt() |> Ben::gt_apa() 
  table <- table |>
    ungroup() |> 
    select(-Source) |> 
    hux() |>
    insert_row("Computer-Generated Likelihoods", colspan = 8, fill = "", after = 1) |> 
    set_bottom_border(row = 1, value = 0.4)|>
    set_top_border(row = 1, value = 0.4)|>
    set_bottom_border(row = 9, value = 0.4)|>
    set_bottom_border(row = 11, value = 0.4) |> 
    set_all_padding(value = 0) |> 
    map_align(by_cols("left", ".",'.','.','.','.','.','.')) |> 
    set_align(row= 2, value = 'left' ) |> 
    set_row_height(0)
  # table <- table |> ungroup() |> select(-Source) |>harcor_to_stargazer(out = "test.html",lineadjust = 2)
  return(table)
}

hcc_tables = nested_data |> 
  filter(!is.na(value)) |> 
  arrange(name) |> 
  mutate(hcc_table = map(data,make_hcctable)) |> 
  ungroup() |> 
  slice(11,14,10,13,12,15,5,7,6,3,4,8,9,1,2,16,17)

data = hcc_tables$data[[1]]

data |> make_hcctable() |> print_html()

hcc_tables$hcc_table[[1]]

for(i in 1:17){
  # [2:13]
  print(hcc_tables$name[i])
  print(hcc_tables$value[i])
  print_html(hcc_tables$hcc_table[[i]]) |> write_lines(file = "Output/accuracy_supp.html", append = T)
}
