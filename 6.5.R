  # 6.5 Evaluate imputation quality
# Checking imputation quality
if(rerun){
library(tidyverse)
library(mice)

imp <- read_rds("Data/Final_Data/imp.rds")
imp_complete <-  read_rds("Data/Final_Data/imp_complete.rds")

# Check imputation quality of imputed vars
imputed_vars = imp$nmis |> enframe() |> filter(value > 0) |> pull(name)
rename_vars <- function(x){
  x |> mutate(name = case_when(name == "gpa" ~ "HSGPA",
                               name == "grad4rates" ~ "4-Year Graduation Rates",
                               name == "grad6rates" ~ "6-Year Graduation Rates",
                               name == "stdtest" ~ "Standardized Test"))
}
rename_hs <- function(x){x |> mutate(hstype = case_when(hstype == "hs_home" ~ "Homeschool",
                                                        hstype == "hs_pubT1" ~ "Title-I Public School",
                                                        hstype == "hs_priv" ~ "Private School",
                                                        hstype == "hs_pubNOt1" ~ "Non Title-I Public School"))}

continuous_imp <-
  imp_complete |>
  filter(.imp != 0) |>
  as_tibble() |>
  select(
    .imp,
    `Highschool GPA` = gpa,
    `Standardized Test Scores` = stdtest,
    `College Graduation Rates` = grad6rates
  ) |>
  pivot_longer(2:4) |>
  ggplot(aes(value)) +
  geom_density(
    data =
      imp_complete |> filter(.imp == 0) |> select(
        `Highschool GPA` = gpa,
        `Standardized Test Scores` = stdtest,
        `College Graduation Rates` = grad6rates
      ) |> pivot_longer(1:3),
    alpha = 1,
    size = .5,
    color = "red"
  ) +
  geom_density(aes(group = .imp), alpha = .3, size = .05) +
  facet_wrap( ~ name, scales = "free") +
  labs(x = NULL, y = "Density") +
  egg::theme_article()
continuous_imp

hstype_imp <- 
  imp_complete |> 
  mutate(title1hs = ifelse(title1hs == 1, "Yes","No")) |> 
  filter(.imp != 0) |> 
  as_tibble() |> 
  select(.imp,`Title-I Highschool`=title1hs) |>
  pivot_longer(2) |> 
  group_by(.imp,name,value) |> 
  count() |> 
  group_by(.imp) |> 
  mutate(f = n/sum(n)) |> 
  ggplot(aes(value, f*100))+
  geom_point(data =  imp_complete |> 
               mutate(title1hs = ifelse(title1hs == 1, "Yes","No")) |> 
               filter(.imp == 0, !is.na(title1hs)) |> 
               as_tibble() |> 
               select(.imp,`Title-I Highschool`=title1hs) |>
               pivot_longer(2) |> 
               group_by(.imp,name,value) |> 
               count() |> 
               group_by(.imp) |> 
               mutate(f = n/sum(n)),
             size = 3, color = "red")+
  geom_point(size = 2, pch = 1)+
  # scale_y_log10()+
  labs(x= NULL, y = "Percentage")+
  facet_wrap( ~ name, scales = "free") +
  # coord_flip()+
  egg::theme_article()
hstype_imp

imp_qual <- ggpubr::ggarrange(continuous_imp,hstype_imp, widths = c(3,1))
imp_qual
ggsave("Output/imp_Imputation_quality.pdf",width = 10,height = 4)
ggsave("Output/imp_Imputation_quality.png",width = 10,height = 4)
}
