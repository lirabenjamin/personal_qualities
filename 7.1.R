# 7.1 Compare model scores and human ratings across subgroups
library(tidyverse)
dev <- read_rds("Data/Final_Data/development.rds")
full <- read_rds("Data/Final_Data/full_data.rds")

# Data Preparation --------------------------------------------------------

if(rerun){
nd <- 
  dev |> 
  select(prosocial_hr:goal_ms, female:title1hs) |> 
  pivot_longer(prosocial_hr:goal_ms, values_to = "pq_score") |> 
  separate(name, c('pq','source'),"_") |> 
  fastDummies::dummy_cols(c('parentdegree', 'race'),ignore_na = T, remove_selected_columns = T) |> 
  select(pq, source, pq_score, everything()) |> 
  pivot_longer(female:race_Missing, names_to = "demo_group", values_to = "demo") |> 
  group_by(source, demo_group, pq) |> 
  nest() |> 
  mutate(dataset = "dev")

nd_full <- 
  full |> 
  select(prosocial:goal, female:title1hs) |> 
  rename_at(1:7, function(x){paste0(x,"_ms")}) |> 
  pivot_longer(prosocial_ms:goal_ms, values_to = "pq_score") |> 
  separate(name, c('pq','source'),"_") |> 
  fastDummies::dummy_cols(c('parentdegree', 'race'),ignore_na = T, remove_selected_columns = T) |> 
  select(pq, source, pq_score, everything()) |> 
  pivot_longer(female:race_Missing, names_to = "demo_group", values_to = "demo") |> 
  group_by(source, demo_group, pq) |> 
  nest() |> 
  mutate(dataset = "full")

nd$data[[1]] |> rstatix::t_test(pq_score ~ demo)


# MCCs --------------------------------------------------------------------
library(magrittr)
get_cor = function(x){cor.test(x$pq_score,x$demo) |> broom::tidy()}
nd |> 
  mutate(cor = map(data,get_cor)) |> 
  unnest(cor) |> 
  ungroup() |> 
  summarise(mean_phi = mean(abs(estimate)))

nd |> 
  mutate(cor = map(data,get_cor)) |> 
  unnest(cor) |> 
  ungroup() |> 
  filter(source == "hr") |> 
  mutate(pad = p.adjust(p.value,method = "fdr")) |> 
  filter(pq == "prosocial" & demo_group == "female")

getcor <- function(data){data %$% cor.test(pq_score,demo) |> broom::tidy() |> select(r = estimate, r_p = p.value, r_lo = conf.low, r_hi = conf.high)}

nd |> filter(source == "hr", demo_group == "ell", pq == "goal") |> ungroup() |> select(data) |> unnest(data) |> getcor()

# Run t tests and cohen's D -----------------------------------------------
cohens_ds <- 
  nd |> 
  bind_rows(nd_full) |> 
  mutate(r = map(data,getcor),
         d = map(data, ~psych::cohen.d(formula(glue::glue("pq_score ~ demo")),data=.)),
         d = map(d, function(x){as_tibble(x$cohen.d)}),
         t = map(data,function(x){x |> rstatix::t_test(pq_score~demo)})) |> 
  unnest(d)

write_rds(cohens_ds, file = "Output/demo_cohens_ds.rds")

ds_compare <- full |> 
  select(stdtest, matches('OSA'),female:title1hs) |> 
  pivot_longer(stdtest:OSAsport, names_to = 'outcome', values_to = "outcome_score") |> 
  fastDummies::dummy_cols(c('parentdegree', 'race'),ignore_na = T, remove_selected_columns = T) |> 
  select(outcome,outcome_score, everything()) |> 
  pivot_longer(female:race_Missing, names_to = "demo_group", values_to = "demo") |> 
  group_by(demo_group, outcome) |> 
  nest() |> 
  mutate(dataset = "full") |> 
  mutate(d = map(data, ~psych::cohen.d(formula(glue::glue("outcome_score ~ demo")),data=.)),
         d = map(d, function(x){as_tibble(x$cohen.d)})) |> 
  unnest(d)
write_rds(ds_compare, file = "Output/ds_compare.rds")
}
# Checkpoint --------------------------------------------------------------
library(tidyverse)
cohens_ds <- read_rds("Output/demo_cohens_ds.rds")
ds_compare <- read_rds("Output/ds_compare.rds")
dev <- read_rds("Data/Final_Data/development.rds")
full <- read_rds("Data/Final_Data/full_data.rds")


# unnest(d) |> 
# Ben::recode_with_csv("Analysis/Renamer.csv", pq) |> 
# Ben::recode_with_csv("Analysis/Renamer.csv", demo_group) |> 
# Ben::recode_with_csv("Analysis/Renamer.csv", source) |> 
# mutate(demo_group = str_remove_all(demo_group, "race_")) %>% 
# ungroup() |>
# mutate(demo_group = fct_inorder(demo_group) %>% fct_rev) %>% 
# mutate(pq = fct_inorder(pq) %>% rev)



# Things referenced in text -----------------------------------------------
# Prosocial and female
cohens_ds |>
  group_by(source) |> 
  unnest(t) |> 
  unnest(r) |> 
  mutate(padjust_d = p.adjust(p, method = 'fdr'),
         padjust_r = p.adjust(r_p, method = 'fdr')) |> 
  filter(pq == "prosocial" , demo_group == "female") |> 
  select(r, r_p, effect, padjust_d)

# Absolute averages
cohens_ds |> 
  group_by(source, dataset) |> 
  unnest(r) |> 
  summarise(d = mean(abs(effect)),
            r = mean(abs(r)))


# Comparison groups
ds_compare |> 
  mutate(category = ifelse(outcome == "stdtest", "stdtest","OSA")) |> 
  group_by(category) |> 
  summarise(d = mean(abs(effect)))

# Plots -------------------------------------------------------------------
demo_ds_3samples = cohens_ds |> 
  Ben::recode_with_csv("Analysis/Renamer.csv", pq) |> 
  Ben::recode_with_csv("Analysis/Renamer.csv", demo_group) |> 
  ungroup() |> 
  mutate(
    pq = str_replace_all(pq, " ","\n"),
    pq = fct_inorder(pq),
    demo_group = str_replace_all(demo_group, "Race/ethnicity_","Race: "),
    demo_group = str_replace_all(demo_group, "Number of parents with college degrees_","Parents w/ degree: "),
         demo_group = fct_inorder(demo_group) |> fct_rev(),
        source = ifelse(source== "hr", "Human Rating\n","Computer-Generated Likelihood\n"),
        dataset = ifelse(dataset == 'dev',"Development Sample", "Holdout Sample")) |> 
  ggplot(aes(demo_group, effect, ymin = lower, ymax = upper, color = paste(source,dataset)))+
  geom_hline(yintercept = 0, size = .25)+
  geom_point(position = position_dodge(width = .3))+
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0,position = position_dodge(width = .3), alpha = .5)+
  facet_grid(~pq)+
  coord_flip()+
  egg::theme_article()+
  scale_color_brewer(palette = "Set1")+
  labs(x = NULL, y = "Cohen's D", color = "Source")+
  theme(legend.position = "bottom")
demo_ds_3samples
ggsave("Output/demo_cohend_all.pdf",width = 9, height = 4)

demo_rs_3samples = cohens_ds |> 
  unnest(r) |> 
  ggplot(aes(demo_group, r, ymin = r_lo, ymax = r_hi, color = paste(source,dataset)))+
  geom_hline(yintercept = 0, size = .25)+
  geom_point(position = position_dodge(width = .3))+
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0,position = position_dodge(width = .3), alpha = .5)+
  facet_grid(~pq)+
  coord_flip()+
  egg::theme_article()+
  scale_color_brewer(palette = "Set1")+
  labs(x = NULL, y = "r", color = "Source")+
  theme(legend.position = "bottom")

cohens_ds |> 
  arrange(source) |> 
  mutate(source = paste(dataset,source)) |> 
  ggplot(aes(effect, fill = source, col = source))+
  geom_density(alpha = .3)+
  geom_point(aes(y = rep(c(.25,.5, .75), each = nrow(cohens_ds)/3)))+
  egg::theme_article()+
  scale_fill_brewer(palette = "Set1")+
  scale_color_brewer(palette = "Set1")+
  labs(x = "Cohen's D for Demographic Groups", y = "Density", fill = NULL,col = NULL)+
  theme(legend.position = c(.8,.8))
ggsave("Output/demo_cohend_human_vs_model_alldev_hist.pdf", width = 4, height = 3)
ggsave("Output/demo_cohend_human_vs_model_alldev_hist.png", width = 4, height = 3)

cohens_ds |> 
  arrange(source) |> 
  unnest(r) |> 
  mutate(source = paste(dataset,source)) |> 
  ggplot(aes(r, fill = source, col = source))+
  geom_density(alpha = .3)+
  geom_point(aes(y = rep(c(.25,.5, .75), each = nrow(cohens_ds)/3)))+
  egg::theme_article()+
  scale_fill_brewer(palette = "Set1")+
  scale_color_brewer(palette = "Set1")+
  labs(x = "Point-biserial or Matthews Correlation Coefficients", y = "Density", fill = NULL,col = NULL)+
  theme(legend.position = c(.8,.8))
ggsave("Output/demo_r_human_vs_model_alldev_hist.pdf", width = 4, height = 3)
ggsave("Output/demo_r_human_vs_model_alldev_hist.png", width = 4, height = 3)
