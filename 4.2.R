# Human computer correlation and by demographic subgroups (table 3)
library(tidyverse)
library(broom)
library(gt)

dev <- read_rds("Data/Final_Data/development.rds")

dev.n <- dev %>% 
  pivot_longer(prosocial_hr:goal_ms) %>% 
  separate(name, c('pq','source')) %>% 
  spread(source, value) %>% 
  select(id,pq,ms,hr) |> 
  group_by(pq) %>% nest()

dev.nn <- 
  dev |> 
  select(-response, -c(OSAn:grad6)) |> 
  mutate(ell = factor(ell, labels=c("Native Speaker","Learner")),
         female = factor(female, labels = c("Male","Female")),
         parentmarried = factor(parentmarried,labels = c("Paretns not married", "Parents married")),
         title1hs = factor(title1hs, labels = c("Non-Title-I School","Title-I Public School"))) |> 
  pivot_longer(prosocial_hr:goal_ms) |> 
  separate(name, c('pq','source')) %>% 
  spread(source, value) |> 
  pivot_longer(female:title1hs) |> 
  group_by(name,value,pq) %>% 
  nest() |> 
  mutate(pq = factor(pq, levels = v_bert, labels=v_bert)) |> 
  arrange(pq)
dev.nn

# Computing Correlations --------------------------------------------------
getcor = function(x){cor.test(x$hr,x$ms)}
get_k_alpha = function(x){DescTools::KrippAlpha(x, method = "interval")}

correlations_overall = dev.n %>%  
  mutate(cor = map(data,getcor),
         cor = map(cor,tidy)) %>% 
  unnest(cor) 

correlations = dev.nn %>% 
  filter(!is.na(value)) |> 
  mutate(cor = map(data,getcor),
         cor = map(cor,tidy)) %>% 
  unnest(cor) 


# Significantly different from reference ------------------------------------
reference_cors <- correlations |> 
  select(pq, name, value, estimate, parameter) |> 
  mutate(reference =  case_when(
    value == "Female" ~ 'reference',
    value == "White" ~ 'reference',
    value == "None" ~ 'reference',
    value == "Parents married" ~ 'reference',
    value == "Native Speaker" ~ 'reference',
    value == "Title-I Public School" ~ 'reference',
    T ~ 'not reference'
  )) |> 
  filter(reference == "reference") |> 
  ungroup() |> 
  select(pq, name, estimate_ref = estimate, parameter_ref = parameter, value_ref = value)

tidy_cocor = function(cocor){tibble(r1 = cocor@r1.jk,n1 = cocor@n1,r2 = cocor@r2.hm,n2 = cocor@n2,dif = cocor@diff, z = cocor@fisher1925$statistic, p = cocor@fisher1925$p.value)}

table = correlations |> 
  select(pq, name, value, estimate, parameter) |> 
  left_join(reference_cors) |> 
  ungroup() |> 
  mutate(
    # Parameters are dfs (N - 2), so adding the two back up given that cocor requires sample sizes
    cocor = pmap(list(estimate, estimate_ref, (parameter + 2), (parameter_ref + 2)),cocor::cocor.indep.groups),
    cocor = map(cocor, tidy_cocor)
  ) |> 
  unnest(cocor) |> 
  arrange(p) |> 
  mutate(reference = ifelse(value_ref == value, "reference",'not reference')) |> 
  filter(reference != "reference") |> 
  mutate(padjust = p.adjust(p, method = "fdr")) |> 
  select(pq, name, value, r1,n1,r2,n2, dif,z,p, padjust) |> 
  rename(Comparison_r = r1,
         Comparison_n= n1,
         Reference_r = r2,
         Reference_n = n2, 
         Difference_r = dif,
         Difference_z = z,
         Difference_p = p, 
         Difference_padj = padjust) |> 
  arrange(pq, name) |> 
  group_by(pq, name) |> 
  mutate(name = ifelse(name %in% c("race",'parentdegree'),name,"Other demographics")) |> 
  Ben::recode_with_csv("Analysis/Renamer.csv", pq) |> 
  Ben::recode_with_csv("Analysis/Renamer.csv", name) |> 
  Ben::recode_with_csv("Analysis/Renamer.csv", value)

write_rds(table,"Output/comparative_accuracy.rds")

# How many are significantly differnet
num = table |> filter(Difference_padj < .05) |> nrow()
denom = table |> nrow()
num/denom

# Who is favored?
table |> filter(Difference_padj < .05) |> 
  select(name, value, Difference_r)

# Absolute value average of differences
table |> 
  filter(Difference_padj < .05) |> 
  select(name, value, Difference_r) |> 
  ungroup() |> 
  summarise(absr = mean(abs(Difference_r)), 
            min = min(Difference_r),
            max = max(Difference_r))

# Table
table |> 
  gt::gt() |> 
  gt::tab_spanner_delim("_") |> 
  gt::fmt_number(c(4,6,8,9), decimals = 2) |> 
  gt::fmt_number(c(10,11), decimals = 3) |> 
  # gt::tab_style(cell_fill(),cells_body(rows = Difference_p < .05)) |> 
  gt::tab_style(cell_fill(),cells_body(rows = Difference_padj < .05)) |> 
  Ben::gt_apa()

table |> select(pq, name, value,Comparison_r, Reference_r, Difference_r, Difference_p) |> 
  mutate(cell = glue::glue("{Ben::numformat(Comparison_r)} - {Ben::numformat(Reference_r)} = {Ben::numformat(Difference_r)}{Ben::codeps(Difference_p)}")) |> 
  # mutate(cell = glue::glue("{Ben::numformat(Difference_r)}{Ben::codeps(Difference_p)}")) |> 
  select(pq, name, value, cell) |> 
  mutate(cell = str_remove_all(cell, "†")) |> 
  spread(pq, cell) |> 
  ungroup() |> 
  slice(8,9,7,11,10,1,2,4,6,3,5) |> 
  group_by(name) |> 
  gt::gt() |> 
  Ben::gt_apa()
# Positive values mean that convergent validity is higher in the comparison group, or lower in the reference group. 

table |>
  mutate(name = ifelse(name == "Race/ethnicity", "Race/ethnicity (vs. White)",name)) |> 
  mutate(name = ifelse(name == "Number of parents with college degrees", "Number of parents with college degrees (vs. None)",name)) |> 
  select(pq, name, value,Comparison_r, Reference_r, Difference_r, Difference_padj) |> 
  mutate(Difference_r = glue::glue("{Ben::numformat(Difference_r)}{Ben::codeps(Difference_padj)}")) |> 
  select(-Difference_padj) |> 
  mutate(Difference_r = str_remove_all(Difference_r, "†")) |> 
  rename(C = Comparison_r, R = Reference_r, D = Difference_r) |> 
  pivot_wider(names_from = pq, values_from = c(C,R,D), names_glue = "{pq}_{.value}") |>
  mutate_at(3:(3+13), Ben::numformat) |> 
  select(1,2,seq(3,23,7),seq(4,23,7),seq(5,23,7),seq(6,23,7),seq(7,23,7),seq(8,23,7),seq(9,23,7)) |> 
  ungroup() |> 
  slice(7,9,8,10,6,4,3,2,5,1,11) |> 
  group_by(name) |> 
  gt::gt() |> 
  gt::tab_spanner_delim("_") |> 
  Ben::gt_apa()


# Formatting --------------------------------------------------------------
correlations <- 
  correlations |> 
  Ben::recode_with_csv("Analysis/Renamer.csv", pq) |> 
  Ben::recode_with_csv("Analysis/Renamer.csv", name) |> 
  mutate(pq = fct_inorder(pq))

correlations_overall <- 
  correlations_overall |> 
  Ben::recode_with_csv("Analysis/Renamer.csv", pq) |> 
  mutate(pq = fct_inorder(pq))

correlations_overall |> pull(estimate) |> Ben::average_correlations()

create_composite <-
  function(data,selection, name, na.rm = T) {
    data |>
      dplyr::rowwise() |>
      dplyr::mutate({{name}} := mean(c({{selection}}, na.rm = T))) |>
      dplyr::ungroup()
  }


# Ns ----------------------------------------------------------------------

Ns = correlations |> 
  select(pq, name, value, parameter) |> 
  group_by(value) |> 
  summarise(n = mean(parameter))



# Discriminant Correlations for Table 2 -----------------------------------
dev = read_rds("Data/Final_Data/development.rds")

# Nest data
nested_data <-  
  dev.nn |>
  unnest(data) |> 
  pivot_wider(names_from = pq, values_from = c(hr, ms), names_glue = "{pq}_{.value}")|>
  group_by(name, value) |>
  select(colnames(dev)[c(1,3:16)]) |>
  nest()


get_average_discriminant_cors <- function(data) {
  data <- data |>
    select(prosocial_hr:goal_ms) |>
    corrr::correlate() |>
    # Make long format, each r only once
    corrr::stretch(remove.dups = T) |>
    filter(!is.na(r)) |>
    separate(x, c('x_pq', 'x_s')) |>
    separate(y, c('y_pq', 'y_s')) |>
    # Remove correlation with the same pq (convergent validity)
    filter(x_pq != y_pq) |>
    # Remove human human, and computer computer correlations
    filter(x_s != y_s)
  r1 = data |> pull(r) |> Ben::average_correlations() |> round(2)
  r2 = data |> mutate(r = abs(r)) |> pull(r) |> Ben::average_correlations() |> round(2)
  r3 = data |> pull(r) |> min() |> round(2)
  r4 = data |> pull(r) |> max() |> round(2)
  
  result  = tibble(avg = r1,absavg = r2, min = r3, max = r4)
  return(result)
  
}

discriminant <- nested_data |>
  mutate(avg_disc_cor = map(data, get_average_discriminant_cors)) |> 
  unnest(avg_disc_cor) |> 
  Ben::recode_with_csv("Analysis/Renamer.csv", name) |> 
  Ben::recode_with_csv("Analysis/Renamer.csv", value) |> 
  mutate(pq = "Overall")

discriminant
# Table -------------------------------------------------------------------
table3 <- 
  correlations %>%
  ungroup() |>
  bind_rows(correlations_overall) %>%
  mutate(
    value = ifelse(is.na(value), "Overall", as.character(value)),
    name = ifelse(is.na(name), "Overall", as.character(name))
  ) |>
  select(name, value, pq, estimate) %>%
  pivot_wider(names_from = pq, values_from = c(estimate)) |>
  rowwise() |>
  mutate(`Convergent Validity` = Ben::average_correlations(
    c(
      `Goal pursuit`,
      Leadership,
      `Mastery orientation`,
      Perseverance,
      `Prosocial purpose` ,
      `Intrinsic motivation`,
      Teamwork
    )
  )) |> 
  filter(name != "Overall") |> 
  left_join(Ns) |> 
  select(name,value, n, everything()) |> 
  left_join(discriminant |> select(name, value, 
                                   `Average Discriminant Validity` = avg,
                                   `Absolute Average Discriminant Validity` = absavg,
                                   `Minimum Discriminant Validity` = min,
                                   `Maximum Discriminant Validity` = max,
                                   )) |> 
  ungroup() |> 
  arrange(name) |> 
  slice(11,14,10,13,12,15,7,9,8,3,4,5,6,1,2,16,17) |> 
  mutate_at(1:2, fct_inorder) |> 
  group_by(name) %>% 
  gt() %>% 
  fmt_number(decimals = 2,columns = 4:14) %>% 
  fmt_number(decimals = 0,columns = 3) %>% 
  tab_style(cell_borders(weight = px(0)), locations = cells_body()) %>%
  tab_style(cell_text(indent  = px(15)), locations = cells_body(columns = 2)) %>%
  tab_style(cell_borders(weight = px(0)), locations = cells_row_groups()) %>% 
  tab_options(data_row.padding = px(1))
table3 
table3|> gtsave("Output/accuracy_detailed.tex")
table3|> gtsave("Output/accuracy_detailed.html")

# Plot --------------------------------------------------------------------
correlations %>%
  ungroup() |> 
  mutate(
    pq = str_replace_all(pq, " ", "\n"),
    pq = fct_inorder(pq)
    ) |> 
  ggplot(aes(value, estimate, color = estimate))+
  geom_hline(aes(yintercept = estimate), 
             data = correlations_overall |> mutate(
               pq = str_replace_all(pq, " ", "\n"),
               pq = fct_inorder(pq)
             ) ,
             color = "red", alpha = .5)+
  geom_point()+
  geom_errorbar(aes(ymin= conf.low, ymax = conf.high), width = 0)+
  coord_flip(ylim = c(.45,.9))+
  scale_y_continuous(breaks  = c(.5,.6, .7,.8, .9), labels = c('.5', '.6', '.7','.8','.9'))+
  scale_color_viridis_c()+
  egg::theme_article()+
  facet_grid(name~pq, scales = "free",space = "free")+
  theme(legend.position = "none")+
  labs(y = "Human Computer Correlation", x = NULL)+
  theme(strip.text.y = element_blank())
ggsave("Output/accuracy_human_computer.pdf",width = 7.5, height = 4)
ggsave("Output/accuracy_human_computer.png",width = 7, height = 5)

correlations_overall |>
  ungroup() |> 
  mutate( pq = str_replace_all(pq, " ", "\n"),pq = fct_inorder(pq)) |> 
  select(pq,  estimate, conf.low, conf.high) |> 
  ggplot(aes(ordered(pq), estimate))+
  geom_col()+
  geom_text(aes(label = Ben::numformat(estimate), y = conf.high+.02))+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = .3)+
  labs(x = NULL, y = "Convergent Validity")+
  egg::theme_article()
ggsave("Output/accuracy_human_computer_overall.pdf",width = 10, height = 6)
ggsave("Output/accuracy_human_computer_overall.png",width = 7, height = 5)

correlations_overall |>ungroup() |> 
  mutate(pq = fct_inorder(pq)) |> 
  mutate(estimate = c(.83, .78, .61, .73, .66,.63,.57)) |> 
  select(pq,  estimate, conf.low, conf.high) |> 
  ggplot(aes(ordered(pq), estimate))+
  geom_col()+
  geom_text(aes(label = Ben::numformat(estimate), y = estimate+.02))+
  labs(x = "Personal Quality", y = "Interrater Reliability")
ggsave("Output/interrater reliability.pdf",width = 10, height = 6)
ggsave("Output/accuracy_human_computer_overall.png",width = 7, height = 5)
