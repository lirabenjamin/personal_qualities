# T2: Human Computer Correlations -----------------------------------------
library(gt)
dev <- read_rds("Data/Final_Data/development.rds")


t2 = dev |> 
  select(matches("hr"),matches('ms')) |> 
  Ben::HARcor() 

hmean = t2 |> slice(15) |> t() |> enframe() |> slice(2:8) |> mutate(value = as.character(value)) |> pull(value)
compmean = t2 |> slice(15) |> t() |> enframe() |> slice(9:15) |> mutate(value = as.character(value)) |> pull(value)
irr = c('.83', '.78', '.61', '.73', '.66', '.63', '.57')
table2 <- t2 |> 
  filter(str_detect(var,"ms")) |>
  select(1:8) |> 
  rbind(c("Human Interrater Reliability",irr)) |> 
  rbind(c("Frequency of Human Rating", hmean)) |> 
  rbind(c("Mean of Computer Generated Likelihood", compmean)) |> 
  separate(var, c("Personal Quality","Source"), "_") |> 
  Ben::recode_with_csv("Analysis/Renamer.csv", `Personal Quality`) |> 
  Ben::recode_with_csv("Analysis/Renamer.csv", `Source`) |> 
  group_by(Source) |> 
  gt::gt()
table2

gtsave(table2, "Output/accuracy_overall.html")
gtsave(table2, "Output/accuracy_overall.tex")

# Average correlation
diagonal <- 
  dev |> 
  select(id,matches("hr"),matches('ms')) |> 
  pivot_longer(2:15) |> 
  separate(name, c('pq','source')) |> 
  pivot_wider(names_from = source, values_from = value) |> 
  group_by(pq) |> 
  nest() |>
  mutate(cor = map_dbl(data,function(x){cor(x$hr, x$ms, use = 'pairwise.complete.obs')})) |> 
  ungroup()

diagonal |> summarise(mean = Ben::average_correlations(cor), min = min(cor), max = max(cor))

dev |> 
  select(matches("hr"),matches('ms')) |> 
  corrr::correlate() |> 
  corrr::stretch() |> 
  separate(x, c('pq1', 's1')) |> 
  separate(y, c('pq2', 's2')) |> 
  filter(s1 != s2) |> 
  filter(pq1 != pq2) |> 
  summarise(mean = Ben::average_correlations(r), min = min(r), max = max(r))

# Correlation between interrater reliability and human computer correlation
irr = c(.83, .78, .61, .73, .66, .63, .57)
cor.test(irr, diagonal$cor)
