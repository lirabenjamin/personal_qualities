# Model interpretability
library(tidyverse)
theme_set(egg::theme_article())

attributions <- read_csv("Data/Final_Data/word_attributions.csv") |> filter(!is.na(model))

attributions |> count(model)

bad_ones <- attributions |> filter(is.na(model))
bad_ones <- bad_ones |> select(-5)
colnames(bad_ones) = c("word","score","id","model")
bad_ones <- bad_ones |> mutate(score = as.numeric(score), id = as.character(id))

bad_ones |> count(model)

attributions <- 
  attributions |> 
  filter(!is.na(model)) |> 
  bind_rows(bad_ones)


attributions <- attributions |> select(-1)
write_csv(attributions,"Data/Final_Data/word_attributions_fixed_raw.csv")
attributions = read_csv("Data/Final_Data/word_attributions_fixed_raw.csv")

attributions |> 
  mutate(word = str_replace_na(word)) |> 
  group_by(model, id) |> 
  summarise(text = str_c(word, collapse = " ")) |> 
  filter(str_detect(text,"unte")) |> 
  pull(text) |> 
  sample(1)

attributions |> 
  mutate(nextt = paste(lag(word),word)) |> 
  filter(str_detect(nextt,"gru")) |> 
  count(nextt)

attributions <- 
  attributions |> 
  mutate(word = tolower(word)) |> 
  group_by(model, word) |> 
  summarise(score = mean(score),
            n = n())

attributions |> 
  select(-score) |> 
  pivot_wider(names_from = model, values_from = n)

attributions |> filter(word == "0.1083481219989251")

write_csv(attributions,"Data/Final_Data/word_attributions_fixed_summary.csv")

library(tidytext)
attributions |> 
  mutate(model = model |> tm::removeNumbers() |> str_remove("--")) |> 
  filter(!is.na(model), n > 3) |> 
  filter(score > 0) |> 
  group_by(model) |> 
  slice_max(abs(score), n = 10) |> 
  arrange(score) |> 
  mutate(word = fct_inorder(word),
         direction = ifelse(sign(score)==1 , "positive","negative")) |> 
  ggplot(aes(word, abs(score), fill = direction))+
    geom_col()+
    facet_wrap(~model, scales = "free", nrow= 2)+
  coord_flip()+
  scale_fill_brewer(palette = "Set1")+
  labs(x = "Token", y = "Attribution Score")+
  theme(legend.position = c(.9, .2))
ggsave("Output/interpret.pdf", width = 8, height = 4)  

# Get correlation between freq and importance
library(magrittr)
attributions |> 
  mutate(model = model |> tm::removeNumbers() |> str_remove("--")) |> 
  filter(!is.na(model), n > 3) |> 
  filter(score > 0) |> 
  group_by(model) |> 
  arrange(score) |> 
  mutate(word = fct_inorder(word),
         direction = ifelse(sign(score)==1 , "positive","negative")) |> 
  # filter(model == "leadership") |> 
  ungroup() %$%
  cor.test(score, n,alternative = "two.sided", method = "pearson")

# Direct model
attributions <- read_csv("Data/Final_Data/word_attributions_direct.csv", col_names = F) 
colnames(attributions)  = c("word","score", "x3", "model")
attributions <- 
  attributions |> 
  mutate(word = tolower(word)) |> 
  group_by(model, word) |> 
  summarise(score = mean(score),
            n = n())

library(tidytext)
attributions |> 
  mutate(direction = ifelse(sign(score)==1 , "positive","negative")) |> 
  mutate(model = model |> tm::removeNumbers() |> str_remove("--")) |> 
  filter(!is.na(model), n > 5) |> 
  group_by(direction,model) |> 
  slice_max(abs(score), n = 20) |> 
  arrange(abs(score)) |> 
  mutate(word = fct_inorder(word),
         direction = ifelse(sign(score)==1 , "Positive","Negative")) |> 
  filter(word != "NA") |> 
  ggplot(aes(word, abs(score), fill = direction))+
  geom_col(show.legend = F)+
  facet_wrap(~direction, scales = "free", ncol= 2)+
  coord_flip()+
  scale_fill_brewer(palette = "Set1")+
  labs(x = "Token", y = "Attribution Score")+
  theme(legend.position = c(.9, .2),
        # panel.border = element_blank(),
        # axis.line = element_line(size = .2),
        # strip.background =  element_rect(fill = "gray90"),
        # strip.text = element_text(face = "bold")
        )
ggsave("Output/interpret_direct.pdf", width = 8, height = 4)  
 