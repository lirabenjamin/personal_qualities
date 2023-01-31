library(tidyverse)

coder_long <- read_csv("Data/Final_Data/coding_data.csv")
dev <- read_rds("Data/Final_Data/development.rds")
reliability_sample <-
  coder_long |> filter(n > 1) |> filter(coder != 'consensus')


reliability_nest <-
  reliability_sample |>
  group_by(name) |>
  nest()

get_alpha = function(x, level = 'nominal') {
  a <- x %>%
    group_by(id) %>%
    mutate(coder = 1:n()) %>%
    select(id, coder, value) |>
    pivot_wider(names_from = coder, values_from = value) %>%
    ungroup() %>%
    select(-id) %>%
    as.matrix() %>%
    t() %>%
    DescTools::KrippAlpha(method = level)
  return(tibble(
    n = a$subjects,
    alpha = a$value,
    raters = a$raters
  ))
}


# Get KA
alphas <- reliability_nest |>
  ungroup() |>
  slice(6, 4, 7, 1, 2, 5, 3) |>
  mutate(alpha = map(data, get_alpha, "ordinal")) |>
  unnest(alpha)

alphas |>
  ggplot(aes(name, alpha)) +
  geom_col() +
  geom_text(aes(label = Ben::numformat(alpha), y = alpha + .02))

# Get one of each to compare hc and hh apples to apples
comparison <-
  reliability_sample |>
  group_by(name, id) |>
  slice(1:2) |>
  mutate(coder = 1:2) |>
  unite(name, c(name, coder)) |>
  select(id, name, value) |>
  spread(name, value) |>
  ungroup() |>
  left_join(dev |> select(id, matches("ms")) |> rename(learning_ms = mastery_ms)) %>%
  select(all_of(sort(colnames(.)))) |>
  corrr::correlate() |>
  corrr::stretch() |>
  separate(x , c("x", "source_x")) |>
  separate(y , c("y", "source_y")) |>
  filter(x == y) |>
  filter(!is.na(r)) |>
  filter(
    (source_x == "1" & source_y == "2") |
      (source_x == "1" & source_y == "ms") |
      (source_x == "2" & source_y == "ms")
  ) |>
  mutate(comparison = case_when(
    (source_x == "1" & source_y == "2") ~ "hh",
    (source_x == "1" & source_y == "ms") ~ "hc",
    (source_x == "2" & source_y == "ms") ~ "hc"
  )) |>
  group_by(comparison, x) |>
  summarise(r = mean(r))

comparison |>
  ggplot(aes(x, y = r, fill = comparison)) +
  geom_col(position = 'dodge') +
  scale_fill_brewer(palette = "Set1")

comparison |>
  ggplot(aes(
    comparison,
    x,
    fill = r,
    label = Ben::numformat(r)
  )) +
  geom_tile() +
  geom_text()

t_test = function(data, x, y, paired = paired) {
  formula = formula(glue("{as.character(y)} ~ {as.character(x)}"))
  t = data |> t_test(formula, paired = paired)
  d = data |> rstatix::cohens_d(formula, paired = paired)
  msd = data |>
    group_by(group) |>
    summarise(mean = mean(y),
              sd = sd(y))
}


comparison |>
  ungroup() |>
  rstatix::t_test(r ~ comparison, paired = T) |>
  left_join(comparison |>
              ungroup() |>
              rstatix::cohens_d(r ~ comparison, paired = T)) |>
  left_join(
    comparison |>
      group_by(comparison) |>
      summarise(mean = mean(r),
                sd = sd(r)) |>
      pivot_wider(names_from = comparison, values_from = c(mean, sd)) |>
      mutate(.y. = "r")
  )
