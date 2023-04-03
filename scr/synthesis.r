library(tidyverse)

# Excel file downloaded from Google Sheet:
# https://docs.google.com/spreadsheets/d/
# 1xvDJwiLBoI2oz8fxHJ5MjNmiju_RAlK7RJv-wXe1DAs/
# here::here("data/IMSR composite techniques.xlsx") %>%
#   readxl::read_xlsx() %>%
googlesheets4::read_sheet(
  ss = "1xvDJwiLBoI2oz8fxHJ5MjNmiju_RAlK7RJv-wXe1DAs", sheet = 1L
) |> 
  select(seq(2L, 4L)) %>%
  rename_with(~ str_replace(snakecase::to_snake_case(.), "_s$", "s")) %>%
  mutate(across(c(approach_types, terms), ~ str_split(., "(;|,) *"))) %>%
  print() -> composite_studies

# years of publication
composite_studies %>%
  transmute(year = as.integer(str_remove(citation, "^.*, "))) %>%
  ggplot(aes(x = year)) +
  geom_bar() +
  theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  labs(x = NULL, y = NULL) ->
  year_plot
ggsave(here::here("fig/fig-years.png"), year_plot, width = 5, height = 2)

# approach types
composite_studies %>%
  select(citation, approach_types) %>%
  unnest(approach_types) %>%
  mutate(approach_types = str_remove_all(approach_types, "^\\[|\\]$")) %>%
  print() -> composite_approaches
# approaches to ignore (check whether each study has one unignored)
drop_types <- c(
  "NN?", "unclear",
  "interactive implementation",
  "similarity matching", "whole-population weight learning"
)
# frequency table
composite_approaches %>%
  count(approach_types) %>%
  arrange(desc(n)) %>%
  filter(! approach_types %in% drop_types) %>%
  print(n = Inf)

# similarity learning
composite_approaches %>%
  filter(str_detect(approach_types, "similarity learning")) %>%
  select(citation) %>%
  inner_join(composite_approaches, by = "citation") %>%
  filter(! str_detect(approach_types, "similarity learning"))
# modeling on similarity cohorts
composite_approaches %>%
  filter(str_detect(approach_types, "modeling on similarity cohorts")) %>%
  select(citation) %>%
  inner_join(composite_approaches, by = "citation") %>%
  filter(! str_detect(approach_types, "modeling on similarity cohorts"))

# terms
composite_studies %>%
  select(citation, terms) %>%
  unnest(terms) %>%
  mutate(terms = str_remove_all(terms, "^\\[|\\]$")) %>%
  print() -> composite_terms
# frequency table
composite_terms %>%
  mutate(terms = tolower(terms)) %>%
  mutate(terms = str_replace(terms, "modeling", "model")) %>%
  mutate(terms = str_replace(terms, "sampling", "sample")) %>%
  mutate(terms = str_replace(terms, "cbr", "case-based reasoning")) %>%
  count(terms) %>%
  arrange(desc(n), terms) %>%
  print(n = Inf)

