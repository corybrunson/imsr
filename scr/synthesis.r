library(tidyverse)
library(ggmosaic)

# scrape and format table summarizing studies included in synthesis
# Excel file downloaded from Google Sheet:
# https://docs.google.com/spreadsheets/d/
# 1tpWMhYH2pyRT55K7n2J2XFs-kEV_JTuCDmXzJ4BBgNo/
googlesheets4::read_sheet(
  ss = "1tpWMhYH2pyRT55K7n2J2XFs-kEV_JTuCDmXzJ4BBgNo", sheet = 1L
) |>
  rename_with(snakecase::to_snake_case) |>
  filter(is.na(synthesis)) |> 
  arrange(date_of_publication) |> 
  transmute(
    Citation = paste0("@", zotero_key),
    Date = format(date_of_publication, "%Y %b %d"),
    Task = objective,
    Aim = ifelse(generalizable_knowledge == "Science", "Knowledge", "Practice"),
    Source = type_of_data,
    Type = range_of_data,
    `Cases` = number_of_cases_incidents,
    `Features` = number_of_predictors_features
  ) |>
  mutate(Task = ifelse(Task == "Recommendation", "Decision Support", Task)) |> 
  mutate(across(c(Cases, Features), sapply, FUN = format, big.mark = ",")) |>
  mutate(across(c(Cases, Features), sapply, FUN = paste0, collapse = "; ")) |>
  # mutate(across(c(Cases, Features), str_split, pattern = "; ")) |> 
  # mutate(Dimensions = purrr::map2_chr(
  #   Cases, Features,
  #   ~ str_c(.x, .y, sep = " × ", collapse = "; ")
  # )) |> 
  mutate(across(
    everything(),
    # ~ stringr::str_replace_all(., "<U+001B>", "")
    textclean::replace_non_ascii
  )) ->
  tab_synthesis
# write to file, to be read into document
write_rds(tab_synthesis, file = here::here("tab/synthesis.rds"))

# mosaic plot of task, aim, and source
tab_synthesis %>%
  mutate(Source = factor(
    Source,
    c("Clinical", "Laboratory", "Imaging",
      "Clinical, Laboratory", "Clinical, Imaging", "Patient-reported")
  )) %>%
  mutate(Task = factor(
    Task,
    c("Prediction", "Clustering", "Diagnosis", "Prognosis", "Decision Support")
  )) %>%
  mutate(Aim = factor(Aim, c("Knowledge", "Practice"))) %>%
  # mutate(across(c(Source, Task, Aim), fct_infreq)) %>%
  count(Source, Task, Aim) %>%
  ggplot() +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  geom_mosaic(
    aes(x = product(Task, Source), fill = Task, weight = n),
    # aes(x = product(Task, Source), fill = Task, alpha = Aim, weight = n),
    divider = mosaic("v")
  ) +
  # coord_flip() +
  scale_y_productlist("Source", labels = ggmosaic:::product_labels()) +
  scale_fill_brewer(type = "qual", na.value = "#000000") +
  scale_alpha_manual(values = c(Knowledge = .5, Practice = 1))

# Excel file downloaded from Google Sheet:
# https://docs.google.com/spreadsheets/d/
# 1xvDJwiLBoI2oz8fxHJ5MjNmiju_RAlK7RJv-wXe1DAs/
# here::here("data/IMSR composite techniques.xlsx") %>%
#   readxl::read_xlsx() %>%
googlesheets4::read_sheet(
  ss = "1xvDJwiLBoI2oz8fxHJ5MjNmiju_RAlK7RJv-wXe1DAs", sheet = 1L
) |> 
  filter(is.na(Synthesis)) %>%
  select(seq(3L, 5L)) %>%
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
print(year_plot)
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

# 
