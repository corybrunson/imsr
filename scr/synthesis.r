library(tidyverse)
library(ggmosaic)

# add citation numbers from most recent LaTeX compilation
here::here("docs/Manuscript/Manuscript.tex") %>%
  read_lines() %>%
  enframe(name = NULL, value = "line") %>%
  filter(str_detect(line, "\\{ref-")) %>%
  transmute(
    number = row_number(),
    ref = str_replace(line, "^.*\\{ref-([A-Za-z]+[0-9]{4}[a-z]*)\\}.*$", "\\1")
  ) %>%
  print() -> citation_numbers

# scrape and format table summarizing studies included in synthesis
# Excel file downloaded from Google Sheet:
# https://docs.google.com/spreadsheets/d/
# 1tpWMhYH2pyRT55K7n2J2XFs-kEV_JTuCDmXzJ4BBgNo/
googlesheets4::read_sheet(
  ss = "1tpWMhYH2pyRT55K7n2J2XFs-kEV_JTuCDmXzJ4BBgNo", sheet = 1L
) %>%
  rename_with(snakecase::to_snake_case) %>%
  filter(is.na(synthesis)) %>%
  arrange(date_of_publication) %>%
  inner_join(citation_numbers, by = c("zotero_key" = "ref")) %>%
  transmute(
    Number = number,
    Citation = zotero_key,
    Date = format(date_of_publication, "%Y %b %d"),
    Task = objective,
    Aim = ifelse(generalizable_knowledge == "Science", "Knowledge", "Practice"),
    Source = type_of_data,
    Type = range_of_data,
    `Cases` = number_of_cases_incidents,
    `Features` = number_of_predictors_features
  ) %>%
  mutate(Task = ifelse(Task == "Recommendation", "Decision Support", Task)) %>% 
  mutate(across(
    c(Cases, Features),
    \(x) sapply(x, FUN = format, big.mark = ",")
  )) %>% 
  mutate(across(
    c(Cases, Features),
    \(x) sapply(x, FUN = paste0, collapse = "; ")
  )) %>%
  # mutate(across(c(Cases, Features), str_split, pattern = "; ")) %>% 
  # mutate(Dimensions = purrr::map2_chr(
  #   Cases, Features,
  #   ~ str_c(.x, .y, sep = " × ", collapse = "; ")
  # )) %>% 
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
  # count(Source, Task, Aim) %>%
  group_by(Source, Task, Aim) %>%
  summarize(
    n = n(),
    Refs = str_c("[", str_c(sort(Number), collapse = ","), "]")
  ) %>%
  ungroup() %>%
  print() ->
  mosaic_data
mosaic_labs <- with(mosaic_data, str_c(Aim, Task, Source, sep = "\n"))
mosaic_refs <- mosaic_data$Refs
mosaic_data %>%
  ggplot() +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  # force vertical axis labels to respect binnings without reference to 'Aim'
  stat_mosaic(
    aes(x = product(Task, Source), fill = Task, weight = n, label = Refs),
    divider = mosaic("v"),
    fill = "transparent"
  ) +
  stat_mosaic(
    aes(x = product(Task, Source), fill = Task, alpha = Aim, weight = n),
    divider = mosaic("v")
  ) +
  stat_mosaic_text(
    # geom = "label",
    aes(x = product(Aim, Task, Source), weight = n,
        label = mosaic_refs[match(after_stat(label), mosaic_labs)]),
    divider = mosaic("v"), size = 3
  ) +
  scale_y_productlist("Source") +
  # scale_y_productlist("Source", labels = ggmosaic:::product_labels()) +
  scale_fill_brewer(type = "qual", na.value = "#000000") +
  scale_alpha_manual(values = c(Knowledge = .5, Practice = 1)) ->
  properties_mosaic
print(properties_mosaic)
ggsave(
  here::here("fig/fig-properties.png"), properties_mosaic,
  width = 8, height = 4
)

# Excel file downloaded from Google Sheet:
# https://docs.google.com/spreadsheets/d/
# 1xvDJwiLBoI2oz8fxHJ5MjNmiju_RAlK7RJv-wXe1DAs/
# here::here("data/IMSR composite techniques.xlsx") %>%
#   readxl::read_xlsx() %>%
googlesheets4::read_sheet(
  ss = "1xvDJwiLBoI2oz8fxHJ5MjNmiju_RAlK7RJv-wXe1DAs", sheet = 1L
) %>% 
  filter(is.na(Synthesis)) %>%
  transmute(
    Reference = Citation,
    # Citation = paste0("@", `Zotero key`),
    Citation = sapply(
      str_split(`Zotero key`, "; "),
      function(s) str_c(s, collapse = "; ")
    ),
    Elements = str_remove_all(`Approach Type(s)`, "\\[|\\]"),
    Terminology = `Term(s)`,
    Outcomes = Outcome
  ) |> 
  mutate(Elements = str_replace_all(Elements, ",", ";")) |> 
  left_join(select(tab_synthesis, Citation, Date), by = "Citation") |> 
  arrange(Date) |> 
  print() -> composite_studies
# write to file, to be read into document
write_rds(composite_studies, file = here::here("tab/composite.rds"))

# years of publication
composite_studies %>%
  # only first of any multiple reports of same study
  # mutate(across(c(Reference, Citation), ~ str_remove(., ";.*$"))) %>%
  # un-nest multiple reports of same study
  mutate(across(c(Reference, Citation), ~ str_split(., "; *"))) %>%
  unnest(c(Reference, Citation)) %>%
  mutate(Year = as.integer(str_remove(Reference, "^.*, "))) %>%
  left_join(citation_numbers, by = c("Citation" = "ref")) %>%
  # select(Citation, Year) %>% print(n = Inf)
  group_by(Year) %>%
  summarize(
    Count = n(),
    Refs = str_c("[", str_c(sort(number), collapse = ",\n"), "]")
  ) %>%
  ggplot(aes(x = Year, y = Count, label = Refs)) +
  geom_col() +
  geom_text(vjust = 1, size = 3, color = "white") +
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
  # un-nest multiple reports of same study
  mutate(across(c(Reference, Citation), ~ str_split(., "; *"))) %>%
  unnest(c(Reference, Citation)) %>%
  mutate(across(c(Elements, Terminology), ~ str_split(., "(;|,) *"))) %>%
  select(Citation, Reference, Elements) %>%
  mutate(across(c(Reference, Citation), ~ str_remove(., ";.*$"))) %>%
  left_join(citation_numbers, by = c("Citation" = "ref")) %>%
  select(-Citation) %>%
  unnest(Elements) %>%
  print() -> composite_approaches
# approaches to ignore (check whether each study has one unignored)
drop_types <- c(
  "NN?", "unclear",
  "interactive implementation",
  "similarity matching", "whole-population weight learning"
)
# frequency table
composite_approaches %>%
  count(Elements) %>%
  arrange(desc(n)) %>%
  filter(! Elements %in% drop_types) %>%
  print(n = Inf)
# frequency plot
composite_approaches %>%
  filter(! Elements %in% c("NN?", "unclear")) %>%
  mutate(year = as.integer(str_remove(Reference, "^.*, "))) %>%
  group_by(Elements) %>%
  summarize(
    count = n(),
    earliest_use = min(year),
    refs = str_c("[", str_c(sort(number), collapse = ","), "]")
  ) %>%
  mutate(Elements = fct_reorder(Elements, earliest_use)) %>%
  ggplot(aes(x = Elements, y = count, label = refs)) +
  theme_bw() + theme(panel.grid.major.y = element_blank()) +
  geom_col() +
  geom_text(hjust = 1, size = 3, color = "white") +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(
    breaks = seq(0L, nrow(composite_approaches), 2L),
    minor_breaks = NULL
  ) +
  coord_flip() +
  labs(x = NULL, y = NULL) ->
  method_freq
print(method_freq)
ggsave(here::here("fig/fig-methods.png"), method_freq, width = 8, height = 3)

# terms
composite_studies %>%
  select(citation = Citation, terms = Terminology) %>%
  unnest(terms) %>%
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
