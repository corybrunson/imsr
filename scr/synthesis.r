library(tidyverse)
library(ggmosaic)

googlesheets4::gs4_auth()

#' Read data.

# journals of references in previous reviews
sr_journals <- readxl::read_xlsx(here::here("data/SR-journals.xlsx"))

# scrape and format table summarizing studies included in synthesis
# Excel file downloaded from Google Sheet:
# https://docs.google.com/spreadsheets/d/
# 1tpWMhYH2pyRT55K7n2J2XFs-kEV_JTuCDmXzJ4BBgNo/
properties_inclusion <- googlesheets4::read_sheet(
  ss = "1tpWMhYH2pyRT55K7n2J2XFs-kEV_JTuCDmXzJ4BBgNo", sheet = 1L
)

# Excel file downloaded from Google Sheet:
# https://docs.google.com/spreadsheets/d/
# 1xvDJwiLBoI2oz8fxHJ5MjNmiju_RAlK7RJv-wXe1DAs/
# here::here("data/IMSR composite techniques.xlsx") %>%
#   readxl::read_xlsx() %>%
composite_techniques <- googlesheets4::read_sheet(
  ss = "1xvDJwiLBoI2oz8fxHJ5MjNmiju_RAlK7RJv-wXe1DAs", sheet = 1L
)

# methodological specifications within general framework
framework_specializations <- googlesheets4::read_sheet(
  ss = "1xvDJwiLBoI2oz8fxHJ5MjNmiju_RAlK7RJv-wXe1DAs", sheet = 2L
)

# add citation numbers from most recent LaTeX compilation
# here::here("docs/Manuscript/Manuscript.tex") %>%
here::here("docs/Localized-Modeling/Localized-Modeling.tex") %>%
  read_lines() %>%
  enframe(name = NULL, value = "line") %>%
  filter(str_detect(line, "\\{ref-")) %>%
  transmute(
    number = row_number(),
    ref = str_replace(line, "^.*\\{ref-([A-Za-z]+[0-9]{4}[a-z]*)\\}.*$", "\\1")
  ) %>%
  print() -> citation_numbers

#' Prepare tables.

# synthesis table
properties_inclusion %>%
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
    Cases = number_of_cases_incidents,
    Features = number_of_predictors_features
  ) %>%
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
  )) %>%
  print() -> tab_synthesis
# write to file, to be read into document
write_rds(tab_synthesis, file = here::here("data/synthesis.rds"))

# methodological table
composite_techniques %>% 
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
  ) %>% 
  mutate(Elements = str_replace_all(Elements, ",", ";")) %>% 
  left_join(select(tab_synthesis, Citation, Date), by = "Citation") %>% 
  arrange(Date) %>% 
  print() -> composite_studies
# write to file, to be read into document
write_rds(composite_studies, file = here::here("data/composite.rds"))

# save ID column for studies described by multiple citations
composite_studies %>% 
  transmute(Citation = str_split(Citation, "; "), ID = row_number()) %>% 
  unnest(Citation) %>% 
  print(n = Inf) -> citation_id
write_rds(citation_id, here::here("data/citations.rds"))

# table of evaluations and comparisons
eval_comp_incl <- c(
  "Wyns2004", "Song2006", "Elter2007", "Xu2008", "Kasabov2010",
  "Liang2015", "CampilloGimenez2013", "Malykh2018", "Zhang2018",
  "Nicolas2014", "Ma2020",# "Tang2021",
  "Park2006", "Lowsky2013", "Ng2015", "Lee2015", "Lee2017", "Wang2019",
  "Doborjeh2022", "Liu2022"
)
properties_inclusion %>%
  rename_with(snakecase::to_snake_case) %>%
  filter(zotero_key %in% eval_comp_incl) %>%
  transmute(
    zotero_key, date_of_publication,
    proposal = proposed_methods,
    cases = number_of_cases_incidents,
    features = number_of_predictors_features,
    evaluation = evaluation_comparison_approach,
    measure = performance_measure,
    performance = performance_values
  ) %>%
  # break `cases`, `features`, and `performance` together
  mutate(across(c(cases, features), \(l) str_split(l, "; "))) %>%
  # break `performance` into nested data with task, method, value
  mutate(performance = str_split(performance, "\n")) %>% 
  unnest(c(cases, features, performance)) %>%
  separate(col = performance, into = c("task", "results"), sep = ": ") %>%
  mutate(results = str_split(results, "; ")) %>% unnest(results) %>%
  mutate(results = str_remove(results, " \\(.*\\)$")) %>%
  separate(col = results, into = c("method", "performance"), sep = ", ") %>%
  # remove flagged ('original') comparators
  filter(! str_detect(performance, "\\*$")) %>%
  # ensure consistent formatting
  mutate(performance = ifelse(
    measure == "Accuracy" & ! str_detect(performance, "\\%$"),
    str_c(format(as.double(performance) * 100, trim = TRUE), "%"),
    performance
  )) |> 
  # sort by date, citation, and appearance of method
  mutate(order = as.integer(interaction(
    date_of_publication, zotero_key, row_number(), lex.order = TRUE
  ))) %>%
  # put proposals and comparators in separate columns, carriage-returned
  mutate(proposal = str_split(proposal, ", ")) %>%
  mutate(role = map2(
    method, proposal,
    ~ if (.x %in% .y) "proposal" else "comparator"
  )) %>%
  unite(col = "results", method, performance, sep = ": ") %>%
  group_by(across(-c(order, results))) %>%
  summarize(order = min(order), results = list(results)) %>%
  ungroup() %>%
  arrange(order) %>%
  select(-order) %>%
  print() -> tab_eval_comp
# write to file, to be read into document
write_rds(tab_eval_comp, file = here::here("data/eval-comp.rds"))

# table of specifications from general framework to retrieved studies
framework_specializations |> 
  rename(Citation = `Zotero key`) |> 
  mutate(across(
    c(Relevance, Retrieval, Adaptation),
    \(x) str_replace_all(x, "\\) \\(", ",")
  )) |> 
  mutate(across(
    c(Relevance, Retrieval, Adaptation),
    \(x) str_replace_all(x, "\\n", "; ")
  )) |> 
  print() -> framework_specs
# write to file, to be read into document
write_rds(framework_specs, file = here::here("data/framework.rds"))

#' Print summaries.

# journal frequencies of previous reviews' references
sr_journals %>%
  mutate(journal = str_remove(journal, "^[0-9]{4} *")) %>%
  mutate(journal = str_remove(journal, "^[0-9]{1,2}[a-z]{2} *")) %>%
  mutate(journal = str_remove(journal, " *\\([A-Z0-9]+\\)$")) %>%
  mutate(journal = tolower(journal)) %>%
  group_by(journal) %>%
  count() %>%
  drop_na() %>%
  arrange(desc(n)) %>%
  filter(n > 2L) %>%
  print(n = Inf)

# journal frequencies of synthesized studies
properties_inclusion %>% 
  select(journal = Journal) %>% 
  mutate(journal = str_remove(journal, "^[0-9]{4} *")) %>%
  mutate(journal = str_remove(journal, "^[0-9]{1,2}[a-z]{2} *")) %>%
  mutate(journal = str_remove(journal, " *\\([A-Z0-9]+\\)$")) %>%
  mutate(journal = tolower(journal)) %>%
  group_by(journal) %>%
  count() %>%
  drop_na() %>%
  arrange(desc(n)) %>%
  print(n = Inf)

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

#' Prepare plots.

# mosaic plot of task, aim, and source
tab_synthesis %>%
  mutate(Source = factor(
    Source,
    c("Clinical", "Laboratory", "Imaging",
      "Clinical, Laboratory", "Clinical, Imaging", "Clinical, Environmental",
      "Patient-reported")
  )) %>%
  mutate(Task = factor(
    Task,
    c("Diagnosis", "Prognosis", "Forecast", "Recommendation")
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
  # FIXME: remove reference annotations if journal does not enumerate them
  # stat_mosaic_text(
  #   # geom = "label",
  #   aes(x = product(Aim, Task, Source), weight = n,
  #       label = mosaic_refs[match(after_stat(label), mosaic_labs)]),
  #   divider = mosaic("v"), size = 3
  # ) +
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
ggsave(
  here::here("fig/fig-properties.jpg"), properties_mosaic,
  width = 8, height = 4
)

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
  # geom_text(vjust = 1, size = 3, color = "white") +
  theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  labs(x = NULL, y = NULL) ->
  year_plot
print(year_plot)
ggsave(here::here("fig/fig-years.png"), year_plot, width = 8, height = 2)
ggsave(here::here("fig/fig-years.jpg"), year_plot, width = 8, height = 2)

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
mid_newline <- function(s) {
  p_mid <- map2_int(
    str_locate_all(s, ","), str_count(s, ","),
    ~ if (nrow(.x) < 4L) NA_integer_ else .x[ceiling((.y + 1) / 2), 1L]
  )
  map2_chr(
    s, p_mid,
    ~ if (is.na(.y)) .x else
      str_c(str_sub(.x, end = .y), "\n  ", str_sub(.x, start = .y + 1L))
  )
}
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
  mutate(refs = mid_newline(str_c(" ", refs))) %>% 
  ggplot(aes(x = Elements, y = count, label = refs)) +
  theme_bw() + theme(panel.grid.major.y = element_blank()) +
  geom_col() +
  # geom_text(hjust = 0, size = 3) +
  # expand_limits(y = c(NA_real_, 17)) +
  # geom_text(hjust = 1, size = 3, color = "white") +
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
ggsave(here::here("fig/fig-methods.jpg"), method_freq, width = 8, height = 3)

# extract evaluations of proposed methods
tab_eval_comp |> 
  pivot_wider(
    id_cols = c(zotero_key, date_of_publication, measure, task, cases, features),
    names_from = role, values_from = results
  ) |> 
  transmute(
    zotero_key, Date = as_date(date_of_publication),
    Measure = measure, Problem = task, Cases = cases, Features = features,
    proposal
  ) |> 
  arrange(Date) |> 
  unnest(proposal) |> 
  separate_wider_delim(
    proposal, delim = ": ",
    names = c("Proposal", "Performance")
  ) |> 
  mutate(across(
    c(Cases, Features),
    \(s) as.double(str_remove(s, ","))
  )) |> 
  mutate(Performance = ifelse(
    str_detect(Performance, "%$"),
    as.double(str_remove(Performance, "%$")) / 100,
    as.double(Performance)
  )) |> 
  left_join(
    select(tab_synthesis, zotero_key = Citation, Source, Type, Task),
    by = "zotero_key"
  ) ->
  tab_eval_prop
# visualize relationships between data size and performance measures
tab_eval_prop |> 
  add_count(Measure, name = "Count") |> 
  filter(Count > 2) |> 
  ggplot(aes(x = Cases, y = Performance)) +
  theme_bw() +
  facet_wrap(vars(Measure), scales = "free") +
  geom_point(aes(color = Task, shape = Source, size = Features^(2/3))) +
  ggrepel::geom_text_repel(
    aes(label = Proposal),
    size = 3, max.overlaps = Inf
  ) +
  scale_size_area(max_size = 10) +
  scale_shape_manual(values = c(16, 15, 17, 5, 6, 3, 4)) +
  guides(
    size = guide_legend(ncol = 3, order = 1),
    shape = guide_legend(order = 2),
    color = guide_legend(ncol = 2, order = 3)
  ) +
  labs(size = "Features") +
  theme(legend.position = "bottom") ->
  performance_determinants
print(performance_determinants)
# for consistent zoom with transpose orientation, set height here to width above
ggsave(
  here::here("fig/fig-performance.png"), performance_determinants,
  width = 13, height = 8
)
ggsave(
  here::here("fig/fig-performance.jpg"), performance_determinants,
  width = 13, height = 8
)
