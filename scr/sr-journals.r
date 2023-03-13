library(tidyverse)
library(readxl)

here::here("data/SR-journals.xlsx") %>%
  read_xlsx() %>%
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
