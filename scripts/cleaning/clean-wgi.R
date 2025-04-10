library(tidyverse)

wgi <- readxl::read_xlsx("data/macro/WGI/wgidataset.xlsx") %>%
  dplyr::select(code, year, indicator, estimate) %>%
  mutate(
    estimate = parse_number(estimate, na = c("", ".."))
  ) %>%
  pivot_wider(names_from = "indicator",
              values_from = "estimate") %>%
  setNames(c("CC", "Year", 
             "Corruption", "Government", "Stability", "Law", "Regulation", "Voice"))
