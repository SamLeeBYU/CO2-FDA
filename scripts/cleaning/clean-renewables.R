library(tidyverse)

renewable_shares <- read_csv("../../data/energy/renewables/share-electricity-renewables.csv") %>%
  setNames(c("Entity", "CC", "Year", "Renewable_Share")) %>%
  dplyr::select(CC, Year, Renewable_Share) %>%
  filter(!is.na(CC))
