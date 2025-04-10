library(tidyverse)

renewable_shares <- read_csv("data/energy/renewables/renewables.csv",
  skip = 4) %>% pivot_longer(cols=as.character(1960:2023),
                             values_to = "Renewable_Share", names_to = "Year") %>%
  mutate(
    Year = as.integer(Year)
  ) %>%
  setNames(c("Country", "CC", "Indicator", "Code", "Blank", "Year", "Renewable_Share")) %>%
  dplyr::select(CC, Year, Renewable_Share) %>%
  filter(!is.na(CC))
