library(tidyverse)

inflation <- read_csv("data/macro/inflation/API_NY.GDP.DEFL.KD.ZG_DS2_en_csv_v2_76059.csv",
                  skip = 4) %>% pivot_longer(cols=as.character(1960:2023),
                                             values_to = "Inflation", names_to = "Year") %>%
  mutate(
    Year = as.integer(Year)
  ) %>%
  setNames(c("Country", "CC", "Indicator", "Code", "Blank", "Year", "Inflation")) %>%
  dplyr::select(CC, Year, Inflation)
