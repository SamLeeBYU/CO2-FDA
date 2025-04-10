library(tidyverse)

interest <- read_csv("data/macro/interest/API_FR.INR.RINR_DS2_en_csv_v2_76154.csv",
                      skip = 4) %>% pivot_longer(cols=as.character(1960:2023),
                                                 values_to = "Interest", names_to = "Year") %>%
  mutate(
    Year = as.integer(Year)
  ) %>%
  setNames(c("Country", "CC", "Indicator", "Code", "Blank", "Year", "Interest")) %>%
  dplyr::select(CC, Year, Interest)
