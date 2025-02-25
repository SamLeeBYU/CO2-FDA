library(tidyverse)

unemployment <- read_csv("../data/macro/unemployment/API_SL.UEM.TOTL.NE.ZS_DS2_en_csv_v2_76310.csv",
                     skip = 4) %>% pivot_longer(cols=as.character(1960:2023),
                                                values_to = "UE", names_to = "Year") %>%
  mutate(
    Year = as.integer(Year)
  ) %>%
  setNames(c("Country", "CC", "Indicator", "Code", "Blank", "Year", "UE")) %>%
  dplyr::select(CC, Year, UE)
