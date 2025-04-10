library(tidyverse)

gdppc <- read_csv("data/macro/GDPPC/API_NY.GDP.PCAP.CD_DS2_en_csv_v2_76317.csv",
                  skip = 4) %>% pivot_longer(cols=as.character(1960:2023),
                                             values_to = "GDPPC", names_to = "Year") %>%
  mutate(
    Year = as.integer(Year),
    GDPPC = GDPPC/1000
  ) %>%
  setNames(c("Country", "CC", "Indicator", "Code", "Blank", "Year", "GDPPC")) %>%
  dplyr::select(CC, Year, GDPPC)
