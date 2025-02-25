library(tidyverse)

hdi <- gdppc <- read_csv("../data/macro/HDI/human-development-index.csv") %>%
  setNames(c("Entity", "CC", "Year", "HDI")) %>%
  dplyr::select(CC, Year, HDI)
