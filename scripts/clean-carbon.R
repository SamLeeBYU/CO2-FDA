#Author: Sam Lee
#This script loads in the EDGAR carbon emissions data set by country
#and augments the GDP per capita data for years 1990-2023

library(tidyverse)

co2 <- readxl::read_xlsx("../data/energy/emissions/CO2.xlsx",
                         sheet = "fossil_CO2_per_capita_by_countr", 
                         range = "A1:BE209") %>% 
  pivot_longer(cols = as.character(1970:2023),
               names_to = "Year",
               values_to = "CO2") %>%
  setNames(c("Metric", "CC", "Country", "Year", "CO2")) %>%
  mutate(
    Year = as.integer(Year)
  ) %>%
  filter(
    Year >= 1990
  ) %>% dplyr::select(CC, Country, Year, CO2)

co2.gdp <- readxl::read_xlsx("data/emissions/CO2.xlsx",
                             sheet = "fossil_CO2_per_GDP_by_country",
                             range="A1:AK209") %>%
  pivot_longer(cols = as.character(1990:2023),
               names_to = "Year",
               values_to = "CO2_GDP") %>%
  setNames(c("Metric", "CC", "Country", "Year", "CO2_GDP")) %>%
  mutate(
    Year = as.integer(Year)
  ) %>% dplyr::select(
    CC, Country, Year, CO2_GDP
  )

#CO2 per capita = CO_2/GDP x GDP per capita =>
# GDP per capita = CO2 per capita / (CO2 / GDP)
co2 <- co2 %>% left_join(co2.gdp, by=join_by(CC, Country, Year)) %>% mutate(
  GDPPC = CO2/CO2_GDP
) %>% dplyr::select(CC, Country, Year, CO2, GDPPC) %>%
  na.omit()