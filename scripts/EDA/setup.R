library(tidyverse)
library(npreg)
library(refund)

emissions <- read_csv("data/clean/emissions.csv")
carbon <- read_csv("data/clean/carbon.csv") %>% 
  filter(Country != "Palau")

carbon.dat <- carbon %>% dplyr::select(Country, Year, CO2) %>%
  pivot_wider(
    names_from = Year,
    values_from = CO2
  )

W <- as.matrix(carbon.dat[,str_c(1970:2023)])
rownames(W) <- carbon.dat$Country
colnames(W) <- 1970:2023
