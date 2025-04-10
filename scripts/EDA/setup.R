library(tidyverse)
library(npreg)
library(refund)

#Theme
library(sysfonts)

font_add("cm", regular="fonts/cmunrm.ttf")
showtext::showtext_auto()

theme <- theme_minimal(base_size = 24, base_family = "cm") +
  theme(
    #axis.text.x = element_text(angle = 75, hjust = 1),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    axis.text = element_text(face = "bold"),
    panel.background = element_rect(fill = "#F7F7F7"),
    panel.grid.major = element_line(color = "#E3E3E3"),
    panel.grid.minor = element_line(color = "#F0F0F0"),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

blue <- "#66a3ff"
orange <- "#ffb366"

emissions <- read_csv("data/clean/emissions.csv")
carbon <- read_csv("data/clean/carbon.csv") %>% 
  filter(Country != "Palau")

carbon.dat <- carbon %>% dplyr::select(Country, Year, log.CO2) %>%
  pivot_wider(
    names_from = Year,
    values_from = log.CO2
  )

W <- as.matrix(carbon.dat[,str_c(1970:2023)])
rownames(W) <- carbon.dat$Country
colnames(W) <- 1970:2023
