library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

source("scripts/EDA/setup.R")

carbon <- read_csv("data/clean/carbon.csv") %>%
  filter(Year == 2020)

world <- ne_countries(scale = "medium", returnclass = "sf")

map_data <- world %>%
  filter(continent != "Antarctica") %>%
  left_join(carbon, by = c("iso_a3" = "CC"))

custom_colors <- scale_fill_gradient2(
  low = "#1ca364",     
  mid = "#EEFFEE",     
  high = "#fcba03",    
  midpoint = 0,        
  name = "",
  na.value = "gray85"
)

ggplot(map_data) +
  geom_sf(aes(fill = log.CO2), size = 1) +
  custom_colors +
  theme_void(base_size = 24, base_family = "cm") +
  theme(
    legend.key.width = unit(0.2, "cm"),
    legend.title.align = 0.5,           
    legend.text = element_text(size = 24),
    legend.title = element_text(size = 24)
  )+
  labs(
    caption = "Source: EDGAR"
  )
