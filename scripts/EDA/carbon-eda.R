library(tidyverse)

emissions <- read_csv("../../data/clean/emissions.csv")

ggplot(emissions, aes(x=Year, y=CO2, group=CC))+
  geom_line(color="gray", alpha=0.5)

# emissions.model <- plm::plm(CO2 ~ GDPPC + Renewable_Share + HDI + Inflation + Interest + UE + Corruption + Government + Stability + Law + Regulation + Voice,
#                       model = "fd",
#                       index = c("CC", "Year"),
#                             data = emissions)
# 
# 
# summary(emissions.model)
