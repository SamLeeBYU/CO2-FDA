#Load in all cleaned data sets and merge them together
source("clean-carbon.R")
source("clean-gdp.R")
source("clean-renewables.R")
source("clean-hdi.R")
source("clean-inflation.R")
source("clean-interest.R")
source("clean-unemployment.R")
source("clean-wgi.R")

emissions <- co2 %>% left_join(renewable_shares, by=join_by(CC, Year)) %>%
  left_join(hdi, by=join_by(CC, Year)) %>%
  left_join(inflation, by=join_by(CC, Year)) %>%
  left_join(interest, by=join_by(CC, Year)) %>%
  left_join(unemployment, by=join_by(CC, Year)) %>%
  left_join(wgi, by=join_by(CC, Year))

head(emissions)

write_csv(emissions, "../data/clean/emissions.csv")
