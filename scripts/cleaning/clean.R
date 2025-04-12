#Load in all cleaned data sets and merge them together
source("scripts/cleaning/clean-carbon.R")
source("scripts/cleaning/clean-gdp.R")
source("scripts/cleaning/clean-renewables.R")
source("scripts/cleaning/clean-hdi.R")
source("scripts/cleaning/clean-inflation.R")
source("scripts/cleaning/clean-interest.R")
source("scripts/cleaning/clean-unemployment.R")
source("scripts/cleaning/clean-wgi.R")

emissions.df <- co2 %>% left_join(renewable_shares, by=join_by(CC, Year)) %>%
  left_join(hdi, by=join_by(CC, Year)) %>%
  left_join(inflation, by=join_by(CC, Year)) %>%
  left_join(interest, by=join_by(CC, Year)) %>%
  left_join(unemployment, by=join_by(CC, Year)) %>%
  left_join(wgi, by=join_by(CC, Year))

head(emissions.df)

write_csv(emissions.df, "data/clean/emissions.csv")

co2.s <- 1990:2023
s <- 1990:2020
wgi.s <- c(1996, 1998, 2000, 2002:2023)

countries_list <- list(
  c(co2[co2$Year %in% co2.s,]$CC), c(hdi[hdi$Year %in% s,]$CC), c(renewable_shares[renewable_shares$Year %in% s,]$CC),
  c(inflation[inflation$Year %in% s,]$CC), c(interest[interest$Year %in% s,]$CC), c(unemployment[unemployment$Year %in% s,]$CC),
  c(wgi[wgi$Year %in% s,]$CC)
)

countries <- Reduce(intersect, countries_list) %>% unique() %>% sort()

#Create Functional Data Matrices
create.W <- function(s, dat, var){
  dat <- dat %>% dplyr::select(CC, Year, var) %>%
    filter(Year %in% s & CC %in% countries) %>%
    pivot_wider(
      names_from = Year,
      values_from = var
    ) %>%
      arrange(CC)
  
  W <- as.matrix(dat[,-1])
  rownames(W) <- countries
  colnames(W) <- s
  return(W)
}

emissions <- list()
emissions$carbon <- create.W(co2.s, co2, "log.CO2")
emissions$energy <- create.W(s, renewable_shares, "Renewable_Share")
emissions$gdp <- create.W(s, co2, "log.GDPPC")
emissions$hdi <- create.W(s, hdi, "HDI")
emissions$inflation <- create.W(s, inflation, "Inflation")
emissions$interest <- create.W(s, interest, "Interest")
emissions$unemployment <- create.W(s, unemployment, "UE")
emissions$corruption <- create.W(wgi.s, wgi, "Corruption")
emissions$Government <- create.W(wgi.s, wgi, "Government")
emissions$Stability <- create.W(wgi.s, wgi, "Stability")
emissions$Law <- create.W(wgi.s, wgi, "Law")
emissions$Regulation <- create.W(wgi.s, wgi, "Regulation")
emissions$Voice <- create.W(wgi.s, wgi, "Voice")

saveRDS(emissions, file="data/clean/emissions.rds")
