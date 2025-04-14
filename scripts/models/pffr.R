library(refund)

co2.s <- 1990:2023
s <- 1990:2020
wgi.s <- c(1996, 1998, 2000, 2002:2023)

emissions <- readRDS("data/clean/emissions.rds")

CCs <- emissions$carbon %>% rownames()
countries <- c("USA", "CHN", "IND", "NOR")
countries.index <- which(CCs %in% countries)

#Mean Imputations #############################################
na.test <- function(var){
  nas <- sum(is.na(emissions[[var]]))
  n <- prod(dim(emissions[[var]]))
  return(nas/n)
}

for(v in names(emissions)){
  print(str_c(v, ": ", 100*round(na.test(v), 4), "%"))
}

emissions$energy <- apply(emissions$energy, 2, function(col) {
  col[is.na(col)] <- mean(col, na.rm = TRUE)
  return(col)
})

emissions$hdi <- apply(emissions$hdi, 2, function(col) {
  col[is.na(col)] <- mean(col, na.rm = TRUE)
  return(col)
})

emissions$inflation <- apply(emissions$inflation, 2, function(col) {
  col[is.na(col)] <- mean(col, na.rm = TRUE)
  return(col)
})

emissions$corruption <- apply(emissions$corruption, 2, function(col) {
  col[is.na(col)] <- mean(col, na.rm = TRUE)
  return(col)
})

emissions$Government <- apply(emissions$Government, 2, function(col) {
  col[is.na(col)] <- mean(col, na.rm = TRUE)
  return(col)
})

emissions$Stability <- apply(emissions$Stability, 2, function(col) {
  col[is.na(col)] <- mean(col, na.rm = TRUE)
  return(col)
})

emissions$Regulation <- apply(emissions$Regulation, 2, function(col) {
  col[is.na(col)] <- mean(col, na.rm = TRUE)
  return(col)
})

for(v in names(emissions)){
  print(str_c(v, ": ", 100*round(na.test(v), 4), "%"))
}

##########################################################

plot.fitted <- function(curve, yhat){
  plt <- ggplot(mapping=aes(x=co2.s))+
    geom_line(aes(y=emissions$carbon[curve,], color = "Observed"), linewidth=1)+
    geom_line(aes(y=yhat[curve,], color = "Fitted"), linewidth=1)+
    labs(
      color = "Curve",
      x = "Year",
      y = expression("Log "~CO[2]~" Emissions")
    )+
    scale_color_manual(
      values = c("Fitted" = "#1ca364", "Observed" = "black")
    )+
    ggtitle(str_c("Carbon Emissions for ", CCs[curve]))+
    theme
  return(plt)
}

fr2 <- function(y, y.hat, y.bar){
  n = nrow(y)
  ssr <- sapply(1:n, function(i){
    (y[i,]-y.hat[i,])^2
  }) %>% rowSums()
  sse <- sapply(1:n, function(i){
    (y[i,]-y.bar)^2
  }) %>% rowSums()
  #Returns a length(s) x 1 vector
  return(1-ssr/sse)
}

Y.bar <- colMeans(emissions$carbon)

model.default <- pffr(
  carbon ~ ff(energy, xind = s, yind = co2.s) + 
           ff(gdp, xind = s, yind = co2.s) + 
           ff(hdi, xind = s, yind = co2.s) + 
           ff(inflation, xind = s, yind = co2.s) + 
           ff(corruption, xind = wgi.s, yind = co2.s) + 
           ff(Government, xind = wgi.s, yind = co2.s) +
           ff(Stability, xind = wgi.s, yind = co2.s) + 
           ff(Law, xind = wgi.s, yind = co2.s) + 
           ff(Regulation, xind = wgi.s, yind = co2.s) + 
           ff(Voice, xind = wgi.s, yind = co2.s),
  yind = co2.s,
  data = emissions,
  bs.yindex = list(bs = "ps", k = 4, m = c(2, 2)),
  bs.int = list(bs = "ps", k = 30, m = c(2,2))
)
saveRDS(model.default, "scripts/models/model_default.RDS")

yhat.default <- matrix(model.default$fitted.values, nrow=nrow(emissions$carbon),
               ncol=length(co2.s), byrow = T)
colnames(yhat.default) <- co2.s
rownames(yhat.default) <- rownames(emissions$carbon)

fitted.plots.default <- lapply(countries.index, function(cc){
  plot.fitted(cc, yhat.default)
})

wrap_plots(fitted.plots.default, ncol = 2)

delta <- 20
model <- pffr(
  carbon ~ ff(energy, xind = s, yind = co2.s, limits=function(s, t){
    s >= pmax(1990, t-delta)
  }) + ff(gdp, xind = s, yind = co2.s, limits=function(s, t){
    s >= pmax(1990, t-delta)
  }) + ff(hdi, xind = s, yind = co2.s, limits=function(s, t){
    s >= pmax(1990, t-delta)
  }) + ff(inflation, xind = s, yind = co2.s, limits=function(s, t){
    s >= pmax(1990, t-delta)
  }) + ff(corruption, xind = wgi.s, yind = co2.s, limits=function(s, t){
    s >= pmax(1996, t-delta)
  }) + ff(Government, xind = wgi.s, yind = co2.s, limits=function(s, t){
    s >= pmax(1996, t-delta)
  }) + ff(Stability, xind = wgi.s, yind = co2.s, limits=function(s, t){
    s >= pmax(1996, t-delta)
  }) + ff(Law, xind = wgi.s, yind = co2.s, limits=function(s, t){
    s >= pmax(1996, t-delta)
  }) + ff(Regulation, xind = wgi.s, yind = co2.s, limits=function(s, t){
    s >= pmax(1996, t-delta)
  }) + ff(Voice, xind = wgi.s, yind = co2.s, limits=function(s, t){
    s >= pmax(1996, t-delta)
  }),
  yind = co2.s,
  data = emissions,
  bs.yindex = list(bs = "ps", k = 4, m = c(2, 2)),
  bs.int = list(bs = "ps", k=50, m = c(2,2))
)
saveRDS(model, "scripts/models/model.RDS")

yhat <- matrix(model$fitted.values, nrow=nrow(emissions$carbon),
               ncol=length(co2.s), byrow = T)
colnames(yhat) <- co2.s
rownames(yhat) <- rownames(emissions$carbon)

fitted.plots <- lapply(countries.index, function(cc){
  plot.fitted(cc, yhat)
})

wrap_plots(fitted.plots, ncol = 2)

fr2.default <- fr2(emissions$carbon, yhat.default, Y.bar)
fr2.historical <- fr2(emissions$carbon, yhat, Y.bar)

ggplot(
  mapping=aes(x = co2.s),
) +
  geom_line(aes(y = fr2.default, color="Default"), linewidth=2)+
  geom_line(aes(y = fr2.historical, color="Historical"), linewidth=2)+
  labs(
    x = "Year",
    y = "R-Squared",
    color = "Model"
  )+
  theme

#############################################################################

#Renewable Energy Consumption Coefficient
pffr.coefs <- coef(model)
energy.coef <- pffr.coefs$smterms[[2]]$coef
colnames(energy.coef)[1:2] <- c("s", "t")

energy.coef %>%
  dplyr::filter(s >= pmax(1990, t - delta), s <= t) %>%
  ggplot(aes(x = s, y = t, fill = value)) +
    geom_raster() +
    scale_fill_gradient2(
      low = "#1ca364",
      mid = "#EEFFEE",
      high = "#fcba03",
      midpoint = 0,
      name = expression(hat(beta[1])(s, t))
    )+
    labs(
      x = "s (History)",
      y = "t (Response)",
      fill = expression(hat(beta)(s, t)),
      title = "Estimated Coefficient Surface for Share of Renewable Energy Consumption"
    ) +
    theme
