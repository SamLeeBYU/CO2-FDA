library(patchwork)

source("scripts/EDA/setup.R")

################################################################################
### DATA VISUALIZATION ###
################################################################################

#Functional Boxplots
fda::fbplot(t(W), xlab="Year (from 1970)", ylab="(Metric Tons of) Carbon Emissions per capita")

#Visualizing data

country_names <- c("QAT" = "Qatar", "USA" = "United States", "IND" = "India", 
                   "CHN" = "China", "NGA" = "Nigeria")

country_colors <- c("China" = "#E63946",        # Deep Red (originally Qatar)
                    "United States" = "#1D35AA", # Navy Blue
                    "India" = "#F4A261",        # Warm Orange
                    "Qatar" = "#2A9D8F",        # Teal Green (originally China)
                    "Nigeria" = "#8E44AD")      # Royal Purple

carbon_filtered <- carbon %>%
  filter(CC != "PLW") %>%
  mutate(Country = recode(CC, !!!country_names))  # Replace codes with names

ggplot() +
  geom_line(data = carbon_filtered, 
            aes(x = Year, y = CO2, group = CC), 
            color = "gray70", alpha = 0.5) +
  geom_line(data = carbon_filtered %>% filter(CC %in% names(country_names)), 
            aes(x = Year, y = CO2, group = Country, color = Country), 
            linewidth = 1, alpha=0.8) +
  scale_color_manual(values = country_colors, name = "Country") +  
  labs(
    x = "Year",
    y = expression("Metric Tons of"~CO[2]~"Emissions per Capita")
  )+
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "#FAFAFA"),
    panel.grid.major = element_line(color = "#EAEAEA"),
    panel.grid.minor = element_line(color = "#F5F5F5")
  )

################################################################################
### MEAN ESTIMATION ###
################################################################################

#Cross-sectional mean estimation
mu.hat <- colMeans(W)

#Using penalty chosen by GCV
mu.splines <- ss(x = carbon$Year, y = carbon$CO2,
                 method = "GCV")$y

mu.df <- data.frame(
  Year = as.integer(colnames(W)),
  MuHat = mu.hat,
  MuSplines = mu.splines
)

mu.df %>%
  ggplot() +
    geom_line(data = mu.df, aes(x = Year, y = MuHat), color = "black", linewidth=1)+
    geom_line(data = mu.df, aes(x = Year, y = MuSplines), color = "red", linewidth=1)+
  labs(
    x = "Year",
    y = expression("Metric Tons of"~CO[2]~"Emissions per Capita")
  )+
  # Enhanced theme with a soft background and subtle grid
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "#FAFAFA"), # Soft background
    panel.grid.major = element_line(color = "#EAEAEA"), # Light gray major grid
    panel.grid.minor = element_line(color = "#F5F5F5") # Even lighter minor grid
  )

ggplot() +
  # Plot gray background lines for all countries
  geom_line(data = carbon_filtered, 
            aes(x = Year, y = CO2, group = CC), 
            color = "gray70", alpha = 0.5) +  # Light gray for background countries
  geom_line(data = mu.df, aes(x = Year, y = MuHat), color = "black", linewidth=1)+
  geom_line(data = mu.df, aes(x = Year, y = MuSplines), color = "red", linewidth=1)+
  labs(
    x = "Year",
    y = expression("Metric Tons of"~CO[2]~"Emissions per Capita")
  )+
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "#FAFAFA"),
    panel.grid.major = element_line(color = "#EAEAEA"),
    panel.grid.minor = element_line(color = "#F5F5F5")
  )


################################################################################
### COVARIANCE ESTIMATION ###
################################################################################

#Sandwich smoother
W.tilde = scale(W, center = mu.splines, scale=F)
blt.fit <- fpca.face(Y = W.tilde,
                     argvals = 1970:2023,
                     knots = 10,
                     npc = 10,
                     center = F,
                     var = TRUE)

Khat.tilde <- blt.fit$efunctions %*% diag(blt.fit$evalues) %*%
  t(blt.fit$efunctions)

Covdf <- expand.grid(s = 1970:2023, t = 1970:2023)
Covdf$CS <- c(cov(W))
Covdf$Khat <- c(Khat.tilde)

CovPlot.CS <- ggplot(Covdf, aes(x = s, y = t)) + 
  geom_raster(aes(fill = CS)) + 
  scale_fill_viridis_c(
    values = scales::rescale(c(
      seq(0, 300, 50)
  ))) + 
  xlab('s') + ylab('t') + theme_bw() + 
  ggtitle('Cross-Sectional Covariance')+
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "#FAFAFA"),
    panel.grid.major = element_line(color = "#EAEAEA"),
    panel.grid.minor = element_line(color = "#F5F5F5")
  )

CovPlot.Smooth <- ggplot(Covdf, aes(x = s, y = t)) + 
  geom_raster(aes(fill = Khat)) + 
  scale_fill_viridis_c(
    name = "Covariance",
    values = scales::rescale(c(
      seq(0, 300, 50))))+ 
  xlab('s') + theme_bw() + 
  ggtitle('Covariance w/ Sandwhich Smoother') +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "#FAFAFA"),
    panel.grid.major = element_line(color = "#EAEAEA"),
    panel.grid.minor = element_line(color = "#F5F5F5")
  )

CovPlot.CS + CovPlot.Smooth

#Variance Plot

ggplot(mapping=aes(x = 1970:2023,
                   y = diag(Khat.tilde)))+
  geom_line(linewidth = 2)+
  labs(
    x = "Year",
    y = ""
  )+
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "#FAFAFA"),
    panel.grid.major = element_line(color = "#EAEAEA"),
    panel.grid.minor = element_line(color = "#F5F5F5")
  )

################################################################################
### FPCA ###
################################################################################

pcaX <- prcomp(W)
lambdaX <- pcaX$sdev^2
PhiX <- pcaX$rotation 

#Plot the first 4 estimated principal component functions on the same set of axes, using a different color for each, and include a legend.  What is the fraction of variance explained by these first 4 components?
  
Component <- rep(c("j = 1", "j = 2", "j = 3", "j = 4"),
                 each = length(1970:2023))
ggplot(mapping = aes(x = rep(1970:2023, 4),
                     y = c(PhiX[,1:4]),
                     color = Component)) + 
  geom_line()+
  labs(
    x = "Year (s)",
    y = expression(
      "Principal Component Function " ~ Phi[j](s)),
    color = "Component"
  )+
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "#FAFAFA"),
    panel.grid.major = element_line(color = "#EAEAEA"),
    panel.grid.minor = element_line(color = "#F5F5F5")
  )

sum(lambdaX[1:4])/sum(lambdaX)

#Smoothed FPCA

PhiW = blt.fit$efunctions
ggplot(mapping = aes(x = rep(1970:2023, 4),
                     y = c(PhiW[,1:4]),
                     color = Component)) + 
  geom_line()+
  labs(
    x = "Year (s)",
    y = expression(
      "Principal Component Function " ~ Phi[j](s)),
    color = "Component"
  )+
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "#FAFAFA"),
    panel.grid.major = element_line(color = "#EAEAEA"),
    panel.grid.minor = element_line(color = "#F5F5F5")
  )

sum(blt.fit$evalues[1:4])/sum(blt.fit$evalues)
