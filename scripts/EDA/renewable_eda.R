source("scripts/cleaning/clean-renewables.R")

################################################################################
### DATA VISUALIZATION ###
################################################################################

#Preprocessing

avail.data <- renewable_shares %>% group_by(Year) %>%
  summarize(
    n = n()
  )

ggplot(avail.data, aes(x=Year, y=n))+
  geom_point()+
  labs(
    y = "Number of Curves (Countries)"
  )+
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "#FAFAFA"),
    panel.grid.major = element_line(color = "#EAEAEA"),
    panel.grid.minor = element_line(color = "#F5F5F5")
  )

renewable.dat <- renewable_shares %>% dplyr::select(CC, Year, Renewable_Share) %>%
  pivot_wider(
    names_from = Year,
    values_from = Renewable_Share
  ) %>% filter(CC != "PLW")

W <- as.matrix(renewable.dat[,str_c(1985:2023)])
rownames(W) <- renewable.dat$CC
colnames(W) <- 1985:2023

#Data Visualization
country_names <- c("QAT" = "Qatar", "USA" = "United States", "IND" = "India", 
                   "CHN" = "China", "NGA" = "Nigeria")

country_colors <- c("China" = "#E63946",    
                    "United States" = "#1D35AA",
                    "India" = "#F4A261",       
                    "Qatar" = "#2A9D8F",       
                    "Nigeria" = "#8E44AD")    

renewables_filtered <- renewable_shares %>%
  filter(CC != "PLW") %>%
  mutate(Country = recode(CC, !!!country_names))  # Replace codes with names

ggplot() +
  geom_line(data = renewables_filtered, 
            aes(x = Year, y = Renewable_Share, group = CC), 
            color = "gray70", alpha = 0.5) +
  geom_line(data = renewables_filtered %>% filter(CC %in% names(country_names)), 
            aes(x = Year, y = Renewable_Share, group = Country, color = Country), 
            linewidth = 1, alpha=0.8) +
  scale_color_manual(values = country_colors, name = "Country") +  
  labs(
    x = "Year",
    y = "Renewable Energy Share (%)"
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
mu.hat <- apply(W, 2, function(x){
  mean(x, na.rm=T)
})

#Using penalty chosen by GCV
mu.splines <- ss(x = renewables_filtered$Year, y = renewables_filtered$Renewable_Share,
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
    y = expression("Renewable Energy Share (%)")
  )+
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "#FAFAFA"),
    panel.grid.major = element_line(color = "#EAEAEA"),
    panel.grid.minor = element_line(color = "#F5F5F5")
  )

ggplot() +
  geom_line(data = renewables_filtered, 
            aes(x = Year, y = Renewable_Share, group = CC), 
            color = "gray70", alpha = 0.5) +
  geom_line(data = mu.df, aes(x = Year, y = MuHat), color = "black", linewidth=1)+
  geom_line(data = mu.df, aes(x = Year, y = MuSplines), color = "red", linewidth=1)+
  labs(
    x = "Year",
    y = expression("Renewable Energy Share (%)")
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

W.tilde = scale(W, center = mu.splines, scale=F)

L <- fdapace::MakeFPCAInputs(IDs = renewables_filtered$CC,
                                    renewables_filtered$Year,
                                    renewables_filtered$Renewable_Share)

cov.sparse.gcv <- fdapace::FPCA(L$Ly, L$Lt,
                                optns = list(
                                  'kernel' = 'epan',
                                  'methodBwCov' = 'GCV',
                                  'error' = T,
                                  'dataType' = 'Sparse',
                                  'useBinnedCov' = F
                                ))

Covdf <- expand.grid(s = cov.sparse.gcv$workGrid, t = cov.sparse.gcv$workGrid)
Covdf$Khat <- c(cov.sparse.gcv$smoothedCov)

ggplot(Covdf, aes(x = s, y = t)) + 
  geom_raster(aes(fill = Khat)) + 
  scale_fill_viridis_c(
    name = "Covariance",
    values = scales::rescale(c(
      seq(0, 1000, 50))))+ 
  xlab('s') + theme_bw() + 
  ggtitle('Local Linear Covariance Est. w/ GCV') +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "#FAFAFA"),
    panel.grid.major = element_line(color = "#EAEAEA"),
    panel.grid.minor = element_line(color = "#F5F5F5")
  )

#Variance Plot

ggplot(mapping=aes(x = cov.sparse.gcv$workGrid,
                   y = diag(cov.sparse.gcv$smoothedCov)))+
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

#Smoothed FPCA

PhiW = cov.sparse.gcv$phi
Component <- rep(c("j = 1", "j = 2", "j = 3", "j = 4"),
                 each = length(cov.sparse.gcv$workGrid))
ggplot(mapping = aes(x = rep(cov.sparse.gcv$workGrid, 4),
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