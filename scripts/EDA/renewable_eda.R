library(ggrepel)

source("scripts/cleaning/clean-renewables.R")

################################################################################
### DATA VISUALIZATION ###
################################################################################

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


#Preprocessing

s = 1990:2020

avail.data <- renewable_shares %>% group_by(Year) %>%
  summarize(
    n = mean(!is.na(Renewable_Share))
  )

ggplot(avail.data, aes(x=Year, y=n))+
  geom_point()+
  labs(
    y = "Proportion of Available Renewable Energy Data"
  )+
  theme

renewable.dat <- renewable_shares %>% dplyr::select(CC, Year, Renewable_Share) %>%
  pivot_wider(
    names_from = Year,
    values_from = Renewable_Share
  ) %>% filter(CC != "PLW")

W <- as.matrix(renewable.dat[,str_c(s)])
rownames(W) <- renewable.dat$CC
colnames(W) <- s

#Data Visualization
country_names <- c("QAT" = "Qatar", "USA" = "United States", "IND" = "India", 
                   "CHN" = "China", "NGA" = "Nigeria")

label_data <- renewables_filtered %>%
  filter(CC %in% names(country_names)) %>%
  group_by(Country) %>%
  slice_sample(n = 1)

country_colors <- c("China" = "#E63946",    
                    "United States" = "#1D35AA",
                    "India" = "#F4A261",       
                    "Qatar" = "#2A9D8F",       
                    "Nigeria" = "#8E44AD")    

renewables_filtered <- renewable_shares %>%
  filter(CC != "PLW" & Year %in% s) %>%
  na.omit() %>%
  mutate(Country = recode(CC, !!!country_names))  # Replace codes with names

ggplot() +
  geom_line(data = renewables_filtered, 
            aes(x = Year, y = Renewable_Share, group = CC), 
            color = "gray70", alpha = 0.5) +
  geom_line(data = renewables_filtered %>% filter(CC %in% names(country_names)), 
            aes(x = Year, y = Renewable_Share, group = Country, color = Country), 
            linewidth = 1, alpha = 0.8, show.legend = F) +
  geom_label_repel(data = label_data,
                   aes(x = Year, y = Renewable_Share, label = Country, color = Country),
                   size = 8,
                   direction = "y",
                   segment.color = NA,
                   box.padding = 0.4,
                   show.legend = FALSE) +
  scale_color_manual(values = country_colors) +
  labs(
    x = "Year",
    y = "Renewable Energy Share (%)"
  )+
  theme

################################################################################
### MEAN ESTIMATION ###
################################################################################

#Cross-sectional mean estimation
mu.hat <- apply(W, 2, function(x){
  mean(x, na.rm=T)
})

#Using penalty chosen by GCV
mu.splines <- npreg::ss(x = renewables_filtered$Year, y = renewables_filtered$Renewable_Share,
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
  theme

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
  theme

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
                                  'useBinnedCov' = T,
                                  'dataType' = 'Sparse',
                                  'methodSelectK' = 4
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
  theme

#Variance Plot

ggplot(mapping=aes(x = cov.sparse.gcv$workGrid,
                   y = diag(cov.sparse.gcv$smoothedCov)))+
  geom_line(linewidth = 2)+
  labs(
    x = "Year",
    y = "Variance"
  )+
  theme

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
  geom_line(linewidth=1.2)+
  labs(
    x = "Year (s)",
    y = expression(
      "Principal Component Function " ~ Phi[j](s)),
    color = "Component"
  )+
  theme
