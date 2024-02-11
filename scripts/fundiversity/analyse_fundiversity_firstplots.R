library(tidyverse)
library(patchwork)
library(ggblend)

fd_benchmark <- read.csv("data/processed/FD/fundiversity/fundiv_metrics_benchmarks.csv") %>% 
  select(-X)
fd_random <- read.csv("data/processed/FD/fundiversity/fundiv_metrics_random.csv") %>% 
  select(-X) %>% 
  mutate(scenario = "random")
fd_extinction <- read.csv("data/processed/FD/fundiversity/fundiv_metrics_extinction.csv") %>% 
  select(-X) %>% 
  mutate(scenario = "extinction")
rich <- read.csv("data/processed/FD/fundiversity/richness.csv") %>% 
  select(-X)


suffix1 <- "_current"
new_column_names <- paste0(names(fd_benchmark), suffix1)
colnames(fd_benchmark) <- new_column_names


fd_dfs_fundiv <- rbind(fd_extinction, fd_random) %>% 
  left_join(fd_benchmark, by = c("site" = "site_current")) %>% 
  left_join(rich) %>% 
  mutate(nbsp_change = nbsp_current - nbsp) %>% 
  mutate(FRic_change = FRic_current - FRic) %>% 
  mutate(FEve_change = FEve_current - FEve) %>% 
  mutate(FDis_change = FDis_current - FDis) %>% 
  mutate(FDiv_change = FDiv_current - FDiv) %>% 
  mutate(Q_change = Q_current - Q) %>% 
  mutate(nbsp_prop = nbsp/nbsp_current) %>% 
  mutate(FRic_prop_change = 1- (FRic/FRic_current)) %>% 
  mutate(nbsp_prop_change = 1-(nbsp/nbsp_current)) %>% 
  mutate(FDis_prop_change = 1- (FDis/FDis_current)) %>% 
  mutate(FEve_prop_change = 1- (FEve/FEve_current)) %>% 
  mutate(FDiv_prop_change = 1- (FDiv/FDiv_current)) %>% 
  mutate(Q_prop_change = 1- (Q/Q_current)) %>% 
  mutate()


ggplot(fd_dfs_fundiv, aes(nbsp_prop_change, abs(FDis_change), colour = scenario)) +
  geom_point()  +
  geom_smooth(method = "lm", se = TRUE) +
  xlab("% species richness loss") +
  ylab("% functional richness loss") +
  ylim(0, 2.25) +
  theme_bw()

#make a three panel plot
a <- ggplot(fd_dfs_fundiv, aes(nbsp_prop_change, FRic_change, color = scenario)) +
  geom_point(alpha = 0.7, size = 0.2) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.2) +
  xlab("% species richness loss") +
  ylab("% functional richness loss") +
  # ylim(0, 1) +
  theme_bw()+
  theme(legend.title=element_blank()) + 
  ggtitle("a") 
b <- ggplot(fd_dfs_fundiv, aes(nbsp_prop_change, abs(FEve_change), colour = scenario)) +
  geom_point(alpha = 0.7, size = 0.2)  +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.2) +
  xlab("% species richness loss") +
  ylab("change in functional evenness") +
  # ylim(0, 2.25) +
  theme_bw()+
  theme(legend.title=element_blank()) + 
  ggtitle("b") 
c <- ggplot(fd_dfs_fundiv, aes(nbsp_prop_change, abs(FDis_change), colour = scenario)) +
  geom_point(alpha = 0.7, size = 0.2)  +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.2) +
  xlab("% species richness loss") +
  ylab("change in functional dispersion") +
  ylim(0, 2.25) +
  theme_bw()+
  theme(legend.title=element_blank()) + 
  ggtitle("c") 
  
a/b/c


#have a look by veg type or rainfall
#bring back plot info
bnfs_raw <- read.csv("data/raw/bionet_flora_survey/vegetation-condition-benchmarks-cover-and-richness-raw-data-v1-2.csv") %>% 
  mutate(CensusID = as.character(CensusID))
plot_info <- read.csv("data/raw/bionet_flora_survey/CensusList.csv") %>% 
  mutate(CensusID = as.character(Census.DB.ID)) %>% 
  right_join(bnfs_raw, by = "CensusID") %>% 
  select(site = Census.Key,
         Elevation..m.:Bioregion,
         IBRA7:Class.IBRA,
         Rain12) %>% 
  distinct() 

plot_clim <- read.csv("data/processed/bnfs_plots_with_current_future_climate.csv") %>% 
  rename(site = "CensusKey")

fd_with_info <- fd_dfs_fundiv %>% 
  full_join(plot_info, by = "site") %>% 
  left_join(plot_clim)%>% 
  mutate(rain_percentile = cut(precip_historical , 
                               quantile(precip_historical , probs = seq(0, 1, 0.1), na.rm = TRUE), 
                               labels = seq(0, 90, 10), 
                               include.lowest = TRUE))%>% 
  mutate(temp_percentile = cut(tas_historical , 
                               quantile(tas_historical , probs = seq(0, 1, 0.1), na.rm = TRUE), 
                               labels = seq(0, 90, 10), 
                               include.lowest = TRUE))
  

ggplot(fd_with_info, aes(nbsp_prop_change, VegetationFormation, fill = VegetationFormation)) +
  geom_boxplot() +
  theme(legend.position = NA) +
  theme_bw()

####rain and temp

fd_with_info %>% 
  filter(!is.na(precip_historical)) %>% 
  ggplot(aes(nbsp_prop_change, FRic_prop_change, colour = scenario)) +
  geom_point(alpha = 0.5, size = 0.5)  +
  geom_smooth(method = "lm") +
  xlab("% species richness loss") +
  ylab("% functional richness loss") +
  ylim(0,1) + 
  theme_bw() +
  facet_wrap(~rain_percentile)

fd_with_info %>% 
  filter(!is.na(tas_historical)) %>% 
  ggplot(aes(nbsp_prop_change, FRic_prop_change, colour = scenario)) +
  geom_point(alpha = 0.5, size = 0.5)  +
  geom_smooth(method = "lm") +
  xlab("% species richness loss") +
  ylab("% functional richness loss") +
  ylim(0,1) + 
  theme_bw() +
  facet_wrap(~temp_percentile)

#now going to try and put this into one figure

## Split the data frame based on the 'category' column
df_list_rain_percentiles <- split(fd_with_info, fd_with_info$rain_percentile)

## Create a list to store ggplot objects
plot_list_rainperc <- list()

## Loop through each subset and create ggplot
for (i in seq_along(df_list_rain_percentiles)) {
  plot <- ggplot(df_list_rain_percentiles[[i]], aes(nbsp_prop_change, FRic_prop_change, colour = scenario)) +
    geom_point(alpha = 0.8, size = 0.2)   +
    geom_smooth(method = "lm")  +
    ylim(0,1) + 
    xlim(0,1) +
    theme_bw() +
    facet_wrap(~rain_percentile, scales = "fixed") +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "none")
    # Store the ggplot object in the list
  plot_list_rainperc[[i]] <- plot
}

## Combine ggplot objects using patchwork
final_plot_rain <- wrap_plots(plot_list_rainperc, ncol = 2)

#again for tas
## Split the data frame based on the 'category' column
df_list_tas_percentiles <- split(fd_with_info, fd_with_info$temp_percentile)

## Create a list to store ggplot objects
plot_list_tasperc <- list()

## Loop through each subset and create ggplot
for (i in seq_along(df_list_tas_percentiles)) {
  plot <- ggplot(df_list_tas_percentiles[[i]], aes(nbsp_prop_change, FRic_prop_change, colour = scenario)) +
    geom_point(alpha = 0.8, size = 0.2)  +
    geom_smooth(method = "lm")  +
    ylim(0,1) + 
    xlim(0,1) +
    theme_bw() +
    facet_wrap(~temp_percentile, scales = "fixed") +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "none")
  # Store the ggplot object in the list
  plot_list_tasperc[[i]] <- plot
}

## Combine ggplot objects using patchwork
final_plot_tas <- wrap_plots(plot_list_tasperc, ncol = 2)

#combine them 
final_plot_rain | final_plot_tas

#########

ggplot(fd_with_info, aes(nbsp_prop_change, abs(FEve_change), colour = scenario)) +
  geom_point()  +
  geom_smooth(method = "lm") +
  xlab("species richness loss") +
  ylab("functional richness loss") +
  # ylim(0,1) + 
  theme_bw() +
  facet_wrap(~rain_percentile)

richness_aov<- lm(FRic_prop_change ~ nbsp_prop_change * precip_historical, data = fd_with_info)
summary(richness_aov)
hist(richness_aov$residuals)

