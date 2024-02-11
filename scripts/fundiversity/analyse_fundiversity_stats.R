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
  group_by(site) %>%
  mutate(Difference_changein_Fric = c(NA, diff(FRic_change))) %>%
  mutate(Difference_changein_FEve = c(NA, diff(FEve_change))) %>%
  mutate(Difference_changein_FDis = c(NA, diff(FDis_change))) %>%
  mutate(Difference_changein_Fric_prop = c(NA, diff(FRic_prop_change)))%>%
  mutate(Difference_changein_FEve_prop = c(NA, diff(FEve_prop_change)))%>%
  mutate(Difference_changein_FDis_prop = c(NA, diff(FDis_prop_change)))

par(mfrow = c(1, 3))
ggplot(fd_dfs_fundiv, aes(nbsp_prop_change, Difference_changein_Fric_prop)) +
  geom_point()
ggplot(fd_dfs_fundiv, aes(nbsp_prop_change, Difference_changein_FEve)) +
  geom_point()
ggplot(fd_dfs_fundiv, aes(nbsp_prop_change, Difference_changein_FDis)) +
  geom_point()

rich.aov <- aov(FEve ~  scenario, data = fd_dfs_fundiv)
summary(rich.aov)
par(mfrow = c(1, 2))
hist(rich.aov$residuals)
plot(rich.aov, which = 2)


########

plot_clim <- read.csv("data/processed/bnfs_plots_with_current_future_climate.csv") %>% 
  rename(site = "CensusKey") %>% 
  right_join(fd_dfs_fundiv)

ggplot(plot_clim, aes(tas_historical, FRic_change, colour = scenario)) +
  geom_point() +
  geom_smooth(method = lm)

res <- lm(FEve_change ~ scenario * precip_historical, data = plot_clim)
summary(res)
