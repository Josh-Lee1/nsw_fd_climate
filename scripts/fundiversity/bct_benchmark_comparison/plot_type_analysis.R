library(tidyverse)
library(lme4)


bct <- read.csv("data/processed/BCT_fundiversity/BCT_fundiv_w_form.csv") %>% 
  select(site = GlobalID, 
         FRic:sp_richness, 
         vegetation_formation) %>% 
  mutate(plot_type = "BCT")
bench <- read.csv("data/processed/FD/fundiversity/fundiv_bench_with_form.csv") %>% 
  select(site, 
         FRic:sp_richness, 
         vegetation_formation = "VegetationFormation") %>% 
  mutate(plot_type = "Benchmark")

all <- bind_rows(bct, bench)
all$vegetation_formation <- factor(all$vegetation_formation)
all$FRic_log <- log(all$FRic)
all$FEve_log <- log(all$FEve)
all$FDis_log <- log(all$FDis)


# all.aov.sr <- aov(sp_richness ~ plot_type * vegetation_formation,  data = all)
# summary(all.aov.sr)

#
all.ft <- lmer(sp_richness ~ plot_type + (1 | vegetation_formation), data = all, REML = F)
all.ft.0 <- lmer(sp_richness ~ (1 | vegetation_formation), data = all, REML = F)
anova(all.ft.0, all.ft)
confint(all.ft)

