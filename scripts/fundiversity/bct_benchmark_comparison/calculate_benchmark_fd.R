library(tidyverse)

#get the fundiversity results for bct and benchmark
bct_fd <- read.csv("data/processed/BCT_fundiversity/fundiv_metrics_bct_mon_sites.csv") %>% select(-c(X))

bench_sp_rich <- read.csv("data/processed/FD/fundiversity/benchmark_sp_richness.csv")
bench_fd <- read.csv("data/processed/FD/fundiversity/fundiv_metrics_benchmarks.csv") %>% select(-c(X)) %>%
  left_join(bench_sp_rich)
#get all of the plot metadata 
#class+bioregion

veg_condition <- read.csv("data/raw/bionet_flora_survey/vegetation-condition-benchmarks-cover-and-richness-raw-data-v1-2.csv")
censuslist <- read.csv("data/raw/bionet_flora_survey/CensusList.csv") %>% 
  rename(CensusID = "Census.DB.ID") %>% 
  right_join(veg_condition, by = "CensusID")

site_info_with_fd <- censuslist %>% 
  select(CensusID, Census.Key, Elevation..m., IBRA.Subregion, IBRA7:Class.IBRA) %>%
  distinct() %>% 
  rename(site = "Census.Key") %>% 
  left_join(bench_fd, by = "site")


# write.csv(site_info_with_fd, "data/processed/FD/fundiversity/fundiv_bench_with_form.csv", row.names = FALSE)

# Get the means for each fd metric for each grouping (class by bioregion). 

fd_benchmarks_grouped_calculated <- site_info_with_fd %>%
  group_by(VegetationFormation) %>% 
  reframe(count = n(),
            mean_fric = mean(FRic, na.rm = TRUE),
            max_fric = max(FRic, na.rm = TRUE),
            min_fric = min(FRic, na.rm = TRUE),
            mean_feve = mean(FEve, na.rm = TRUE),
            max_feve = max(FEve, na.rm = TRUE),
            min_feve = min(FEve, na.rm = TRUE),
            mean_fdis = mean(FDis, na.rm = TRUE),
            max_fdis = max(FDis, na.rm = TRUE),
            min_fdis = min(FDis, na.rm = TRUE),
          mean_sric = mean(sp_richness, na.rm = TRUE),
          max_sric = max(sp_richness, na.rm = TRUE),
          min_sric = min(sp_richness, na.rm = TRUE))

is.na(fd_benchmarks_grouped_calculated)<-sapply(fd_benchmarks_grouped_calculated, is.infinite)
fd_benchmarks_grouped_calculated[is.na(fd_benchmarks_grouped_calculated)]<-NA

#make pointrange plot
fd_benchmarks_grouped_calculated %>% 
  arrange(mean_fric) %>%
  mutate(VegetationFormation=factor(VegetationFormation, levels=VegetationFormation)) %>%  
  ggplot(aes(VegetationFormation, mean_fric)) +
  geom_pointrange(ymin = fd_benchmarks_grouped_calculated$min_fric, ymax = fd_benchmarks_grouped_calculated$max_fric) +
  coord_flip()

#

p <- site_info_with_fd %>%
  group_by(VegetationFormation) %>% 
  summarise(p10 = quantile(FRic, 0.10, na.rm = TRUE),
            p90 = quantile(FRic, 0.90, na.rm = TRUE),
            median = median(FRic, na.rm = TRUE),
            sd = sd(FRic, na.rm = TRUE))

p %>%
  arrange(median) %>%
  mutate(VegetationFormation=factor(VegetationFormation, levels=VegetationFormation)) %>%  
  ggplot(aes(VegetationFormation, median)) +
  geom_pointrange(ymin = (p$p10), ymax = (p$p90)) +
  coord_flip()
