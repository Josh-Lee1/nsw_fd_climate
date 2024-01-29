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

# Get the means for each fd metric for each grouping (class by bioregion). 

fd_benchmarks_grouped_calculated <- site_info_with_fd %>%
  group_by(Class.IBRA) %>% 
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
  mutate(Class.IBRA=factor(Class.IBRA, levels=Class.IBRA)) %>%  
  ggplot(aes(Class.IBRA, mean_fric)) +
  geom_pointrange(ymin = fd_benchmarks_grouped_calculated$min_fric, ymax = fd_benchmarks_grouped_calculated$max_fric)
