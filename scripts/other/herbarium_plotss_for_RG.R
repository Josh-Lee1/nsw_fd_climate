library(tidyverse)
library(arrow)

#ala observations
ala_obs <- read_parquet("data/processed/obs_for_climate_calc_2023-08-24.parquet")

herb <- ala_obs %>% 
  filter(basisOfRecord == "PRESERVED_SPECIMEN") %>% 
  mutate(year = as.numeric(substr(eventDate, start = 1, stop = 4),
                           decade = substr(eventDate, start = 1, stop = 3)))

  
ggplot(herb, aes(year)) +
  geom_histogram() +
  theme_bw()

institution_summary <- herb %>% 
  group_by(institutionCode) %>% 
  summarise(n_records = n_distinct(recordID)) %>% 
  left_join(herb) %>% 
  filter(n_records > 30000) %>% 
  group_by(institutionCode, decade) %>% 
  summarise(decaderecords = n_distinct(recordID)) %>% 
  ungroup() %>% 
  mutate(decade = as.numeric(decade), 
         Decade = decade*10) %>% 
  filter(institutionCode != "NSW Dept of Planning, Industry and Environment")

ggplot(institution_summary, aes(Decade, decaderecords, colour = institutionCode)) +
  geom_line(size = 1) +
  ylab("number of records") +
  theme_bw()
