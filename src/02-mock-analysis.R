

source("0-config.R")

dfull <- readRDS(here("data/compiled_raw_MICS_survey.rds"))

d <- dfull %>% filter(country="Bangladesh")
