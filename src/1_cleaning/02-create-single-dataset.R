
rm(list=ls())
setwd(here::here())
source("0-config.R")
gc()
load(here("data/raw_MICS_surveys.rdata"))

d <- bind_rows(lapply(ls(pattern="df_"),get))
gc()
dim(d)
colnames(d)
saveRDS(d, here("data/compiled_raw_MICS_survey.rds"))

table(d$country, d$san_cat_lab)

