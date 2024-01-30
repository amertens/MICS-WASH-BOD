

rm(list=ls())
source("0-config.R")



#load clean data
d <- readRDS(here("data/compiled_clean_MICS_survey.rds"))
unique(d$country)

set.seed(12345)
res_adj_bin <- run_MICS_regressions(outcomes = c("stunt","diarrhea"), family="modified possion", PAF=T, Wvars=Wvars)
res_adj_bin <- res_adj_bin %>% mutate(adjusted=1)


saveRDS(res_adj_bin, here("results/adj_PAFs.rds"))

res_paf <- res_adj_bin

saveRDS(res_paf, here("results/prim_PAFs.rds"))

