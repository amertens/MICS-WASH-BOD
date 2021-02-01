

rm(list=ls())
source("0-config.R")



#load clean data
d <- readRDS(here("data/compiled_clean_MICS_survey.rds"))

res_unadj_bin <- NULL
#res_unadj_bin <- run_MICS_regressions(outcomes = c("stunt","diarrhea"), family="modified possion", PAF=T, Wvars=NULL)
res_adj_bin <- run_MICS_regressions(outcomes = c("stunt","diarrhea"), family="modified possion", PAF=T, Wvars=Wvars)
res_unadj_bin <- res_unadj_bin %>% mutate(adjusted=0)
res_adj_bin <- res_adj_bin %>% mutate(adjusted=1)


saveRDS(res_unadj_bin, here("results/unadj_PAFs.rds"))
saveRDS(res_adj_bin, here("results/adj_PAFs.rds"))

res_paf <- bind_rows(res_adj_bin, res_unadj_bin)

saveRDS(res_paf, here("results/prim_PAFs.rds"))

