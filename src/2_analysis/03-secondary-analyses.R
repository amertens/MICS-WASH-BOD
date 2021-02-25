

rm(list=ls())
source("0-config.R")



#load clean data
d <- readRDS(here("data/compiled_clean_MICS_survey.rds"))




#-------------------------------------------------
# Unadjusted analysis
#-------------------------------------------------

res_unadj <- res_unadj_bin <- res_unadj_cont <- NULL
res_unadj_bin <- run_MICS_regressions_secondary(outcomes = c("stunt", "wast","diarrhea","ari"), family="modified possion", PAF=F, Wvars=NULL)


res_unadj <- bind_rows(res_unadj_bin, res_unadj_cont)
saveRDS(res_unadj, here("results/unadjusted_RR_secondary.rds"))




#-------------------------------------------------
# Adjusted analysis
#-------------------------------------------------

res_adj <- res_adj_bin <- res_adj_cont <- NULL

d <- droplevels(d)
res_adj_bin <- run_MICS_regressions_secondary(outcomes = c("stunt", "wast","diarrhea","ari"), family="modified possion", PAF=F, Wvars=Wvars)


res_adj <- bind_rows(res_adj_bin, res_adj_cont)
saveRDS(res_adj, here("results/adjusted_RR_secondary.rds"))

