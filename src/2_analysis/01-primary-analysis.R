


rm(list=ls())
source("0-config.R")



#load clean data
d <- readRDS(here("data/compiled_clean_MICS_survey.rds"))

unique(d$country)
                # Y ="stunt"
                # X="WASH"
                # W=NULL
                # weight = "popweight"
                # clustid = "clust_num"
                # family = "gaussian"
                # calc_PAF=T
                # low_risk_level="Improved"
                # return_model=FALSE

                
#d$clust_num <- paste0(d$clust_num, "-",d$HH_num)

#-------------------------------------------------
# Unadjusted analysis
#-------------------------------------------------


res_unadj_bin <- run_MICS_regressions(outcomes = c("stunt", "wast","diarrhea","ari"), family="modified possion", PAF=F, Wvars=NULL, save.data=T)
saveRDS(res_unadj_bin, here("results/unadjusted_RR_bin.rds"))

res_unadj_cont <- run_MICS_regressions(outcomes = c("haz", "whz"), family="gaussian", PAF=F, Wvars=NULL, save.data=T)
saveRDS(res_unadj_cont, here("results/unadjusted_RR_cont.rds"))



res_unadj <- bind_rows(res_unadj_bin, res_unadj_cont)
saveRDS(res_unadj, here("results/unadjusted_RR.rds"))




#-------------------------------------------------
# Adjusted analysis
#-------------------------------------------------

res_adj <- res_adj_bin <- res_adj_cont <- NULL

d <- droplevels(d)
res_adj_bin <- run_MICS_regressions(outcomes = c("stunt", "wast","diarrhea","ari"), family="modified possion", PAF=F, Wvars=Wvars, save.data=T)
saveRDS(res_adj_bin, here("results/adjusted_RR_bin.rds"))

res_adj_cont <- run_MICS_regressions(outcomes = c("haz", "whz"), family="gaussian", PAF=F, Wvars=Wvars, save.data=T)
saveRDS(res_adj_cont, here("results/adjusted_RR_cont.rds"))





res_adj <- bind_rows(res_adj_bin, res_adj_cont)

saveRDS(res_adj, here("results/adjusted_RR.rds"))

