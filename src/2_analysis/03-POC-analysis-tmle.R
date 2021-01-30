


source("0-config.R")


#load clean data
d <- readRDS(here("data/compiled_clean_MICS_survey.rds"))
d <- droplevels(d)




#Notes:
#update for only diarrhea and haz outcomes
#and only WQ exposures

#-------------------------------------------------
# Adjusted analysis
#-------------------------------------------------

res_adj <- res_adj_bin <- res_adj_cont <- NULL

d <- droplevels(d)
res_adj_bin <- run_mics_tmle(outcomes = c("diarrhea"), family="binomial",  Wvars=Wvars)
saveRDS(res_adj_bin, here("results/adjusted_tmle_ests_bin.rds"))

res_adj_cont <- run_mics_tmle(outcomes = c("haz"), family="gaussian", Wvars=Wvars)
saveRDS(res_adj_cont, here("results/adjusted_tmle_ests_cont.rds"))



res_adj <- bind_rows(res_adj_bin, res_adj_cont)

saveRDS(res_adj, here("results/adjusted_tmle_ests.rds"))

