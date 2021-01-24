


source("0-config.R")


#load clean data
dfull <- readRDS(here("data/compiled_clean_MICS_survey.rds"))

#subset to just POC countries
d <- dfull %>% filter(country %in% c("Bangladesh", "Zimbabwe","PakistanPunjab"))
d <- droplevels(d)







#-------------------------------------------------
# Adjusted analysis
#-------------------------------------------------

res_adj <- res_adj_bin <- res_adj_cont <- NULL

d <- droplevels(d)
res_adj_bin <- run_mics_tmle(outcomes = c("stunt", "wast","diarrhea","ari"), family="binomial",  Wvars=Wvars)
res_adj_cont <- run_mics_tmle(outcomes = c("haz", "whz"), family="gaussian", Wvars=Wvars)



res_adj <- bind_rows(res_adj_bin, res_adj_cont)

saveRDS(res_adj, here("results/adjusted_tmle_ests.rds"))

