


source("0-config.R")


#load clean data
dfull <- readRDS(here("data/compiled_clean_MICS_survey.rds"))

#subset to just POC countries
d <- dfull %>% filter(country %in% c("Bangladesh", "Zimbabwe","PakistanPunjab"))
d <- droplevels(d)




d$clust_num <- paste0(d$clust_num, "-",d$HH_num)


#-------------------------------------------------
# Unadjusted analysis
#-------------------------------------------------


res_unadj_bin <- run_mics_tmle(outcomes = c("stunt", "wast","diarrhea","ari"), family="binomial", glm=T, Wvars=NULL)
res_unadj_cont <- run_mics_tmle(outcomes = c("haz", "whz"), family="gaussian", glm=T, Wvars=NULL)

res_unadj <- bind_rows(res_unadj_bin, res_unadj_cont)

saveRDS(res_unadj, here("results/unadjusted_tmle_glm_ests.rds"))



#-------------------------------------------------
# Adjusted analysis
#-------------------------------------------------

res_adj <- res_adj_bin <- res_adj_cont <- NULL

d <- droplevels(d)
res_adj_bin <- run_mics_tmle(outcomes = c("stunt", "wast","diarrhea","ari"), family="binomial", glm=T,  Wvars=Wvars)
res_adj_cont <- run_mics_tmle(outcomes = c("haz", "whz"), family="gaussian", glm=T, Wvars=Wvars)



res_adj <- bind_rows(res_adj_bin, res_adj_cont)

saveRDS(res_adj, here("results/adjusted_tmle_glm_ests.rds"))

