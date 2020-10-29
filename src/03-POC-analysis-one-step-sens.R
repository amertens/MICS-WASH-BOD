



source("0-config.R")


#load clean data
d <- readRDS(here("data/compiled_clean_POC_survey.rds"))



                
d$clust_num <- paste0(d$country,d$clust_num, "-",d$HH_num)

d$country <- "one-step pooled"




res_adj <- res_adj_bin <- res_adj_cont <- NULL

d <- droplevels(d)
res_adj_bin <- run_MICS_regressions(outcomes = c("stunt", "wast","diarrhea","ari"), family="modified possion", PAF=F, Wvars=Wvars)
res_adj_cont <- run_MICS_regressions(outcomes = c("haz", "whz"), family="gaussian", PAF=F, Wvars=Wvars)






res_adj <- bind_rows(res_adj_bin, res_adj_cont)

saveRDS(res_adj, here("results/adjusted_1step_sens.rds"))



