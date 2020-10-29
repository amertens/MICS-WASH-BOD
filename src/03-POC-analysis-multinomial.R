


source("0-config.R")


#load clean data
dfull <- readRDS(here("data/compiled_clean_MICS_survey.rds"))

#subset to just POC countries
d <- dfull %>% filter(country %in% c("Bangladesh", "Zimbabwe","PakistanPunjab"))
d <- droplevels(d)




                
d$clust_num <- paste0(d$clust_num, "-",d$HH_num)
# d$clust_num <- 1:nrow(d)                


# res <- mics_multinomial_regression(d=d,
#                 Y ="stunt",
#                 X="EC_risk_H",
#                 W=Wvars,
#                 weight = "ecpopweight_H",
#                 clustid= "clust_num",
#                 family="modified possion", calc_PAF=F, low_risk_level=1)
# res


#-------------------------------------------------
# Unadjusted analysis
#-------------------------------------------------


res_unadj_bin <- run_mics_multinomial_regressions(outcomes = c("stunt", "wast","diarrhea","ari"), family="modified possion", PAF=F, Wvars=NULL)
res_unadj_cont <- run_mics_multinomial_regressions(outcomes = c("haz", "whz"), family="gaussian", PAF=F, Wvars=NULL)



res_unadj <- bind_rows(res_unadj_bin, res_unadj_cont)

saveRDS(res_unadj, here("results/unadjusted_mult_RR.rds"))




#-------------------------------------------------
# Adjusted analysis
#-------------------------------------------------

res_adj <- res_adj_bin <- res_adj_cont <- NULL

d <- droplevels(d)
res_adj_bin <- run_mics_multinomial_regressions(outcomes = c("stunt", "wast","diarrhea","ari"), family="modified possion", PAF=F, Wvars=Wvars)
res_adj_cont <- run_mics_multinomial_regressions(outcomes = c("haz", "whz"), family="gaussian", PAF=F, Wvars=Wvars)



res_adj <- bind_rows(res_adj_bin, res_adj_cont)

saveRDS(res_adj, here("results/adjusted_mult_RR.rds"))

