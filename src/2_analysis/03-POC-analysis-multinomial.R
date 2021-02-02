


source("0-config.R")


#load clean data
d <- readRDS(here("data/compiled_clean_MICS_survey.rds"))
d <- droplevels(d)


# Y ="stunt"
# X="san_imp_cat"
# W=Wvars
# weight = "popweight"
# clustid= "clust_num"
# family="modified possion"
# calc_PAF=F
# low_risk_level="High coverage"

# d <- d %>% filter(country=="Bangladesh")
#                 
# 
# res <- mics_regression(d=d,
#                                    Y ="stunt",
#                                    X="san_imp_cat",
#                                    W=Wvars,
#                                    weight = "ecpopweight_H",
#                                    clustid= "clust_num",
#                                    family="modified possion", calc_PAF=F, low_risk_level="High coverage")
# res
# 
# 
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
saveRDS(res_adj_bin, here("results/adjusted_mult_bin.rds"))
res_adj_cont <- run_mics_multinomial_regressions(outcomes = c("haz", "whz"), family="gaussian", PAF=F, Wvars=Wvars)
saveRDS(res_adj_cont, here("results/adjusted_mult_cont.rds"))



res_adj <- bind_rows(res_adj_bin, res_adj_cont)

saveRDS(res_adj, here("results/adjusted_mult_RR.rds"))

