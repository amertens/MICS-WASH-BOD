


source("0-config.R")


#load clean data
#d <- readRDS(here("data/compiled_clean_MICS_mortality.rds"))
d <- readRDS(here("data/compiled_clean_MICS_survey.rds"))

d <- d %>% filter(!is.na(mort)) %>% mutate(mort=as.numeric(mort))
table(d$country, d$mort)


Wvars = c( "educ","mage","sex",   
           "rural",   "nhh", "nchild5",
           "floor", "cookstove", "chimney", "fuel", "roof",
           "wall", "own_animals", "HHwealth_quart", "nroom_sleeping")


                
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

d <- droplevels(d)
res_unadj_bin <- run_mics_multinomial_regressions(outcomes = c("mort"), family="modified possion", PAF=F, Wvars=NULL)
res_adj_bin <- run_mics_multinomial_regressions(outcomes = c("mort"), family="modified possion", PAF=F, Wvars=Wvars)



res_unadj_bin <- res_unadj_bin %>% mutate(adjusted=0, analysis="mortality")
res_adj_bin <- res_adj_bin %>% mutate(adjusted=1, analysis="mortality")
res <- bind_rows(res_unadj_bin, res_adj_bin)
saveRDS(res, here("results/mort_mult_RR.rds"))

