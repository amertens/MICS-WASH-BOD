

#Use Basic as the reference for sanitation and hygiene

source("0-config.R")


#load clean data
d <- readRDS(here("data/compiled_clean_MICS_survey.rds"))
d <- droplevels(d)

table(d$san_imp_cat)
table(d$wat_imp_cat)

d$san_imp_cat <- fct_collapse(d$san_imp_cat, Basic = c("High coverage", "Basic"))
table(d$san_imp_cat)

d$wat_imp_cat <- fct_collapse(d$wat_imp_cat, Basic = c("Continuous", "Basic"))
table(d$wat_imp_cat)
     


#-------------------------------------------------
# Unadjusted analysis
#-------------------------------------------------


res_unadj_bin <- run_mics_multinomial_regressions_sens(outcomes = c("stunt", "wast","diarrhea","ari"), family="modified possion", PAF=F, Wvars=NULL)
res_unadj_cont <- run_mics_multinomial_regressions_sens(outcomes = c("haz", "whz"), family="gaussian", PAF=F, Wvars=NULL)



res_unadj <- bind_rows(res_unadj_bin, res_unadj_cont)

saveRDS(res_unadj, here("results/unadjusted_mult_RR_sens.rds"))




#-------------------------------------------------
# Adjusted analysis
#-------------------------------------------------

res_adj <- res_adj_bin <- res_adj_cont <- NULL

d <- droplevels(d)
res_adj_bin <- run_mics_multinomial_regressions_sens(outcomes = c("stunt", "wast","diarrhea","ari"), family="modified possion", PAF=F, Wvars=Wvars)
res_adj_cont <- run_mics_multinomial_regressions_sens(outcomes = c("haz", "whz"), family="gaussian", PAF=F, Wvars=Wvars)



res_adj <- bind_rows(res_adj_bin, res_adj_cont)

saveRDS(res_adj, here("results/adjusted_mult_RR_sens.rds"))

