

source("0-config.R")


#load clean data
d <- readRDS(here("data/compiled_clean_POC_survey.rds"))

d <- d %>% filter(!is.na(mort)) %>% mutate(mort=as.numeric(mort))
table(d$country, d$mort)






d$clust_num <- paste0(d$clust_num, "-",d$HH_num)


res_unadj_bin <- run_MICS_regressions(outcomes = c("mort"), family="modified possion", PAF=F, Wvars=NULL)
res_adj_bin <- run_MICS_regressions(outcomes = c("mort"), family="modified possion", PAF=F, Wvars=Wvars)


res_unadj_bin <- res_unadj_bin %>% mutate(adjusted=0, analysis="mortality")
res_adj_bin <- res_adj_bin %>% mutate(adjusted=1, analysis="mortality")


res_adj <- bind_rows(res_unadj_bin, res_adj_bin)

saveRDS(res_adj, here("results/mort_RR.rds"))




