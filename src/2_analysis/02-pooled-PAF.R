

rm(list=ls())
source("0-config.R")



#load clean data
d <- readRDS(here("data/compiled_clean_MICS_survey.rds"))

d$country2 <- factor(d$country)
d$country <- "pooled"
Wvars <- c(Wvars, "country2")

res_adj_bin <- run_MICS_regressions(outcomes = c("stunt","diarrhea"), family="modified possion", PAF=T, Wvars=Wvars)
res_adj_bin <- res_adj_bin %>% mutate(adjusted=1)




saveRDS(res_adj_bin, here("results/pooled_PAFs.rds"))

