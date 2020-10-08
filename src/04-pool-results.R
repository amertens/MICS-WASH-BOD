


source("0-config.R")

d <- readRDS(here("results/unadjusted_RR.rds"))

head(d)

dbin <- d %>% filter(Y %in% c("ari", "diarrhea", "stunt", "wast"))

RMAest <- d %>% group_by(Y, X) %>%
  do(poolRR(.)) %>% as.data.frame()

RMAest









