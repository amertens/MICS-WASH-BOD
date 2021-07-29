

rm(list=ls())
source("0-config.R")

d <- readRDS(here("results/SRMA_comp_adjusted_RR.rds"))
head(d)

#combine results
table(d$analysis, is.na(d$adjusted))



RMAest_bin <- d %>% filter(!is.na(coef)) %>%
  group_by( Y, X, ref, contrast) %>%
  do(poolRR(.))

RMAest_bin_FE <- d %>% filter(!is.na(coef)) %>%
  group_by( Y, X, ref, contrast) %>%
  do(poolRR(., method="FE")) %>% as.data.frame()

saveRDS(RMAest_bin, here("results/pooled_SRMA_results.rds"))
saveRDS(RMAest_bin_FE, here("results/pooled_SRMA_results_FE.rds"))



d <- readRDS(here("results/SRMA_comp_unadjusted_RR.rds"))
head(d)


RMAest_bin <- d %>% filter(!is.na(coef)) %>%
  group_by( Y, X, ref, contrast) %>%
  do(poolRR(.))



saveRDS(RMAest_bin, here("results/pooled_SRMA_results_unadj.rds"))

