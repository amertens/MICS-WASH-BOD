

rm(list=ls())
source("0-config.R")

d_unadj <- readRDS(here("results/unadjusted_RR.rds")) %>% mutate(analysis="primary", W = "unadjusted")
d_RR_multi_unadj <- readRDS(here("results/unadjusted_mult_RR.rds")) %>% mutate(analysis="primary-multi", W = "unadjusted")
d_RR_adj <- readRDS(here("results/adjusted_bin.rds")) %>% mutate(analysis="primary")
d_RR_multi_adj <- readRDS(here("results/adjusted_mult_RR.rds")) %>% mutate(analysis="primary-multi")
d_cont_adj <- readRDS(here("results/adjusted_cont.rds")) %>% mutate(analysis="primary")
d_tmle_adj <- readRDS(here("results/adjusted_tmle_ests.rds")) %>% mutate(analysis="tmle") %>% rename(coef=est)
d_rural_adj <- readRDS(here("results/adjusted_rural_subgroup.rds")) %>% mutate(analysis="rural", subgroup=str_split(country,"-",simplify = T)[,2], country=str_split(country,"-",simplify = T)[,1])
d_1step_adj <- readRDS(here("results/adjusted_1step_sens.rds")) %>% mutate(analysis="1step")
d_CC_adj <- readRDS(here("results/adjusted_CC_sens.rds")) %>% mutate(analysis="CC")




d <- bind_rows(d_unadj, d_RR_multi_unadj,d_RR_adj, d_RR_multi_adj, d_cont_adj, d_tmle_adj, d_rural_adj, d_1step_adj, d_CC_adj)
d$adjusted <- ifelse(d$W=="unadjusted",0,1)
d$ref[is.na(d$ref)] <- "0"
d$contrast[is.na(d$contrast )] <- "1"
d$subgroup[is.na(d$subgroup )] <- "unstratified"


#TEMP! Drop sparse levels
#d <- d %>% filter(n >50)



head(d)

d$binary <- ifelse(d$Y %in% c("ari", "diarrhea", "stunt", "wast"), 1, 0)

dbin <- d %>% filter(binary==1)

RMAest_bin <- dbin %>% group_by(analysis, Y, X, ref, contrast, adjusted, subgroup) %>%
  do(poolRR(.)) %>% as.data.frame()
RMAest_bin_FE <- dbin %>% group_by(analysis, Y, X, ref, contrast, adjusted, subgroup) %>%
  do(poolRR(., method="FE")) %>% as.data.frame()



dcont <- d %>% filter(binary==0)

RMAest_cont <- dcont %>% group_by(analysis, Y, X, ref, contrast, adjusted, subgroup) %>%
  do(pool.cont(.)) %>% as.data.frame()
RMAest_cont_FE <- dcont %>% group_by(analysis, Y, X, ref, contrast, adjusted, subgroup) %>%
  do(pool.cont(., method="FE")) %>% as.data.frame()




#Combine pooled and country-level results
ind_df <- d %>%  mutate(est=ifelse(is.na(RR),coef,RR)) %>%
  subset(., select =c(analysis, country, Y, X, ref, contrast, est,ci.lb, ci.ub, n,N, binary, adjusted, subgroup))
 

RMAest_cont<-RMAest_cont %>%
  rename(est=ATE, ci.lb=CI1, ci.ub=CI2) %>%
  subset(., select =c(analysis, Y, X,  ref, contrast, est,  ci.lb, ci.ub, adjusted, subgroup)) %>%
  mutate(country="pooled", binary=0)

RMAest_bin<-RMAest_bin %>%
  rename(est=RR, ci.lb=RR.CI1, ci.ub=RR.CI2)  %>%
  subset(., select =c(analysis, Y, X, ref, contrast, est,  ci.lb, ci.ub, adjusted, subgroup)) %>%
  mutate(country="pooled", binary=1)

RMAest_cont_FE<-RMAest_cont_FE %>%
  rename(est=ATE, ci.lb=CI1, ci.ub=CI2) %>%
  subset(., select =c(analysis, Y, X,  ref, contrast, est,  ci.lb, ci.ub, adjusted, subgroup)) %>%
  mutate(country="pooled", binary=0)

RMAest_bin_FE<-RMAest_bin_FE %>%
  rename(est=RR, ci.lb=RR.CI1, ci.ub=RR.CI2)  %>%
  subset(., select =c(analysis, Y, X, ref, contrast, est,  ci.lb, ci.ub, adjusted, subgroup)) %>%
  mutate(country="pooled", binary=1)

df_FE <- bind_rows(RMAest_cont_FE, RMAest_bin_FE) %>% filter(analysis=="primary") %>% mutate(analysis="FE")
df <- bind_rows(ind_df, RMAest_cont, RMAest_bin, df_FE)

head(df)


saveRDS(df, here("results/pooled_POC_results.rds"))


#calc pooled PAF - to add if needed... just report individual PAF?
head(d)

#Save PAF dataset
paf <- d %>% subset(., select=c(country, Y,X, PAF:binary)) %>% filter(!is.na(PAF))
saveRDS(paf, here("results/paf_results.rds"))


#Save just PAF's from significant RR's

paf <- d %>% filter(!is.na(PAF), W!="unadjusted") #, ci.lb>1) %>% subset(., select=c(country, Y,X, PAF:binary)) 
saveRDS(paf, here("results/paf_sig_results.rds"))
