

rm(list=ls())
source("0-config.R")

d_unadj <- d_RR_multi_unadj <- d_adj <- d_RR_multi_adj <- d_tmle_adj <- d_rural_adj <- d_mort <- NULL

d_unadj <- readRDS(here("results/unadjusted_RR.rds")) %>% mutate(analysis="primary", W = "unadjusted")
d_RR_multi_unadj <- readRDS(here("results/unadjusted_mult_RR.rds")) %>% mutate(analysis="primary-multi", W = "unadjusted")
d_adj <- readRDS(here("results/adjusted_RR.rds")) %>% mutate(analysis="primary", temp=1)
d_RR_multi_adj <- readRDS(here("results/adjusted_mult_RR.rds")) %>% mutate(analysis="primary-multi")
d_tmle_adj <- readRDS(here("results/adjusted_tmle_ests.rds")) %>% mutate(analysis="tmle") %>% rename(coef=est)
d_rural_adj <- readRDS(here("results/adjusted_rural_subgroup.rds")) %>% mutate(analysis="rural", subgroup=str_split(country,"-",simplify = T)[,2], country=str_split(country,"-",simplify = T)[,1])
d_mort <- readRDS(here("results/mort_RR.rds")) %>% mutate(analysis="primary") 
d_mort_multi <- readRDS(here("results/mort_mult_RR.rds")) %>% mutate(analysis="primary-multi") 


d <- bind_rows(d_unadj, d_RR_multi_unadj, d_adj, d_RR_multi_adj, d_tmle_adj, d_rural_adj, d_mort, d_mort_multi) %>%
  filter(!is.na(coef))
d$adjusted <- ifelse(d$W=="unadjusted",0,1)
d$ref[is.na(d$ref)] <- "0"
d$contrast[is.na(d$contrast )] <- "1"
d$subgroup[is.na(d$subgroup )] <- "unstratified"

table(d$Y, d$analysis, d$adjusted)

#Drop if country is missing reference category
table(d$X, d$ref)
d <- d %>% filter(!(
  (X=="san_imp_cat" & ref=="Basic")  
)
)


d$binary <- ifelse(d$Y %in% c("ari", "diarrhea", "stunt", "wast", "mort"), 1, 0)

dbin <- d %>% filter(binary==1) %>% mutate(est=RR)

RMAest_bin <- dbin %>% group_by(analysis, Y, X, ref, contrast, adjusted, subgroup) %>%
  do(poolRR(.)) %>% as.data.frame()
RMAest_bin_FE <- dbin %>% group_by(analysis, Y, X, ref, contrast, adjusted, subgroup) %>%
  do(poolRR(., method="FE")) %>% as.data.frame()



dcont <- d %>% filter(binary==0) %>% mutate(est=coef, RR=NA, ci.lb=coef - 1.96*se, ci.ub=coef + 1.96*se )

RMAest_cont <- dcont %>% group_by(analysis, Y, X, ref, contrast, adjusted, subgroup) %>%
  do(pool.cont(.)) %>% as.data.frame()
RMAest_cont_FE <- dcont %>% group_by(analysis, Y, X, ref, contrast, adjusted, subgroup) %>%
  do(pool.cont(., method="FE")) %>% as.data.frame()




#Combine pooled and country-level results
ind_df <- bind_rows(dbin, dcont) %>%  
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

df_FE <- bind_rows(RMAest_cont_FE, RMAest_bin_FE) %>% filter(analysis=="primary"|analysis=="primary-multi") %>% mutate(analysis="FE")
df <- bind_rows(ind_df, RMAest_cont, RMAest_bin, df_FE)

head(df)



# #Get average number of covariates selected
# dfW <- df %>% filter(W!="unadjusted")
# summary(str_count(dfW$W, pattern = ", ") + 1)
# table(str_count(dfW$W, pattern = ", ") + 1)


saveRDS(df, here("results/pooled_raw_results.rds"))


