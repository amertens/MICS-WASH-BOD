


source("0-config.R")

d_unadj <- readRDS(here("results/unadjusted_RR.rds"))
d_unadj$W <- "unadjusted"
d_RR_adj <- readRDS(here("results/adjusted_bin.rds"))
d_cont_adj <- readRDS(here("results/adjusted_cont.rds"))
d_cont_adj$W <- "adjusted"
d_tmle_adj <- readRDS(here("results/adjusted_tmle_ests.rds"))



d <- bind_rows(d_unadj, d_RR_adj, d_cont_adj)
d$adjusted <- ifelse(d$W=="unadjusted",0,1)

#TEMP! Drop sparse levels
d <- d %>% filter(n >50)



head(d)

d$binary <- ifelse(d$Y %in% c("ari", "diarrhea", "stunt", "wast"), 1, 0)

dbin <- d %>% filter(binary==1)

RMAest_bin <- dbin %>% group_by(Y, X, adjusted) %>%
  do(poolRR(.)) %>% as.data.frame()
RMAest_bin_FE <- dbin %>% group_by(Y, X, adjusted) %>%
  do(poolRR(., method="FE")) %>% as.data.frame()



dcont <- d %>% filter(binary==0)

RMAest_cont <- dcont %>% group_by(Y, X, adjusted) %>%
  do(pool.cont(.)) %>% as.data.frame()
RMAest_cont_FE <- dcont %>% group_by(Y, X, adjusted) %>%
  do(pool.cont(., method="FE")) %>% as.data.frame()




#Combine pooled and country-level results
ind_df <- d %>%  mutate(est=ifelse(is.na(RR),coef,RR)) %>%
  subset(., select =c(country, Y, X, est,ci.lb, ci.ub, n,N, binary, adjusted))
 

RMAest_cont<-RMAest_cont %>%
  rename(est=ATE, ci.lb=CI1, ci.ub=CI2) %>%
  subset(., select =c(Y, X, est,  ci.lb, ci.ub, adjusted)) %>%
  mutate(country="pooled", binary=0)

RMAest_bin<-RMAest_bin %>%
  rename(est=RR, ci.lb=RR.CI1, ci.ub=RR.CI2)  %>%
  subset(., select =c(Y, X, est,  ci.lb, ci.ub, adjusted)) %>%
  mutate(country="pooled", binary=1)

df <- bind_rows(ind_df, RMAest_cont, RMAest_bin)

head(df)


saveRDS(df, here("results/pooled_POC_results.rds"))


df <- bind_rows(ind_df, RMAest_cont_FE, RMAest_bin_FE)
saveRDS(df, here("results/pooled_POC_results_FE.rds"))




#--------------------------------------------------------------------
# Pool TMLE results
#--------------------------------------------------------------------



d<-d_tmle_adj
d$coef <- d$est
d$adjusted <- ifelse(d$W=="unadjusted",0,1)

head(d)

d$binary <- ifelse(d$Y %in% c("ari", "diarrhea", "stunt", "wast"), 1, 0)

dbin <- d %>% filter(binary==1)

RMAest_bin <- dbin %>% group_by(Y, X, adjusted) %>%
  do(poolRR(.)) %>% as.data.frame()

RMAest_bin


dcont <- d %>% filter(binary==0)

RMAest_cont <- dcont %>% group_by(Y, X, adjusted) %>%
  do(pool.cont(.)) %>% as.data.frame()

RMAest_cont

#Combine pooled and country-level results
ind_df <- d %>%  mutate(est=ifelse(is.na(RR),coef,RR)) %>%
  subset(., select =c(country, Y, X, est,ci.lb, ci.ub, n,N, binary, adjusted))


RMAest_cont<-RMAest_cont %>%
  rename(est=ATE, ci.lb=CI1, ci.ub=CI2) %>%
  subset(., select =c(Y, X, est,  ci.lb, ci.ub, adjusted)) %>%
  mutate(country="pooled", binary=0)

RMAest_bin<-RMAest_bin %>%
  rename(est=RR, ci.lb=RR.CI1, ci.ub=RR.CI2)  %>%
  subset(., select =c(Y, X, est,  ci.lb, ci.ub, adjusted)) %>%
  mutate(country="pooled", binary=1)

df <- bind_rows(ind_df, RMAest_cont, RMAest_bin)
df$analysis <- "tmle"
  

saveRDS(df, here("results/pooled_POC_tmle_results.rds"))
