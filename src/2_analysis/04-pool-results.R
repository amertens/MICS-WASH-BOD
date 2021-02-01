

rm(list=ls())
source("0-config.R")

d_unadj <- d_RR_multi_unadj <- d_adj <- d_RR_multi_adj <- d_tmle_adj <- d_rural_adj <- d_mort <- NULL

d_unadj <- readRDS(here("results/unadjusted_RR.rds")) %>% mutate(analysis="primary", W = "unadjusted")
d_RR_multi_unadj <- readRDS(here("results/unadjusted_mult_RR.rds")) %>% mutate(analysis="primary-multi", W = "unadjusted")
d_adj <- readRDS(here("results/adjusted_RR.rds")) %>% mutate(analysis="primary", temp=1)
d_RR_multi_adj <- readRDS(here("results/adjusted_mult_RR.rds")) %>% mutate(analysis="primary-multi")
d_tmle_adj <- readRDS(here("results/adjusted_tmle_ests.rds")) %>% mutate(analysis="tmle") %>% rename(coef=est)
d_rural_adj <- readRDS(here("results/adjusted_rural_subgroup.rds")) %>% mutate(analysis="rural", subgroup=str_split(country,"-",simplify = T)[,2], country=str_split(country,"-",simplify = T)[,1])
d_mort <- readRDS(here("results/mort_RR.rds")) %>% mutate(analysis="mortality") %>% 
  #temp
  filter(adjusted==0)



d <- bind_rows(d_unadj, d_RR_multi_unadj, d_adj, d_RR_multi_adj, d_tmle_adj, d_rural_adj, d_mort)
d$adjusted <- ifelse(d$W=="unadjusted",0,1)
d$ref[is.na(d$ref)] <- "0"
d$contrast[is.na(d$contrast )] <- "1"
d$subgroup[is.na(d$subgroup )] <- "unstratified"

table(d$Y, d$analysis, d$adjusted)



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

df_FE <- bind_rows(RMAest_cont_FE, RMAest_bin_FE) %>% filter(analysis=="primary") %>% mutate(analysis="FE")
df <- bind_rows(ind_df, RMAest_cont, RMAest_bin, df_FE)

head(df)






#Clean data for figures
df <- df %>% 
  mutate(
    multinomial = ifelse(X %in% c("EC_risk_H", "EC_risk_S", "wat_imp_cat", "san_imp_cat", "hyg_imp_cat"),1,0),
    Y=case_when(
      Y=="stunt" ~ "Stunting",
      Y=="wast" ~ "Wasting",
      Y=="diarrhea" ~ "Diarrhea",
      Y=="ari" ~ "ARI",
      Y=="haz" ~ "HAZ",
      Y=="whz" ~ "WHZ",
      Y=="mort" ~ "Mortality"
    ),
    Y=factor(Y, levels=c("Diarrhea", "Stunting", "Wasting", "ARI", "HAZ", "WHZ","Mortality")),
    country=case_when(
      country=="pooled" ~ "Pooled",
      country=="PakistanPunjab" ~ "Pakistan",
      country==country ~ country
    ),
    #country=factor(country, levels=rev(c("Bangladesh", "Pakistan", "Zimbabwe", "Pooled"))),
    Xlab = case_when(X=="EC_H" ~ "Uncontaminated\nHH water", 
                  X=="EC_S" ~ "Uncontaminated\nsource water", 
                  X=="san_imp" ~ "Improved\nsanitation", 
                  X=="wat_imp" ~ "Improved\nwater supply", 
                  X=="hyg_imp" ~ "Improved\nhygiene", 
                  X=="WASH" ~ "Improved WASH,\nno contamination",
                  X=="WASH_noEC" ~ "Improved\nWASH",
                  X=="safely_manH20" ~ "Safely managed\ndrinking water",
                  X=="EC_risk_H" ~ "HH water\ncontamination", 
                  X=="EC_risk_S" ~ "Source water\ncontamination", 
                  X=="san_imp_cat" ~ "Sanitation\ncategory", 
                  X=="wat_imp_cat" ~ "Water supply\ncategory", 
                  X=="hyg_imp_cat" ~ "Hygiene\ncategory"),
    Xlab=factor(X, levels = c(
      "Improved\nwater supply", 
      "Improved\nsanitation", 
      "Improved\nhygiene", 
      "Improved\nWASH",
      "Uncontaminated\nHH water", 
      "Uncontaminated\nsource water", 
      "Safely managed\ndrinking water",
      "Improved WASH,\nno contamination",
      "HH water\ncontamination", 
      "Source water\ncontamination", 
      "Sanitation\ncategory", 
      "Water supply\ncategory", 
      "Hygiene\ncategory")),
    contrast = case_when(
      contrast=="1" ~ "Unimproved",
      contrast=="2" ~ "Moderate risk",
      contrast=="3" ~ "High risk", 
      contrast=="4" ~ "Very high risk", 
      contrast==contrast ~ contrast 
    ),
    contrast=factor(contrast, levels=rev(c("Moderate risk", "High risk",  "Very high risk",   "Basic", "Limited",  "No facility", "None",  "Unimproved", "Surface water"))),
    ref = case_when(
      ref=="0" ~ "Improved",
      ref=="1" ~ "Low risk",
      ref==ref ~ ref 
    ),
    exposure_type = ifelse(X %in% c("EC_H","EC_S","WASH", "safely_manH20", "EC_risk_H", "EC_risk_S"),
      "WQ","HH"
    ))

table(d$X)
table(d$Y)




saveRDS(df, here("results/pooled_results.rds"))

#Get average number of covariates selected
dfW <- d %>% filter(W!="unadjusted")
summary(str_count(dfW$W, pattern = ", ") + 1)
table(str_count(dfW$W, pattern = ", ") + 1)

#calc pooled PAF - to add if needed... just report individual PAF?
head(d)

# #Save PAF dataset
# paf <- d %>% subset(., select=c(country, Y,X, PAF:binary)) %>% filter(!is.na(PAF))
# saveRDS(paf, here("results/paf_results.rds"))
# 
# 
# #Save just PAF's from significant RR's
# 
# paf <- d %>% filter(!is.na(PAF), W!="unadjusted", ci.lb>1) %>% subset(., select=c(country, Y,X, PAF:binary)) 
# saveRDS(paf, here("results/paf_sig_results.rds"))
# 
# 
# #Save just PAF's from RR's > 1
# 
# paf <- d %>% filter(!is.na(PAF), W!="unadjusted", RR>1) %>% subset(., select=c(country, Y,X, PAF:binary)) 
# saveRDS(paf, here("results/paf_pos_results.rds"))
