

rm(list=ls())
source("0-config.R")

d_unadj <- d_RR_multi_unadj <- d_adj <- d_RR_multi_adj <- d_tmle_adj <- d_rural_adj <- d_mort <- d_unadj_secondary_POU <- d_adj_secondary_POU <-NULL

d_unadj <- readRDS(here("results/unadjusted_RR.rds")) %>% mutate(analysis="primary", W = "unadjusted", adjusted=0)
d_RR_multi_unadj <- readRDS(here("results/unadjusted_mult_RR.rds")) %>% mutate(analysis="primary-multi", W = "unadjusted", adjusted=0)
d_adj <- readRDS(here("results/adjusted_RR.rds")) %>% mutate(analysis="primary", adjusted=1)
d_RR_multi_adj <- readRDS(here("results/adjusted_mult_RR.rds")) %>% mutate(analysis="primary-multi", adjusted=1)
d_tmle_adj <- readRDS(here("results/adjusted_tmle_ests.rds")) %>% mutate(analysis="tmle", adjusted=1) %>% rename(coef=est)
d_rural_adj <- readRDS(here("results/adjusted_rural_subgroup.rds")) %>% mutate(analysis="rural", adjusted=1, subgroup=str_split(country,"-",simplify = T)[,2], country=str_split(country,"-",simplify = T)[,1])
d_mort <- readRDS(here("results/mort_RR.rds")) %>% mutate(analysis="primary") 
d_mort_multi <- readRDS(here("results/mort_mult_RR.rds")) %>% mutate(analysis="primary-multi") 
d_RR_multi_adj_sens <- readRDS(here("results/adjusted_mult_RR_sens.rds")) %>% mutate(analysis="sens-multi", adjusted=1) #Load the sensitivity multinomial analyses using Basic as a reference level

d_unadj_secondary <- readRDS(here("results/unadjusted_RR_secondary.rds")) %>% mutate(analysis="secondary", W = "unadjusted", adjusted=0)
d_adj_secondary <- readRDS(here("results/adjusted_RR_secondary.rds")) %>% mutate(analysis="secondary", adjusted=1)
d_unadj_secondary_POU <- readRDS(here("results/unadjusted_RR_secondary_POU.rds")) %>% mutate(analysis="secondary POU", W = "unadjusted", adjusted=0)
d_adj_secondary_POU <- readRDS(here("results/adjusted_RR_secondary_POU.rds")) %>% mutate(analysis="secondary POU", adjusted=1)

dim(d_adj_secondary)
d_adj_secondary <- d_adj_secondary %>% filter(!is.na(ci.lb)) %>% group_by(country, analysis, Y, X, ref) %>% mutate(Ncats=n()) %>%
  filter(X %in% c("Piped_san_cat", "san_coverage") & Ncats==2 | 
           X %in% c("imp_off_prem_V_unimp","imp_on_prem_V_imp_off_prem",
                    "imp_on_prem_HQ_V_imp_on_prem_LQ","imp_on_prem_sufficient_V_imp_on_prem_insufficient",
                    "imp_on_prem_sufficient_HQ_V_imp_on_prem_insufficient_LQ") & Ncats==1 )
dim(d_adj_secondary)


#d<- d_adj_secondary %>% filter(X %in% c("Piped_san_cat","san_coverage"))

#only include countries with both subgroups
head(d_rural_adj)
dim(d_rural_adj)
d_rural_adj <- d_rural_adj  %>% group_by(country, analysis, Y, X, ref, contrast) %>% 
  mutate(Nsubgroups=n()) %>% filter(Nsubgroups>1) %>% subset(., select = -c(Nsubgroups))
dim(d_rural_adj)

#combine results
d <- bind_rows(d_unadj, d_RR_multi_unadj, d_adj, d_RR_multi_adj, d_tmle_adj, d_rural_adj, 
               d_mort, d_mort_multi,d_RR_multi_adj_sens, d_unadj_secondary, d_adj_secondary, d_unadj_secondary_POU, d_adj_secondary_POU) %>%
  filter(!is.na(coef))
table(d$analysis, is.na(d$adjusted))

d$ref[is.na(d$ref)] <- "0"
d$contrast[is.na(d$contrast )] <- "1"
d$subgroup[is.na(d$subgroup )] <- "unstratified"

table(d$Y, d$analysis, d$adjusted)

#Drop if country is missing reference category
table(d$X, d$ref)
d <- d %>% filter(!(
  ((X=="san_imp_cat2"|X=="san_imp_cat"|X=="wat_imp_cat") & ref=="Basic" & analysis!="sens-multi")  
)
)

#---------------------------------------------
#classify regions
#---------------------------------------------

# East Asia and the Pacific EAP
# Eastern and Southern Africa ESA
# Europe and Central Asia ECA
# Latin America and Caribbean LAC
# Middle East and North Africa MENA
# South Asia SA
# West and Central Africa WCA
unique(d$country)

#Set up vector
EAP <- c("Mongolia", "Tonga",  "Kiribati", "LaoPDR","Samoa")
ECA <- c("Georgia", "Kosovo")
LAC <- c("Suriname","Paraguay", "DominicanRepublic", "Guyana", "Honduras")
MENA <- c("Algeria","Iraq","Tunisia" )
SA <- c("Bangladesh", "Nepal", "PakistanBaluchistan","PakistanPunjab","PakistanSindh")
ESA <- c("Lesotho", "Madagascar",  "Zimbabwe")
WCA <- c("Chad","CAR","CoteIvoire","Congo",  "DRC", "Gambia", "Ghana", "Guinea Bissau", "Nigeria", "Togo","SierraLeone","Sao Tome and Principe")

country_labs<-c(EAP, ECA, LAC, MENA, SA, ESA, WCA)
unique(d$country)[!(unique(d$country) %in% country_labs)]
country_labs[!(country_labs %in% unique(d$country))]

EAP <- EAP[order(EAP)]
ECA <- ECA[order(ECA)]
LAC <- LAC[order(LAC)]
MENA <- MENA[order(MENA)]
SA <- SA[order(SA)]
ESA <- ESA[order(ESA)]
WCA <- WCA[order(WCA)]

d <- d %>%
  mutate(
    country=case_when(
      # country=="PakistanPunjab" ~ "Pakistan",
      # country=="LaoPDR" ~ "Laos",
      # country=="SierraLeone" ~ "Sierra Leone",
      # country=="Sao Tome and Principe" ~ "Sao Tome+Prin.",
      country==country ~ country
    ),
    region = case_when(
      country %in% EAP ~ "EAP",
      country %in% ECA ~ "ECA",
      country %in% LAC ~ "LAC",
      country %in% MENA ~ "MENA",
      country %in% SA ~ "SA",
      country %in% ESA ~ "ESA",
      country %in% WCA ~ "WCA",
      country %in% c("Pooled - FE","Pooled - RE") ~ "Pooled"
    ),
    region=factor(region, levels=rev(c("WCA", "ESA", "LAC", "SA","EAP","MENA","ECA","Pooled"))),
    country=factor(country, levels=rev(c(WCA, ESA,LAC,SA,EAP,MENA,ECA, "Pooled - FE","Pooled - RE")))
  )


#---------------------------------------------
# Pool
#---------------------------------------------

d$binary <- ifelse(d$Y %in% c("ari", "diarrhea", "stunt", "wast", "mort"), 1, 0)

dbin <- d %>% filter(binary==1) %>% mutate(est=RR)

RMAest_bin <- dbin %>% group_by(analysis, Y, X, ref, contrast, adjusted, subgroup) %>%
  do(poolRR(.)) %>% as.data.frame()
RMAest_bin_FE <- dbin %>% group_by(analysis, Y, X, ref, contrast, adjusted, subgroup) %>%
  do(poolRR(., method="FE")) %>% as.data.frame()
RMAest_bin_region <- dbin %>% group_by(region, analysis, Y, X, ref, contrast, adjusted, subgroup) %>%
  do(poolRR(.)) %>% as.data.frame()


dcont <- d %>% filter(binary==0) %>% mutate(est=coef, RR=NA, ci.lb=coef - 1.96*se, ci.ub=coef + 1.96*se )

RMAest_cont <- dcont %>% group_by(analysis, Y, X, ref, contrast, adjusted, subgroup) %>%
  do(pool.cont(.)) %>% as.data.frame()
RMAest_cont_FE <- dcont %>% group_by(analysis, Y, X, ref, contrast, adjusted, subgroup) %>%
  do(pool.cont(., method="FE")) %>% as.data.frame()
RMAest_cont_region <- dcont %>% group_by(region,analysis, Y, X, ref, contrast, adjusted, subgroup) %>%
  do(pool.cont(.)) %>% as.data.frame()



#Combine pooled and country-level results
ind_df <- bind_rows(dbin, dcont) %>%  
  subset(., select =c(analysis, country, region, Y, X, ref, contrast, est,ci.lb, ci.ub, n,N, binary, adjusted, subgroup))
 

RMAest_cont<-RMAest_cont %>%
  rename(est=ATE, ci.lb=CI1, ci.ub=CI2) %>%
  subset(., select =c(analysis, Y, X,  ref, contrast, est,  ci.lb, ci.ub, adjusted, subgroup)) %>%
  mutate(country="pooled", region="pooled", binary=0)

RMAest_bin<-RMAest_bin %>%
  rename(est=RR, ci.lb=RR.CI1, ci.ub=RR.CI2)  %>%
  subset(., select =c(analysis, Y, X, ref, contrast, est,  ci.lb, ci.ub, adjusted, subgroup)) %>%
  mutate(country="pooled", region="pooled",binary=1)

RMAest_cont_FE<-RMAest_cont_FE %>%
  rename(est=ATE, ci.lb=CI1, ci.ub=CI2) %>%
  subset(., select =c(analysis, Y, X,  ref, contrast, est,  ci.lb, ci.ub, adjusted, subgroup)) %>%
  mutate(country="pooled",region="pooled", binary=0)

RMAest_bin_FE<-RMAest_bin_FE %>%
  rename(est=RR, ci.lb=RR.CI1, ci.ub=RR.CI2)  %>%
  subset(., select =c(analysis, Y, X, ref, contrast, est,  ci.lb, ci.ub, adjusted, subgroup)) %>%
  mutate(country="pooled", region="pooled",binary=1)

RMAest_cont_region <- RMAest_cont_region %>%
  rename(est=ATE, ci.lb=CI1, ci.ub=CI2) %>%
  subset(., select =c(analysis, region, Y, X,  ref, contrast, est,  ci.lb, ci.ub, adjusted, subgroup)) %>%
  mutate(country="pooled", binary=0)  %>% filter(analysis=="primary"|analysis=="primary-multi") %>%
  mutate(analysis=case_when(analysis=="primary"~"region", analysis=="primary-multi"~"region-multi"))

RMAest_bin_region <- RMAest_bin_region %>%
  rename(est=RR, ci.lb=RR.CI1, ci.ub=RR.CI2)  %>%
  subset(., select =c(analysis, region, Y, X, ref, contrast, est,  ci.lb, ci.ub, adjusted, subgroup)) %>%
  mutate(country="pooled", binary=1)  %>% filter(analysis=="primary"|analysis=="primary-multi") %>%
  mutate(analysis=case_when(analysis=="primary"~"region", analysis=="primary-multi"~"region-multi"))


df_FE <- bind_rows(RMAest_cont_FE, RMAest_bin_FE) %>% filter(analysis=="primary"|analysis=="primary-multi"|analysis=="secondary") %>% mutate(analysis="FE")
df <- bind_rows(ind_df, RMAest_cont, RMAest_bin, df_FE, RMAest_cont_region, RMAest_bin_region)

head(df)



# #Get average number of covariates selected
# dfW <- df %>% filter(W!="unadjusted")
# summary(str_count(dfW$W, pattern = ", ") + 1)
# table(str_count(dfW$W, pattern = ", ") + 1)


saveRDS(df, here("results/pooled_raw_results.rds"))


