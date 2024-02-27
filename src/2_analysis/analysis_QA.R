
rm(list=ls())
source("0-config.R")
library(epiR)


#load clean data
d <- readRDS(here("data/compiled_clean_MICS_survey.rds"))
colnames(d)

table(d$diarrhea)
table(d$POU_filter)

prop.table(table(d$diarrhea, d$POU_filter),2)

tab <- rev(as.vector(table( d$POU_filter, d$diarrhea)))
epiR::epi.2by2(tab)

#2x2 bangladesh
dbd <- d %>% filter(country=='Bangladesh')
tab_bd <- rev(as.vector(table( dbd$POU_filter, dbd$diarrhea)))
epiR::epi.2by2(tab_bd)


res <- d %>% group_by(country) %>%
  do(mics_regression(d=.,
                     Y ="diarrhea",
                     X="POU_filter",
                     W=Wvars,
                     weight = "popweight",
                     clustid= "clust_num",
                     family="modified possion", calc_PAF=FALSE, 
                     low_risk_level=""))
poolRR(res)

res_unadj <- d %>% group_by(country) %>%
  do(mics_regression(d=.,
                     Y ="diarrhea",
                     X="POU_filter",
                     W=NULL,
                     weight = "popweight",
                     clustid= "clust_num",
                     family="modified possion", calc_PAF=FALSE, 
                     low_risk_level=""))
poolRR(res_unadj)


#unadjusted matched 2x2 almost exactly. Adjustment severly attenuated the effect
#unadjusted is still less of an effect than from the SRMA?

#Manually adjusting RR for BD
res$W[2]

res_bd <- glm(diarrhea ~ POU_filter + everbf+currbf+aged+HHwealth_quart+wall+chimney+floor+fuel+cookstove+mage, data=dbd, family='poisson')
coef_bd <- summary(res_bd)
exp(coef_bd$coefficients[2,1])

table(dbd$diarrhea)

res_bd <- glm(diarrhea ~ POU_filter +aged+HHwealth_quart+mage, data=dbd, family='poisson')
coef_bd <- summary(res_bd)
exp(coef_bd$coefficients[2,1])

library(tmle)
library(tmle3)


res_tmle <- mics_tmle(d=dbd,
               Y ='diarrhea',
               X="POU_filter",
               W=Wvars,
               glm=F,
               weight = "popweight",
               clustid= "clust_num",
               family='binomial')
res_tmle
