rm(list=ls())
source("0-config.R")



#load clean data
d <- readRDS(here("data/compiled_clean_MICS_survey.rds"))
colnames(d)

#temp!
# d <- d %>% filter(country %in% c("Bangladesh","Nigeria")) %>% droplevels(.)
# Wvars=NULL


# Improved, not on premise vs.	unimproved (ref)
prop.table(table(d$diarrhea, d$imp_off_prem_V_unimp),2)
levels(d$imp_off_prem_V_unimp)
d$imp_off_prem_V_unimp <- factor(d$imp_off_prem_V_unimp, levels=rev(levels(d$imp_off_prem_V_unimp)))

fullres = NULL
res.imp_off_prem_V_unimp <- d %>% group_by(country) %>%
  do(mics_regression(d=.,
                     Y ="diarrhea",
                     X="imp_off_prem_V_unimp",
                     W=Wvars,
                     weight = "popweight",
                     clustid= "clust_num",
                     family="modified possion", calc_PAF=FALSE, 
                     low_risk_level=""))


# Improved, on premise vs.	unimproved
table(d$imp_on_prem_V_imp_off_prem)
table(d$imp_off_prem_V_unimp)
table(d$imp_on_prem_V_imp_off_prem, d$imp_off_prem_V_unimp)
d <- d %>% mutate(imp_on_prem_V_unimp = case_when(
  imp_on_prem_V_imp_off_prem=="Improved, on premise" ~ "Improved, on premise",
  imp_off_prem_V_unimp=="Unimproved" ~ "Unimproved"),
  imp_on_prem_V_unimp = factor(imp_on_prem_V_unimp, levels=c("Unimproved", "Improved, on premise"))
  )
table(d$imp_on_prem_V_unimp)

res.imp_on_prem_V_unimp <- d %>% group_by(country) %>%
  do(mics_regression(d=.,
                     Y ="diarrhea",
                     X="imp_on_prem_V_unimp",
                     W=Wvars,
                     weight = "popweight",
                     clustid= "clust_num",
                     family="modified possion", calc_PAF=FALSE, 
                     low_risk_level=""))

# Improved, on premise vs. Improved, not on premise (ref)
levels(d$imp_on_prem_V_imp_off_prem)
d$imp_on_prem_V_imp_off_prem <- factor(d$imp_on_prem_V_imp_off_prem, levels=rev(levels(d$imp_on_prem_V_imp_off_prem)))
res.imp_on_prem_V_imp_off_prem <- d %>% group_by(country) %>%
  do(mics_regression(d=.,
                     Y ="diarrhea",
                     X="imp_on_prem_V_imp_off_prem",
                     W=Wvars,
                     weight = "popweight",
                     clustid= "clust_num",
                     family="modified possion", calc_PAF=FALSE, 
                     low_risk_level=""))

# Improved, on premise, higher water quality (POU or source) vs	unimproved (ref) 
table(d$imp_on_prem_HQ_V_imp_on_prem_LQ)
table(d$imp_off_prem_V_unimp)
d <- d %>% mutate(imp_on_prem_HQ_V_unimp = case_when(
  imp_on_prem_HQ_V_imp_on_prem_LQ=="Improved, on premise, uncontaminated" ~ "Improved, on premise, uncontaminated",
  imp_off_prem_V_unimp=="Unimproved" ~ "Unimproved"),
  imp_on_prem_HQ_V_unimp = factor(imp_on_prem_HQ_V_unimp, levels=c("Unimproved", "Improved, on premise, uncontaminated"))
)
table(d$imp_on_prem_HQ_V_unimp)

res.imp_on_prem_HQ_V_unimp <- d %>% group_by(country) %>%
  do(mics_regression(d=.,
                     Y ="diarrhea",
                     X="imp_on_prem_HQ_V_unimp",
                     W=Wvars,
                     weight = "ecpopweight_H",
                     clustid= "clust_num",
                     family="modified possion", calc_PAF=FALSE, 
                     low_risk_level=""))

# Improved, on premise, higher water quality (POU or source) vs	Improved, not on premise
table(d$imp_on_prem_HQ_V_imp_on_prem_LQ)
table(d$imp_off_prem_V_unimp)
d <- d %>% mutate(imp_on_prem_HQ_V_imp_off_prem = case_when(
  imp_on_prem_HQ_V_imp_on_prem_LQ=="Improved, on premise, uncontaminated" ~ "Improved, on premise, uncontaminated",
  imp_off_prem_V_unimp=="Improved, off premise" ~ "Improved, off premise"),
  imp_on_prem_HQ_V_imp_off_prem = factor(imp_on_prem_HQ_V_imp_off_prem, levels=c("Improved, off premise", "Improved, on premise, uncontaminated"))
)
table(d$imp_on_prem_HQ_V_imp_off_prem)

res.imp_on_prem_HQ_V_imp_off_prem <- d %>% group_by(country) %>%
  do(mics_regression(d=.,
                     Y ="diarrhea",
                     X="imp_on_prem_HQ_V_imp_off_prem",
                     W=Wvars,
                     weight = "ecpopweight_H",
                     clustid= "clust_num",
                     family="modified possion", calc_PAF=FALSE, 
                     low_risk_level=""))


# Improved, on premise, higher water quality (POU or source) vs Improved, on premise
levels(d$imp_on_prem_HQ_V_imp_on_prem_LQ)
d$imp_on_prem_HQ_V_imp_on_prem_LQ <- factor(d$imp_on_prem_HQ_V_imp_on_prem_LQ, levels=rev(levels(d$imp_on_prem_HQ_V_imp_on_prem_LQ)))
res.imp_on_prem_HQ_V_imp_on_prem_LQ <- d %>% group_by(country) %>%
  do(mics_regression(d=.,
                     Y ="diarrhea",
                     X="imp_on_prem_HQ_V_imp_on_prem_LQ",
                     W=Wvars,
                     weight = "ecpopweight_H",
                     clustid= "clust_num",
                     family="modified possion", calc_PAF=FALSE, 
                     low_risk_level=""))


# Improved, on premise, continuous supply vs	unimproved 
table(d$imp_on_prem_sufficient_V_imp_on_prem_insufficient)
table(d$imp_off_prem_V_unimp)
d <- d %>% mutate(imp_on_prem_sufficient_V_unimp = case_when(
  imp_on_prem_sufficient_V_imp_on_prem_insufficient=="Improved, on premise, sufficient" ~ "Improved, on premise, sufficient",
  imp_off_prem_V_unimp=="Unimproved" ~ "Unimproved"),
  imp_on_prem_sufficient_V_unimp = factor(imp_on_prem_sufficient_V_unimp, levels=c("Unimproved", "Improved, on premise, sufficient"))
)
table(d$imp_on_prem_sufficient_V_unimp)

res.imp_on_prem_sufficient_V_unimp <- d %>% group_by(country) %>%
  do(mics_regression(d=.,
                     Y ="diarrhea",
                     X="imp_on_prem_sufficient_V_unimp",
                     W=Wvars,
                     weight = "popweight",
                     clustid= "clust_num",
                     family="modified possion", calc_PAF=FALSE, 
                     low_risk_level=""))



# Improved, on premise, continuous supply vs	Improved, not on premise
d <- d %>% mutate(imp_on_prem_sufficient_V_imp_off_prem = case_when(
  imp_on_prem_sufficient_V_imp_on_prem_insufficient=="Improved, on premise, sufficient" ~ "Improved, on premise, sufficient",
  imp_off_prem_V_unimp=="Improved, off premise" ~ "Improved, off premise"),
  imp_on_prem_sufficient_V_imp_off_prem = factor(imp_on_prem_sufficient_V_imp_off_prem, levels=c("Improved, off premise", "Improved, on premise, sufficient"))
)
table(d$imp_on_prem_sufficient_V_imp_off_prem)

res.imp_on_prem_sufficient_V_imp_off_prem <- d %>% group_by(country) %>%
  do(mics_regression(d=.,
                     Y ="diarrhea",
                     X="imp_on_prem_sufficient_V_imp_off_prem",
                     W=Wvars,
                     weight = "popweight",
                     clustid= "clust_num",
                     family="modified possion", calc_PAF=FALSE, 
                     low_risk_level=""))



# Improved, on premise, continuous supply vs	Improved, on premise
levels(d$imp_on_prem_sufficient_V_imp_on_prem_insufficient)
d$imp_on_prem_sufficient_V_imp_on_prem_insufficient <- factor(d$imp_on_prem_sufficient_V_imp_on_prem_insufficient, levels=rev(levels(d$imp_on_prem_sufficient_V_imp_on_prem_insufficient)))

res.imp_on_prem_sufficient_V_imp_on_prem_insufficient <- d %>% group_by(country) %>%
  do(mics_regression(d=.,
                     Y ="diarrhea",
                     X="imp_on_prem_sufficient_V_imp_on_prem_insufficient",
                     W=Wvars,
                     weight = "popweight",
                     clustid= "clust_num",
                     family="modified possion", calc_PAF=FALSE, 
                     low_risk_level=""))



# POU chlorine vs	unimproved 
table(d$POU_chlorine)
table(d$wat_imp)
table(d$POU_chlorine, d$wat_imp)

table(d$imp_on_prem_V_imp_off_prem)
table(d$imp_off_prem_V_unimp)

d <- d %>% mutate(POU_chlorine_V_unimp = case_when(
  POU_chlorine=="Treated" & imp_on_prem_V_imp_off_prem=="Improved, on premise" ~ "Improved, on premise, POU chlorine",
  imp_off_prem_V_unimp=="Unimproved" ~ "Unimproved"),
  POU_chlorine_V_unimp = factor(POU_chlorine_V_unimp, levels=c("Unimproved", "Improved, on premise, POU chlorine"))
)
table(d$POU_chlorine_V_unimp)

res.POU_chlorine_V_unimp <- d %>% group_by(country) %>%
  do(mics_regression(d=.,
                     Y ="diarrhea",
                     X="POU_chlorine_V_unimp",
                     W=Wvars,
                     weight = "popweight",
                     clustid= "clust_num",
                     family="modified possion", calc_PAF=FALSE, 
                     low_risk_level=""))


# POU chlorine vs	Improved, not on premise
d <- d %>% mutate(POU_chlorine_V_imp_off_prem = case_when(
  POU_chlorine=="Treated" & imp_on_prem_V_imp_off_prem=="Improved, on premise" ~ "Improved, on premise, POU chlorine",
  imp_off_prem_V_unimp=="Improved, off premise" ~ "Improved, off premise"),
  POU_chlorine_V_imp_off_prem = factor(POU_chlorine_V_imp_off_prem, levels=c("Improved, off premise", "Improved, on premise, POU chlorine"))
)
table(d$POU_chlorine_V_imp_off_prem)

res.POU_chlorine_V_imp_off_prem <- d %>% group_by(country) %>%
  do(mics_regression(d=.,
                     Y ="diarrhea",
                     X="POU_chlorine_V_imp_off_prem",
                     W=Wvars,
                     weight = "popweight",
                     clustid= "clust_num",
                     family="modified possion", calc_PAF=FALSE, 
                     low_risk_level=""))


# POU solar vs	unimproved 
table(d$POU_solar)
table(d$wat_imp)
table(d$POU_solar, d$wat_imp)

table(d$imp_off_prem_V_unimp)

d <- d %>% mutate(POU_solar_V_unimp = case_when(
  POU_solar=="Treated" & imp_on_prem_V_imp_off_prem=="Improved, on premise" ~ "Improved, on premise, POU solar",
  imp_off_prem_V_unimp=="Unimproved" ~ "Unimproved"),
  POU_solar_V_unimp = factor(POU_solar_V_unimp, levels=c("Unimproved", "Improved, on premise, POU solar"))
)
table(d$POU_solar_V_unimp)

res.POU_solar_V_unimp <- d %>% group_by(country) %>%
  do(mics_regression(d=.,
                     Y ="diarrhea",
                     X="POU_solar_V_unimp",
                     W=Wvars,
                     weight = "popweight",
                     clustid= "clust_num",
                     family="modified possion", calc_PAF=FALSE, 
                     low_risk_level=""))


# POU solar vs	Improved, not on premise
d <- d %>% mutate(POU_solar_V_imp_off_prem = case_when(
  POU_solar=="Treated" & imp_on_prem_V_imp_off_prem=="Improved, on premise" ~ "Improved, on premise, POU solar",
  imp_off_prem_V_unimp=="Improved, off premise" ~ "Improved, off premise"),
  POU_solar_V_imp_off_prem = factor(POU_solar_V_imp_off_prem, levels=c("Improved, off premise", "Improved, on premise, POU solar"))
)
table(d$POU_solar_V_imp_off_prem)

res.POU_solar_V_imp_off_prem <- d %>% group_by(country) %>%
  do(mics_regression(d=.,
                     Y ="diarrhea",
                     X="POU_solar_V_imp_off_prem",
                     W=Wvars,
                     weight = "popweight",
                     clustid= "clust_num",
                     family="modified possion", calc_PAF=FALSE, 
                     low_risk_level=""))



# POU filter vs	unimproved 
table(d$POU_filter)
table(d$wat_imp)
table(d$POU_filter, d$wat_imp)

table(d$imp_off_prem_V_unimp)

d <- d %>% mutate(POU_filter_V_unimp = case_when(
  POU_filter=="Treated" & imp_on_prem_V_imp_off_prem=="Improved, on premise" ~ "Improved, on premise, POU filter",
  imp_off_prem_V_unimp=="Unimproved" ~ "Unimproved"),
  POU_filter_V_unimp = factor(POU_filter_V_unimp, levels=c("Unimproved", "Improved, on premise, POU filter"))
)
table(d$POU_filter_V_unimp)

res.POU_filter_V_unimp <- d %>% group_by(country) %>%
  do(mics_regression(d=.,
                     Y ="diarrhea",
                     X="POU_filter_V_unimp",
                     W=Wvars,
                     weight = "popweight",
                     clustid= "clust_num",
                     family="modified possion", calc_PAF=FALSE, 
                     low_risk_level=""))


# POU filter vs	Improved, not on premise
d <- d %>% mutate(POU_filter_V_imp_off_prem = case_when(
  POU_filter=="Treated" & imp_on_prem_V_imp_off_prem=="Improved, on premise" ~ "Improved, on premise, POU filter",
  imp_off_prem_V_unimp=="Improved, off premise" ~ "Improved, off premise"),
  POU_filter_V_imp_off_prem = factor(POU_filter_V_imp_off_prem, levels=c("Improved, off premise", "Improved, on premise, POU filter"))
)
table(d$POU_filter_V_imp_off_prem)

res.POU_filter_V_imp_off_prem <- d %>% group_by(country) %>%
  do(mics_regression(d=.,
                     Y ="diarrhea",
                     X="POU_filter_V_imp_off_prem",
                     W=Wvars,
                     weight = "popweight",
                     clustid= "clust_num",
                     family="modified possion", calc_PAF=FALSE, 
                     low_risk_level=""))
res.POU_filter_V_imp_off_prem





#Basic sanitation (on-site, excluding sewer)	 vs.	Unimproved sanitation
table(d$Piped_san_cat)

d <- d %>% mutate(nonpiped_V_unimp = case_when(
  Piped_san_cat=="Basic, non-sewer" ~ "Basic, non-sewer",
  Piped_san_cat=="Unimproved" ~ "Unimproved"),
  nonpiped_V_unimp = factor(nonpiped_V_unimp, levels=c("Unimproved", "Basic, non-sewer"))
)
table(d$nonpiped_V_unimp)

res.nonpiped_V_unimp <- d %>% group_by(country) %>%
  do(mics_regression(d=.,
                     Y ="diarrhea",
                     X="nonpiped_V_unimp",
                     W=Wvars,
                     weight = "popweight",
                     clustid= "clust_num",
                     family="modified possion", calc_PAF=FALSE, 
                     low_risk_level=""))

#Basic sanitation connected to sewer  vs.		Unimproved sanitation
d <- d %>% mutate(piped_V_unimp = case_when(
  Piped_san_cat=="Sewered" ~ "Sewered",
  Piped_san_cat=="Unimproved" ~ "Unimproved"),
  piped_V_unimp = factor(piped_V_unimp, levels=c("Unimproved", "Sewered"))
)
table(d$piped_V_unimp)

res.piped_V_unimp <- d %>% group_by(country) %>%
  do(mics_regression(d=.,
                     Y ="diarrhea",
                     X="piped_V_unimp",
                     W=Wvars,
                     weight = "popweight",
                     clustid= "clust_num",
                     family="modified possion", calc_PAF=FALSE, 
                     low_risk_level=""))

#Basic sanitation connected to sewer  vs.		Basic sanitation (on-site, excluding sewer)
d <- d %>% mutate(piped_V_nonpiped = case_when(
  Piped_san_cat=="Basic, non-sewer" ~ "Basic, non-sewer",
  Piped_san_cat=="Sewered" ~ "Sewered"),
  piped_V_nonpiped = factor(piped_V_nonpiped, levels=c("Basic, non-sewer", "Sewered"))
)
table(d$piped_V_nonpiped)

res.piped_V_nonpiped <- d %>% group_by(country) %>%
  do(mics_regression(d=.,
                     Y ="diarrhea",
                     X="piped_V_nonpiped",
                     W=Wvars,
                     weight = "popweight",
                     clustid= "clust_num",
                     family="modified possion", calc_PAF=FALSE, 
                     low_risk_level=""))


#Basic sanitation <75% coverage  vs.		Unimproved sanitation
table(d$san_imp_cat)
d <- d %>% mutate(lowcov_V_unimp = case_when(
  san_imp_cat=="Unimproved" ~ "Unimproved",
  san_imp_cat=="Basic" ~ "Basic"),
  lowcov_V_unimp = factor(lowcov_V_unimp, levels=c("Unimproved", "Basic"))
)
table(d$lowcov_V_unimp)

res.lowcov_V_unimp <- d %>% group_by(country) %>%
  do(mics_regression(d=.,
                     Y ="diarrhea",
                     X="lowcov_V_unimp",
                     W=Wvars,
                     weight = "popweight",
                     clustid= "clust_num",
                     family="modified possion", calc_PAF=FALSE, 
                     low_risk_level=""))

#Basic sanitation >75% coverage  vs.		Unimproved sanitation
table(d$san_imp_cat)
d <- d %>% mutate(highcov_V_unimp = case_when(
  san_imp_cat=="Unimproved" ~ "Unimproved",
  san_imp_cat=="High coverage" ~ "High coverage"),
  highcov_V_unimp = factor(highcov_V_unimp, levels=c("Unimproved", "High coverage"))
)
table(d$highcov_V_unimp)

res.highcov_V_unimp <- d %>% group_by(country) %>%
  do(mics_regression(d=.,
                     Y ="diarrhea",
                     X="highcov_V_unimp",
                     W=Wvars,
                     weight = "popweight",
                     clustid= "clust_num",
                     family="modified possion", calc_PAF=FALSE, 
                     low_risk_level=""))

#Basic sanitation >75% coverage  vs.		Basic sanitation <75% coverage
table(d$san_imp_cat)
d <- d %>% mutate(highcov_V_lowcov = case_when(
  san_imp_cat=="Basic" ~ "Basic",
  san_imp_cat=="High coverage" ~ "High coverage"),
  highcov_V_lowcov = factor(highcov_V_lowcov, levels=c("Basic", "High coverage"))
)
table(d$highcov_V_lowcov)

res.highcov_V_lowcov <- d %>% group_by(country) %>%
  do(mics_regression(d=.,
                     Y ="diarrhea",
                     X="highcov_V_lowcov",
                     W=Wvars,
                     weight = "popweight",
                     clustid= "clust_num",
                     family="modified possion", calc_PAF=FALSE, 
                     low_risk_level=""))

save(list=ls(pattern="res\\."), file=here("results/SRMA_comp_adjusted_RR.Rdata"))



fullres <- do.call("rbind", lapply(ls(pattern="res\\."),get))
saveRDS(fullres, here("results/SRMA_comp_adjusted_RR.rds"))

