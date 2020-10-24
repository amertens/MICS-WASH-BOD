
#Make sure to subset HH measurements to the unique household level... right now they are duplicated based on merges with child ID


source("0-config.R")

dfull <- readRDS(here("data/compiled_clean_POC_survey.rds"))

#child health data
ch <- dfull %>% filter(!is.na(haz) | !is.na(waz) | !is.na(ari) | !is.na(diarrhea))

ch %>% group_by(country) %>%
  summarise(N_haz=sum(!is.na(haz)), Mean_HAZ=mean(haz, na.rm=T), Prev_Stunting=mean(haz < (-2), na.rm=T)*100, Prev_Sev_Stunting=mean(haz < (-3), na.rm=T)*100,
            N_whz=sum(!is.na(whz)), Mean_WHZ=mean(whz, na.rm=T), Prev_Wasting=mean(whz < (-2), na.rm=T)*100, Prev_Sev_Wasting=mean(whz < (-3), na.rm=T)*100)
            
            #Note: not looking at underweught
            #N_waz=sum(!is.na(waz)), Mean_WAZ=mean(waz, na.rm=T), Prev_Underweight=mean(waz < (-2), na.rm=T)*100, Prev_Sev_Underweight=mean(waz < (-3), na.rm=T)*100)


ch %>% group_by(country) %>%
  summarise(N_diarrhea_meas=sum(!is.na(diarrhea)), N_diarrhea=sum(diarrhea, na.rm=T),  Prev_diarrhea=mean(diarrhea, na.rm=T)*100,
            N_ARI_meas=sum(!is.na(ari)), N_ARI=sum(ari, na.rm=T),  Prev_ARI=mean(ari, na.rm=T)*100)

#Make a N children and child age/ num kids per HH table
ch %>% group_by(country) %>%
  summarise(N=n(), child_age=mean(aged, na.rm=T)/365, min_child_age=min(aged, na.rm=T)/365, max_child_age=max(aged, na.rm=T)/365)
            

#HH data
d <- dfull %>% filter(!is.na(san_imp) | !is.na(wat_imp) | !is.na(EC_S) | !is.na(EC_H)) %>%
  distinct(country, clust_num, HH_num, .keep_all = T)

d %>% group_by(country) %>%
  summarise(N_households=n(), N_imp_wat=sum(as.numeric(wat_imp)-1, na.rm=T), N_imp_san=sum(as.numeric(san_imp)-1, na.rm=T),  N_imp_hygeine=sum(as.numeric(hyg_imp)-1, na.rm=T), N_imp_WASH=sum(as.numeric(WASH_noEC)-1, na.rm=T))
    
d %>% group_by(country) %>%
  summarise(N_households=n(), N_EC_H=sum(as.numeric(EC_H)-1, na.rm=T), N_EC_S=sum(as.numeric(EC_S)-1, na.rm=T), N_safely_manH20=sum(as.numeric(safely_manH20)-1, na.rm=T),  N_imp_WASH_noEC=sum(as.numeric(WASH)-1, na.rm=T))


d %>% tabyl(country, wat_imp_cat)
d %>% tabyl(country, san_imp_cat)
d %>% tabyl(country, hyg_imp_cat)
d %>% tabyl(country, EC_risk_H)
d %>% tabyl(country, EC_risk_S)

d %>% group_by(country) %>%
  summarise(N_households=n(), N_imp_wat=sum(as.numeric(wat_imp_cat)-1, na.rm=T), N_imp_san=sum(as.numeric(san_imp_cat)-1, na.rm=T),  N_imp_hygeine=sum(as.numeric(hyg_imp_cat)-1, na.rm=T))


            
            
            
            
            
            
            
            
            
