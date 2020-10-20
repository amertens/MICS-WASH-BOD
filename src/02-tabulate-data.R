
#Make sure to subset HH measurements to the unique household level... right now they are duplicated based on merges with child ID


source("0-config.R")
dfull <- readRDS(here("data/compiled_clean_POC_survey.rds"))

#child health data
ch <- dfull %>% filter(!is.na(haz) | !is.na(waz) | !is.na(ari) | !is.na(diarrhea))

ch %>% group_by(country) %>%
  summarise(N_haz=sum(!is.na(haz)), Mean_HAZ=mean(haz, na.rm=T), Prev_Stunting=mean(haz < (-2), na.rm=T)*100, Prev_Sev_Stunting=mean(haz < (-3), na.rm=T)*100,
            N_whz=sum(!is.na(whz)), Mean_WHZ=mean(whz, na.rm=T), Prev_Wasting=mean(whz < (-2), na.rm=T)*100, Prev_Sev_Wasting=mean(whz < (-3), na.rm=T)*100,
            N_waz=sum(!is.na(waz)), Mean_WAZ=mean(waz, na.rm=T), Prev_Underweight=mean(waz < (-2), na.rm=T)*100, Prev_Sev_Underweight=mean(waz < (-3), na.rm=T)*100)


ch %>% group_by(country) %>%
  summarise(N_diarrhea_meas=sum(!is.na(diarrhea)), N_diarrhea=sum(diarrhea, na.rm=T),  Prev_diarrhea=mean(diarrhea, na.rm=T)*100,
            N_ARI_meas=sum(!is.na(ari)), N_ARI=sum(ari, na.rm=T),  Prev_ARI=mean(ari, na.rm=T)*100)

#HH data
d <- dfull %>% filter(!is.na(san_imp) | !is.na(wat_imp) | !is.na(EC_S) | !is.na(EC_H)) %>%
  distinct(country, clust_num, HH_num, .keep_all = T)

d %>% group_by(country) %>%
  summarise(N_households=n(), N_imp_wat=sum(wat_imp, na.rm=T), N_imp_san=sum(san_imp, na.rm=T),  N_imp_hygeine=sum(hyg_imp, na.rm=T), N_imp_WASH=sum(WASH_noEC, na.rm=T))
    
    N_haz=sum(!is.na(haz)), Mean_HAZ=mean(haz, na.rm=T), Prev_Stunting=mean(haz < (-2), na.rm=T)*100, Prev_Sev_Stunting=mean(haz < (-3), na.rm=T)*100,
            
            
            
            
            
            
            
            
            
            
