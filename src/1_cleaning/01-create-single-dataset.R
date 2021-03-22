
rm(list=ls())
source("0-config.R")

#To add (in the future):
        # Samoa (not available yet)

#non-standard survey formats - double check
df_ga <- load_MICS_dataset("Georgia", survey_round=6) #Georgia has an abortion module, not a birth history module. See if birth order can be derived from that
#df_CI <- load_MICS_dataset("CoteIvoire", survey_round=5) #Check online, but the data doesn't have WQ data
#df_cuba <- load_MICS_dataset("Cuba") #check but Cuba doesn't have WQ module
df_PAR <- load_MICS_dataset("Paraguay", survey_round=5) 
df_ni <- load_MICS_dataset("Nigeria", survey_round=5) 
df_cg <- load_MICS_dataset("Congo", survey_round=5) 


#Standard survey formats
df_chad <- load_MICS_dataset("Chad", survey_round=6)
df_CAR <- load_MICS_dataset("CAR", survey_round=6)
df_bd <- load_MICS_dataset("Bangladesh", survey_round=6)
df_pakPun <- load_MICS_dataset("PakistanPunjab", survey_round=6)
df_ze <- load_MICS_dataset("Zimbabwe", survey_round=6)
df_al <- load_MICS_dataset("Algeria", survey_round=6)
df_ks <- load_MICS_dataset("Kosovo", survey_round=6)
df_STP <- load_MICS_dataset("Sao Tome and Principe", survey_round=6)
df_np <- load_MICS_dataset("Nepal", survey_round=6) 
df_DRC <- load_MICS_dataset("DRC", survey_round=6) 
df_ta <- load_MICS_dataset("Tonga", survey_round=6)
df_gb <- load_MICS_dataset("Gambia", survey_round=6)
df_G_B <- load_MICS_dataset("Guinea Bissau", survey_round=6)
df_ki <- load_MICS_dataset("Kiribati", survey_round=6)
df_laPDR <- load_MICS_dataset("LaoPDR", survey_round=6)
df_le <- load_MICS_dataset("Lesotho", survey_round=6)
df_md <- load_MICS_dataset("Madagascar", survey_round=6)
df_mo <- load_MICS_dataset("Mongolia", survey_round=6)
df_SL <- load_MICS_dataset("SierraLeone", survey_round=6)
df_sur <- load_MICS_dataset("Suriname", survey_round=6)
df_tg <- load_MICS_dataset("Togo", survey_round=6)
df_tun <- load_MICS_dataset("Tunisia", survey_round=6)
df_gh <- load_MICS_dataset("Ghana", survey_round=6) 
df_iq <- load_MICS_dataset("Iraq", survey_round=6)

#Two studies with slightly different variable names. Recode:
#Paraguay, Nigeria
df_PAR <- df_PAR %>% rename(
        EU4_lab=HC6_lab,
        EU4=HC6,
        HC6_lab=HC5_lab,
        HC6=HC5,
        HC5_lab=HC4_lab,
        HC5=HC4,
        HC4_lab=HC3_lab,
        HC4=HC3,
        HC3_lab=HC2_lab,
        HC3=HC2)
df_ni <- df_ni %>% rename(
        EU4_lab=HC6_lab,
        EU4=HC6,
        HC6_lab=HC5_lab,
        HC6=HC5,
        HC5_lab=HC4_lab,
        HC5=HC4,
        HC4_lab=HC3_lab,
        HC4=HC3,
        HC3_lab=HC2_lab,
        HC3=HC2)


save(list=ls(pattern="df_"), file=here("data/raw_MICS_surveys.rdata"))

  
#load(here("data/raw_MICS_surveys.rdata"))
d <- bind_rows(lapply(ls(pattern="df_"),get))
               

dim(d)
colnames(d)

saveRDS(d, here("data/compiled_raw_MICS_survey.rds"))

table(d$country, d$san_cat_lab)
