
rm(list=ls())
source("0-config.R")


#NOTES
# Check email... new studies were released
# Check draft report and make sure all countries are included here
#go through and make sure all load correctly
#update covariate data cleaning to use numeric codes rather than language specific, 
# especially if new languages are in the new surveys 
#(save a conversion codebook)

#To add (in the future):
        # Samoa (not available yet)

#To add
chad <- load_MICS_dataset("Chad", survey_round=6)
    chad <- clean_WASH(chad)

table(chad$EC_100_H)
table(chad$wat_class_lab)

CAR <- load_MICS_dataset("CAR", survey_round=6)
  CAR <- clean_WASH(CAR)
  
table(CAR$EC_100_H)
table(CAR$wat_class_lab)





#Newly added:
        #Congo
        #nepal
        #Algeria
        #Kosovo
        #Cuba 2019 Datasets 
        #Sao Tome and Principe 2019 Datasets

bd <- load_MICS_dataset("Bangladesh", survey_round=6)
pakPun <- load_MICS_dataset("PakistanPunjab", survey_round=6)
ze <- load_MICS_dataset("Zimbabwe", survey_round=6)

al <- load_MICS_dataset("Algeria", survey_round=6)
        al <- clean_WASH(al)
ks <- load_MICS_dataset("Kosovo", survey_round=6)
        ks <- clean_WASH(ks)
#cuba <- load_MICS_dataset("Cuba") #check but Cuba doesn't have WQ module
STP <- load_MICS_dataset("Sao Tome and Principe", survey_round=6)
        STP <- clean_WASH(STP)
np <- load_MICS_dataset("Nepal", survey_round=6) 
        np <- clean_WASH(np)
DRC <- load_MICS_dataset("DRC", survey_round=6) 
        DRC <- clean_WASH(DRC)
ta <- load_MICS_dataset("Tonga", survey_round=6)
        ta <- clean_WASH(ta)
gb <- load_MICS_dataset("Gambia", survey_round=6)
        gb <- clean_WASH(gb)
G_B <- load_MICS_dataset("Guinea Bissau", survey_round=6)
        G_B <- clean_WASH(G_B)

cg <- load_MICS_dataset("Congo", survey_round=5) 
ki <- load_MICS_dataset("Kiribati", survey_round=6)
laPDR <- load_MICS_dataset("LaoPDR", survey_round=6)
le <- load_MICS_dataset("Lesotho", survey_round=6)
md <- load_MICS_dataset("Madagascar", survey_round=6)
mo <- load_MICS_dataset("Mongolia", survey_round=6)
ni <- load_MICS_dataset("Nigeria", survey_round=5) 
PAR <- load_MICS_dataset("Paraguay", survey_round=5) 
SL <- load_MICS_dataset("SierraLeone", survey_round=6)
sur <- load_MICS_dataset("Suriname", survey_round=6)
tg <- load_MICS_dataset("Togo", survey_round=6)
tun <- load_MICS_dataset("Tunisia", survey_round=6)
ga <- load_MICS_dataset("Georgia", survey_round=6) #Georgia has an abortion module, not a birth history module. See if birth order can be derived from that
gh <- load_MICS_dataset("Ghana", survey_round=6) 
iq <- load_MICS_dataset("Iraq", survey_round=6)
CI <- load_MICS_dataset("CoteIvoire", survey_round=5) #Check, but I don't think has WQ modules

#Two studies with slightly different variable names. Recode:
#Paraguay, Nigeria
PAR <- PAR %>% rename(
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
ni <- ni %>% rename(
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




ls()


  save(bd, 
        cg,
       CI,
       chad,
       CAR,
        ga,
        gb,
        iq,
        gh,ki,laPDR, 
        DRC, ta, G_B,
        le,md,mo,          
        ni,
        np,
        pakPun, PAR,
        SL,              
        sur, tg, tun, ze,
        al, ks, STP, file=here("data/raw_MICS_surveys.rdata"))

  
#load(here("data/raw_MICS_surveys.rdata"))
d <- bind_rows(bd, 
               cg,
               CI,
               chad,
               CAR,
               #CI,
               ga,
               gb,
               iq,
     gh,ki,laPDR, 
     DRC, ta, G_B,
     le,md,mo,          
     ni,
     np,
     pakPun, PAR,
     SL,              
     sur, tg, tun, ze,
     al, ks, STP)


dim(d)
colnames(d)

saveRDS(d, here("data/compiled_raw_MICS_survey.rds"))

table(d$country, d$san_cat_lab)
