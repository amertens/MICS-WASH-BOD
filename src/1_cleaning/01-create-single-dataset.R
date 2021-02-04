
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
chad <- load_MICS_dataset("Chad")
    chad <- clean_WASH(chad)

table(chad$EC_100_H)
table(chad$wat_class_lab)

CAR <- load_MICS_dataset("CAR")
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

bd <- load_MICS_dataset("Bangladesh")
pakPun <- load_MICS_dataset("PakistanPunjab")
ze <- load_MICS_dataset("Zimbabwe")

al <- load_MICS_dataset("Algeria")
        al <- clean_WASH(al)
ks <- load_MICS_dataset("Kosovo")
        ks <- clean_WASH(ks)
#cuba <- load_MICS_dataset("Cuba") #check but Cuba doesn't have WQ module
STP <- load_MICS_dataset("Sao Tome and Principe")
        STP <- clean_WASH(STP)
np <- load_MICS_dataset("Nepal") 
        np <- clean_WASH(np)
DRC <- load_MICS_dataset("DRC") 
        DRC <- clean_WASH(DRC)
ta <- load_MICS_dataset("Tonga")
        ta <- clean_WASH(ta)
gb <- load_MICS_dataset("Gambia")
        gb <- clean_WASH(gb)
G_B <- load_MICS_dataset("Guinea Bissau")
        G_B <- clean_WASH(G_B)

cg <- load_MICS_dataset("Congo") 
ki <- load_MICS_dataset("Kiribati")
laPDR <- load_MICS_dataset("LaoPDR")
le <- load_MICS_dataset("Lesotho")
md <- load_MICS_dataset("Madagascar")
mo <- load_MICS_dataset("Mongolia")
ni <- load_MICS_dataset("Nigeria") 
PAR <- load_MICS_dataset("Paraguay") 
SL <- load_MICS_dataset("SierraLeone")
sur <- load_MICS_dataset("Suriname")
tg <- load_MICS_dataset("Togo")
tun <- load_MICS_dataset("Tunisia")
ga <- load_MICS_dataset("Georgia") #Georgia has an abortion module, not a birth history module. See if birth order can be derived from that
gh <- load_MICS_dataset("Ghana") 
iq <- load_MICS_dataset("Iraq")
CI <- load_MICS_dataset("CoteIvoire") #Check, but I don't think has WQ modules

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
