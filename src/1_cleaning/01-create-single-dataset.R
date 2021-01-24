

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
        # Tonga
        # Iraq
        # Guinea-Bissau







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
cg <- load_MICS_dataset("Congo") 
        cg <- clean_WASH(cg)
np <- load_MICS_dataset("Nepal") 
        np <- clean_WASH(np)
DRC <- load_MICS_dataset("DRC") 
        DRC <- clean_WASH(DRC)
ta <- load_MICS_dataset("Tonga")
        ta <- clean_WASH(ta)
G_B <- load_MICS_dataset("Guinea Bissau") #don
        G_B <- clean_WASH(G_B)

        
ki <- load_MICS_dataset("Kiribati")
laPDR <- load_MICS_dataset("LaoPDR")
le <- load_MICS_dataset("Lesotho")
md <- load_MICS_dataset("Madagascar")
mo <- load_MICS_dataset("Mongolia")
ni <- load_MICS_dataset("Nigeria") 
par <- load_MICS_dataset("Paraguay") 
SL <- load_MICS_dataset("SierraLeone")
sur <- load_MICS_dataset("Suriname")
tg <- load_MICS_dataset("Togo")
tun <- load_MICS_dataset("Tunisia")
#CI <- load_MICS_dataset("CoteIvoire") #Check, but I don't think has WQ modules
gb <- load_MICS_dataset("Gambia")
ga <- load_MICS_dataset("Georgia") #Georgia has an abortion module, not a birth history module. See if birth order can be derived from that
gh <- load_MICS_dataset("Ghana") 
iq <- load_MICS_dataset("Iraq")

#Two studies with slightly different variable names. Recode:
#Paraguay, Nigeria
par <- par %>% rename(
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
d <- bind_rows(bd, 
               cg,
               #CI,
               ga,
               gb,
               iq,
     gh,ki,laPDR, 
     DRC, ta, G_B,
     le,md,mo,          
     ni,
     np,
     pakPun, par,
     SL,              
     sur, tg, tun, ze,
     al, ks, STP)


dim(d)

saveRDS(d, here("data/compiled_raw_MICS_survey.rds"))
