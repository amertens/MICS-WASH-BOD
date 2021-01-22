

source("0-config.R")


#NOTES
# Check email... new studies were released
# Check draft report and make sure all countries are included here
#go through and make sure all load correctly
#update covariate data cleaning to use numeric codes rather than language specific, 
# especially if new languages are in the new surveys 
#(save a conversion codebook)


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
ks <- load_MICS_dataset("Kosovo")
#cuba <- load_MICS_dataset("Cuba") #check but Cuba doesn't have WQ module
STP <- load_MICS_dataset("Sao Tome and Principe")


cg <- load_MICS_dataset("Congo") 
ki <- load_MICS_dataset("Kiribati")
laPDR <- load_MICS_dataset("LaoPDR")
le <- load_MICS_dataset("Lesotho")
md <- load_MICS_dataset("Madagascar")
mo <- load_MICS_dataset("Mongolia")
np <- load_MICS_dataset("Nepal") 
ni <- load_MICS_dataset("Nigeria") 
par <- load_MICS_dataset("Paraguay") 
SL <- load_MICS_dataset("SierraLeone")
sur <- load_MICS_dataset("Suriname")
tg <- load_MICS_dataset("Togo")
tun <- load_MICS_dataset("Tunisia")
#CI <- load_MICS_dataset("CoteIvoire") #Check, but I don't think has WQ modules
DRC <- load_MICS_dataset("DRC") 
gb <- load_MICS_dataset("Gambia")
ga <- load_MICS_dataset("Georgia") #Georgia has an abortion module, not a birth history module. See if birth order can be derived from that
gh <- load_MICS_dataset("Ghana") 

ls()
d <- bind_rows(bd, 
               cg,
               #CI,
               ga,
               gb,
     gh,ki,laPDR, 
     DRC, 
     le,md,mo,          
     ni,
     np,
     pakPun, par,
     SL,              
     sur, tg, tun, ze,
     al, ks, STP)


dim(d)

saveRDS(d, here("data/compiled_raw_MICS_survey.rds"))
