
rm(list=ls())
setwd(here::here())
source("0-config.R")

setwd("C:/Users/andre/Dropbox/MICS-WASH-data/")


df_al <- load_MICS_dataset("Algeria", survey_round=6, saveCodebook=T)






#To add (in the future):
        # Samoa (not available yet)
#PakistanSindh
#Guyana
df_PakistanSindh <- load_MICS_dataset("PakistanSindh", survey_round=6, saveCodebook=T)
df_guyana <- load_MICS_dataset("Guyana", survey_round=6, saveCodebook=T)

#non-standard survey formats - double check
df_ga <- load_MICS_dataset("Georgia", survey_round=6, saveCodebook=T) #Georgia has an abortion module, not a birth history module. See if birth order can be derived from that
#df_CI <- load_MICS_dataset("CoteIvoire", survey_round=5, saveCodebook=T) #Check online, but the data doesn't have WQ data
#df_cuba <- load_MICS_dataset("Cuba") #check but Cuba doesn't have WQ module
df_PAR <- load_MICS_dataset("Paraguay", survey_round=5, saveCodebook=T, rename_Vars=T) 
#Note, MICS 6 exists for Nigeria but dropped water quality module
df_ni <- load_MICS_dataset("Nigeria", survey_round=5, saveCodebook=T, rename_Vars=T) 
df_cg <- load_MICS_dataset("Congo", survey_round=5, saveCodebook=T) 



#Standard survey formats
df_al <- load_MICS_dataset("Algeria", survey_round=6, saveCodebook=T)
df_chad <- load_MICS_dataset("Chad", survey_round=6, saveCodebook=T)
df_CAR <- load_MICS_dataset("CAR", survey_round=6, saveCodebook=T)
df_bd <- load_MICS_dataset("Bangladesh", survey_round=6, saveCodebook=T)
df_pakPun <- load_MICS_dataset("PakistanPunjab", survey_round=6, saveCodebook=T)
df_ze <- load_MICS_dataset("Zimbabwe", survey_round=6, saveCodebook=T)
df_ks <- load_MICS_dataset("Kosovo", survey_round=6, saveCodebook=T)
df_STP <- load_MICS_dataset("Sao Tome and Principe", survey_round=6, saveCodebook=T)
df_np <- load_MICS_dataset("Nepal", survey_round=6, saveCodebook=T) 
df_DRC <- load_MICS_dataset("DRC", survey_round=6, saveCodebook=T) 
df_ta <- load_MICS_dataset("Tonga", survey_round=6, saveCodebook=T)
df_gb <- load_MICS_dataset("Gambia", survey_round=6, saveCodebook=T)
df_G_B <- load_MICS_dataset("Guinea Bissau", survey_round=6, saveCodebook=T)
df_ki <- load_MICS_dataset("Kiribati", survey_round=6, saveCodebook=T)
df_laPDR <- load_MICS_dataset("LaoPDR", survey_round=6, saveCodebook=T)
df_le <- load_MICS_dataset("Lesotho", survey_round=6, saveCodebook=T)
df_md <- load_MICS_dataset("Madagascar", survey_round=6, saveCodebook=T)
df_mo <- load_MICS_dataset("Mongolia", survey_round=6, saveCodebook=T)
df_SL <- load_MICS_dataset("SierraLeone", survey_round=6, saveCodebook=T)
df_sur <- load_MICS_dataset("Suriname", survey_round=6, saveCodebook=T)
df_tg <- load_MICS_dataset("Togo", survey_round=6, saveCodebook=T)
df_tun <- load_MICS_dataset("Tunisia", survey_round=6, saveCodebook=T)
df_gh <- load_MICS_dataset("Ghana", survey_round=6, saveCodebook=T) 
df_iq <- load_MICS_dataset("Iraq", survey_round=6, saveCodebook=T)
df_sam <- load_MICS_dataset("Samoa", survey_round=6, saveCodebook=T)

#New 2022
df_dr <- load_MICS_dataset("DominicanRepublic", survey_round=6, saveCodebook=T)
df_P_Baluch <- load_MICS_dataset("PakistanBaluchistan", survey_round=6, saveCodebook=T)
df_hond <- load_MICS_dataset("Honduras", survey_round=6, saveCodebook=T)


#WASH codes
lab_al <- readRDS(here("codebooks/wash_codes/Algeria_WASHvars.rds"))
lab_bd <- readRDS(here("codebooks/wash_codes/Bangladesh_WASHvars.rds"))
lab_CAR <- readRDS(here("codebooks/wash_codes/CAR_WASHvars.rds"))
lab_chad <- readRDS(here("codebooks/wash_codes/Chad_WASHvars.rds"))
lab_cg <- readRDS(here("codebooks/wash_codes/Congo_WASHvars.rds"))
lab_DRC <- readRDS(here("codebooks/wash_codes/DRC_WASHvars.rds"))
lab_gb <- readRDS(here("codebooks/wash_codes/Gambia_WASHvars.rds"))
lab_ga <- readRDS(here("codebooks/wash_codes/Georgia_WASHvars.rds"))
lab_gh <- readRDS(here("codebooks/wash_codes/Ghana_WASHvars.rds"))
lab_guy <- readRDS(here("codebooks/wash_codes/Guyana_WASHvars.rds"))
lab_G_B <- readRDS(here("codebooks/wash_codes/Guinea Bissau_WASHvars.rds"))
lab_iq <- readRDS(here("codebooks/wash_codes/Iraq_WASHvars.rds"))
lab_ki <- readRDS(here("codebooks/wash_codes/Kiribati_WASHvars.rds"))
lab_ks <- readRDS(here("codebooks/wash_codes/Kosovo_WASHvars.rds"))
lab_laPDR <- readRDS(here("codebooks/wash_codes/LaoPDR_WASHvars.rds"))
lab_le <- readRDS(here("codebooks/wash_codes/Lesotho_WASHvars.rds"))
lab_md <- readRDS(here("codebooks/wash_codes/Madagascar_WASHvars.rds"))
lab_mo <- readRDS(here("codebooks/wash_codes/Mongolia_WASHvars.rds"))
lab_np <- readRDS(here("codebooks/wash_codes/Nepal_WASHvars.rds"))
lab_ni <- readRDS(here("codebooks/wash_codes/Nigeria_WASHvars.rds"))
lab_pakPun <- readRDS(here("codebooks/wash_codes/PakistanPunjab_WASHvars.rds"))
lab_pakSin <- readRDS(here("codebooks/wash_codes/PakistanSindh_WASHvars.rds"))
lab_PAR <- readRDS(here("codebooks/wash_codes/Paraguay_WASHvars.rds"))
lab_SL <- readRDS(here("codebooks/wash_codes/Sao Tome and Principe_WASHvars.rds"))
lab_STP <- readRDS(here("codebooks/wash_codes/SierraLeone_WASHvars.rds"))
lab_sur <- readRDS(here("codebooks/wash_codes/Suriname_WASHvars.rds"))
lab_tg <- readRDS(here("codebooks/wash_codes/Togo_WASHvars.rds"))
lab_ta <- readRDS(here("codebooks/wash_codes/Tonga_WASHvars.rds"))
lab_tun <- readRDS(here("codebooks/wash_codes/Tunisia_WASHvars.rds"))
lab_ze <- readRDS(here("codebooks/wash_codes/Zimbabwe_WASHvars.rds"))
lab_DR <- readRDS(here("codebooks/wash_codes/DominicanRepublic_WASHvars.rds"))

#Add: P_Balochistan, Honduras, Samoa


labs <- bind_rows(lab_ni, lab_cg, lab_ga, lab_PAR, lab_guy, 
                  lab_al, lab_bd, lab_CAR,  lab_chad,  lab_DRC, lab_G_B,   
                  lab_gb, lab_gh, lab_iq, lab_ki, lab_ks, lab_laPDR, 
                  lab_le, lab_md, lab_mo,  lab_np, lab_pakPun,   lab_pakSin, 
                  lab_SL, lab_STP, lab_sur, lab_ta, lab_tg, lab_tun,lab_ze,
                  lab_DR)

WASH_labs <- labs %>% mutate(country = factor(country, levels = unique(country))) %>%
  spread(key = country, value = label)
WASH_labs <- WASH_labs %>% mutate(variable = case_when(
  name=="EU1" ~ "type of stove",
  name=="EU2" ~ "Cookstove have a chimney",
  name=="EU3" ~ "Cookstove have a fan",
  name=="EU4" ~ "Type of energy source for cookstove",
  name=="HC17" ~ "Household own any animals",
  name=="HC4" ~ "Main material of floor",
  name=="HC5" ~ "Main material of roof",
  name=="HC6" ~ "Main material of exterior wall",
  name=="HW1" ~ "Place where household members most often wash their hands",
  name=="HW2" ~ "Water available at the place for handwashing",
  name=="HW3" ~ "Soap or detergent present at place of handwashing",
  name=="HW4" ~ "Usual place for handwashing",
  name=="HW5" ~ "Soap/other material available for washing hands",
  name=="HW6" ~ "Hand washing material shown",
  name=="WS1" ~ "Main source of drinking water",
  name=="WS11" ~ "Type of toilet facility",
  name=="WS12" ~ "Pit latrine or septic tank ever been emptied",
  name=="WS13" ~ "Place the contents were emptied",
  name=="WS14" ~ "Location of the toilet faciltity",
  name=="WS15" ~ "Toilet facility shared",
  name=="WS2" ~ "Main source of water used for other purposes (if bottled water used for drinking)",
  name=="WS9" ~ "Treat water to make safer for drinking", 
  name=="WS10A" ~ "Water treatment: Boil", 
  name=="WS10B" ~ "Water treatment: Add bleach/chlorine", 
  name=="WS10C" ~ "Water treatment: Strain it through a cloth", 
  name=="WS10D" ~ "Water treatment: Use water filter", 
  name=="WS10E" ~ "Water treatment: Solar disinfection", 
  name=="WQ4" ~ "Water treatment",
  name=="WQ5A" ~ "Water treatment: Boil",
  name=="WQ5B" ~ "Water treatment: Add bleach/chlorine",
  name=="WQ5C" ~ "Water treatment: Strain it through a cloth",
  name=="WQ5D" ~ "Water treatment: Use water filter",
  name=="WQ5E" ~ "Water treatment: Solar disinfection",
  name=="WQ14" ~ "Drink water without making any treatment",
  name=="WQ15A" ~ "Water treatment: Boil",
  name=="WQ15B" ~ "Water treatment: Add bleach/chlorine",
  name=="WQ15C" ~ "Water treatment: Strain it through a cloth",
  name=="WQ15D" ~ "Water treatment: Use water filter",
  name=="WQ15E" ~ "Water treatment: Solar disinfection"
)) %>% relocate(name, variable)
write.csv(WASH_labs, here::here(paste0("codebooks/WASH_vars.csv")))



save(list=ls(pattern="df_"), file=here("data/raw_MICS_surveys.rdata"))

<<<<<<< HEAD:src/1_cleaning/01-create-single-dataset.R
d <- bind_rows(lapply(ls(pattern="df_"),get))
dim(d)
colnames(d)
saveRDS(d, here("data/compiled_raw_MICS_survey.rds"))

table(d$country, d$san_cat_lab)

=======
>>>>>>> ecf7b03a3f58235264277bdbbc020bd164260979:src/1_cleaning/01-clean-individual-datasets.R
setwd(here::here())

