

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


#Newly added:
#Congo
#nepal
#Algeria
#Kosovo
#Cuba 2019 Datasets 
#Sao Tome and Principe 2019 Datasets

chad <- load_MICS_mort_dataset("Chad")
CAR <- load_MICS_mort_dataset("CAR")

bd <- load_MICS_mort_dataset("Bangladesh")
pakPun <- load_MICS_mort_dataset("PakistanPunjab")
ze <- load_MICS_mort_dataset("Zimbabwe")

al <- load_MICS_mort_dataset("Algeria")
ks <- load_MICS_mort_dataset("Kosovo")
#cuba <- load_MICS_mort_dataset("Cuba") #check but Cuba doesn't have WQ module
STP <- load_MICS_mort_dataset("Sao Tome and Principe")
np <- load_MICS_mort_dataset("Nepal") 
DRC <- load_MICS_mort_dataset("DRC") 
ta <- load_MICS_mort_dataset("Tonga")
gb <- load_MICS_mort_dataset("Gambia")
G_B <- load_MICS_mort_dataset("Guinea Bissau")

cg <- load_MICS_mort_dataset("Congo") 
ki <- load_MICS_mort_dataset("Kiribati")
laPDR <- load_MICS_mort_dataset("LaoPDR")
le <- load_MICS_mort_dataset("Lesotho")
md <- load_MICS_mort_dataset("Madagascar")
mo <- load_MICS_mort_dataset("Mongolia")
ni <- load_MICS_mort_dataset("Nigeria") 
PAR <- load_MICS_mort_dataset("Paraguay") 
SL <- load_MICS_mort_dataset("SierraLeone")
sur <- load_MICS_mort_dataset("Suriname")
tg <- load_MICS_mort_dataset("Togo")
tun <- load_MICS_mort_dataset("Tunisia")
CI <- load_MICS_mort_dataset("CoteIvoire") 
#ga <- load_MICS_mort_dataset("Georgia") #Georgia doesn't have mortality
gh <- load_MICS_mort_dataset("Ghana") 
iq <- load_MICS_mort_dataset("Iraq")






ls()


save(bd, 
     cg,
     gb,
     iq,
     chad,
     CAR,
     CI,
     gh,ki,laPDR, 
     DRC, ta, G_B,
     le,md,mo,          
     ni,
     np,
     pakPun, PAR,
     SL,              
     sur, tg, tun, ze,
     al, ks, STP, file=here("data/raw_MICS_mort.rdata"))


#load(here("data/raw_MICS_surveys.rdata"))
d <- bind_rows(bd, 
               cg,
               gb,
               iq,
               chad,
               CAR,
               CI,
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

table(d$country, d$mort)
prop.table(table(d$country, d$mort),1)*100

saveRDS(d, here("data/compiled_raw_MICS_mortality.rds"))

#merge in covariates
cov <- readRDS(here("data/compiled_clean_MICS_survey.rds"))
colnames(cov)
#drop child vars
cov <- cov %>% subset(., select = -c(childLN,diarrhea, ari,mort,fever,cough,resp_healthcare,
                                     diff_breath, congestion, haz, waz, whz,           
                                      stunt, wast, sex, aged, birthord, ID,
                                     wscore, windex5, windex10, wscoreu,
                                     windex5u, windex10u, wscorer, windex5r, windex10r,hhweight,wqhaweight,
                                     wqeweight,wqsaweight, stratum, PSU))


dim(d)
d <- d %>% distinct(country,clust_num, HH_num, childLN, .keep_all = T )
dim(d)

dim(cov)
cov <- cov %>% distinct(country,clust_num, HH_num, .keep_all = T)
dim(cov)

head(d)
head(cov)

d$clust_num <- as.numeric(d$clust_num)
d$HH_num <- as.numeric(d$HH_num)

dim(d)
dim(cov)
df <- inner_join(d, cov, by=c("country","clust_num","HH_num"))
dim(df)

table(df$mort, df$EC_risk_H)

saveRDS(df, here("data/compiled_clean_MICS_mortality.rds"))

