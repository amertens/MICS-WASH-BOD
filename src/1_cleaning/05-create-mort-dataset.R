

rm(list=ls())
source("0-config.R")

df_PakistanSindh <- load_MICS_mort_dataset("PakistanSindh")
df_guyana <- load_MICS_mort_dataset("Guyana")

#non-standard survey formats - double check
df_ga <- load_MICS_mort_dataset("Georgia") 
df_PAR <- load_MICS_mort_dataset("Paraguay") 
df_ni <- load_MICS_mort_dataset("Nigeria") 
df_cg <- load_MICS_mort_dataset("Congo") 


#Standard survey formats
df_chad <- load_MICS_mort_dataset("Chad")
df_CAR <- load_MICS_mort_dataset("CAR")
df_bd <- load_MICS_mort_dataset("Bangladesh")
df_pakPun <- load_MICS_mort_dataset("PakistanPunjab")
df_ze <- load_MICS_mort_dataset("Zimbabwe")
df_al <- load_MICS_mort_dataset("Algeria")
df_ks <- load_MICS_mort_dataset("Kosovo")
df_STP <- load_MICS_mort_dataset("Sao Tome and Principe")
df_np <- load_MICS_mort_dataset("Nepal") 
df_DRC <- load_MICS_mort_dataset("DRC") 
df_ta <- load_MICS_mort_dataset("Tonga")
df_gb <- load_MICS_mort_dataset("Gambia")
df_G_B <- load_MICS_mort_dataset("Guinea Bissau")
df_ki <- load_MICS_mort_dataset("Kiribati")
df_laPDR <- load_MICS_mort_dataset("LaoPDR")
df_le <- load_MICS_mort_dataset("Lesotho")
df_md <- load_MICS_mort_dataset("Madagascar")
df_mo <- load_MICS_mort_dataset("Mongolia")
df_SL <- load_MICS_mort_dataset("SierraLeone")
df_sur <- load_MICS_mort_dataset("Suriname")
df_tg <- load_MICS_mort_dataset("Togo")
df_tun <- load_MICS_mort_dataset("Tunisia")
df_gh <- load_MICS_mort_dataset("Ghana") 
df_iq <- load_MICS_mort_dataset("Iraq")








d <- bind_rows(lapply(ls(pattern="df_"),get))
dim(d)
colnames(d)

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

