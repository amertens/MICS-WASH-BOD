
source(here::here("0-config.R"))


#read child health data
# country="Bangladesh"
# path=paste0(country,"/ch.dta")
# df <- read_dta(data_path(path))
# lab<-makeVlist(df)
# write.csv(lab, here::here(paste0("codebooks/ch_vars.csv")))
# table(df$CA1)


d <- dfull <- readRDS(here("data/compiled_raw_MICS_survey.rds"))


table(d$country)

table(d$country, d$san_imp)
table(d$country, d$storage)
table(d$country, d$childfaeces)
table(d$country, d$treat_any)

#Sought care for respiratory infection
table(d$country, d$CA20)
table(d$country, is.na(d$CA20))



table(d$country, d$EC_result_H)
table(d$country, d$EC_result_H100)
table(d$EC_result_H, d$EC_risk_H_1, d$country)
table(d$EC_result_H100, d$EC_risk_H_1, d$country)


table(d$country, d$EC_risk_H_1)
table(d$country, d$EC_risk_H_2)
table(d$country, d$EC_risk_H_3)
table(d$country, d$EC_risk_H_4)



table(d$country, d$EC_risk_S_1)
table(d$country, d$EC_risk_S_2)
table(d$country, d$EC_risk_S_3)
table(d$country, d$EC_risk_S_4)


d <- d %>% filter(EC_result_H!=0)


prop.table(table(d$country, d$EC_risk_H_1),1)*100
prop.table(table(d$country, d$EC_risk_H_2),1)*100
prop.table(table(d$country, d$EC_risk_H_3),1)*100
prop.table(table(d$country, d$EC_risk_H_4),1)*100

prop.table(table(d$country, d$EC_risk_S_1),1)*100
prop.table(table(d$country, d$EC_risk_S_2),1)*100
prop.table(table(d$country, d$EC_risk_S_3),1)*100
prop.table(table(d$country, d$EC_risk_S_4),1)*100

d <- d %>% mutate(
  EC_risk_H = 
  case_when(
    EC_risk_H_1 ==100 ~ 1,   
    EC_risk_H_2 ==100 ~ 2,   
    EC_risk_H_3 ==100 ~ 3,   
    EC_risk_H_4 ==100 ~ 4   
  ),
  EC_risk_S = 
    case_when(
      EC_risk_S_1 ==100 ~ 1,   
      EC_risk_S_2 ==100 ~ 2,   
      EC_risk_S_3 ==100 ~ 3,   
      EC_risk_S_4 ==100 ~ 4   
    )
  )
table(d$EC_risk_S)
table(d$EC_risk_H)
table(is.na(d$EC_risk_S))
table(is.na(d$EC_risk_H))


table(d$CA1)
table(is.na(d$CA1))
d$CA1[d$CA1==8] <- NA
d$CA1[d$CA1==9] <- NA

table(d$country, d$CA1)
prop.table(table(d$country, d$CA1),1)*100

df <- d %>% filter(CA1==1)
table(df$country, df$EC_risk_S)
table(df$country, df$EC_risk_H)


country="Bangladesh"
path=paste0(country,"/",country,"_cleaned.dta")
bh_path=paste0(country,"/bh.sav")
bh <- read_sav(data_path(bh_path))

head(bh)
lab<-makeVlist(bh)
write.csv(lab, here::here(paste0("codebooks/bh_vars.csv")))



path=paste0(country,"/",country,"_cleaned.dta")
hh_path=paste0(country,"/hh.sav")
hh <- read_sav(data_path(hh_path))

head(hh)
lab<-makeVlist(hh)
write.csv(lab, here::here(paste0("codebooks/hh_vars.csv")))


