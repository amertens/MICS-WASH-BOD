


source("0-config.R")



bd <- load_MICS_dataset("Bangladesh")
cg <- load_MICS_dataset("Congo")
iq <- load_MICS_dataset("Iraq")

dim(bd)
dim(cg)
dim(iq)
d <- bind_rows(bd, cg, iq)
dim(d)

table(d$country)

table(d$country, d$san_imp)
table(d$country, d$storage)
table(d$country, d$childfaeces)
table(d$country, d$treat_any)

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


d_wc <- d %>% filter(EC_result_H!=0)


prop.table(table(d_wc$country, d_wc$EC_risk_H_1),1)
prop.table(table(d_wc$country, d_wc$EC_risk_H_2),1)
prop.table(table(d_wc$country, d_wc$EC_risk_H_3),1)
prop.table(table(d_wc$country, d_wc$EC_risk_H_4),1)



prop.table(table(d_wc$country, d_wc$EC_risk_S_1),1)
prop.table(table(d_wc$country, d_wc$EC_risk_S_2),1)
prop.table(table(d_wc$country, d_wc$EC_risk_S_3),1)
prop.table(table(d_wc$country, d_wc$EC_risk_S_4),1)





