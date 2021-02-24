
rm(list=ls())
source("0-config.R")

df <- readRDS(here("results/pooled_raw_results.rds"))


#Set up vector
EAP <- c("Mongolia", "Tonga",  "Kiribati", "Laos")
ECA <- c("Georgia", "Kosovo")
LAC <- c("Suriname","Paraguay" )
MENA <- c("Algeria","Iraq","Tunisia" )
SA <- c("Bangladesh", "Nepal", "Pakistan")
ESA <- c("Lesotho", "Madagascar",  "Zimbabwe")
WCA <- c("Chad","CAR","CoteIvoire","Congo",  "DRC", "Gambia", "Ghana", "Guinea Bissau", "Nigeria", "Togo", "Sierra Leone", "Sao Tome+Prin.")

EAP <- EAP[order(EAP)]
ECA <- ECA[order(ECA)]
LAC <- LAC[order(LAC)]
MENA <- MENA[order(MENA)]
SA <- SA[order(SA)]
ESA <- ESA[order(ESA)]
WCA <- WCA[order(WCA)]

#XXXXXXXXXXXXXXXXXXXXXXXXX
# TEMP drop san_imp_cat2
#XXXXXXXXXXXXXXXXXXXXXXXXX

table(df$X)
df <-  df %>% filter(X!="san_imp_cat2")

#Add reference categories with ref label
head(df)
dref <- df %>% distinct(analysis, country, Y, X, ref, binary, adjusted, subgroup) %>%
  mutate(contrast=ref, est=ifelse(binary==1,1,0), reflab="(ref.)")
df <- bind_rows(df, dref)

unique(df$X[df$analysis=="secondary"])
unique(df$contrast[df$analysis=="secondary"])

table(df$X, df$ref)
table(df$X, df$contrast)

#Clean data for figures
df <- df %>% 
  mutate(
    country=case_when(
      country=="PakistanPunjab" ~ "Pakistan",
      country=="LaoPDR" ~ "Laos",
      country=="SierraLeone" ~ "Sierra Leone",
      country=="Sao Tome and Principe" ~ "Sao Tome+Prin.",
      country=="pooled" & analysis=="FE" ~ "Pooled - FE",
      country=="pooled" & analysis!="FE" ~ "Pooled - RE",
      country==country ~ country
    ),
    region = case_when(
      country %in% EAP ~ "EAP",
      country %in% ECA ~ "ECA",
      country %in% LAC ~ "LAC",
      country %in% MENA ~ "MENA",
      country %in% SA ~ "SA",
      country %in% ESA ~ "ESA",
      country %in% WCA ~ "WCA",
      country=="Pooled - RE" & analysis %in% c("region", "region-multi") ~ region,
      country %in% c("Pooled - FE","Pooled - RE") ~ "Pooled"
    ),
    region=factor(region, levels=rev(c("WCA", "ESA", "LAC", "SA","EAP","MENA","ECA","Pooled"))),
    country=factor(country, levels=rev(c(WCA, ESA,LAC,SA,EAP,MENA,ECA, "Pooled - FE","Pooled - RE"))),
    multinomial = ifelse(X %in% c("EC_risk_H", "EC_risk_S", "wat_imp_cat", "san_imp_cat", "hyg_imp_cat"),1,0),
    Y=case_when(
      Y=="stunt" ~ "Stunting",
      Y=="wast" ~ "Wasting",
      Y=="diarrhea" ~ "Diarrhea",
      Y=="ari" ~ "ARI",
      Y=="haz" ~ "HAZ",
      Y=="whz" ~ "WHZ",
      Y=="mort" ~ "Mortality"
    ),
    Y=factor(Y, levels=c("Diarrhea", "Stunting", "Wasting", "ARI", "HAZ", "WHZ","Mortality")),
    Xlab = case_when(X=="EC_H" ~ "Contaminated\nHH water", 
                     X=="EC_S" ~ "Contaminated\nsource water", 
                     X=="san_imp" ~ "Unimproved\nsanitation", 
                     X=="piped_san" ~ "Non-piped\nsanitation", 
                     X=="wat_imp" ~ "Unimproved\nwater supply", 
                     X=="hyg_imp" ~ "Unimproved\nhygiene", 
                     X=="WASH" ~ "Not improved WASH\nwith no contamination",
                     X=="WASH_noEC" ~ "Unimproved\nWASH",
                     X=="safely_manH20" ~ "Unsafely managed\ndrinking water",
                     X=="EC_risk_H" ~ "HH water\ncontamination", 
                     X=="EC_risk_S" ~ "Source water\ncontamination", 
                     X=="san_imp_cat" ~ "Sanitation\ncategory", 
                     X=="san_imp_cat2" ~ "Safely managed\nsanitation", 
                     X=="wat_imp_cat" ~ "Water supply\ncategory", 
                     X=="hyg_imp_cat" ~ "Hygiene\ncategory",
X=="Piped_san_cat" ~ "Piped sanitation",
X=="san_coverage" ~ "Sanitation Coverage",
X=="imp_off_prem_V_unimp" ~ "Unimproved\n(ref: Imp. off prem.)",
X=="imp_on_prem_V_imp_off_prem" ~ "Imp. off prem.\n(ref: Imp. on prem.)",
X=="imp_on_prem_HQ_V_imp_on_prem_LQ" ~ "Imp. on prem. HQ\n(ref: Imp. on prem. LQ)"


xxxxxxxxxxxxxx
Need to fix the Xlab and Xlab2 below to match above and rerun figures with updated data and X-labels.
Then update website
xxxxxxxxxxxxxxxx

X=="imp_on_prem_sufficient_V_imp_on_prem_insufficient" ~ "Imp. on prem. suff.\nV Imp. on prem. insuff.",
X=="imp_on_prem_sufficient_HQ_V_imp_on_prem_insufficient_LQ" ~ "Imp. on prem. suff. HQ\nV Imp. on prem. insuff. LQ"),
    Xlab=factor(Xlab, levels = rev(c(
      "Unimproved\nwater supply", 
      "Unimproved\nsanitation", 
      "Non-piped\nsanitation", 
      "Unimproved\nhygiene", 
      "Unimproved\nWASH",
      "Contaminated\nHH water", 
      "Contaminated\nsource water", 
      "Unsafely managed\ndrinking water",
      "Not improved WASH\nwith no contamination",
      "HH water\ncontamination", 
      "Source water\ncontamination", 
      "Sanitation\ncategory", 
      "Safely managed\nsanitation",
      "Water supply\ncategory", 
      "Hygiene\ncategory",
      "Piped sanitation",
      "Sanitation Coverage",
      "Imp. off prem.\nV Unimp.",
      "Imp. on prem.\nV Imp. off prem.",
      "Imp. on prem. HQ\nV Imp. on prem. LQ",
      "Imp. on prem. suff.\nV Imp. on prem. insuff.",
      "Imp. on prem. suff. HQ\nV Imp. on prem. insuff. LQ"))),
    Xlab2 = case_when(X=="EC_H" ~ "Contaminated HH water", 
                     X=="EC_S" ~ "Contaminated source water", 
                     X=="san_imp" ~ "Unimproved sanitation",
                     X=="piped_san" ~ "Non-piped sanitation", 
                     X=="wat_imp" ~ "Unimproved water supply", 
                     X=="hyg_imp" ~ "Unimproved hygiene", 
                     X=="WASH" ~ "Not improved WASH with no contamination",
                     X=="WASH_noEC" ~ "Unimproved WASH",
                     X=="safely_manH20" ~ "Unsafely managed drinking water",
                     X=="EC_risk_H" ~ "HH water contamination", 
                     X=="EC_risk_S" ~ "Source water contamination", 
                     X=="san_imp_cat" ~ "Sanitation category", 
                     X=="san_imp_cat2" ~ "Safely managed sanitation", 
                     X=="wat_imp_cat" ~ "Water supply category", 
                     X=="hyg_imp_cat" ~ "Hygiene category",
                     X=="Piped_san_cat" ~ "Piped sanitation",
                     X=="san_coverage" ~ "Sanitation Coverage",
                     X=="imp_off_prem_V_unimp" ~ "Imp. off prem. V Unimp.",
                     X=="imp_on_prem_V_imp_off_prem" ~ "Imp. on prem. V Imp. off prem.",
                     X=="imp_on_prem_HQ_V_imp_on_prem_LQ" ~ "Imp. on prem. HQ V Imp. on prem. LQ",
                     X=="imp_on_prem_sufficient_V_imp_on_prem_insufficient" ~ "Imp. on prem. suff. V Imp. on prem. insuff.",
                     X=="imp_on_prem_sufficient_HQ_V_imp_on_prem_insufficient_LQ" ~ "Imp. on prem. suff. HQ V Imp. on prem. insuff. LQ"),
    Xlab2=factor(Xlab2, levels = rev(c(
      "Unimproved water supply", 
      "Unimproved sanitation", 
      "Non-piped sanitation", 
      "Unimproved hygiene", 
      "Unimproved WASH",
      "Contaminated HH water", 
      "Contaminated source water", 
      "Unsafely managed drinking water",
      "Not improved WASH with no contamination",
      "HH water contamination", 
      "Source water contamination", 
      "Sanitation category", 
      "Safely managed sanitation", 
      "Water supply category", 
      "Hygiene category",
      "Piped sanitation",
      "Sanitation Coverage",
      "Imp. off prem. V Unimp.",
      "Imp. on prem. V Imp. off prem.",
      "Imp. on prem. HQ V Imp. on prem. LQ",
      "Imp. on prem. suff. V Imp. on prem. insuff.",
      "Imp. on prem. suff. HQ V Imp. on prem. insuff. LQ"))),
    contrast = case_when(
      contrast=="0" & !grepl("EC",X)~ "Improved",
      contrast=="1" & !grepl("EC",X)~ "Unimproved",
      contrast=="0" & grepl("EC",X) ~ "Uncontaminated",
      contrast=="1" & grepl("EC_risk",X) ~ "Low risk",
      contrast=="1" & grepl("EC",X) ~ "Contaminated",
      contrast=="2" ~ "Moderate risk",
      contrast=="3" ~ "High risk", 
      contrast=="Not piped" ~ "Non-piped", 
      contrast=="4" ~ "Very high risk", 
      contrast==contrast ~ contrast 
    ),
    contrast=factor(contrast, levels=rev(
      c("Piped","Non-piped","Safe","Safely managed","Unsafe","Improved","Uncontaminated",
        "Contaminated","Low risk","Moderate risk", "High risk",  "Very high risk",  
        "High coverage", "Continuous",  "Basic", "Limited",  "No facility", "None",   
        "Surface water", "Unimproved",
        "Basic, non-sewer","Sewered",                               
        "Improved, off premise",
        "Improved, on premise, contaminated","Improved, on premise, insufficient",
        "Improved, on premise, LQ, insufficient", "Improved, on premise",
        "Improved, on premise, uncontaminated","Improved, on premise, sufficient",
        "Improved, on premise, HQ, sufficient"  ))),
    ref = case_when(
      ref=="0" & !grepl("EC",X)~ "Improved",
      ref=="0" & grepl("EC",X) ~ "Uncontaminated",
      ref=="1" & grepl("EC_risk",X) ~ "Low risk",
      ref==ref ~ ref 
    ),
    exposure_type = ifelse(X %in% c("EC_H","EC_S","WASH", "safely_manH20", "EC_risk_H", "EC_risk_S"),
                           "WQ","HH"
    )) %>%
  filter(!is.na(est)) %>%
  arrange(Y,X,region, binary, adjusted, subgroup, country) %>%
  mutate(countrylab=paste0(country, " (",N,")"),
         countrylab=case_when(grepl("Pooled",country)==TRUE ~ as.character(country),
                              grepl("Pooled",country)==FALSE ~ countrylab),
         countrylab=factor(countrylab, levels=unique(countrylab))) 




#mark significant estimates
df <- df %>% mutate(sig = factor(case_when(
  Y %in% c("HAZ","WHZ") & ((ci.lb<0 & ci.ub<0) | (ci.lb>0 & ci.ub>0)) ~ 1,
  Y %in% c("HAZ","WHZ") & ((ci.lb<0 & ci.ub>0)) ~ 0,
  !(Y %in% c("HAZ","WHZ")) & ((ci.lb<1 & ci.ub<1) | (ci.lb>1 & ci.ub>1)) ~ 1,
  !(Y %in% c("HAZ","WHZ")) & ((ci.lb<1 & ci.ub>1)) ~ 0), levels=c("0","1"), labels=c("Not sig.","Sig.")))
df$sig[is.na(df$sig)] <- "Not sig."
table(df$sig)

saveRDS(df, here("results/pooled_results.rds"))

