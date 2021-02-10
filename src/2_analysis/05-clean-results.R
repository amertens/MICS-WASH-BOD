
rm(list=ls())
source("0-config.R")

df <- readRDS(here("results/pooled_raw_results.rds"))






#Add reference categories with ref label
head(df)
dref <- df %>% distinct(analysis, country, Y, X, ref, binary, adjusted, subgroup) %>%
  mutate(contrast=ref, est=ifelse(binary==1,1,0), reflab="(ref.)")
df <- bind_rows(df, dref)

unique(df$contrast)

table(df$X, df$ref)
table(df$X, df$contrast)

#Clean data for figures
df <- df %>% 
  mutate(
    country = case_when(country=="pooled" & analysis=="FE" ~ "Pooled - FE",
                        country=="pooled" & analysis!="FE" ~ "Pooled - RE",
                        country==country ~ country),
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
                     X=="wat_imp" ~ "Unimproved\nwater supply", 
                     X=="hyg_imp" ~ "Unimproved\nhygiene", 
                     X=="WASH" ~ "Not improved WASH\nwith no contamination",
                     X=="WASH_noEC" ~ "Unimproved\nWASH",
                     X=="safely_manH20" ~ "Unsafely managed\ndrinking water",
                     X=="EC_risk_H" ~ "HH water\ncontamination", 
                     X=="EC_risk_S" ~ "Source water\ncontamination", 
                     X=="san_imp_cat" ~ "Sanitation\ncategory", 
                     X=="wat_imp_cat" ~ "Water supply\ncategory", 
                     X=="hyg_imp_cat" ~ "Hygiene\ncategory"),
    Xlab=factor(Xlab, levels = rev(c(
      "Unimproved\nwater supply", 
      "Unimproved\nsanitation", 
      "Unimproved\nhygiene", 
      "Unimproved\nWASH",
      "Contaminated\nHH water", 
      "Contaminated\nsource water", 
      "Unsafely managed\ndrinking water",
      "Not improved WASH\nwith no contamination",
      "HH water\ncontamination", 
      "Source water\ncontamination", 
      "Sanitation\ncategory", 
      "Water supply\ncategory", 
      "Hygiene\ncategory"))),
    Xlab2 = case_when(X=="EC_H" ~ "Contaminated HH water", 
                     X=="EC_S" ~ "Contaminated source water", 
                     X=="san_imp" ~ "Unimproved sanitation", 
                     X=="wat_imp" ~ "Unimproved water supply", 
                     X=="hyg_imp" ~ "Unimproved hygiene", 
                     X=="WASH" ~ "Not improved WASH with no contamination",
                     X=="WASH_noEC" ~ "Unimproved WASH",
                     X=="safely_manH20" ~ "Unsafely managed drinking water",
                     X=="EC_risk_H" ~ "HH water contamination", 
                     X=="EC_risk_S" ~ "Source water contamination", 
                     X=="san_imp_cat" ~ "Sanitation category", 
                     X=="wat_imp_cat" ~ "Water supply category", 
                     X=="hyg_imp_cat" ~ "Hygiene category"),
    Xlab2=factor(Xlab2, levels = rev(c(
      "Unimproved water supply", 
      "Unimproved sanitation", 
      "Unimproved hygiene", 
      "Unimproved WASH",
      "Contaminated HH water", 
      "Contaminated source water", 
      "Unsafely managed drinking water",
      "Not improved WASH with no contamination",
      "HH water contamination", 
      "Source water contamination", 
      "Sanitation category", 
      "Water supply category", 
      "Hygiene category"))),
    contrast = case_when(
      contrast=="0" & !grepl("EC",X)~ "Improved",
      contrast=="1" & !grepl("EC",X)~ "Unimproved",
      contrast=="0" & grepl("EC",X) ~ "Uncontaminated",
      contrast=="1" & grepl("EC_risk",X) ~ "Low risk",
      contrast=="1" & grepl("EC",X) ~ "Contaminated",
      contrast=="2" ~ "Moderate risk",
      contrast=="3" ~ "High risk", 
      contrast=="4" ~ "Very high risk", 
      contrast==contrast ~ contrast 
    ),
    contrast=factor(contrast, levels=rev(c("Safe","Unsafe","Improved","Uncontaminated","Contaminated","Low risk","Moderate risk", "High risk",  "Very high risk",  "High coverage", "Continuous",  "Basic", "Limited",  "No facility", "None",   "Surface water", "Unimproved"))),
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


saveRDS(df, here("results/pooled_results.rds"))

