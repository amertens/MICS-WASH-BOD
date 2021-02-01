
rm(list=ls())
source("0-config.R")

df <- readRDS(here("results/pooled_raw_results.rds"))


#classify regions
# East Asia and the Pacific EAP
# Eastern and Southern Africa ESA
# Europe and Central Asia ECA
# Latin America and Caribbean LAC
# Middle East and North Africa MENA
# South Asia SA
# West and Central Africa WCA
unique(df$country)

#Set up vector
EAP <- c("Mongolia", "Tonga",  "Kiribati", "Laos")
ECA <- c("Georgia", "Kosovo")
LAC <- c("Suriname","Paraguay" )
MENA <- c("Algeria","Iraq","Tunisia" )
SA <- c("Bangladesh", "Nepal", "Pakistan")
ESA <- c("Lesotho", "Madagascar",  "Zimbabwe")
WCA <- c("Congo",  "DRC", "Gambia", "Ghana", "Guinea Bissau", "Nigeria", "Togo", "Sierra Leone", "Sao Tome and Principe")

EAP <- EAP[order(EAP)]
ECA <- ECA[order(ECA)]
LAC <- LAC[order(LAC)]
MENA <- MENA[order(MENA)]
SA <- SA[order(SA)]
ESA <- ESA[order(ESA)]
WCA <- WCA[order(WCA)]




#to do:
#order countries alphabetically then by region
#add a country lab with (N=.) in the margin
#seperate "pooled" in forest plots


df$country


"pooled" 

#Clean data for figures
df <- df %>% 
  mutate(
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
    country=case_when(
      country=="pooled" & analysis=="FE" ~ "Pooled - FE",
      country=="pooled" & analysis!="FE" ~ "Pooled - RE",
      country=="PakistanPunjab" ~ "Pakistan",
      country=="LaoPDR" ~ "Laos",
      country=="SierraLeone" ~ "Sierra Leone",
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
      country %in% c("Pooled - FE","Pooled - RE") ~ "Pooled"
    ),
    region=factor(region, levels=rev(c("WCA", "ESA", "LAC", "SA","EAP","MENA","ECA","Pooled"))),
    country=factor(country, levels=rev(c(WCA, ESA,LAC,SA,EAP,MENA,ECA, "Pooled - FE","Pooled - RE"))),
    Xlab = case_when(X=="EC_H" ~ "Uncontaminated\nHH water", 
                     X=="EC_S" ~ "Uncontaminated\nsource water", 
                     X=="san_imp" ~ "Improved\nsanitation", 
                     X=="wat_imp" ~ "Improved\nwater supply", 
                     X=="hyg_imp" ~ "Improved\nhygiene", 
                     X=="WASH" ~ "Improved WASH,\nno contamination",
                     X=="WASH_noEC" ~ "Improved\nWASH",
                     X=="safely_manH20" ~ "Safely managed\ndrinking water",
                     X=="EC_risk_H" ~ "HH water\ncontamination", 
                     X=="EC_risk_S" ~ "Source water\ncontamination", 
                     X=="san_imp_cat" ~ "Sanitation\ncategory", 
                     X=="wat_imp_cat" ~ "Water supply\ncategory", 
                     X=="hyg_imp_cat" ~ "Hygiene\ncategory"),
    Xlab=factor(Xlab, levels = rev(c(
      "Improved\nwater supply", 
      "Improved\nsanitation", 
      "Improved\nhygiene", 
      "Improved\nWASH",
      "Uncontaminated\nHH water", 
      "Uncontaminated\nsource water", 
      "Safely managed\ndrinking water",
      "Improved WASH,\nno contamination",
      "HH water\ncontamination", 
      "Source water\ncontamination", 
      "Sanitation\ncategory", 
      "Water supply\ncategory", 
      "Hygiene\ncategory"))),
    contrast = case_when(
      contrast=="1" ~ "Unimproved",
      contrast=="2" ~ "Moderate risk",
      contrast=="3" ~ "High risk", 
      contrast=="4" ~ "Very high risk", 
      contrast==contrast ~ contrast 
    ),
    contrast=factor(contrast, levels=rev(c("Moderate risk", "High risk",  "Very high risk",   "Basic", "Limited",  "No facility", "None",  "Unimproved", "Surface water"))),
    ref = case_when(
      ref=="0" ~ "Improved",
      ref=="1" ~ "Low risk",
      ref==ref ~ ref 
    ),
    exposure_type = ifelse(X %in% c("EC_H","EC_S","WASH", "safely_manH20", "EC_risk_H", "EC_risk_S"),
                           "WQ","HH"
    )) %>%
  filter(!is.na(est)) %>%
  arrange(Y,X,region, binary, adjusted, subgroup, country) %>%
  mutate(countrylab=paste0(country, " (N=",N,")"),
         countrylab=case_when(grepl("Pooled",country)==TRUE ~ as.character(country),
                              grepl("Pooled",country)==FALSE ~ countrylab),
         countrylab=factor(countrylab, levels=unique(countrylab))) 


saveRDS(df, here("results/pooled_results.rds"))

