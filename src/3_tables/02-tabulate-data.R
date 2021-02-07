
#Make sure to subset HH measurements to the unique household level... right now they are duplicated based on merges with child ID

rm(list=ls())
source("0-config.R")
library(table1)
library(rvest)


#Make function out of this then combine N's and percents
tab_cat <- function(d, x){
  d <- d %>% rename(x=!!(x))
  df <- d %>% tabyl(country, x) 
  cattab <- df[,-c(1,ncol(df))]
  rowtotal <- rowSums(cattab)
  coltotal <- as.data.frame(t(colSums(cattab)))
  cattab <- bind_rows(coltotal, cattab)
  rownames(coltotal) <- "Total"
  rownames(cattab) <- c("Total",as.character(df[,1]))
  df2 <- round(prop.table(as.matrix(cattab),1)*100, 1)    
  df2[is.nan(df2)] <- 0
  
  for(i in 1:ncol(cattab)){
    cattab[,i]<- prettyNum(cattab[,i],big.mark=",")
  }
  
  cattab <- as.matrix(cattab)
  df2 <- as.matrix(df2)
  tab <- as.data.frame(matrix( paste0(cattab, " (", df2,"%)", sep=""), 
                               nrow=nrow(df2), dimnames=dimnames(cattab) ))
  tab$Total <- c(sum(rowtotal),rowtotal)
  tab$Missing <- c("",as.character(df[,ncol(df)]))
  return(tab)
}


dfull <- readRDS(here("data/compiled_clean_MICS_survey.rds"))%>%
  subset(., select= - c(mort))
mort <- readRDS(here("data/compiled_clean_MICS_mortality.rds")) %>%
  subset(., select=c(country,clust_num,HH_num,childLN, mort))

dim(dfull)
dim(mort)
dfull <- full_join(dfull, mort, by=c("country","clust_num","HH_num","childLN"))
dim(dfull)

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

#Clean data for figures
dfull <- dfull %>% 
  mutate(
    # Y=case_when(
    #   Y=="stunt" ~ "Stunting",
    #   Y=="wast" ~ "Wasting",
    #   Y=="diarrhea" ~ "Diarrhea",
    #   Y=="ari" ~ "ARI",
    #   Y=="haz" ~ "HAZ",
    #   Y=="whz" ~ "WHZ",
    #   Y=="mort" ~ "Mortality"
    # ),
    # Y=factor(Y, levels=c("Diarrhea", "Stunting", "Wasting", "ARI", "HAZ", "WHZ","Mortality")),
    country=case_when(
      country=="PakistanPunjab" ~ "Pakistan",
      country=="LaoPDR" ~ "Laos",
      country=="SierraLeone" ~ "Sierra Leone",
      country=="Sao Tome and Principe" ~ "Sao Tome+Prin.",
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
    region=factor(region, levels=(c("WCA", "ESA", "LAC", "SA","EAP","MENA","ECA")))) %>%
    arrange(region, country) %>%
    mutate(country=factor(country, levels=unique(country)))

  #   Xlab2 = case_when(X=="EC_H" ~ "Contaminated HH water", 
  #                     X=="EC_S" ~ "Contaminated source water", 
  #                     X=="san_imp" ~ "Unimproved sanitation", 
  #                     X=="wat_imp" ~ "Unimproved water supply", 
  #                     X=="hyg_imp" ~ "Unimproved hygiene", 
  #                     X=="WASH" ~ "Not improved WASH with no contamination",
  #                     X=="WASH_noEC" ~ "Unimproved WASH",
  #                     X=="safely_manH20" ~ "Unsafely managed drinking water",
  #                     X=="EC_risk_H" ~ "HH water contamination", 
  #                     X=="EC_risk_S" ~ "Source water contamination", 
  #                     X=="san_imp_cat" ~ "Sanitation category", 
  #                     X=="wat_imp_cat" ~ "Water supply category", 
  #                     X=="hyg_imp_cat" ~ "Hygiene category"),
  #   Xlab2=factor(Xlab2, levels = rev(c(
  #     "Unimproved water supply", 
  #     "Unimproved sanitation", 
  #     "Unimproved hygiene", 
  #     "Unimproved WASH",
  #     "Contaminated HH water", 
  #     "Contaminated source water", 
  #     "Unsafely managed drinking water",
  #     "Not improved WASH with no contamination",
  #     "HH water contamination", 
  #     "Source water contamination", 
  #     "Sanitation category", 
  #     "Water supply category", 
  #     "Hygiene category"))),





Wvars <- c("educ",
           "mage",
           "aged",
           "sex",
           "birthord", 
           "rural",
           "everbf", 
           "currbf",
           "nhh",
           "nchild5",
           "floor",
           "cookstove",
           "chimney",
           "fuel",
           "roof",
           "wall",
           "own_animals",
           "HHwealth_quart",
           "nroom_sleeping")




#table1
Wvars[!(Wvars %in% colnames(dfull))]
df <- dfull %>% subset(., select = c("country", Wvars))
tab1 <- table1(~. |country, format_number = TRUE, data=df)

tab1 <- as.data.frame(read_html(tab1) %>% html_table(fill=TRUE))

#Drop country tabs
tab1 <- tab1[-c(1:27),]
tab1$Var.1

#put total column first
tab1 <- tab1[,c(1,ncol(tab1),2:(ncol(tab1)-1))]


tab1 <- tab1 %>%
  mutate(Var.1=case_when(
    Var.1== "HHwealth_quart" ~ "Asset-based wealth index",
    Var.1== "educ" ~ "Maternal education",
    Var.1== "mage" ~ "Maternal age",
    Var.1== "everbf" ~ "Ever breastfed",
    Var.1== "currbf" ~ "Currently breastfeeding",
    Var.1== "aged" ~ "Child age",
    Var.1== "sex" ~ "Child sex",
    Var.1== "birthord" ~ "Birth order",
    Var.1== "rural" ~ "Urban/rural location",
    Var.1== "nhh" ~ "Number of household residents",
    Var.1== "nchild5" ~ "Number of children under 5yrs",
    Var.1== "floor" ~ "Type of flooring",
    Var.1== "roof" ~ "Type of roof",
    Var.1== "wall" ~ "Type of wall",
    Var.1== "own_animals" ~ "Animal ownership",
    Var.1== "cookstove" ~ "Cooking stove type",
    Var.1== "fuel" ~ "Cooking fuel",
    Var.1== "chimney" ~ "Chimney in kitchen",
    Var.1== Var.1 ~ Var.1
  ))

tab1[tab1=="NA (NA)"] <- "0 (0%)"
tab1[tab1=="NA [NA, NA]"] <- "0 [0, 0]"
colnames(tab1) <- gsub(".N."," ",colnames(tab1))
colnames(tab1) <- gsub("\\."," ",colnames(tab1))
colnames(tab1) <- gsub("[0123456789]+"," ",colnames(tab1))
colnames(tab1) <- gsub("   ","",colnames(tab1))
colnames(tab1) <- gsub("  ","",colnames(tab1))


saveRDS(tab1, file=here("tables/tab1.rds"))

#child health data
ch <- dfull %>% filter(!is.na(haz) | !is.na(waz) | !is.na(ari) | !is.na(diarrhea) | !is.na(mort)) %>%
  distinct(country, clust_num, HH_num, childLN, .keep_all = T)


#Make a N children and child age/ num kids per HH table
child_tab <- ch %>% group_by(country) %>%
  summarise(`Number of\nchildren`=n(), `Mean\nchild age`=mean(aged, na.rm=T)/365)

anthro_tab <- ch %>% group_by(country) %>%
  summarise(`N HAZ`=sum(!is.na(haz)), `Mean HAZ`=mean(haz, na.rm=T), `Prev. Stunting`=mean(haz < (-2), na.rm=T)*100, 
            `N WHZ`=sum(!is.na(whz)), `Mean WHZ`=mean(whz, na.rm=T), `Prev. Wasting`=mean(whz < (-2), na.rm=T)*100,)
            
    

infection_tab <- ch %>% group_by(country) %>%
  summarise(N_diarrhea_meas=sum(!is.na(diarrhea)), N_diarrhea=sum(diarrhea, na.rm=T),  Prev_diarrhea=mean(diarrhea, na.rm=T)*100,
            N_ARI_meas=sum(!is.na(ari)), N_ARI=sum(ari, na.rm=T),  Prev_ARI=mean(ari, na.rm=T)*100)



#HH data
d <- dfull %>% filter(!is.na(san_imp) | !is.na(wat_imp) | !is.na(EC_S) | !is.na(EC_H)) %>%
  distinct(country, clust_num, HH_num, .keep_all = T)


wat_imp_cat <- tab_cat(d, "wat_imp_cat")
san_imp_cat <- tab_cat(d, "san_imp_cat")
hyg_imp_cat <- tab_cat(d, "hyg_imp_cat")
EC_risk_H <- tab_cat(d, "EC_risk_H")
EC_risk_S <- tab_cat(d, "EC_risk_S")


#Make combined table of improved water/san/hygeine
wat_imp <- tab_cat(d, "wat_imp")
san_imp <- tab_cat(d, "san_imp")
hyg_imp <- tab_cat(d, "hyg_imp")
WASH_noEC <- tab_cat(d, "WASH_noEC")

WASHtab <- as.data.frame(bind_cols(wat_imp[,1],san_imp[,1],hyg_imp[,1],WASH_noEC[,1]))
rownames(WASHtab) <- rownames(wat_imp)
colnames(WASHtab) <- c("Improved\nWater","Improved\nSanitation","Improved\nHygeine","Improved\nWASH")


#Sample characteristics table
HH_tab <- d %>% group_by(country) %>% summarize(`N clusters`=length(unique(clust_num)), `N households`=n())
HH_tab <- left_join(HH_tab, child_tab, by="country") 


#Make N (%) in the outcome tables and use   cattab[,1]<- prettyNum(cattab[,1],big.mark=",")


#Save tables
save(tab1, WASHtab, wat_imp_cat, san_imp_cat, hyg_imp_cat, EC_risk_H, EC_risk_S,
     HH_tab, anthro_tab, infection_tab,
     file=here("tables/table_objects.Rdata"))

            
