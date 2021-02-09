
#Make sure to subset HH measurements to the unique household level... right now they are duplicated based on merges with child ID

rm(list=ls())
source("0-config.R")
library(table1)
library(rvest)


#Make function out of this then combine N's and percents
tab_cat <- function(d, x){
  d <- d %>% rename(x=!!(x))
  df <- d %>% tabyl(Country, x) 
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


dfull_noMort <- readRDS(here("data/compiled_clean_MICS_survey.rds"))%>%
  subset(., select= - c(mort))
mort <- readRDS(here("data/compiled_clean_MICS_mortality.rds")) %>%
  subset(., select=c(country,clust_num,HH_num,childLN, mort))

dim(dfull_noMort)
dim(mort)
dfull <- full_join(dfull_noMort, mort, by=c("country","clust_num","HH_num","childLN")) %>% rename(Country=country)
dim(dfull)

#Set up vector
EAP <- c("Mongolia", "Tonga",  "Kiribati", "Laos")
ECA <- c("Georgia", "Kosovo")
LAC <- c("Suriname","Paraguay" )
MENA <- c("Algeria","Iraq","Tunisia" )
SA <- c("Bangladesh", "Nepal", "Pakistan")
ESA <- c("Lesotho", "Madagascar",  "Zimbabwe")
WCA <- c("Chad","CAR","Central African Rep.","Cote d'Ivoire","Congo",  "DRC", "Dem. Rep. of the Congo", "Gambia", "Ghana", "Guinea Bissau", "Nigeria", "Togo", "Sierra Leone", "Sao Tome+Prin.")

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
    Country=case_when(
      Country=="PakistanPunjab" ~ "Pakistan",
      Country=="LaoPDR" ~ "Laos",
      Country=="SierraLeone" ~ "Sierra Leone",
      Country=="Sao Tome and Principe" ~ "Sao Tome+Prin.",
      Country=="CAR" ~ "Central African Rep.",
      Country=="CoteIvoire" ~ "Cote d'Ivoire",
      Country=="DRC" ~ "Dem. Rep. of the Congo",
      Country==Country ~ Country
    ),
    region = case_when(
      Country %in% EAP ~ "EAP",
      Country %in% ECA ~ "ECA",
      Country %in% LAC ~ "LAC",
      Country %in% MENA ~ "MENA",
      Country %in% SA ~ "SA",
      Country %in% ESA ~ "ESA",
      Country %in% WCA ~ "WCA",
      Country %in% c("Pooled - FE","Pooled - RE") ~ "Pooled"
    ),
    region=factor(region, levels=(c("WCA", "ESA", "LAC", "SA","EAP","MENA","ECA")))) %>%
    arrange(region, Country) %>%
    mutate(Country=factor(Country, levels=unique(Country)))




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
df <- dfull %>% filter(!is.na(san_imp) | !is.na(wat_imp) | !is.na(hyg_imp) | !is.na(EC_S) | !is.na(EC_H)) %>%
  subset(., select = c("Country", Wvars))
tab1 <- table1(~. |Country, format_number = TRUE, data=df)

tab1 <- as.data.frame(read_html(tab1) %>% html_table(fill=TRUE))

#Drop Country tabs
tab1 <- tab1[-c(1:30),]
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
    Var.1== "nroom_sleeping" ~ "Number of bedrooms",
    Var.1== Var.1 ~ paste0("   ",Var.1)
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
ch <- dfull %>% filter(!is.na(haz) | !is.na(whz) | !is.na(ari) | !is.na(diarrhea) | !is.na(mort)) %>%
  distinct(Country, clust_num, HH_num, childLN, .keep_all = T)


#Make a N children and child age/ num kids per HH table
child_tab <- ch %>% group_by(Country) %>%
  summarise(`Number of\nchildren`=n(), `Mean\nchild age`=mean(aged, na.rm=T)/365)

anthro_tab <- ch %>% group_by(Country) %>%
  summarise(`N HAZ`=sum(!is.na(haz)), `Mean HAZ`=mean(haz, na.rm=T), `Prev. Stunting`=mean(haz < (-2), na.rm=T)*100, 
            `N WHZ`=sum(!is.na(whz)), `Mean WHZ`=mean(whz, na.rm=T), `Prev. Wasting`=mean(whz < (-2), na.rm=T)*100,)
            
    

infection_tab <- ch %>% group_by(Country) %>%
  mutate(mort=as.numeric(mort)) %>%
  summarise(N_diarrhea_meas=sum(!is.na(diarrhea)), N_diarrhea=sum(diarrhea, na.rm=T),  Prev_diarrhea=mean(diarrhea, na.rm=T)*100,
            N_ARI_meas=sum(!is.na(ari)), N_ARI=sum(ari, na.rm=T),  Prev_ARI=mean(ari, na.rm=T)*100,
            N_mort_meas=sum(!is.na(mort)), N_mort=sum(mort, na.rm=T),  Prev_mort=mean(mort, na.rm=T)*100) %>%
  as.data.frame()
infection_tab[is.na(infection_tab)] <- 0
colnames(infection_tab) <- c("Country","N diarrhea meas", "Diarrhea cases", "Prev. diarrhea", "N ARI meas.", "ARI cases", "Prev. ARI" , "N mortality meas.", "Deaths", "Mortality rate")

#HH data
d <- dfull %>% filter(!is.na(san_imp) | !is.na(wat_imp) | !is.na(EC_S) | !is.na(EC_H)) %>%
  distinct(Country, clust_num, HH_num, .keep_all = T)


wat_imp_cat <- tab_cat(d, "wat_imp_cat")
san_imp_cat <- tab_cat(d, "san_imp_cat")
hyg_imp_cat <- tab_cat(d, "hyg_imp_cat")
EC_risk_H <- tab_cat(d, "EC_risk_H") %>% rename("Low risk"="1", "Moderate risk"="2", "High risk"="3",  "Very high risk"="4")
EC_risk_S <- tab_cat(d, "EC_risk_S") %>% rename("Low risk"="1", "Moderate risk"="2", "High risk"="3",  "Very high risk"="4")


#Make combined table of improved water/san/hygeine
wat_imp <- tab_cat(d, "wat_imp")
san_imp <- tab_cat(d, "san_imp")
hyg_imp <- tab_cat(d, "hyg_imp")
WASH_noEC <- tab_cat(d, "WASH_noEC")

WASHtab <- as.data.frame(bind_cols(wat_imp[,1],san_imp[,1],hyg_imp[,1],WASH_noEC[,1]))
rownames(WASHtab) <- rownames(wat_imp)
colnames(WASHtab) <- c("Improved\nWater","Improved\nSanitation","Improved\nHygeine","Improved\nWASH")


#Sample characteristics table
HH_tab <- d %>% group_by(Country) %>% summarize(`N clusters`=length(unique(clust_num)), `N households`=n())
HH_tab <- left_join(HH_tab, child_tab, by="Country") 


#Make N (%) in the outcome tables and use   cattab[,1]<- prettyNum(cattab[,1],big.mark=",")

# #Outcome missingness
# #table1
# Yvars = c("diarrhea", "ari", "mort", "haz","whz", "stunt", "wast")
# Yvars = c("diarrhea", "ari", "haz","whz", "stunt", "wast")
# 
# df <- dfull_noMort %>%
#   distinct(country, clust_num, HH_num, childLN, .keep_all = T) %>% subset(., select = c("country", Yvars)) %>% 
#   filter(!is.na(haz) | !is.na(whz) | !is.na(ari) | !is.na(diarrhea)) 
# df2 <- mort %>% 
#   distinct(country, clust_num, HH_num, childLN, .keep_all = T) %>% subset(., select = c("country", "mort"))
# 
# 
# miss_tab = df %>% 
#   group_by(country) %>%
#   summarise_each(funs(mean(is.na(.))*100))
# miss_tab2 = df2 %>% 
#   group_by(country) %>%
#   summarise_each(funs(mean(is.na(.))*100))
# 
# colnames(miss_tab) <- c("Country", "Diarrhea", "ARI", "Mortality", "HAZ", "WHZ", "Stunting", "Wasting")
# 

#Save tables
save(tab1, WASHtab, wat_imp_cat, san_imp_cat, hyg_imp_cat, EC_risk_H, EC_risk_S,
     HH_tab, anthro_tab, infection_tab, 
     file=here("tables/table_objects.Rdata"))

            
