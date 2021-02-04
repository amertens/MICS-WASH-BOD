
#Make sure to subset HH measurements to the unique household level... right now they are duplicated based on merges with child ID


source("0-config.R")
library(table1)
library(rvest)

dfull <- readRDS(here("data/compiled_clean_MICS_survey.rds"))

dfull <- dfull %>% mutate(
  country=case_when(
    country=="PakistanPunjab" ~ "Pakistan",
    country=="LaoPDR" ~ "Laos",
    country=="SierraLeone" ~ "Sierra Leone",
    country=="Sao Tome and Principe" ~ "Sao Tome+Prin.",
    country==country ~ country
  )
)

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
tab1 <- table1(~. |country, data=df)

tab1 <- as.data.frame(read_html(tab1) %>% html_table(fill=TRUE))

#Drop country tabs
tab1 <- tab1[-c(1:27),]
tab1$Var.1

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
ch <- dfull %>% filter(!is.na(haz) | !is.na(waz) | !is.na(ari) | !is.na(diarrhea))


#Make a N children and child age/ num kids per HH table
child_tab <- ch %>% group_by(country) %>%
  summarise(N=n(), child_age=mean(aged, na.rm=T)/365, min_child_age=min(aged, na.rm=T)/365, max_child_age=max(aged, na.rm=T)/365)



anthro_tab <- ch %>% group_by(country) %>%
  summarise(N_haz=sum(!is.na(haz)), Mean_HAZ=mean(haz, na.rm=T), Prev_Stunting=mean(haz < (-2), na.rm=T)*100, Prev_Sev_Stunting=mean(haz < (-3), na.rm=T)*100,
            N_whz=sum(!is.na(whz)), Mean_WHZ=mean(whz, na.rm=T), Prev_Wasting=mean(whz < (-2), na.rm=T)*100, Prev_Sev_Wasting=mean(whz < (-3), na.rm=T)*100)
            
    

infection_tab <- ch %>% group_by(country) %>%
  summarise(N_diarrhea_meas=sum(!is.na(diarrhea)), N_diarrhea=sum(diarrhea, na.rm=T),  Prev_diarrhea=mean(diarrhea, na.rm=T)*100,
            N_ARI_meas=sum(!is.na(ari)), N_ARI=sum(ari, na.rm=T),  Prev_ARI=mean(ari, na.rm=T)*100)


#HH data
d <- dfull %>% filter(!is.na(san_imp) | !is.na(wat_imp) | !is.na(EC_S) | !is.na(EC_H)) %>%
  distinct(country, clust_num, HH_num, .keep_all = T)

d %>% group_by(country) %>%
  summarise(N_households=n(), N_imp_wat=sum(as.numeric(wat_imp)-1, na.rm=T), N_imp_san=sum(as.numeric(san_imp)-1, na.rm=T),  N_imp_hygeine=sum(as.numeric(hyg_imp)-1, na.rm=T), N_imp_WASH=sum(as.numeric(WASH_noEC)-1, na.rm=T))
    
d %>% group_by(country) %>%
  summarise(N_households=n(), N_EC_H=sum(as.numeric(EC_H)-1, na.rm=T), N_EC_S=sum(as.numeric(EC_S)-1, na.rm=T), N_safely_manH20=sum(as.numeric(safely_manH20)-1, na.rm=T),  N_imp_WASH_noEC=sum(as.numeric(WASH)-1, na.rm=T))


d %>% tabyl(country, wat_imp_cat)
d %>% tabyl(country, san_imp_cat)
d %>% tabyl(country, hyg_imp_cat)
d %>% tabyl(country, EC_risk_H)
d %>% tabyl(country, EC_risk_S)



#Make function out of this then combine N's and percents
tab_cat <- function(d, x){
  d <- d %>% rename(x=!!(x))
  df <- d %>% tabyl(country, x) 
  cattab <- df[,-c(1,ncol(df))]
  rowtotal <- rowSums(cattab)
  rownames(cattab) <- df[,1]
  df2 <- round(prop.table(as.matrix(cattab),1)*100, 1)    
  
  
  
  cattab <- as.matrix(cattab)
  df2 <- as.matrix(df2)
  tab <- as.data.frame(matrix( paste0(cattab, " (", df2,"%)", sep=""), 
                               nrow=nrow(df), dimnames=dimnames(cattab) ))
  tab$Missing <- df[,ncol(df)]
  tab$Total <- rowtotal
  tab$Missing <- df[,ncol(df)]
  return(tab)
}

tab_cat(d, "wat_imp_cat")
tab_cat(d, "san_imp_cat")
tab_cat(d, "hyg_imp_cat")
tab_cat(d, "EC_risk_H")
tab_cat(d, "EC_risk_S")


            
            
