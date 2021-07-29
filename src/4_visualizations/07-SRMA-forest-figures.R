
rm(list=ls())
source("0-config.R")

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


#load data
d <- readRDS(here("results/SRMA_comp_adjusted_RR.rds")) %>%
   rename(est=RR) %>%
   mutate(
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
      region=factor(region, levels=rev(c("WCA", "ESA", "LAC", "SA","EAP","MENA","ECA","Pooled"))),
      country=factor(country, levels=rev(c(WCA, ESA,LAC,SA,EAP,MENA,ECA, "Pooled - FE","Pooled - RE")))
   ) %>%  
   subset(., select =c(country, region, Y, X, ref, contrast, est,ci.lb, ci.ub, n,N))
dRE <- readRDS(here("results/pooled_SRMA_results.rds"))%>%
   rename(est=RR, ci.lb=RR.CI1, ci.ub=RR.CI2)  %>%
   subset(., select =c(Y, X, ref, contrast, est,  ci.lb, ci.ub)) %>%
   mutate(country="Pooled - RE", region="pooled",binary=1)
dFE <- readRDS(here("results/pooled_SRMA_results_FE.rds"))%>%
   rename(est=RR, ci.lb=RR.CI1, ci.ub=RR.CI2)  %>%
   subset(., select =c(Y, X, ref, contrast, est,  ci.lb, ci.ub)) %>%
   mutate(country="Pooled - FE", region="pooled",binary=1)
d_unadj <- readRDS(here("results/pooled_SRMA_results_unadj.rds"))%>%
   rename(est=RR, ci.lb=RR.CI1, ci.ub=RR.CI2)  %>%
   subset(., select =c(Y, X, ref, contrast, est,  ci.lb, ci.ub)) %>%
   mutate(country="Pooled - unadj.", region="pooled",binary=1)


d <- bind_rows(d ,dRE, dFE, d_unadj)
   



#Clean data for figures
   

df <- d %>% 
   mutate(
      X = factor(X),
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
         country %in% c("Pooled - FE","Pooled - RE", "Pooled - unadj.") ~ "Pooled"
      ),
      region=factor(region, levels=rev(c("WCA", "ESA", "LAC", "SA","EAP","MENA","ECA","Pooled"))),
      country=factor(country, levels=rev(c(WCA, ESA,LAC,SA,EAP,MENA,ECA,"Pooled - unadj.", "Pooled - FE","Pooled - RE"))),
      multinomial = ifelse(X %in% c("EC_risk_H", "EC_risk_S", "wat_imp_cat", "san_imp_cat", "hyg_imp_cat","Piped_san_cat","san_coverage"),1,0),
      Y=case_when(
         Y=="stunt" ~ "Stunting",
         Y=="wast" ~ "Wasting",
         Y=="diarrhea" ~ "Diarrhea",
         Y=="ari" ~ "ARI",
         Y=="haz" ~ "HAZ",
         Y=="whz" ~ "WHZ",
         Y=="mort" ~ "Mortality"
      ),
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
           "Basic, non-sewer","Sewered", 
           "Treated", "Untreated",
           "High coverage",  "Continuous",  "Basic", "Limited",   "None", 
           "Surface water", "Unimproved", "No facility",
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
   arrange(Y,X,region, binary, country) %>%
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




# levels(d$Y)
# unique(d$X)
# d$X <- factor(d$X, levels =c("EC_risk_H", "EC_risk_S", "wat_imp_cat", "san_imp_cat","hyg_imp_cat",
#                              "EC_H","EC_S",  "wat_imp", "san_imp", "hyg_imp", "WASH", "safely_manH20", "WASH_noEC",
#                              "piped_san" ,"san_imp_cat2",
#                              "Piped_san_cat",
#                              "san_coverage",
#                              "imp_off_prem_V_unimp",     
#                              "imp_on_prem_V_imp_off_prem",
#                              "imp_on_prem_HQ_V_imp_on_prem_LQ",
#                              "imp_on_prem_sufficient_V_imp_on_prem_insufficient",  
#                              "imp_on_prem_sufficient_HQ_V_imp_on_prem_insufficient_LQ"))
# levels(d$X)


d<-df
j <- levels(d$X)[1]
plist <- list()

#Add pooled FE to multinomial...


 for(j in levels(d$X)){
   df <- d %>% filter( X==j) %>% 
      droplevels(.) 
   
   if(nrow(df)>0){
   
   Xlabel <- as.character(df$X[1])  
   Xlabel <- gsub("_"," ",Xlabel)
   Xlabel <- gsub(" V","\nVS ",Xlabel)
   reference <- df$ref[1]  
   
    p <-  ggplot(df, aes(y=est, x=countrylab, color=region)) +
      geom_point(aes(shape=sig)) + 
      geom_linerange(aes(ymin=ci.lb, ymax=ci.ub )) +
      scale_color_manual(values=tableau11[c(1, 8:2,9,10,11)]) +
      geom_hline(yintercept = 1) +
      geom_vline(xintercept = 3.5, linetype="dashed") +
      scale_y_continuous(trans='log10') +
       scale_shape_manual(values=c(19,13), guide=FALSE) +
      coord_flip() + 
      xlab("") + ylab(paste0("Relative Risk (ref=",reference,")")) +
       ggtitle(paste0("Exposure: ", Xlabel))
   
   
    plist[[length(plist)+1]] <- p
    names(plist)[length(plist)] <- j
   }
  } 


 plist[[1]]



#-------------------------------------------------------------
# save figures
#-------------------------------------------------------------

saveRDS(plist, file=here("figures/forest_figure_objects_SRMA.rds"))






 


