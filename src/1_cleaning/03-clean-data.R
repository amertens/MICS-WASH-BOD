
#------------------------------------------------------
# load packages and data
#------------------------------------------------------

rm(list=ls())
source("0-config.R")


dfull <- readRDS(here("data/compiled_raw_MICS_survey.rds"))

d <- dfull



#------------------------------------------------------
# clean identifiers
#------------------------------------------------------

d$HH_num <- as.numeric(d$HH_num)
d$clust_num <- as.numeric(d$clust_num)
d$ecpopweight_H <- as.numeric(d$ecpopweight_H)
d$ecpopweight_S <- as.numeric(d$ecpopweight_S)
d$popweight <- as.numeric(d$popweight)

d$EC_cfu_H <- as.numeric(d$EC_cfu_H)
d$EC_cfu_S <- as.numeric(d$EC_cfu_S)


#------------------------------------------------------
# clean outcomes and exposures
#------------------------------------------------------

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
    ),
    EC_risk_H = factor(EC_risk_H, levels=c("1","2","3","4")),
    EC_risk_S = factor(EC_risk_S, levels=c("1","2","3","4"))
  )

table(d$EC_risk_S)
table(d$EC_risk_H)
table(d$country, d$EC_risk_S)
table(d$country, d$EC_risk_H)
table(is.na(d$EC_risk_S))
table(is.na(d$EC_risk_H))
prop.table(table(is.na(d$EC_risk_S)))
prop.table(table(is.na(d$EC_risk_H)))

#code handwashing
#Only households where handwashing facility was observed by the interviewer (HW1=1, 2, 3) 
#and households with no handwashing facility (HW1=4) are included in the denominator of 
#the indicator (HW1=5, 6, and 9 [if any] are excluded). Households with water at handwashing 
#facility (HW2=1) and soap or other cleansing agent at handwashing facility 
#(HW7=A or B) are included in the numerator.

#Denominators are obtained by weighting the number of households by the total number of 
#household members (HH48).
# table(d$HW1)
# table(d$HW2)
# table(d$HW7A)
# table(d$HW7B)
# table(d$HW7C)

# table(d$country, d$HW2)
# table(d$country, d$HW7A)
# table(d$country, d$HW7B)
# 
# 
# table(d$country, d$HW3BA)
# table(d$country, d$HW3BB)
# table(d$country, d$HW3BC)
# table(d$country, d$HW3A)

table(d$HW7A, d$HW3BA)
table(d$HW7A=="A", d$HW3BA=="A")



d$hyg_imp <- factor(ifelse(d$HW2==1 & 
                             ((d$HW7A=="A" & !is.na(d$HW7A))|
                                (d$HW7B=="B" & !is.na(d$HW7B))|
                                (d$HW3BA=="A" & !is.na(d$HW3BA))|
                                (d$HW3BB=="B" & !is.na(d$HW3BB))|
                                (d$HW3BC=="C" & !is.na(d$HW3BC))), 
                           "Improved","Unimproved"), levels=c("Improved","Unimproved"))
d$hyg_imp[(d$HW2==9 | d$HW1>4 | 
             (is.na(d$HW7A)&is.na(d$HW7B)&is.na(d$HW3BA)&is.na(d$HW3BB)&is.na(d$HW3BC)))] <- NA
table(d$hyg_imp)
prop.table(table(d$hyg_imp))
table(d$country, d$hyg_imp)
prop.table(table(d$country, d$hyg_imp),1)




# 1.	None (no facility)
# 2.	Limited - availability of a handwashing facility on premises without soap or water
# 3.	Basic - availability of a handwashing facility on premises with soap and water (highest measured level of service measured)
table(d$country,d$HW2)
d <- d %>% mutate(
  hyg_imp_cat = case_when(
    HW2 == 2 ~"None",
    HW2 == 1 & hyg_imp != "Improved" ~"Limited",
    hyg_imp == "Improved" ~"Basic"
  ),
  hyg_imp_cat = factor(hyg_imp_cat, levels=c( "Basic", "Limited","None"))
)
d$hyg_imp_cat[is.na(d$HW2)] <- NA
table(d$hyg_imp_cat)
table(d$country,d$hyg_imp_cat)


#Recode improved sanitation and water
#https://www.who.int/water_sanitation_health/monitoring/oms_brochure_core_questionsfinal24608.pdf
d<- d %>% mutate(
  san_imp = case_when(
    san_imp==0 ~ "Unimproved",
    san_imp==1 ~ "Improved",
    is.na(san_imp) ~ NA_character_
  ),
  wat_imp = case_when(
    wat_imp==0 ~ "Unimproved",
    wat_imp==1 ~ "Improved",
    is.na(wat_imp) ~ NA_character_
  )
)


table(d$san_imp)
d$san_imp<-factor(d$san_imp, levels=c("Improved","Unimproved"))
table(d$san_imp)
table(d$country, d$san_imp)

table(d$wat_imp)
d$wat_imp<-factor(d$wat_imp, levels=c("Improved","Unimproved"))
table(d$wat_imp)
table(d$country, d$wat_imp)


#Code piped versus non-piped sanination category
d$piped_san <- factor(ifelse(d$WS11=="11","Piped","Not piped"), levels=c("Piped","Not piped"))
d$piped_san[d$WS11 %in% c("95","96","99")|is.na(d$WS11)] <- NA
table(d$piped_san)


#Recode categorical sanitation and water
# 1.	Open defecation (no service)
# 2.	Unimproved - use of pit latrines without a slab or platform, hanging latrines or bucket latrines
# 3.	Limited - use of improved facilities (flush/pour flush to piped sewer system, septic tanks or pit latrines; ventilated improved pit latrines, composting toilets or pit latrines with slabs) shared between two or more households
# 4.	Basic - use of improved facilities which are not shared with other households
# 5.	Basic with high community coverage (>75% of the population in the community use basic sanitation services, highest measured level of service)

table(d$WS11_lab, d$WS11)
table(d$san_cat_lab)
table(d$WS15, d$WS15_lab)
table(d$country, d$WS15, d$san_cat_lab)
table(d$country, is.na(d$WS15), d$san_cat_lab)

#calculate levels without community coverage
d <- d %>% mutate(
  san_imp_cat_basic = case_when(san_cat_lab=="No facility"~"No facility",
                          san_cat_lab=="Unimproved"~"Unimproved",
                          san_cat_lab=="Improved" & (WS15!="2"|is.na(WS15))~"Limited", #Note is.na is not needed currently because no improved is missing WS15
                          san_cat_lab=="Improved" & (WS15=="2")~"Basic"
  ))
table(d$san_imp_cat_basic)
table(is.na(d$san_imp_cat_basic), d$san_imp)
table(d$country, d$san_imp_cat_basic)

#calculate community coverage 
d <- d %>% group_by(country, clust_num) %>% 
  mutate(san_coverage= mean(san_imp_cat_basic=="Basic"), high_coverage=ifelse(san_coverage>0.75,1,0)) %>%
  ungroup()

summary(d$san_coverage)
prop.table(table(d$san_imp_cat_basic, d$high_coverage),1)
table(d$san_cat_lab, d$high_coverage, d$WS15)

table(d$country, d$WS15)
table(d$country, d$san_cat_lab, d$WS15)

#add in community coverage level
d <- d %>% mutate(
  san_imp_cat = case_when(san_cat_lab=="No facility"~"No facility",
                           san_cat_lab=="Unimproved"~"Unimproved",
                           san_cat_lab=="Improved" & (WS15!="2"|is.na(WS15))~"Limited",
                          san_cat_lab=="Improved" & (WS15=="2") & high_coverage==0~"Basic",
                          san_cat_lab=="Improved" & (WS15=="2") & high_coverage==1~"High coverage"
                           ),
  san_imp_cat = factor(san_imp_cat, levels=rev(c("No facility", "Unimproved", "Limited", "Basic", "High coverage")))
)
table(d$san_imp_cat)
table(d$country, d$san_imp_cat)
table(d$san_imp, d$san_imp_cat)


#Make 2nd category with Safely Managed as the highest level of service
#https://washdata.org/monitoring/sanitation
d <- d %>% mutate(
  san_imp_cat2 = case_when(san_cat_lab=="No facility"~"No facility",
                          san_cat_lab=="Unimproved"~"Unimproved",
                          san_cat_lab=="Improved" & (WS15!="2"|is.na(WS15))~"Limited",
                          san_cat_lab=="Improved" & (WS15=="2") & WS12 %in% c("1","3") & WS13 %in% c("1","11","2","3","31","21","4","41")~"Safely managed",
                          san_cat_lab=="Improved" & (WS15=="2")~"Basic"
  ),
  san_imp_cat2 = factor(san_imp_cat2, levels=rev(c("No facility", "Unimproved", "Limited", "Basic", "Safely managed")))
)
table(d$san_imp_cat2)
prop.table(table(d$san_imp_cat2))*100
table(d$country, d$san_imp_cat2)
table(d$san_imp_cat, d$san_imp_cat2)

#Piped_san_cat
# a. Unimproved (no facility, unimproved and limited combined) (baseline)
# b. Basic, non-sewer
# c. Basic connected to sewer
table(d$san_imp_cat_basic, d$piped_san)
d <- d %>% mutate(
  Piped_san_cat = case_when(
    san_imp_cat_basic!="Basic" ~ "Unimproved",
    piped_san=="Not piped" ~ "Basic, non-sewer",
    piped_san=="Piped" ~ "Sewered"
  ),
  Piped_san_cat = factor(Piped_san_cat, levels = rev(c("Sewered","Basic, non-sewer","Unimproved")))
)
table(d$Piped_san_cat)
table(d$Piped_san_cat, d$san_imp_cat_basic, d$piped_san)

# san_coverage
# a. Unimproved (no facility, unimproved and limited combined) (baseline)
# b. Basic (<75% coverage)
# c. Basic (???75% coverage)
table(d$san_imp_cat)
d <- d %>% mutate(
  san_coverage = case_when(
    san_imp_cat %in% c("Limited", "Unimproved", "No facility") ~ "Unimproved",
    san_imp_cat=="Basic" ~ "Basic",
    san_imp_cat=="High coverage" ~ "High coverage"
  ),
  san_coverage = factor(san_coverage, levels = rev(c("High coverage","Basic","Unimproved")))
)
table(d$san_coverage)
table(d$country, d$san_coverage)


# 1.	Surface water (no service)
# 2.	Unimproved - drinking water from an unprotected dug well or unprotected spring
# 3.	Limited - drinking water from an improved source for which collection time exceeds 30 minutes for a roundtrip including queuing.
# 4.	Basic - drinking water from an improved source (piped water, boreholes or tubewells, protected dug wells, protected springs, rainwater, and packaged or delivered water), provided collection time is not more than 30 minutes for a roundtrip including queuing,
# 5.	Continuous drinking water on premises from an improved water source (highest measured level of service).


d$continious_wat <- ifelse(d$WS3<=2 & d$wat_class_lab=="Piped water", 1, 0)
d$continious_wat[is.na(d$WS3)|d$WS3==9|d$wat_class_lab=="Missing"|is.na(d$wat_class_lab)] <- NA
table(d$continious_wat)


# table(d$WS3, d$wat_class_lab)
# table(is.na(d$WS3), d$wat_class_lab)
# table(d$WS4, d$wat_class_lab)
# table(is.na(d$WS4), d$wat_class_lab)
# table(d$WS7, d$wat_class_lab)
# table(is.na(d$WS7), d$wat_class_lab)

# table(d$WS7, d$WS7_lab)
# 
# table(d$WS3, d$WS7, d$wat_imp)
# table(d$WS3, d$WS7, is.na(d$WS4))
# 
# 
# table(d$country, d$wat_class_lab)
# table(d$country, d$wat_imp)
# table(d$wat_class_lab, d$WS4)
# table(d$wat_class_lab, is.na(d$WS3))
# table(d$wat_class_lab, is.na(d$WS4))
# table(d$wat_class_lab, is.na(d$WS3))

# WS3	Location of the water source
# WS4	Time (in minutes) to get water and come back
# WS7	There been any time in the last month without sufficient water


# d <- d %>% mutate(
#   wat_imp_cat = case_when(wat_class_lab=="Surface water"~"Surface water",
#                           wat_class_lab=="Unprotected wells and springs"~"Unimproved",
#                           wat_imp=="Unimproved"~"Unimproved",
#                           wat_imp=="Improved" & WS4>30  ~"Limited",
#                           wat_imp=="Improved" & (WS4<=30 | is.na(WS4))~"Basic",
#                           wat_imp=="Improved" & (WS4<=30 | is.na(WS4)) & WS3<=2 & WS7==2~"Continuous"
#   ),
#   wat_imp_cat = factor(wat_imp_cat, levels=rev(c("Surface water", "Unimproved", "Limited", "Basic","Continuous")))
# )



d$WS3 <- as.numeric(d$WS3)
d$WS4 <- as.numeric(d$WS4)

d$wat_imp_cat <- NA
d$wat_imp_cat[d$wat_class_lab=="Surface water"] <- "Surface water"
d$wat_imp_cat[d$wat_class_lab=="Unprotected wells and springs"] <- "Unimproved"
d$wat_imp_cat[d$wat_imp=="Improved"& d$WS4>30] <- "Limited"
d$wat_imp_cat[d$wat_imp=="Improved"& (d$WS4<=30 | is.na(d$WS4))] <- "Basic"
#d$wat_imp_cat[d$wat_imp=="Improved"& (d$WS4<=30 | is.na(d$WS4)) &  d$WS3<=2 & d$WS7==2] <- "Continuous"
d$wat_imp_cat[d$wat_imp=="Improved"& (d$WS4<=30 | is.na(d$WS4)) &  d$WS3<=2 & (d$WS7!=1|is.na(d$WS7))] <- "Continuous"
d$wat_imp_cat = factor(d$wat_imp_cat, levels=rev(c("Surface water", "Unimproved", "Limited", "Basic","Continuous")))

table(d$country, d$wat_imp_cat)
d$wat_imp_cat[is.na(d$wat_class_lab)|d$wat_class_lab=="Missing"|is.na(d$wat_imp)] <- NA
#d$wat_imp_cat[d$wat_imp=="Improved" & (d$WS3==9|is.na(d$WS3)|d$WS4>=998|is.na(d$WS4))] <- NA
table(d$country, d$wat_imp_cat)

table((d$wat_class_lab), d$wat_imp)
table(is.na(d$wat_imp_cat), d$wat_imp)
table(d$wat_imp_cat)



#code any contamination
d$EC_S <- factor(ifelse(d$EC_risk_S==1, "Uncontaminated", "Contaminated"), levels=c("Uncontaminated", "Contaminated"))
d$EC_S[is.na(d$EC_risk_S)] <- NA
d$EC_H <- factor(ifelse(d$EC_risk_H==1, "Uncontaminated", "Contaminated"), levels=c("Uncontaminated", "Contaminated"))
d$EC_H[is.na(d$EC_risk_H)] <- NA

#Code safely managed
d$safely_manH20 <- factor(ifelse(d$EC_H== "Uncontaminated" & d$wat_imp== "Improved", "Safe", "Unsafe"), levels=c("Safe", "Unsafe"))
d$safely_manH20[is.na(d$EC_H)|is.na(d$wat_imp)] <- NA
table(d$safely_manH20)
table(d$country, d$safely_manH20)


#Code most-improved WASH
d$WASH <- factor(ifelse(d$san_imp=="Improved" & d$wat_imp=="Improved" & d$hyg_imp=="Improved" & d$EC_H=="Uncontaminated","Improved", "Unimproved"), levels=c("Improved", "Unimproved"))
d$WASH[(d$san_imp=="Improved" | d$wat_imp=="Improved" | d$hyg_imp=="Improved" | d$EC_H=="Uncontaminated") & d$WASH=="Unimproved"] <- NA

#Code most-improved WASH (no contamination measures)
d$WASH_noEC <-  factor(ifelse(d$san_imp=="Improved" & d$wat_imp=="Improved" & d$hyg_imp=="Improved", "Improved", "Unimproved"), levels=c("Improved", "Unimproved"))
d$WASH_noEC[is.na(d$san_imp)|is.na(d$wat_imp)|is.na(d$hyg_imp)] <- NA

table(d$WASH)
table(d$WASH_noEC)


#Code secondary water contrasts
# 1. Improved, not on premise versus unimproved source (unimproved and surface together)
table(d$wat_imp_cat)
d <- d %>% mutate(
  imp_off_prem_V_unimp = case_when(
  wat_imp_cat %in% c("Unimproved","Surface water") ~ "Unimproved",
  wat_imp=="Improved" & WS3 %in% c(3,4) ~ "Improved, off premise"
          ),
  imp_off_prem_V_unimp = factor(imp_off_prem_V_unimp, levels = c("Improved, off premise","Unimproved"))
  )
table(d$imp_off_prem_V_unimp)
table(d$country, d$imp_off_prem_V_unimp)
table(d$country, is.na(d$imp_off_prem_V_unimp))
table(d$wat_imp_cat, is.na(d$imp_off_prem_V_unimp))

# 2. Improved on premise versus Improved, not on premise
d <- d %>% mutate(
  imp_on_prem_V_imp_off_prem = case_when(
    wat_imp=="Improved" & WS3 %in% c(1,2) ~ "Improved, on premise",
    wat_imp=="Improved" & WS3 %in% c(3,4) ~ "Improved, off premise"
    ),
  imp_on_prem_V_imp_off_prem = factor(imp_on_prem_V_imp_off_prem, levels = c("Improved, on premise","Improved, off premise"))
  )
table(d$imp_on_prem_V_imp_off_prem)
table(d$wat_imp_cat, (d$imp_on_prem_V_imp_off_prem))
table(d$wat_imp_cat, is.na(d$imp_on_prem_V_imp_off_prem))

# 3. Improved, on premise, high water quality versus Improved, on premise, not HQ water
d <- d %>% mutate(
  imp_on_prem_HQ_V_imp_on_prem_LQ = case_when(
    wat_imp=="Improved" & WS3 %in% c(1,2) & EC_H=="Uncontaminated" ~ "Improved, on premise, uncontaminated",
    wat_imp=="Improved" & WS3 %in% c(1,2) & EC_H=="Contaminated" ~ "Improved, on premise, contaminated"
  ),
  imp_on_prem_HQ_V_imp_on_prem_LQ = factor(imp_on_prem_HQ_V_imp_on_prem_LQ, levels = c("Improved, on premise, uncontaminated","Improved, on premise, contaminated"))
)
table(d$imp_on_prem_HQ_V_imp_on_prem_LQ)
table(d$country, d$imp_on_prem_HQ_V_imp_on_prem_LQ)

# 4. Improved, on premise, continuous supply/sufficient water versus improved on premise, non-sufficient
d <- d %>% mutate(
  imp_on_prem_sufficient_V_imp_on_prem_insufficient = case_when(
    wat_imp=="Improved" & WS3 %in% c(1,2) & (d$WS7!=1|is.na(d$WS7)) ~ "Improved, on premise, sufficient",
    wat_imp=="Improved" & WS3 %in% c(1,2) & d$WS7==1 ~ "Improved, on premise, insufficient"
  ),
  imp_on_prem_sufficient_V_imp_on_prem_insufficient = factor(imp_on_prem_sufficient_V_imp_on_prem_insufficient, levels = c("Improved, on premise, sufficient","Improved, on premise, insufficient"))
)
table(d$imp_on_prem_sufficient_V_imp_on_prem_insufficient)
table(d$country, d$imp_on_prem_sufficient_V_imp_on_prem_insufficient, d$diarrhea)

# 5. Improved, on premise, HQ water plus sufficient water versus improved, on premise, not HQ water and non-sufficient
d <- d %>% mutate(
  imp_on_prem_sufficient_HQ_V_imp_on_prem_insufficient_LQ = case_when(
    wat_imp=="Improved" & WS3 %in% c(1,2) & (d$WS7!=1|is.na(d$WS7)) & EC_H=="Uncontaminated"  ~ "Improved, on premise, HQ, sufficient",
    wat_imp=="Improved" & WS3 %in% c(1,2) & d$WS7==1 & EC_H=="Contaminated" ~ "Improved, on premise, LQ, insufficient"
  ),
  imp_on_prem_sufficient_HQ_V_imp_on_prem_insufficient_LQ = factor(imp_on_prem_sufficient_HQ_V_imp_on_prem_insufficient_LQ, levels = c("Improved, on premise, HQ, sufficient","Improved, on premise, LQ, insufficient"))
)
table(d$imp_on_prem_sufficient_HQ_V_imp_on_prem_insufficient_LQ)
table(d$country, d$imp_on_prem_sufficient_HQ_V_imp_on_prem_insufficient_LQ)

# 6. Improved, on premise, HQ water plus sufficient water versus improved, on premise, HQ water and non-sufficient
d <- d %>% mutate(
  imp_on_prem_sufficient_HQ_V_imp_on_prem_insufficient_HQ = case_when(
    wat_imp=="Improved" & WS3 %in% c(1,2) & (d$WS7!=1|is.na(d$WS7)) & EC_H=="Uncontaminated"  ~ "Improved, on premise, HQ, sufficient",
    wat_imp=="Improved" & WS3 %in% c(1,2) & d$WS7==1 & EC_H=="Uncontaminated" ~ "Improved, on premise, HQ, insufficient"
  ),
  imp_on_prem_sufficient_HQ_V_imp_on_prem_insufficient_HQ = factor(imp_on_prem_sufficient_HQ_V_imp_on_prem_insufficient_HQ, levels = c("Improved, on premise, HQ, sufficient","Improved, on premise, HQ, insufficient"))
)
table(d$imp_on_prem_sufficient_HQ_V_imp_on_prem_insufficient_HQ)
table(d$country, d$imp_on_prem_sufficient_HQ_V_imp_on_prem_insufficient_HQ)

# 7. Improved, on premise, LQ water plus sufficient water versus improved, on premise, LQ water and non-sufficient
d <- d %>% mutate(
  imp_on_prem_sufficient_LQ_V_imp_on_prem_insufficient_LQ = case_when(
    wat_imp=="Improved" & WS3 %in% c(1,2) & (d$WS7!=1|is.na(d$WS7)) & EC_H=="Contaminated"  ~ "Improved, on premise, LQ, sufficient",
    wat_imp=="Improved" & WS3 %in% c(1,2) & d$WS7==1 & EC_H=="Contaminated" ~ "Improved, on premise, LQ, insufficient"
  ),
  imp_on_prem_sufficient_LQ_V_imp_on_prem_insufficient_LQ = factor(imp_on_prem_sufficient_LQ_V_imp_on_prem_insufficient_LQ, levels = c("Improved, on premise, LQ, sufficient","Improved, on premise, LQ, insufficient"))
)
table(d$imp_on_prem_sufficient_LQ_V_imp_on_prem_insufficient_LQ)
table(d$country, d$imp_on_prem_sufficient_LQ_V_imp_on_prem_insufficient_LQ)

#-------------------------------------------------------------------
# Outcome coding
#-------------------------------------------------------------------

#Rename outcome variables
d <- d %>% rename(
  fever=CA14,
  cough=CA16,
  diff_breath=CA17,
  congestion=CA18,
  resp_healthcare=CA20,
  haz=HAZ2,
  waz=WAZ2,
  whz=WHZ2
) 

head(d)

#Clean outcome variables

#anthropometry
d$haz[d$HAZFLAG==1] <- NA
d$waz[d$WAZFLAG==1] <- NA
d$whz[d$WHZFLAG==1] <- NA

d <- d %>% mutate(
  haz=as.numeric(haz),
  waz=as.numeric(waz),
  whz=as.numeric(whz),
  stunt = 1*(haz < -2 ),
  wast = 1*(whz < -2 ),
  uwt = 1*(waz < -2 )
)
table(d$HAZFLAG)

table(d$country,d$stunt)


#diarrhea
d$diarrhea <- ifelse(d$CA1=="1",1,0)
d$diarrhea[d$CA1=="8"|d$CA1=="9"|is.na(d$CA1)] <- NA
table(d$diarrhea)
table(d$country,d$diarrhea)

#ARI symptoms
table(d$diff_breath)
table(d$congestion)
table(d$cough)

#(cough OR rapid/difficult breathing) AND problem in chest
d$ari <- ifelse(d$congestion %in% c(1,3) & (d$diff_breath==1 | d$cough==1), 1, 0)
d$ari[(d$diff_breath==8 | d$diff_breath==9) &
        (d$congestion==8 | d$congestion==9) &
        (d$cough==8 | d$cough==9)] <- NA
d$ari[is.na(d$diff_breath) & is.na(d$congestion) & is.na(d$cough)]<- NA
# table(d$ari)
# table(is.na(d$ari))
# table(1*is.na(d$ari),is.na(d$diarrhea))
# 
# table(d$country,is.na(d$ari),is.na(d$diarrhea))


#Bangladesh disease prevalence should be 6.9% diarrhea, 2.0% ARI,  23.5% Fever
prop.table(table(d$ari[d$country=="Bangladesh"]))*100
prop.table(table(d$diarrhea[d$country=="Bangladesh"]))*100
prop.table(table(d$fever[d$country=="Bangladesh"]))*100


#------------------------------------------------------
# rename and clean covariates
#------------------------------------------------------


# Adjustment covariates:
# a.	Asset-based wealth index 
# b.	Parental education: melevel
  # table(d$country, is.na(d$melevel))
  # table(d$country, is.na(d$helevel))
  # table(d$country, is.na(d$helevel1))
  d$melevel[is.na(d$melevel)&!is.na(d$helevel)] <- d$helevel[is.na(d$melevel)&!is.na(d$helevel)] 
  d$melevel[is.na(d$melevel)&!is.na(d$helevel1)] <- d$helevel1[is.na(d$melevel)&!is.na(d$helevel1)] 
  #round(prop.table(table(d$country, is.na(d$melevel)),1)*100,2)

# c.	Parental age: HHAGE 
# d.	Breastfeeding history: BD2, BD3
# e.	Child age: CAGED
# f.	Child sex: HL4
# g.	Birth order: brthord
# h.	Urban/rural location: area_type #NOTE: is this right?
# i.	Household type/construction: HC3, HC5, HC6
# j.	Number of household residents: HH48
  #hhsize 5: hhsize5 if is.na(HH48)
  # Labels:
  #   value          label
  # 0  <5 hh members
  # 1 >=5 hh members
  #table(d$country, is.na(d$HH48))
  d$HH48[is.na(d$HH48)&!is.na(d$hhsize5)] <- d$hhsize5[is.na(d$HH48)&!is.na(d$hhsize5)] 
  #table(d$country, is.na(d$HH48))
  
# k.	Number of children under 5yrs in the household: HH51 
  table(d$country, is.na(d$HH51))
  d$HH51[is.na(d$HH51)&!is.na(d$HH14)] <- d$HH14[is.na(d$HH51)&!is.na(d$HH14)] 
  table(d$country, is.na(d$HH51))
# l.	Type of flooring: HC4
# m.	Presence of animals in the household: animals #NOTE: check if indicator
# n.	Cooking stove type: EU1, If missing, use electric stove as an asset 
            #table(d$country, is.na(d$EU1)) 
            temp <- ifelse(d$HC9C=="1","1","9")
             temp[d$HC9C=="9"] <- "99"
             d$EU1[is.na(d$EU1)] <- temp[is.na(d$EU1)]
            #table(d$country, is.na(d$EU1)) 
             

#Calculate household wealth (dropping WASH assets)
df <- d %>% subset(., select = c(country, clust_num, HH_num, HC7A:HC15)) %>%
  distinct(country, clust_num, HH_num, .keep_all = TRUE)

res <- df %>% group_by(country) %>%
  do(assetPCA(.))

dim(d)
dim(res)

rm(df)
rm(dfull)
gc()



#Clean other covariates
d <- d %>% subset(., select = c(country, 
                                clust_num,
                                HH_num, 
                                #HH.LN, 
                                childLN,
                                san_imp, 
                                wat_imp, 
                                hyg_imp, 
                                san_imp_cat,
                                san_imp_cat2,
                                wat_imp_cat,
                                hyg_imp_cat,
                                piped_san,
                                safely_manH20, 
                                EC_S, EC_H, 
                                EC_risk_S, 
                                EC_risk_H, 
                                EC_cfu_H, EC_cfu_S,
                                WASH, 
                                WASH_noEC,
                                Piped_san_cat,
                                san_coverage,
                                imp_off_prem_V_unimp,
                                imp_on_prem_V_imp_off_prem,
                                imp_on_prem_HQ_V_imp_on_prem_LQ,
                                imp_on_prem_sufficient_V_imp_on_prem_insufficient,
                                imp_on_prem_sufficient_HQ_V_imp_on_prem_insufficient_LQ,
                                imp_on_prem_sufficient_HQ_V_imp_on_prem_insufficient_HQ,
                                imp_on_prem_sufficient_LQ_V_imp_on_prem_insufficient_LQ,
                                diarrhea, 
                                ari,
                                mort,
                                fever, 
                                cough, 
                                resp_healthcare, 
                                diff_breath, 
                                congestion, 
                                haz,waz,whz,
                                stunt, wast, 
                                ecpopweight_H, 
                                ecpopweight_S, 
                                popweight,
                                ID, 
                                wscore,
                                windex5,
                                windex10,
                                wscoreu,
                                windex5u,
                                windex10u,
                                wscorer,
                                windex5r,
                                windex10r,
                                hhweight,
                                wqhaweight,
                                wqeweight,
                                wqsaweight,
                                stratum,
                                PSU,
                                #helevel, #education level
                                melevel, #education level
                                HHAGE, #age of hh head
                                CAGED,
                                HL4, #child sex
                                brthord, 
                                area_type, #urban/rural
                                BD2, #ever breastfed
                                BD3, #current breastfed
                                HH48, #number of hh members
                                HH51, #number of kids <5
                                HC4, #main material of floor
                                EU1, #type of cookstove used
                                EU2, #cookstove have chimney
                                # EU3, #cookstove have a fan - too sparse
                                EU4, #type of energy source of cookstove
                                HC17, #any animals, 
                                HC5, #roof material
                                HC6, #wall material
                                HC3#, #number of rooms used for sleeping
                                # HHwealth,
                                # HHwealth_quart
    )) %>% 
  rename(
    #educ=helevel, #education level
    educ=melevel,#education level
    mage=HHAGE, #age of hh head
    aged=CAGED,
    sex=HL4, #child sex
    birthord=brthord, #Need to add in birthorder
    urban_rural=area_type, #urban/rural
    everbf=BD2, #ever breastfed
    currbf= BD3, #current breastfed
    nhh=HH48, #number of hh members
    nchild5=HH51, #number of kids <5
    floor=HC4, #main material of floor
    cookstove=EU1, #type of cookstove used
    chimney=EU2, #cookstove have chimney
    # fan=EU3, #cookstove have a fan
    fuel=EU4, #type of energy source of cookstove
    own_animals=HC17, #animals, 
    roof=HC5, #roof material
    wall=HC6, #wall material
    nroom_sleeping=HC3 #number of rooms used for sleeping
  ) %>%
  mutate(
    mage = ifelse(mage %in% c("95","97","98","99"),NA,mage), #95  ==   95+, 98  ==    DK, 99== Missing
      mage = as.numeric(mage),
    aged = ifelse(aged =="9999",NA,aged), 
      aged = as.numeric(aged),
    sex = factor(sex),
    educ = ifelse(educ %in% c("9","99"),NA,educ), 
      #educ = as.numeric(educ),
    birthord = factor(birthord),
    rural = ifelse(urban_rural == "2","Rural","Urban"),
    rural = ifelse(is.na(urban_rural),"Missing",rural),
    own_animals = ifelse(own_animals=="9",NA,own_animals),
    own_animals = ifelse(own_animals=="2",0,own_animals),
      own_animals = factor(own_animals),
    everbf = ifelse(everbf=="9"|everbf== "8",NA,everbf),
      everbf = factor(everbf), 
    currbf = ifelse(currbf=="9"|currbf=="8",NA,currbf),
      currbf = factor(currbf), 
    nhh = as.numeric(nhh),
    nchild5 = as.numeric(nchild5),
    floor = factor(floor),  #Need to clean categories
    cookstove = factor(cookstove), #Need to clean categories
    chimney = ifelse(chimney=="9"|chimney=="8",NA,chimney),
      chimney = factor(chimney),    
    # fan = ifelse(fan=="9"|fan=="8",NA,fan),
    #   fan = factor(fan),  
    fuel = factor(fuel), #Need to clean categories
    #need to add animals
    roof = factor(roof), #Need to clean categories
    wall = factor(wall), #Need to clean categories
    nroom_sleeping = ifelse(nroom_sleeping=="99",NA,nroom_sleeping)#,
    #nroom_sleeping = as.numeric(nroom_sleeping)
  )


#merge in HH wealth
gc()
d <- left_join(d, res, by = c("country","clust_num","HH_num"))
dim(d)
table(is.na(d$HHwealth_quart))
table(d$country, is.na(d$HHwealth_quart))

d$HHwealth_quart <-as.character(d$HHwealth_quart)
d$HHwealth_quart[is.na(d$HHwealth_quart)] <- "missing"
table(d$HHwealth_quart)
d$HHwealth_quart <-factor(d$HHwealth_quart, levels=c("WealthQ1", "WealthQ2", "WealthQ3", "WealthQ4", "missing"))


saveRDS(d, here("data/compiled_intermediate_MICS_survey.rds"))

