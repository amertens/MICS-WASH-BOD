
#------------------------------------------------------
# load packages and data
#------------------------------------------------------

rm(list=ls())
source("0-config.R")


d <- readRDS(here("data/compiled_raw_MICS_survey.rds"))

#TEMP
#d <- dfull %>% filter(country %in% c("Bangladesh", "Zimbabwe","PakistanPunjab"))



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
    EC_risk_H = factor(EC_risk_H, levels=c("4","3","2","1")),
    EC_risk_S = factor(EC_risk_S, levels=c("4","3","2","1"))
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
table(d$HW1)
table(d$HW2)
table(d$HW7A)
table(d$HW7B)
table(d$HW7C)

table(d$country, d$HW2)
table(d$country, d$HW7A)
table(d$country, d$HW7B)


HW3BB
HW3BC
table(d$country, d$HW3BA)
table(d$country, d$HW3BB)
table(d$country, d$HW3BC)
table(d$country, d$HW3A)

table(d$HW7A, d$HW3BA)
table(d$HW7A=="A", d$HW3BA=="A")



d$hyg_imp <- factor(ifelse(d$HW2==1 & 
                             ((d$HW7A=="A" & !is.na(d$HW7A))|
                                (d$HW7B=="B" & !is.na(d$HW7B))|
                                (d$HW3BA=="A" & !is.na(d$HW3BA))|
                                (d$HW3BB=="B" & !is.na(d$HW3BB))|
                                (d$HW3BC=="C" & !is.na(d$HW3BC))), 
                           "Improved","Unimproved"), levels=c("Improved","Unimproved"))
d$hyg_imp[(d$HW2==9 | d$HW1>4)] <- NA
table(d$hyg_imp)
prop.table(table(d$hyg_imp))
table(d$country, d$hyg_imp)

# 1.	None (no facility)
# 2.	Limited - availability of a handwashing facility on premises without soap or water
# 3.	Basic - availability of a handwashing facility on premises with soap and water (highest measured level of service measured)
d <- d %>% mutate(
  hyg_imp_cat = case_when(
    HW2 == 2 ~"None",
    HW2 == 1 & hyg_imp != "Improved" ~"Limited",
    hyg_imp == "Improved" ~"Basic"
  ),
  hyg_imp_cat = factor(hyg_imp_cat, levels=c("None", "Limited", "Basic"))
)
table(d$hyg_imp_cat)


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


#Recode categorical sanitation and water
# 1.	Open defecation (no service)
# 2.	Unimproved - use of pit latrines without a slab or platform, hanging latrines or bucket latrines
# 3.	Limited - use of improved facilities (flush/pour flush to piped sewer system, septic tanks or pit latrines; ventilated improved pit latrines, composting toilets or pit latrines with slabs) shared between two or more households
# 4.	Basic - use of improved facilities which are not shared with other households
# 5.	Basic with high community coverage (>75% of the population in the community use basic sanitation services, highest measured level of service)

table(d$WS11_lab, d$WS11)
table(d$san_cat_lab)

d <- d %>% mutate(
  san_imp_cat = case_when(san_cat_lab=="No facility"~"No facility",
                           san_cat_lab=="Unimproved"~"Unimproved",
                           san_cat_lab=="Improved" & (WS15!="2")~"Limited",
                           san_cat_lab=="Improved" & (WS15=="2")~"Basic"
                           ),
  san_imp_cat = factor(san_imp_cat, levels=c("No facility", "Unimproved", "Limited", "Basic"))
)
table(d$san_imp_cat)
table(d$country, d$san_imp_cat)




# 1.	Surface water (no service)
# 2.	Unimproved - drinking water from an unprotected dug well or unprotected spring
# 3.	Limited - drinking water from an improved source for which collection time exceeds 30 minutes for a roundtrip including queuing.
# 4.	Basic - drinking water from an improved source (piped water, boreholes or tubewells, protected dug wells, protected springs, rainwater, and packaged or delivered water), provided collection time is not more than 30 minutes for a roundtrip including queuing,
# 5.	Continuous drinking water on premises from an improved water source (highest measured level of service).
table(d$WS1_lab, d$WS1)
table(d$wat_class_lab, d$wat_imp)
table(d$WS3_lab, d$WS3)

d$WS3 <- as.numeric(d$WS3)
d$WS4 <- as.numeric(d$WS4)
d$continious_wat <- ifelse(d$WS3<=2 & d$wat_class_lab=="Piped water", 1, 0)


d <- d %>% mutate(
  wat_imp_cat = case_when(wat_class_lab=="Surface water"~"Surface water",
                          wat_class_lab=="Unprotected wells and springs"~"Unimproved",
                          wat_imp=="Unimproved"~"Unimproved",
                          wat_imp=="Improved" & d$WS4>30~"Limited",
                          wat_imp=="Improved" & d$WS4<=30 & (WS3>2 | WS7!="2")~"Basic",
                          wat_imp=="Improved" & WS3<=2 & WS7=="2"~"Continuous",
  ),
  wat_imp_cat = factor(wat_imp_cat, levels=c("Surface water", "Unimproved", "Limited", "Basic","Continuous"))
)
table(d$wat_imp_cat)
table(d$country, d$wat_imp_cat)



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

# d %>% group_by(country) %>%
#   summarise(N_households=n(), N_imp_wat=sum(wat_imp, na.rm=T), N_imp_san=sum(san_imp, na.rm=T),  N_imp_hygeine=sum(hyg_imp, na.rm=T), N_imp_WASH=sum(WASH_noEC, na.rm=T))


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


#diarrhea
d$diarrhea <- ifelse(d$CA1=="1",1,0)
d$diarrhea[d$CA1=="8"|d$CA1=="9"] <- NA
table(d$diarrhea)

#ARI symptoms
table(d$diff_breath)
table(d$congestion)
table(d$cough)

#(cough OR rapid/difficult breathing) AND problem in chest
d$ari <- ifelse(d$congestion %in% c(1,3) & (d$diff_breath==1 | d$cough==1), 1, 0)
d$ari[(d$diff_breath==8 | d$diff_breath==9) &
        (d$congestion==8 | d$congestion==9) &
        (d$cough==8 | d$cough==9)] <- NA
table(d$ari)


#------------------------------------------------------
# rename and clean covariates
#------------------------------------------------------


# Adjustment covariates:
# a.	Asset-based wealth index (excluding WASH variables): wscore NOTE: IS THIS RIGHT?
# b.	Parental education: helevel
# c.	Parental age: HHAGE NOTE: can I get both parent's age?
# d.	Breastfeeding history: BD2, BD3
# e.	Child age: CAGED
# f.	Child sex: HL4
# g.	Birth order: brthord
# h.	Urban/rural location: area_type #NOTE: is this right?
# i.	Household type/construction: HC3, HC5, HC6
# j.	Number of household residents: BD3
# k.	Number of children under 5yrs in the household: HH51 
# l.	Type of flooring: HC4
# m.	Presence of animals in the household: animals #NOTE: check if indicator
# n.	Cooking stove type: EU1


#Calculate household wealth (dropping WASH assets)
df <- d %>% subset(., select = c(country, clust_num, HH_num, HC7A:HC15)) %>%
  distinct(country, clust_num, HH_num, .keep_all = TRUE)

res <- df %>% group_by(country) %>%
  do(assetPCA(.))

dim(d)
dim(res)

d <- left_join(d, res, by = c("country","clust_num","HH_num"))
dim(d)
table(is.na(d$HHwealth_quart))
table(d$country, is.na(d$HHwealth_quart))

d$HHwealth_quart <-as.character(d$HHwealth_quart)
d$HHwealth_quart[is.na(d$HHwealth_quart)] <- "missing"
table(d$HHwealth_quart)
d$HHwealth_quart <-factor(d$HHwealth_quart, levels=c("WealthQ1", "WealthQ2", "WealthQ3", "WealthQ4", "missing"))



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
                                wat_imp_cat,
                                hyg_imp_cat,
                                safely_manH20, 
                                EC_S, EC_H, 
                                EC_risk_S, 
                                EC_risk_H, 
                                EC_cfu_H, EC_cfu_S,
                                WASH, 
                                WASH_noEC,
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
                                helevel, #education level
                                HHAGE, #age of hh head
                                CAGED,
                                HL4, #child sex
                                brthord, 
                                area_type, #urban/rural
                                BD2, #ever breastfed
                                BD3, #current breastfed
                                HH48, #number og hh members
                                HH51, #number of kids <5
                                HC4, #main material of floor
                                EU1, #type of cookstove used
                                EU2, #cookstove have chimney
                                # EU3, #cookstove have a fan - too sparse
                                EU4, #type of energy source of cookstove
                                HC17, #any animals, 
                                HC5, #roof material
                                HC6, #wall material
                                HC3, #number of rooms used for sleeping
                                HHwealth,
                                HHwealth_quart
    )) %>% 
  rename(
    educ=helevel, #education level
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
    mage = ifelse(mage=="95+","97",mage),
    mage = as.numeric(mage),
    aged = as.numeric(aged),
    sex = factor(sex),
    educ = ifelse(educ%in% c("Manquant/NSP","Missing/DK","DK/Missing","DK / Missing"),NA,educ), 
      #educ = as.numeric(educ),
    birthord = factor(birthord),
    rural = ifelse(urban_rural %in% c("Rural","RURAL"),"Rural","Urban"),
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

saveRDS(d, here("data/compiled_intermediate_MICS_survey.rds"))

