
#------------------------------------------------------
# load packages and data
#------------------------------------------------------

source("0-config.R")

dfull <- readRDS(here("data/compiled_raw_MICS_survey.rds"))

#------------------------------------------------------
# clean outcomes and exposures
#------------------------------------------------------

d <- dfull %>% mutate(
  EC_risk_H = 
    case_when(
      EC_risk_H_1 ==100 ~ 1,   
      EC_risk_H_2 ==100 ~ 2,   
      EC_risk_H_3 ==100 ~ 3,   
      EC_risk_H_4 ==100 ~ 4,
      TRUE ~ NA_real_
    ),
  EC_risk_S = 
    case_when(
      EC_risk_S_1 ==100 ~ 1,   
      EC_risk_S_2 ==100 ~ 2,   
      EC_risk_S_3 ==100 ~ 3,   
      EC_risk_S_4 ==100 ~ 4,
      TRUE ~ NA_real_   
    ),
  stunt = 1*(HAZ2 < -2 ),
  wast = 1*(WHZ2 < -2 )
)

table(d$EC_risk_S)
table(d$EC_risk_H)
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

d$hyg_imp <- ifelse(d$HW2==1 & (d$HW7A=="A"|d$HW7B=="B"), 1, 0)
d$hyg_imp[(d$HW2==9 | !(d$HW1 %in% c(1,2,3,4)))] <- NA
table(d$hyg_imp)
prop.table(table(d$hyg_imp))

#Code most-improved WASH
d$WASH <- ifelse(d$san_imp==1 & d$wat_imp==1 & d$EC_risk_H==4, 1, 0)

#code any contamination
d$EC_S <- ifelse(d$EC_risk_S==1, 1, 0)
d$EC_S[is.na(d$EC_risk_S)] <- NA
d$EC_H <- ifelse(d$EC_risk_H==1, 1, 0)
d$EC_H[is.na(d$EC_risk_H)] <- NA

#Code safely managed
d$safely_manH20 <- ifelse(d$EC_H==1 & d$wat_imp==1, 1, 0)
d$safely_manH20[is.na(d$EC_H)|is.na(d$wat_imp)] <- NA
table(d$safely_manH20)
table(d$country, d$safely_manH20)


#Rename variables
d <- d %>% rename(
  diarrhea=CA1,
  fever=CA14,
  cough=CA16,
  diff_breath=CA17,
  congestion=CA18,
  resp_healthcare=CA20
) 

head(d)

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









d <- d %>% subset(., select = c(country, 
                                clust_num,
                                HH_num, 
                                LN,
                                san_imp, 
                                wat_imp, 
                                hyg_imp, 
                                san_cat, 
                                safely_manH20, 
                                EC_S, EC_H, 
                                EC_risk_S, 
                                EC_risk_H, 
                                WASH, 
                                diarrhea, 
                                fever, 
                                cough, 
                                resp_healthcare, 
                                diff_breath, 
                                congestion, 
                                stunt, 
                                wast, 
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
                                EU3, #cookstove have a fan
                                EU4, #type of energy source of cookstove
                                animals, 
                                HC5, #roof material
                                HC6, #wall material
                                HC3 #number of rooms used for sleeping
    )) %>% 
  rename(
    educ=helevel, #education level
    mage=HHAGE, #age of hh head
    aged=CAGED,
    sex=HL4, #child sex
    birthord=brthord,
    urban_rural=area_type, #urban/rural
    everbf=BD2, #ever breastfed
    currbf= BD3, #current breastfed
    HH48, #number of hh members
    HH51, #number of kids <5
    HC4, #main material of floor
    EU1, #type of cookstove used
    EU2, #cookstove have chimney
    EU3, #cookstove have a fan
    EU4, #type of energy source of cookstove
    animals, 
    HC5, #roof material
    HC6, #wall material
    HC3 #number of rooms used for sleeping
  )


#ARI symptoms
table(d$diff_breath)
table(d$congestion)
table(d$cough)

table(d$cough, d$congestion)

saveRDS(d, here("data/compiled_clean_MICS_survey.rds"))
