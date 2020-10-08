
#------------------------------------------------------
# load packages and data
#------------------------------------------------------

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

#------------------------------------------------------
# clean outcomes and exposures
#------------------------------------------------------

d <- d %>% mutate(
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
  EC_risk_H = factor(EC_risk_H),
  EC_risk_S = factor(EC_risk_S)
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

d$hyg_imp <- ifelse(d$HW2 %in% c("Eau est disponible", "EAU EST DISPONIBLE", "WATER IS AVAILABLE") & (d$HW7A=="A"|d$HW7B=="B"), 1, 0)
d$hyg_imp[(d$HW2 %in% c("NO RESPONSE", "NON REPONSE") | !grepl("^O(B|b)", d$HW1))] <- NA
table(d$hyg_imp)
prop.table(table(d$hyg_imp))

#Recode improved sanitation and water
table(d$san_imp)
d$san_imp<-as.numeric(factor(d$san_imp, levels=c("Unimproved","Improved")))-1
table(d$san_imp)

table(d$wat_imp)
d$wat_imp<-as.numeric(factor(d$wat_imp, levels=c("Unimproved","Improved")))-1
table(d$wat_imp)

#Code most-improved WASH
d$WASH <- ifelse(d$san_imp==1 & d$wat_imp==1 & d$hyg_imp==1 & d$EC_risk_H==4, 1, 0)
d$WASH[is.na(d$san_imp)|is.na(d$wat_imp)|is.na(d$hyg_imp)|is.na(d$EC_risk_H)] <- NA

#Code most-improved WASH (no contamination measures)
d$WASH_noEC <- ifelse(d$san_imp==1 & d$wat_imp==1, 1, 0)
d$WASH_noEC[is.na(d$san_imp)|is.na(d$wat_imp)|is.na(d$hyg_imp)] <- NA

table(d$WASH)
table(d$WASH_noEC)



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


d <- d %>% subset(., select = c(country, 
                                clust_num,
                                HH_num, 
                                HH.LN, childLN,
                                san_imp, 
                                wat_imp, 
                                hyg_imp, 
                                san_cat, 
                                safely_manH20, 
                                EC_S, EC_H, 
                                EC_risk_S, 
                                EC_risk_H, 
                                WASH, 
                                WASH_noEC,
                                diarrhea, 
                                ari,
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
                                EU3, #cookstove have a fan
                                EU4, #type of energy source of cookstove
                                #animals, 
                                HC5, #roof material
                                HC6, #wall material
                                HC3 #number of rooms used for sleeping
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
    fan=EU3, #cookstove have a fan
    fuel=EU4, #type of energy source of cookstove
    #animals, 
    roof=HC5, #roof material
    wall=HC6, #wall material
    nroom_sleeping=HC3 #number of rooms used for sleeping
  ) %>%
  mutate(
    mage = ifelse(mage=="95+","97",mage),
    mage = as.numeric(mage),
    aged = as.numeric(aged),
    sex = factor(sex),
    educ = ifelse(educ%in% c("Manquant/NSP","Missing/DK","DK/Missing","DK / Missing"),NA,educ), #is "9" also missing?
      #educ = as.numeric(educ),
    birthord = factor(birthord),
    rural = ifelse(urban_rural %in% c("Rural","RURAL"),"Rural","Urban"),
    rural = ifelse(is.na(urban_rural),"Missing",rural),
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
    fan = ifelse(fan=="9"|fan=="8",NA,fan),
      fan = factor(fan),  
    fuel = factor(fuel), #Need to clean categories
    #need to add animals
    roof = factor(roof), #Need to clean categories
    wall = factor(wall), #Need to clean categories
    nroom_sleeping = ifelse(nroom_sleeping=="99",NA,nroom_sleeping)#,
    #nroom_sleeping = as.numeric(nroom_sleeping)
  )

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Clean covariate missingness and collapse factor levels:
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

#factor to clean variables to easier to recode levels
clean_factor = function(x){
  x<-iconv(x, "UTF-8", "UTF-8",sub='')
  x <- tolower(as.character(x))
  x <- gsub(" ","",x)
  x <- gsub("-",".",x)
  x <- gsub("/",".",x)
  x <- gsub(",",".",x)
  x <- gsub("\\'",".",x)
  x <- gsub("\\'",".",x)
  x <- gsub("\\(",".",x)
  x <- gsub("\\)",".",x)
  x <- gsub("\\+",".plus",x)
  x <- fct_explicit_na(x, "missing")
  print(paste(unique(x), collapse = ' = "",\n') %>% cat())
  return(x)
}

#----------------------------------------------------------------------
# education
#----------------------------------------------------------------------
d$educ<-clean_factor(d$educ)
d$educ <- recode(d$educ, 
            higher.plus = "higher",
            secondary = "secondary",
            primary = "primary",
            pre.primaryornone = "none",
            secondary.plus = "secondary",
            jss.jhs.middle = "secondary",
            sss.shs.secondary = "secondary",
            higher = "higher",
            primarycomplete.juniorsecondaryincomplete = "primary",
            pre.primary.none.primaryincomplete = "none",
            juniorsecondarycomplete.belowsr.secondary2ndlevel = "secondary",
            seniorsecondary2ndlevelandabove = "secondary",
            uppersecondary = "secondary",
            lowersecondary = "secondary",
            postsecondary.nontertiary = "higher",
            noneorece = "none",
            primaryornone = "primary",
            secondaire.plus = "secondary",
            primaire = "primary",
            préscolaireousansinstruction = "none",
            lowersecondary.basic. = "primary",
            college.university = "higher",
            vocational = "higher",
            none.preschool = "none",
            middle = "secondary",
            uppersecondary.plus = "secondary",
            ece.pre.primaryandnone = "none",
            secondaireetplus = "secondary",
            aucun.prscolaire = "none",
            #nsp.manquant= NA, #missing
            .default = "missing")

d$educ <- factor(d$educ, levels = c("none","primary","secondary", "higher","missing"))

table(d$educ)

#----------------------------------------------------------------------
# floor
#----------------------------------------------------------------------
d$floor <- clean_factor(d$floor)

#coded from https://dhsprogram.com/pubs/pdf/AS61/AS61.pdf
d$floor <- recode(d$floor, 
      parquetorpolishedwood = "improved",
      cement = "improved",
      earth.sand = "unimproved",
      woodplanks = "improved",
      ceramictiles = "improved",
      palm.bamboo.betelnut = "improved",
      vinylorasphaltstrips = "improved",
      carpet = "improved",
      other = "missing",
      dung = "unimproved",
      missing = "missing",
      linoleum.tapeh. = "improved",
      palm.bamboo = "improved",
      terrazzo = "improved",
      stone = "improved",
      tebaa.coconutleafstrips. = "unimproved",
      coconutweavedmat = "unimproved",
      gravel = "unimproved",
      pandanusweavvedmat = "unimproved",
      plywood = "improved",
      earth.mud = "unimproved",
      ciment = "improved",
      terre.sable = "unimproved",
      autre = "missing",
      planchesdebois = "improved",
      moquette.tapis = "improved",
      bandesdevinyleoud.asphalte = "improved",
      carrelageenceramique = "improved",
      parquetenboisouboispoli = "improved",
      palme.bambou = "unimproved",
      natte = "unimproved",
      bouse = "unimproved",
      finishedmaterialwoodparquet.polishedwood = "improved",
      finishedmaterialconcrete.cement = "improved",
      rudimentarymaterialwood.timber = "improved",
      finishedmateriallaminatepanels = "improved",
      finishedmaterialtile = "improved",
      other.specify. = "missing",
      naturalfloorearth.sand = "unimproved",
      naturalfloordung = "unimproved",
      dk = "missing",
      ceramictiles.marble.chips = "improved",
      bricksfloor = "improved",
      noresponse = "missing",
      terre.nature = "unimproved",
      vinyleoubandesd.asphalte = "improved",
      .default = "missing")

table(d$floor)

#----------------------------------------------------------------------
# cookstove
#----------------------------------------------------------------------
table(d$cookstove)

d$cookstove <- clean_factor(d$cookstove)
d$cookstove <- recode(d$cookstove, 
                      liquefiedpetroleumgas.lpg..cookinggasstove = "improved",
                      traditionalsolidfuelstove = "unimproved",
                      electricstove = "improved",
                      biogasstove = "improved",
                      liquidfuelstove = "improved",
                      pipednaturalgasstove = "improved",
                      threestonestove.openfire = "unimproved",
                      other = "missing",
                      nofoodcookedinhousehold = "missing",
                      missing = "missing",
                      manufacturedsolidfuelstove = "improved",
                      solarcooker = "improved",
                      manufacturedsolidfuelstove.coalpot = "improved",
                      noresponse = "missing",
                      kerosene = "improved",
                      twostonestove.openfire = "unimproved",
                      cuisiniereacombustiblesolide = "unimproved",
                      cuisinieretraditionnelleacombustiblesolide = "unimproved",
                      cuisiniereelectrique = "improved",
                      feusur3pierres.feuouvert = "unimproved",
                      cuisiniereacombustibleliquide = "improved",
                      autre = "missing",
                      cuisinereabiogaz = "improved",
                      cuiiniereagazliquide.gpl. = "improved",
                      cuiiniereagaznaturel = "improved",
                      pasderepaspréparédansmenage = "missing",
                      cuisinieresolaire = "improved",
                      nonreponse = "missing",
                      electricstove.cooker = "improved",
                      other.specify. = "missing",
                      feusurtroispirres.feuouvert = "unimproved",
                      cuisinirecombustiblesolide = "unimproved",
                      cuisinirelocalecombustiblesolide = "unimproved",
                      cuisinireegazdeptroleliquefi.gpl. = "improved",
                      pasderepasprepardanslemnage = "missing",
                      cuisinirecombustibleliquide = "improved",
                      cuisinirelectrique = "improved",
                      cuisiniregaznaturel = "improved",
                      cuisinireboigaz = "improved",
                      cuisiniereagazliquide.gpl. = "improved",
                      cuisiniereagaznaturel = "improved",
                      cusinieresur3pierres.feuouvert = "unimproved",
                      pasderepasprpardansmenage  = "missing",         
                  .default = "missing")

table(d$cookstove)


#----------------------------------------------------------------------
# roof
#----------------------------------------------------------------------
table(d$roof)

d$roof <- clean_factor(d$roof)
d$roof <- recode(d$roof, 
                 metal.tin = "improved",
                 cement = "improved",
                 thatch.palmleaf.nipapalm = "unimproved",
                 rusticmat = "unimproved",
                 noroof = "unimproved",
                 wood = "improved",
                 roofingshingles = "improved",
                 calamine.cementfibre = "improved",
                 ceramictiles = "improved",
                 other = "missing",
                 palm.bamboo = "unimproved",
                 sod = "unimproved",
                 missing = "missing",
                 cardboard = "unimproved",
                 thatch.palmleaf = "unimproved",
                 woodplanks = "improved",
                 metal.tin.corrugatedironsheet = "improved",
                 thatch.palmleaf.rafia = "unimproved",
                 mud.mudbrick.earth = "unimproved",
                 slate.asbestos = "improved",
                 cardboard.polythenesheet = "improved",
                 metal.tin.alluminium = "improved",
                 metal.corrugated.zincsheets. = "improved",
                 ceramic.claytiles = "improved",
                 asbestos.cementfibre = "improved",
                 tole.metal.aluminium = "improved",
                 ciment = "improved",
                 tuilesenceramique = "improved",
                 planchesenbois = "improved",
                 toitdebardeaux.shingles. = "improved",
                 calamine.fibredeciment = "improved",
                 natte = "unimproved",
                 chaume.feuilledepalme = "unimproved",
                 mottesd.herbes = "unimproved",
                 palmier.bambou.zozoro = "unimproved",
                 autre = "missing",
                 bois = "improved",
                 pasdetoit = "unimproved",
                 metal.iron = "improved",
                 dirt.stonewithmud = "unimproved",
                 leadsheet.pitch = "improved",
                 wavesteelrooftile = "improved",
                 other.specify. = "missing",
                 noresponse = "missing",
                 metal.tin.t.iron.girders = "improved",
                 wood.woodenbeams = "improved",
                 metal.tin.corrugatedironsheets.zinc. = "improved",
                 metal.aluminium = "improved",
                 herbes = "unimproved",
                 ciment.dalle = "improved",
                 palmier.bambou = "unimproved",
                 carton = "improved",
                 tuiles = "improved",
                 asbestos = "improved",
                 .default = "missing")
table(d$roof)

#----------------------------------------------------------------------
# chimney, fan
#----------------------------------------------------------------------
table(d$chimney)

d$chimney <- fct_explicit_na(d$chimney, "missing")
d$fan <- fct_explicit_na(d$fan, "missing")


d <- d %>% mutate(
  chimney = case_when(
    chimney=="missing" ~ "missing",  
    chimney=="NO" ~ "no",  
    chimney=="YES" ~ "yes",  
    chimney=="NO RESPONSE" ~ "missing",  
    chimney=="NON REPONSE" ~ "missing",  
    chimney=="DON?T KNOW" ~ "missing",  
    chimney=="DK" ~ "missing",  
    chimney=="OUI" ~ "yes",  
    chimney=="NSP" ~ "missing",  
    chimney=="NON" ~ "no",  
    chimney=="NO RESPONSE" ~ "missing"
  ),
  fan = case_when(
    fan=="missing" ~ "missing",  
    fan=="NO" ~ "no",  
    fan=="YES" ~ "yes",  
    fan=="NO RESPONSE" ~ "missing",  
    fan=="NON REPONSE" ~ "missing",  
    fan=="DON?T KNOW" ~ "missing",  
    fan=="DK" ~ "missing",  
    fan=="OUI" ~ "yes",  
    fan=="NSP" ~ "missing",  
    fan=="NON" ~ "no",  
    fan=="NO RESPONSE" ~ "missing"
  )
)

table(d$chimney)
table(d$fan)

#----------------------------------------------------------------------
# wall
#----------------------------------------------------------------------

table(d$wall)

d$wall <- clean_factor(d$wall)
d$wall <- recode(d$wall, 
                 bricks = "finished",
                 tin = "finished",
                 cement = "finished",
                 stonewithlime.cement = "finished",
                 cementblocks = "finished",
                 cane.palm.trunks = "natural",
                 woodplanks.shingles = "finished",
                 reusedwood = "rudimentary",
                 uncoveredadobe = "rudimentary",
                 nowalls = "natural",
                 bamboowithmud = "rudimentary",
                 coveredadobe = "finished",
                 cardboard = "rudimentary",
                 bamboowithpolithine = "rudimentary",
                 dirt = "natural",
                 stonewithmud = "rudimentary",
                 other = "missing",
                 missing = "missing",
                 plywood = "rudimentary",
                 mud.mudbricks = "rudimentary",
                 bamboowithcement = "rudimentary",
                 earth.mud.mudbricks = "natural",
                 slates.abestos = "finished",
                 coconutmidrip.tebaa. = "natural",
                 metal.tin.alluminium = "finished",
                 pandanusroot = "natural",
                 thatch = "natural",
                 bamboomat = "natural",
                 bamboolattice = "natural",
                 bamboo.bamboowithdryleaf = "natural",
                 metal.corrugated.zincsheets. = "finished",
                 sod.mud.dung = "natural",
                 cane.treetrunks = "natural",
                 ciment = "finished",
                 briques = "finished",
                 planchesdebois.bardeaux = "finished",
                 boisrecycle = "rudimentary",
                 blocsdeciment = "finished",
                 boue = "natural",
                 adobe.bancorecouvert = "finished",
                 pierreavecboue = "rudimentary",
                 pierreavecchaux.ciment = "finished",
                 carton = "rudimentary",
                 contreplaque = "rudimentary",
                 bambouavecboue = "rudimentary",
                 adobenonrecouvert.banco = "rudimentary",
                 cane.palme.troncs.zozoro = "natural",
                 pasdemurs = "natural",
                 autre = "missing",
                 brickregular = "finished",
                 brickpolishing = "finished",
                 finishedmaterialwoodtimber = "finished",
                 finishedmaterialblock = "finished",
                 rudimentarywallsstonewithmud = "rudimentary",
                 plastering = "finished",
                 finishedmaterialcement = "finished",
                 pvcsheets = "finished",
                 noresponse = "missing",
                 other.specify. = "missing",
                 metalcoating = "finished",
                 corrugatedironsheets.zinc. = "finished",
                 cane.palme.troncs.claie = "natural",
                 toles = "natural",
                 nonreponse = "missing",
                 cane.palme.troncs = "natural",
                 dirt.mud.poleanddagga. = "natural",
                 cardboard.carton = "finished",
                 cane.trunks = "natural",
                 .default = "missing")
table(d$wall)


#----------------------------------------------------------------------
# fuel
#----------------------------------------------------------------------

table(d$fuel)

#https://dhsprogram.com/data/Guide-to-DHS-Statistics/Cooking_Fuel.htm
#Solid fuels include coal/lignite, charcoal, wood, straw/shrub/grass, agricultural crops, and animal dung. 
#Clean fuels include electricity, liquefied petroleum gas (LPG), natural gas, and biogas.

d$fuel <- clean_factor(d$fuel)
d$fuel <- recode(d$fuel, 
                 missing = "missing",
                 wood = "solid",
                 cropresidue.grass.straw.shrubs = "solid",
                 animaldung.waste = "solid",
                 sawdust = "solid",
                 processedbiomass.pellets.orwoodchips = "solid",
                 coal.lignite = "solid",
                 other = "missing",
                 kerosene.paraffin = "clean",
                 gasoline.diesel = "clean",
                 garbage.plastic = "solid",
                 charcoal = "solid",
                 alcohol.ethanol = "clean",
                 copracake = "solid",
                 charbondebois = "solid",
                 bois = "solid",
                 charbon.lignite = "solid",
                 boused.animaux.dechets = "solid",
                 residusagricoles.herbes.pailles.arbustes = "solid",
                 biomassmanufacturee.granules.oucopeauxdebois = "solid",
                 petrole.paraffine = "clean",
                 sciure = "solid",
                 autre = "missing",
                 nonreponse = "missing",
                 essence.diesel = "clean",
                 alcool.ethanol = "clean",
                 ordures.plastique = "solid",
                 other.specify. = "missing",
                 improvedfuel = "clean",
                 noresponse = "missing",
                 rsidusdeculture.herbes.pailles.arbustes = "solid",
                 ptrole.parafine.gaz. = "clean",
                 biomassemanufacture.pelletsoucopeauxdebois. = "solid",
                 biomassmanufacturee.pellets.oucopeauxdebois = "solid",
                 residusdeculture.herbes.pailles.arbustes = "solid",
                 .default = "missing")
table(d$fuel)


#----------------------------------------------------------------------
# other covariates
#----------------------------------------------------------------------

table(d$sex)

table(d$aged==9999)
d$aged[d$aged==9999] <- NA
table(is.na(d$aged),is.na(d$haz)) #age is not missing for any anthro measurement

summary(d$mage)

table(d$birthord)
class(d$birthord)

table(d$rural)
d$rural <- as.factor(d$rural)
d$fan <- as.factor(d$fan)
d$chimney <- as.factor(d$chimney)

table(d$everbf)
table(d$currbf)
table(is.na(d$everbf))
table(is.na(d$currbf))
d$everbf <- fct_explicit_na(d$everbf, "missing")
d$currbf <- fct_explicit_na(d$currbf, "missing")



#----------------------------------------------------------------------
# Reorder levels
#----------------------------------------------------------------------

d$floor <- fct_relevel(d$floor, c("unimproved", "improved", "missing"))
d$cookstove <- fct_relevel(d$cookstove, c("unimproved", "improved", "missing"))
d$chimney <- fct_relevel(d$chimney, c("no", "yes", "missing"))
d$fan <- fct_relevel(d$fan, c("no", "yes", "missing"))
d$roof <- fct_relevel(d$roof, c("unimproved", "improved", "missing"))
d$wall <- fct_relevel(d$wall, c("natural", "rudimentary", "finished", "missing"))




#----------------------------------------------------------------------
#TEMP: single imputation by country
#----------------------------------------------------------------------
d$nroom_sleeping <- as.numeric(d$nroom_sleeping)
d$birthord <- as.numeric(d$birthord)


d <- d %>% group_by(country) %>%
  mutate(
    birthord=ifelse(is.na(birthord), median(birthord, na.rm=T), birthord),
    mage=ifelse(is.na(mage), median(mage, na.rm=T), mage),
    aged=ifelse(is.na(aged), median(aged, na.rm=T), aged),
    nhh=ifelse(is.na(nhh), median(nhh, na.rm=T), nhh),
    nroom_sleeping=ifelse(is.na(nroom_sleeping), median(nroom_sleeping, na.rm=T), nroom_sleeping),
    nchild5=ifelse(is.na(nchild5), median(nchild5, na.rm=T), nchild5)
    )



saveRDS(d, here("data/compiled_clean_MICS_survey.rds"))
