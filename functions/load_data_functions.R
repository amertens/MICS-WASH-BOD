
#load from data path
data_path = function(path){
  paste0(data_dir, path)
}

#extract stata labels
makeVlist <- function(dta) { 
  labels <- sapply(dta, function(x) attr(x, "label"))
  labs <- tibble(name = names(labels),
                 label = labels)
  labs$label<- as.character(labs$label)
  return(labs)
}



namekey <- c(
  HH1="clust_num",
  HH2="HH_num",
  HINT="HINT",
  HH3="HH3",
  HH4="HH4",
  HH5D="HH5D",
  HH5M="HH5M",
  HH5Y="HH5Y",
  HH6="area_type",
  HH7="region",
  HH7A="HH7A",
  HH9="HH_selected_household",
  HH9A="HH9A",
  HH9B="HH9B",
  HH10="HH_selected_blank",
  HH11H="HH11H",
  HH11M="HH11M",
  HH12="HH12",
  HH46="HH46",
  HH13H="HH13H",
  HH13M="HH13M",
  HH14="HH14",
  HH15="HH15",
  HH16="HH16",
  HH17="HH17",
  HH26A="HH26A",
  HH26B="HH26B",
  HH26C="HH26C",
  HH33="HH33",
  HH44="HH44",
  HH47="HH47",
  HH48="HH48",
  HH49="HH49",
  HH51="HH51",
  HH52="HH52",
  INTROHL="INTROHL",
  HHAUX="HHAUX",
  HHWQ="HHWQ",
  HHFIN="HHFIN",
  HC1A="HC1A",
  HC1B="HC1B",
  HC2="HC2",
  HC3="HC3",
  HC4="HC4",
  HC5="HC5",
  HC6="HC6",
  HC7A="HC7A",
  HC7B="HC7B",
  HC7C="HC7C",
  HC7D="HC7D",
  HC7E="HC7E",
  HC7F="HC7F",
  HC7G="HC7G",
  HC8="HC8",
  HC9A="HC9A",
  HC9B="HC9B",
  HC9C="HC9C",
  HC9D="HC9D",
  HC9E="HC9E",
  HC9F="HC9F",
  HC10A="HC10A",
  HC10B="HC10B",
  HC10C="HC10C",
  HC10D="HC10D",
  HC10E="HC10E",
  HC10F="HC10F",
  HC10G="HC10G",
  HC10H="HC10H",
  HC10I="HC10I",
  HC10J="HC10J",
  HC11="HC11",
  HC12="HC12",
  HC13="HC13",
  HC14="HC14",
  HC15="HC15",
  HC16="HC16",
  HC17="HC17",
  HC18A="HC18A",
  HC18B="HC18B",
  HC18C="HC18C",
  HC18D="HC18D",
  HC18E="HC18E",
  HC18F="HC18F",
  HC18G="HC18G",
  HC18H="HC18H",
  HC18I="HC18I",
  HC19="HC19",
  EU1="EU1",
  EU2="EU2",
  EU3="EU3",
  EU4="EU4",
  EU5="EU5",
  EU9="EU9",
  WS1="WS1",
  WS2="WS2",
  WS3="WS3",
  WS4="WS4",
  WS5="WS5",
  WS6="WS6",
  WS7="WS7",
  WS8="WS8",
  WS9="WS9",
  WS10A="WS10A",
  WS10B="WS10B",
  WS10C="WS10C",
  WS10D="WS10D",
  WS10E="WS10E",
  WS10F="WS10F",
  WS10X="WS10X",
  WS10Z="WS10Z",
  WS10NR="WS10NR",
  WS11="WS11",
  WS12="WS12",
  WS13="WS13",
  WS14="WS14",
  WS15="WS15",
  WS16="WS16",
  WS17="WS17",
  HW1="HW1",
  HW2="HW2",
  HW3="HW3",
  HW4="HW4",
  HW5="HW5",
  HW6="HW6",
  HW7A="HW7A",
  HW7B="HW7B",
  HW7C="HW7C",
  HW7NR="HW7NR",
  SA1="SA1",
  SA2="SA2",
  WQ1="WQ1",
  WQ2="WQ2",
  WQ3="measurer_id",
  WQ4="WQ4",
  WQ5D="WQ5D",
  WQ5M="WQ5M",
  WQ5Y="WQ5Y",
  WQ5A="WQ5A",
  WQ5B="WQ5B",
  WQ5C="WQ5C",
  WQ6="WQ6",
  WQ7="WQ7",
  WQ8="WQ8",
  WQ31="WQ31",
  WQ10H="WQ10H",
  WQ10M="WQ10M",
  WQ23H="WQ23H",
  WQ23M="WQ23M",
  WQ11="HH_water_sample_provided",
  WQ12="observe_HH_sample_collection",
  WQ12A="WQ12A",
  WQ14="WQ14",
  WQ15A="WQ15A",
  WQ15B="WQ15B",
  WQ15C="WQ15C",
  WQ15D="WQ15D",
  WQ15E="WQ15E",
  WQ15F="WQ15F",
  WQ15X="WQ15X",
  WQ15Z="WQ15Z",
  WQ15NR="WQ15NR",
  WQ17="source_HH_sample",
  WQ18="permission_water_sample_SC",
  WQ19="EC_SC_tst_conducted",
  WQ19B="WQ19B",
  WQ19C="WQ19C",
  WQ20A="WQ20A",
  WQ20B="WQ20B",
  WQ20C="EC_BK_tst_conducted",
  WQ24D="WQ24D",
  WQ24M="WQ24M",
  WQ24Y="WQ24Y",
  WQ25H="WQ25H",
  WQ25M="WQ25M",
  WQ26="EC_100_H",
  WQ27="EC_100_S",
  WQ29="EC_100_B",
  HHSEX="hhsex",
  HHAGE="HHAGE",
  HH53="HH53",
  HH55="HH55",
  HH56="HH56",
  ethnicity="ethnicity",
  helevel="helevel",
  wscore="wscore",
  windex5="windex5",
  windex10="windex10",
  wscoreu="wscoreu",
  windex5u="windex5u",
  windex10u="windex10u",
  wscorer="wscorer",
  windex5r="windex5r",
  windex10r="windex10r",
  hhweight="hhweight",
  wqhaweight="wqhaweight",
  wqeweight="wqeweight",
  wqsaweight="wqsaweight",
  stratum="stratum",
  PSU="PSU"
)

# calc_wash <- function(d){
#   # san_imp	RECODE of WS11 (Type of toilet facility)
#   # san_cat	RECODE of WS11 (Type of toilet facility)
#   # EC_risk_H_1	Low risk: E. coli < 1 cfu/100 mL
#   # EC_risk_H_2	Moderate risk: E. coli 1-10 cfu/100 mL
#   # EC_risk_H_3	High risk: E. coli 11-100 cfu/100 mL
#   # EC_risk_H_4	Very high risk: E. coli >100 cfu/100 mL
#   # EC_result_S	Is an E. coli test result recorded
#   # EC_result_S100	Is an E. coli test result recorded
#   # EC_risk_S_1	Low risk: E. coli < 1 cfu/100 mL
#   # EC_risk_S_2	Moderate risk: E. coli 1-10 cfu/100 mL
#   # EC_risk_S_3	High risk: E. coli 11-100 cfu/100 mL
#   # EC_risk_S_4	Very high risk: E. coli >100 cfu/100 mL
#   # ecpopweight_H	NULL
#   # ecpopweight_S	NULL
#   # popweight	NULL
# }


load_MICS_dataset <- function(country, survey_round, saveCodebook=F, rename_Vars=F){
 
  #path=paste0(country,"/",country,"_cleaned.dta")
  ch_path=paste0(country,"/ch.sav")
  bh_path=paste0(country,"/bh.sav")
  hh_path=paste0(country,"/hh.sav")
  hl_path=paste0(country,"/hl.sav")
  
      d <- NULL
      try(d <- read_sav(data_path(hh_path)))
      if(is.null(d)){d <- read_dta(data_path(paste0(country,"/hh.dta")))} 
  
  #Merge with namekey. Make sure all datasets have ecoli and weights
      #to do: use excel sheet to get the variable name conversion
      d <- plyr::rename( d, replace=namekey, warn_missing=T)
      suppressWarnings(try(d <- d %>% rename(clust_num=HH1, HH_num=HH2)))
      
      
    #Rename variables if needed
  if(survey_round==5){
    if(is.null(d$WS15)){
      d <- d %>% 
        subset(., select= -c(WS11)) %>%
        rename(
        WS11=WS8,
        WS15=WS9
      )
    }
    if(is.null(d$WS4)){
      try(d <- d %>% rename(WS4=WS4A))
    }
    if(is.null(d$WS7)){
      try(d <- d %>% rename(WS7=WS5A))
    }
  }
  
    for(i in colnames(d)){
      if(class(d[[i]])[1]=="haven_labelled"){
        #d[[i]] <- as_factor(d[[i]])
        d <- bind_cols(d, temporary=as_factor(d[[i]]))
        colnames(d)[ncol(d)] <- paste0(i,"_lab")
      }
    }
    
    #load and merge child health
    ch <- read_sav(data_path(ch_path))
    ch <- ch %>% rename(clust_num=HH1, HH_num=HH2)
    
    if("UF3" %in% colnames(ch)){
      ch <- ch %>% rename(childLN=UF3)
    }else{
      ch$childLN=NA
    }
    if(!("UF4" %in% colnames(ch))){
      ch <- ch %>% rename(UF4=UF6)
    }
    #subset to relevant CH variables
    if(!is.null(ch$CAGED)){
      ch <- ch[,colnames(ch)[colnames(ch) %in% c("clust_num", "HH_num", "UF4", "childLN", "melevel", "HL4", "CAGED", "BD2", "BD3", "HAZ2", "WAZ2", "WHZ2", 
                                                 "HAZFLAG", "WAZFLAG", "WHZFLAG", "CA1","CA14","CA16","CA17","CA18","CA20")]]
      # ch <- ch %>% subset(., select = c(clust_num, HH_num, 
      #                                   childLN, HL4, CAGED, BD2, BD3, HAZ2, WAZ2, WHZ2, HAZFLAG, WAZFLAG, WHZFLAG, CA1,CA14,CA16,CA17,CA18,CA20))
    }else{
      ch <- ch[,colnames(ch)[colnames(ch) %in% c("clust_num", "HH_num", "UF4", "childLN", "melevel","HL4", "CAGE", "BD2", "BD3", "HAZ2", "WAZ2", "WHZ2", 
                                                 "HAZFLAG", "WAZFLAG", "WHZFLAG", "CA1","CA14","CA16","CA17","CA18","CA20")]]
      # ch <- ch %>% subset(., select = c(clust_num, HH_num, 
      #                                   childLN, HL4, CAGE, BD2, BD3, HAZ2, WAZ2, WHZ2, HAZFLAG, WAZFLAG, WHZFLAG, CA1,CA14,CA16,CA17,CA18,CA20)) %>%
      ch <- ch %>%  mutate(CAGED=CAGE*30.4167) %>%
        subset(., select = -c(CAGE))
    }
    try(bh <- read_sav(data_path(bh_path)))
    
    try(bh$mort <- ifelse(bh$BH5==2,1,0))
    try(bh$mort <- ifelse(bh$BH5==9,NA,bh$mort))
    # summary(d$BH9C)
    # table(d$BH9U, d$BH9N)
    
    try(bh <- bh  %>% rename(clust_num=HH1, HH_num=HH2, 
                             #HH.LN=LN, 
                             childLN=BH8))
    #subset to relevant BH variables
    try(bh <- bh[,colnames(bh) %in% c("clust_num", "HH_num","childLN", "brthord", "mort")])
    
  
    
    # Relations with: hl.sav, tn.sav, wm.sav, bh.sav, fg.sav, mm.sav, ch.sav, fs.sav and mn.sav
    # Base key variables: HH1 (cluster number) and HH2 (household number)
    # 
    # Instruction to the users:
    #   When merging household members', women's, children's and other data files with their households, you need to use the cluster numbers (variable HH1) and household numbers (variable HH2) as key variables. Since there is a "one-to-many" relationship between households and individuals, you should start with the individual data: household member, women or child, as your "base" (or 'active data set') and locate the correct household for each member, meaning that you should be merging the household data sets onto household members', women's or children's data, and not the other way around. 
    dim(ch)
    dim(d)
    d2 <- full_join(ch, d, by = c("clust_num","HH_num"))
    dim(d2)
    
    # dim(hh)
    # d3 <- NULL
    # try(d3 <- full_join(d2, hh, by = c("clust_num","HH_num")))
    # if(is.null(d3)){
    #   cat("HH dataset is missing!\n")
    #   d3<-d2
    #   }
    # dim(d3)
    
    #table(is.na(bh$brthord))
    d3<-NULL
    try(d3 <- left_join(d2, bh, by = c("clust_num","HH_num","childLN")))
    if(is.null(d3)){d3<-d2}
       
    hl <- NULL
    try(if(is.null(d3$HHAGE)){
      hl <- read_sav(data_path(hl_path))
      #lab<-makeVlist(hl)
      hl2 <- NULL
      try(
        hl2 <- hl %>%
        rename(UF4=HL8,
               HH_num=HH2,
               clust_num=HH1,
               HHAGE=ED2A) %>%
        select(clust_num, HH_num, UF4, HHAGE) %>% filter(UF4!=0))
      if(is.null(hl2)){
        hl2 <- hl %>%
          rename(UF4=HL7,
                 HH_num=HH2,
                 clust_num=HH1,
                 HHAGE=HL6) %>%
          select(clust_num, HH_num, UF4, HHAGE) %>% filter(UF4!=0)
      }
      
      dim(d3)
      dim(hl2)
      d3 <- left_join(d3, hl2, by=c("clust_num","HH_num","UF4"))
      dim(d3)
    })

  
    df <- data.frame(d3, country= country)
    
    
    if(rename_Vars){
      df <- df %>% rename(
        # HW3=HW4,
        # HW3_lab=HW4_lab,
        EU4=HC6,
        EU4_lab=HC6_lab,
        EU4=HC6,
        HC6_lab=HC5_lab,
        HC6=HC5,
        HC5_lab=HC4_lab,
        HC5=HC4,
        HC4_lab=HC3_lab,
        HC4=HC3,
        HC3_lab=HC2_lab,
        HC3=HC2) 
    }

    
    
  if(saveCodebook){
    lab<-makeVlist(df)
    write.csv(lab, here::here(paste0("codebooks/",country,"_vars.csv")))
    WASHlab <- df[,which(colnames(df) %in% c("HW1", "HW2", "HW3","HW4","HW5","HW6",
    "WS11","WS12", "WS13","WS14","WS15","WS1","WS2","HC4","EU1","EU2","EU3","HC17",
    "HC5","HC6","EU4", "WS9", "WS10A", "WS10B", "WS10C", "WS10D", "WS10E", 
    "WQ4",
    "WQ5A",
    "WQ5B",
    "WQ5C",
    "WQ5D",
    "WQ5E",
    "WQ14",
    "WQ15A",
    "WQ15B",
    "WQ15C",
    "WQ15D",
    "WQ15E"))]
    WASHlab <- makeVlist(WASHlab)  
    WASHlab$country=country
    saveRDS(WASHlab, here::here(paste0("codebooks/wash_codes/",country,"_WASHvars.rds")))
    }
  
  df <- df %>%
    mutate_all(as.character)
  
  df <- df[,!grepl("\\.",colnames(df))]
  df <- clean_WASH(df)
  
  return(df)
}





#clean WASH variables
clean_WASH <- function(d){
  
  d <- d %>% mutate(
    wat_class_lab = case_when(
      WS1=="11" ~ "Piped water", # PIPED WATER: PIPED INTO DWELLING
      WS1=="12" ~ "Piped water", # PIPED WATER: PIPED TO YARD / PLOT
      WS1=="13" ~ "Piped water", # PIPED WATER: PIPED TO NEIGHBOUR
      WS1=="14" ~ "Piped water", # PIPED WATER: PUBLIC TAP / STANDPIPE
      WS1=="21" ~ "Boreholes/Tubewells", # TUBE WELL / BOREHOLE
      WS1=="22" ~ "Boreholes/Tubewells", # TUBE WELL / BOREHOLE UNPROTECTED WELL
      WS1=="23" ~ "Boreholes/Tubewells", # HAND PUMP (Machincal)
      WS1=="31" ~ "Protected wells and springs", #DUG WELL: PROTECTED WELL
      WS1=="32" ~ "Unprotected wells and springs", #DUG WELL: UNPROTECTED WELL
      WS1=="41" ~ "Protected wells and springs", #SPRING: PROTECTED SPRING
      WS1=="42" ~ "Unprotected wells and springs", #SPRING: UNPROTECTED SPRING
      WS1=="51" ~ "Rainwater", # RAINWATER
      WS1=="52" ~ "Tank", #OWN CEMENT OR OTHER TANK
      WS1=="53" ~ "Tank", #NEIGHBOUR?S CEMENT OR OTHER TANK
      WS1=="54" ~ "Tank", #COMMUNITY CEMENT OR OTHER TANK
      WS1=="61" ~ "Delivered water", #TANKER-TRUCK
      WS1=="62" ~ "Delivered water", #BIDON, BASSIN,SEAU LIVRE A DOMICILE
      WS1=="71" ~ "Delivered water", #CART WITH SMALL TANK
      WS1=="72" ~ "Delivered water", #WATER KIOSK
      WS1=="73" ~ "Delivered water", #WATER KIOSK NOT CONNECTED WITH PIPED WATER
      WS1=="81" ~ "Surface water", #SURFACE WATER
      WS1=="82" ~ "Desalination plant", #DISALINATION PLANT WATER
      WS1=="91" ~ "Packaged water", #PACKAGED WATER: BOTTLED WATER
      WS1=="92" ~ "Packaged water", #PACKAGED WATER: SACHET WATER
      WS1=="93" ~ "Packaged water", #PACKAGED WATER: DESALINIZED & STERILIZED WATER
      WS1=="96" ~ "Missing", #OTHER
      WS1=="99" ~ "Missing", #NO RESPONSE
      is.na(WS1)~ "Missing"), 
    wat2_class_lab = case_when(
      WS2=="11" ~ "Piped water", # PIPED WATER: PIPED INTO DWELLING
      WS2=="12" ~ "Piped water", # PIPED WATER: PIPED TO YARD / PLOT
      WS2=="13" ~ "Piped water", # PIPED WATER: PIPED TO NEIGHBOUR
      WS2=="14" ~ "Piped water", # PIPED WATER: PUBLIC TAP / STANDPIPE
      WS2=="21" ~ "Boreholes/Tubewells", # TUBE WELL / BOREHOLE
      WS2=="22" ~ "Boreholes/Tubewells", # TUBE WELL / BOREHOLE UNPROTECTED WELL
      WS2=="23" ~ "Boreholes/Tubewells", # HAND PUMP (Machincal)
      WS2=="31" ~ "Protected wells and springs", #DUG WELL: PROTECTED WELL
      WS2=="32" ~ "Unprotected wells and springs", #DUG WELL: UNPROTECTED WELL
      WS2=="41" ~ "Protected wells and springs", #SPRING: PROTECTED SPRING
      WS2=="42" ~ "Unprotected wells and springs", #SPRING: UNPROTECTED SPRING
      WS2=="51" ~ "Rainwater", # RAINWATER
      WS2=="52" ~ "Tank", #OWN CEMENT OR OTHER TANK
      WS2=="53" ~ "Tank", #NEIGHBOUR?S CEMENT OR OTHER TANK
      WS2=="54" ~ "Tank", #COMMUNITY CEMENT OR OTHER TANK
      WS2=="61" ~ "Delivered water", #TANKER-TRUCK
      WS2=="62" ~ "Delivered water", #BIDON, BASSIN,SEAU LIVRE A DOMICILE
      WS2=="71" ~ "Delivered water", #CART WITH SMALL TANK
      WS2=="72" ~ "Delivered water", #WATER KIOSK
      WS2=="73" ~ "Delivered water", #WATER KIOSK NOT CONNECTED WITH PIPED WATER
      WS2=="81" ~ "Surface water", #SURFACE WATER
      WS2=="82" ~ "Desalination plant", #DISALINATION PLANT WATER
      WS2=="91" ~ "Packaged water", #PACKAGED WATER: BOTTLED WATER
      WS2=="92" ~ "Packaged water", #PACKAGED WATER: SACHET WATER
      WS2=="93" ~ "Packaged water", #PACKAGED WATER: DESALINIZED & STERILIZED WATER
      WS2=="96" ~ "Missing", #OTHER
      WS2=="99" ~ "Missing", #NO RESPONSE
      is.na(WS2)~ "Missing"), #
    wat_imp = as.character(case_when(
      
      #https://washdata.org/monitoring/drinking-water
      #Note: Improved drinking water sources are those that have the potential to 
      #deliver safe water by nature of their design and construction, and include:
      #piped water, boreholes or tubewells, protected dug wells, protected springs, 
      #rainwater, and packaged or delivered water
      #file:///C:/Users/andre/Downloads/JMP-2017-tr-smdw%20(1).pdf
      wat_class_lab %in% c("Surface water","Unprotected wells and springs")  ~ 0,
      wat_class_lab %in% c("Protected wells and springs","Piped water","Boreholes/Tubewells", "Tank", "Desalination plant","Packaged water","Delivered water","Rainwater")  ~ 1,
      # wat_class_lab=="Packaged water" & wat2_class_lab %in% c("Rainwater","Surface water","Unprotected wells and springs","Delivered water")  ~ 0,
      # wat_class_lab=="Packaged water" & wat2_class_lab %in% c("Protected wells and springs","Piped water","Boreholes/Tubewells")  ~ 1,
      wat_class_lab == "Missing" ~ NA_real_
    )),
    san_cat_lab = NA,
    san_cat_lab = case_when(
       # 11: FLUSH / POUR FLUSH: FLUSH TO PIPED SEWER SYSTEM   
       # 12: FLUSH / POUR FLUSH: FLUSH TO SEPTIC TANK     
       # 13: FLUSH / POUR FLUSH: FLUSH TO PIT LATRINE    
       # 14: FLUSH / POUR FLUSH: FLUSH TO OPEN DRAIN         
       # 15: Flush to unknown place / Not sure / DK where                          
       # 18: FLUSH / POUR FLUSH: FLUSH TO DONT KNOW WHERE           
       # 21: PIT LATRINE: VENTILATED IMPROVED PIT LATRINE                                                        
       # 22: PIT LATRINE: PIT LATRINE WITH SLAB                                                     
       # 23: PIT LATRINE: PIT LATRINE WITHOUT SLAB / OPEN PIT                             
       # 24: PIT LATRINE WITH SEAT                                  
       # 25: Latrine as <fa> n without slab, without roof or door
       # 31: COMPOSTING TOILET                                                    
       # 41: BUCKET                                                 
       # 51: HANGING TOILET / HANGING LATRINE          
       # 61: MOBILE TOILET
       # 95: NO FACILITY / BUSH / FIELD                          
       # 96: OTHER   
       # 99: NO RESPONSE  
    
      #https://washdata.org/monitoring/sanitation
      #Improved sanitation facilities are those designed to hygienically separate excreta from
      #human contact, and include: flush/pour flush to piped sewer system, septic tanks or pit 
      #latrines; ventilated improved pit latrines, composting toilets or pit latrines with slabs
       WS11 %in% c("95") ~ "No facility",
       WS11 %in% c("14","15","18","23","24","25","41","51") ~ "Unimproved",
       #WS15 == "2" & WS11 %in% c("11","12","13","21","22","31","")~ "Improved",
       WS11 %in% c("11","12","13","21","22","31","61")~ "Improved",
       WS11 %in% c("96","99") ~ NA_character_,
       is.na(WS11) ~ NA_character_
    ),
    san_imp = as.character(case_when(
      san_cat_lab %in% c("Unimproved","No facility")  ~ 0,
      san_cat_lab == c("Improved")  ~ 1,
      san_cat_lab == "Missing" | is.na(san_cat_lab) ~ NA_real_
      ))
    )
  
  try(d <- d %>% rename(
    popweight = hhweight
  ))
  try(d <- d %>% rename(
    ecpopweight_H = wqhweight,
    ecpopweight_S = wqsweight
  ))
  try(d <- d %>% rename(
    ecpopweight_H = wqhaweight,
    ecpopweight_S = wqsaweight
  ))
  if(is.null(d$ecpopweight_H)){
    try(d <- d %>% mutate(
      ecpopweight_H = wqweight,
      ecpopweight_S = wqweight
    ))
  }
  if(is.null(d$ecpopweight_H)){
    try(d <- d %>% mutate(
      ecpopweight_H = popweight,
      ecpopweight_S = popweight
    ))
  }
  
  
  d$EC_100_H <- as.numeric(d$EC_100_H)
  d$EC_100_S <- as.numeric(d$EC_100_S)
d$EC_risk_H_1 <- ifelse(d$EC_100_H==0,"1","0") 	#Low risk: E. coli < 1 cfu/100 mL
  d$EC_risk_H_1[is.na(d$EC_100_H)] <-  NA
d$EC_risk_H_2 <- ifelse(d$EC_100_H>0 & d$EC_100_H<11,"1","0") 	#Moderate risk: E. coli 1-10 cfu/100 mL
  d$EC_risk_H_2[is.na(d$EC_100_H)] <-  NA
d$EC_risk_H_3 <- ifelse(d$EC_100_H>10 & d$EC_100_H<101,"1","0") 	#High risk: E. coli 11-100 cfu/100 mL
  d$EC_risk_H_3[is.na(d$EC_100_H)] <-  NA
d$EC_risk_H_4 <- ifelse(d$EC_100_H>100,"1","0") 	#Very high risk: E. coli >100 cfu/100 mL
  d$EC_risk_H_4[is.na(d$EC_100_H)] <-  NA
d$EC_risk_S_1 <- ifelse(d$EC_100_S==0,"1","0") 	#Low risk: E. coli < 1 cfu/100 mL
  d$EC_risk_S_1[is.na(d$EC_100_S)] <-  NA
d$EC_risk_S_2 <- ifelse(d$EC_100_S>0 & d$EC_100_S<11,"1","0") 	#Moderate risk: E. coli 1-10 cfu/100 mL
  d$EC_risk_S_2[is.na(d$EC_100_S)] <-  NA
d$EC_risk_S_3 <- ifelse(d$EC_100_S>10 & d$EC_100_S<101,"1","0") 	#High risk: E. coli 11-100 cfu/100 mL
  d$EC_risk_S_3[is.na(d$EC_100_S)] <-  NA
d$EC_risk_S_4 <- ifelse(d$EC_100_S>100,"1","0") 	#Very high risk: E. coli >100 cfu/100 mL
  d$EC_risk_S_4[is.na(d$EC_100_S)] <-  NA
  d$EC_100_H <- as.character(d$EC_100_H)
  d$EC_100_S <- as.character(d$EC_100_S)

  return(d)
}





load_MICS_mort_dataset <- function(country){
  bh_path=paste0(country,"/bh.sav")
  hh_path=paste0(country,"/hh.sav")
  
  d <- read_sav(data_path(hh_path))
  d <- plyr::rename( d, replace=namekey, warn_missing=F)
  d <- d[, unique(colnames(d))]
  d <- d %>% mutate(
    svd = as.numeric(HH5D),
    svm = as.numeric(HH5M),
    svy = as.numeric(HH5Y)
  ) %>%
    mutate(svtime = lubridate::ymd(paste(svy,svm,svd,sep="-"))) %>%
    subset(., select = c(clust_num, HH_num, svtime))
  head(d)
  
  try(bh <- read_sav(data_path(bh_path)))
  try(bh$mort <- ifelse(bh$BH5==2,1,0))
  try(bh$mort <- ifelse(bh$BH5==9,NA,bh$mort))
  if(is.null(bh$BH4D))(bh$BH4D <- 15)
  try(bh <- bh  %>% rename(clust_num=HH1, HH_num=HH2, 
                           childLN=BH8,
                           age=BH6,
                           sex=BH3,
                           agedthm=BH9C,
                           agedthu=BH9U,
                           agedthn=BH9N,
                           birthday= BH4D,
                           birthmn= BH4M,
                           birthyr= BH4Y
  )%>%
    mutate( birthday= ifelse(birthday==99,NA,birthday),
            birthmn= ifelse(birthmn==99,NA,birthmn),
            birthyr= ifelse(birthyr==9999,NA,birthyr),
            dob = lubridate::ymd(
              paste(birthyr,birthmn,birthday,sep="-")),
            agedthu=as.numeric(agedthu),
            agedthn=as.numeric(agedthn),
            agedth = case_when(agedthu==1 ~ agedthn/365,
                               agedthu==2 ~ agedthn/12,
                               agedthu==3 ~ agedthn,
                               agedthu==9 ~ NA_real_))) 
  
  #subset to relevant BH variables
  try(bh <- bh[,colnames(bh) %in% c("clust_num", "HH_num","childLN", "mort","sex","age","agedth","dob")])
  
  dim(bh)
  dim(d)
  d2 <- full_join(bh, d, by = c("clust_num","HH_num"))
  dim(d2)
  
  d3 <- d2 %>% mutate(time_from_birth= lubridate::time_length(svtime-dob,unit="years")) %>% 
    #drop children older than 5 who died, and children who died more than 5 years prior to survey
    filter(time_from_birth <= 5 & time_from_birth >= 0)
  dim(d3)
  table(d3$mort)  
  summary(d3$time_from_birth)
  summary(d3$agedth)
  summary(d3$age)
  
  d4 <- d3 %>% subset(., select=c(clust_num, HH_num,childLN,sex,mort))
  
  df <- data.frame(d4, country= country)
  df <- df %>%
    mutate_all(as.character)
  
  return(df)
}
