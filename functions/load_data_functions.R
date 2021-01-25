
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

country="Nigeria"
country="Cuba"

load_MICS_dataset <- function(country, saveCodebook=F){
  path=paste0(country,"/",country,"_cleaned.dta")
  ch_path=paste0(country,"/ch.sav")
  bh_path=paste0(country,"/bh.sav")
  hh_path=paste0(country,"/hh.sav")
  
  d <- NULL
  try(d <- read_dta(data_path(path)))
  if(is.null(d)){
    d <- read_sav(data_path(hh_path))
    
    #to do: use excel sheet to get the variable name conversion
    d <- plyr::rename( d, replace=namekey, warn_missing=F)
    #Generate the new variables
    #d <-clean_WASH(d)
    
  }

  for(i in colnames(d)){
    if(class(d[[i]])[1]=="haven_labelled"){
      #d[[i]] <- as_factor(d[[i]])
      d <- bind_cols(d, temporary=as_factor(d[[i]]))
      colnames(d)[ncol(d)] <- paste0(i,"_lab")
    }
  }

  if(isFile(data_path(hh_path))){
    hh <- read_sav(data_path(hh_path))
  }else{
    hh_path=paste0(country,"/hh.dta")
    hh <- read_dta(data_path(hh_path))
  }
  summary(hh$WQ26)

  dim(hh)
  hh <- hh %>% rename(  clust_num=HH1, HH_num=HH2, EC_cfu_H=WQ26, EC_cfu_S=WQ27) %>% 
    filter(!is.na(EC_cfu_H)|!is.na(EC_cfu_S)) %>%
    subset(., select = c(clust_num, HH_num, EC_cfu_H, EC_cfu_S))
  dim(hh)
  head(hh)
  
  
  
  #load and merge child health
  ch <- read_sav(data_path(ch_path))
  ch <- ch %>% rename(clust_num=HH1, HH_num=HH2)
  
  if("UF3" %in% colnames(ch)){
    ch <- ch %>% rename(childLN=UF3)
  }else{
    ch$childLN=NA
  }
  #subset to relevant CH variables
  if(!is.null(ch$CAGED)){
    ch <- ch[,colnames(ch)[colnames(ch) %in% c("clust_num", "HH_num", "childLN", "HL4", "CAGED", "BD2", "BD3", "HAZ2", "WAZ2", "WHZ2", 
                                               "HAZFLAG", "WAZFLAG", "WHZFLAG", "CA1","CA14","CA16","CA17","CA18","CA20")]]
    # ch <- ch %>% subset(., select = c(clust_num, HH_num, 
    #                                   childLN, HL4, CAGED, BD2, BD3, HAZ2, WAZ2, WHZ2, HAZFLAG, WAZFLAG, WHZFLAG, CA1,CA14,CA16,CA17,CA18,CA20))
  }else{
    ch <- ch[,colnames(ch)[colnames(ch) %in% c("clust_num", "HH_num", "childLN", "HL4", "CAGE", "BD2", "BD3", "HAZ2", "WAZ2", "WHZ2", 
                                               "HAZFLAG", "WAZFLAG", "WHZFLAG", "CA1","CA14","CA16","CA17","CA18","CA20")]]
    # ch <- ch %>% subset(., select = c(clust_num, HH_num, 
    #                                   childLN, HL4, CAGE, BD2, BD3, HAZ2, WAZ2, WHZ2, HAZFLAG, WAZFLAG, WHZFLAG, CA1,CA14,CA16,CA17,CA18,CA20)) %>%
    ch <- ch %>%  mutate(CAGED=CAGE*30.4167) %>%
      subset(., select = -c(CAGE))
  }
  try(bh <- read_sav(data_path(bh_path)))
  
  try(bh$mort <- ifelse(bh$BH5==2,1,0))
  # summary(d$BH9C)
  # table(d$BH9U, d$BH9N)
  
  try(bh <- bh  %>% rename(clust_num=HH1, HH_num=HH2, 
                           #HH.LN=LN, 
                           childLN=BH8))
  #subset to relevant BH variables
  try(bh <- bh %>% subset(., select = c(clust_num, HH_num, 
                                        #HH.LN, 
                                        childLN, brthord, mort))) 
  
  
  
  # Relations with: hl.sav, tn.sav, wm.sav, bh.sav, fg.sav, mm.sav, ch.sav, fs.sav and mn.sav
  # Base key variables: HH1 (cluster number) and HH2 (household number)
  # 
  # Instruction to the users:
  #   When merging household members', women's, children's and other data files with their households, you need to use the cluster numbers (variable HH1) and household numbers (variable HH2) as key variables. Since there is a "one-to-many" relationship between households and individuals, you should start with the individual data: household member, women or child, as your "base" (or 'active data set') and locate the correct household for each member, meaning that you should be merging the household data sets onto household members', women's or children's data, and not the other way around. 
  dim(ch)
  dim(d)
  d2 <- full_join(ch, d, by = c("clust_num","HH_num"))
  dim(d2)
  
  dim(hh)
  d3 <- full_join(d2, hh, by = c("clust_num","HH_num"))
  dim(d3)
  
  #table(is.na(bh$brthord))
  try(d3 <- left_join(d3, bh, by = c("clust_num","HH_num","childLN")))
  dim(d3)
  #table(is.na(df$brthord))
  
  if(saveCodebook){
    lab<-makeVlist(d3)
    write.csv(lab, here::here(paste0("codebooks/",country,"_vars.csv")))
  }
  
  df <- data.frame(d3, country= country)
  df <- df %>%
    mutate_all(as.character)
  
  
  
  return(df)
}





#clean WASH variables
clean_WASH <- function(d){
  colnames(d)
  table(d$WS1)
  table(d$WS1_lab)
  
  d <- d %>% mutate(
    wat_class_lab = case_when(
      WS1=="11" ~ "Piped water",
      WS1=="12" ~ "Piped water",
      WS1=="13" ~ "Piped water",
      WS1=="14" ~ "Piped water",
      WS1=="21" ~ "Boreholes/Tubewells",
      WS1=="22" ~ "Boreholes/Tubewells",
      WS1=="23" ~ "Boreholes/Tubewells",
      WS1=="31" ~ "Protected wells and springs",
      WS1=="32" ~ "Unprotected wells and springs",
      WS1=="41" ~ "Protected wells and springs",
      WS1=="42" ~ "Unprotected wells and springs",
      WS1=="51" ~ "Rainwater",
      WS1=="61" ~ "Delivered water",
      WS1=="62" ~ "Delivered water",
      WS1=="71" ~ "Delivered water",
      WS1=="72" ~ "Delivered water",
      WS1=="73" ~ "Delivered water",
      WS1=="81" ~ "Surface water",
      WS1=="82" ~ "Surface water",
      WS1=="91" ~ "Packaged water",
      WS1=="92" ~ "Packaged water",
      WS1=="93" ~ "Packaged water",
      WS1=="96" ~ "Missing",
      WS1=="99" ~ "Missing",
      is.na(WS1)~ "Missing"),
    wat2_class_lab = case_when(
      WS2=="11" ~ "Piped water",
      WS2=="12" ~ "Piped water",
      WS2=="13" ~ "Piped water",
      WS2=="14" ~ "Piped water",
      WS2=="21" ~ "Boreholes/Tubewells",
      WS2=="22" ~ "Boreholes/Tubewells",
      WS2=="23" ~ "Boreholes/Tubewells",
      WS2=="31" ~ "Protected wells and springs",
      WS2=="32" ~ "Unprotected wells and springs",
      WS2=="41" ~ "Protected wells and springs",
      WS2=="42" ~ "Unprotected wells and springs",
      WS2=="51" ~ "Rainwater",
      WS2=="61" ~ "Delivered water",
      WS2=="62" ~ "Delivered water",
      WS2=="71" ~ "Delivered water",
      WS2=="72" ~ "Delivered water",
      WS2=="73" ~ "Delivered water",
      WS2=="81" ~ "Surface water",
      WS2=="82" ~ "Surface water",
      WS2=="91" ~ "Packaged water",
      WS2=="92" ~ "Packaged water",
      WS2=="93" ~ "Packaged water",
      WS2=="96" ~ "Missing",
      WS2=="99" ~ "Missing",
      is.na(WS2)~ "Missing"),
    wat_imp = as.character(case_when(
      wat_class_lab %in% c("Rainwater","Surface water","Unprotected wells and springs","Delivered water")  ~ 0,
      wat_class_lab %in% c("Protected wells and springs","Piped water","Boreholes/Tubewells")  ~ 1,
      wat_class_lab=="Packaged water" & wat2_class_lab %in% c("Rainwater","Surface water","Unprotected wells and springs","Delivered water")  ~ 0,
      wat_class_lab=="Packaged water" & wat2_class_lab %in% c("Protected wells and springs","Piped water","Boreholes/Tubewells")  ~ 1,
      wat_class_lab == "Missing" ~ NA_real_
    )),
    san_cat_lab = NA,
    san_cat_lab = case_when(
       WS11 %in% c("95") ~ "No facility",
       WS11 %in% c("14","18","23","51") ~ "Unimproved",
       WS15 == "2" & WS11 %in% c("11","12","13","21","22","31","")~ "Improved",
       WS11 %in% c("96","99") ~ NA_character_,
       is.na(san_cat_lab) ~ "Unimproved"
    ),
    san_imp = as.character(case_when(
      san_cat_lab %in% c("Unimproved","No facility")  ~ 0,
      san_cat_lab == c("Improved")  ~ 1,
      san_cat_lab == "Missing" | is.na(san_cat_lab) ~ NA_real_
      )),
    ecpopweight_H = wqhweight,
    ecpopweight_S = wqsweight,
    popweight = hhweight
    )
  
  d$EC_100_H <- as.numeric(d$EC_100_H)
  d$EC_100_S <- as.numeric(d$EC_100_S)
d$EC_risk_H_1 <- ifelse(d$EC_100_H==0,"100","0") 	#Low risk: E. coli < 1 cfu/100 mL
  d$EC_risk_H_1[is.na(d$EC_100_H)] <-  NA
d$EC_risk_H_2 <- ifelse(d$EC_100_H>0 & d$EC_100_H<11,"100","0") 	#Moderate risk: E. coli 1-10 cfu/100 mL
  d$EC_risk_H_2[is.na(d$EC_100_H)] <-  NA
d$EC_risk_H_3 <- ifelse(d$EC_100_H>10 & d$EC_100_H<101,"100","0") 	#High risk: E. coli 11-100 cfu/100 mL
  d$EC_risk_H_3[is.na(d$EC_100_H)] <-  NA
d$EC_risk_H_4 <- ifelse(d$EC_100_H>100,"100","0") 	#Very high risk: E. coli >100 cfu/100 mL
  d$EC_risk_H_4[is.na(d$EC_100_H)] <-  NA
d$EC_risk_S_1 <- ifelse(d$EC_100_S==0,"100","0") 	#Low risk: E. coli < 1 cfu/100 mL
  d$EC_risk_S_1[is.na(d$EC_100_S)] <-  NA
d$EC_risk_S_2 <- ifelse(d$EC_100_S>0 & d$EC_100_S<11,"100","0") 	#Moderate risk: E. coli 1-10 cfu/100 mL
  d$EC_risk_S_2[is.na(d$EC_100_S)] <-  NA
d$EC_risk_S_3 <- ifelse(d$EC_100_S>10 & d$EC_100_S<101,"100","0") 	#High risk: E. coli 11-100 cfu/100 mL
  d$EC_risk_S_3[is.na(d$EC_100_S)] <-  NA
d$EC_risk_S_4 <- ifelse(d$EC_100_S>100,"100","0") 	#Very high risk: E. coli >100 cfu/100 mL
  d$EC_risk_S_4[is.na(d$EC_100_S)] <-  NA
  d$EC_100_H <- as.character(d$EC_100_H)
  d$EC_100_S <- as.character(d$EC_100_S)

#   table(d$EC_risk_H_1)
#   table(d$EC_risk_H_2)
#   table(d$EC_risk_H_3)
#   table(d$EC_risk_H_4)
#   table(d$EC_risk_S_1)
#   table(d$EC_risk_S_2)
#   table(d$EC_risk_S_3)
#   table(d$EC_risk_S_4)
  
  
  
 #Variables to add: 
  # EC_risk_H_1
  # EC_risk_H_2
  # EC_risk_H_3
  # EC_risk_H_4
  # EC_risk_S_1
  # EC_risk_S_2
  # EC_risk_S_3
  # EC_risk_S_4
  
  
  return(d)
#  table(d$WS8) 
#  table(d$WS9) 
#  
#  table(d$wat_class_lab)
#  table(d$wat_imp)
#  table(d$san_class_lab)
#  table(d$san_imp)
#        
#   
# unique(d$wat_class_lab)
# unique(d$wat_imp)
# 
#   res <- bd %>% group_by(WS11) %>%
#     do(res=paste0(.$WS11[1],": ", unique(.$WS11_lab)))
#   res[[2]]
#   res2 <- bd %>% group_by(WS15) %>%
#     do(res=paste0(.$WS15[1],": ", unique(.$WS15_lab)))
#   res2[[2]]
# 
# 
#   table(bd$san_cat_lab)
#   table(bd$san_imp)
  
  
}











