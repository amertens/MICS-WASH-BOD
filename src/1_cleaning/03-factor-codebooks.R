rm(list=ls())
source("0-config.R")
library(washb)

gc()
d <- readRDS(here("data/compiled_raw_MICS_survey.rds"))
d <- d %>% subset(., select=c(HW1:HW6, WS1,WS2,WS11:WS14, helevel, area_type, HC4:HC6, EU1:EU4, HC17,
                              HW1_lab:HW6_lab, WS1_lab,WS2_lab,WS11_lab:WS14_lab, helevel_lab, area_type_lab, HC4_lab:HC6_lab, EU1_lab:EU4_lab, HC17_lab))
gc()

sink("codebooks/factor_levels.txt") 

#To recode:

cat("\nHW1_lab, #Place where household members most often wash their hands\n")
res <- d %>% group_by(HW1) %>%
  do(res=paste0(.$HW1[1],": ", unique(.$HW1_lab)))
res[[2]]

cat("\nHW2_lab, #Water available at the place for handwashing\n")
res <- d %>% group_by(HW2) %>%
  do(res=paste0(.$HW2[1],": ", unique(.$HW2_lab)))
res[[2]]

cat("\nHW3_lab, #Soap or detergent present at place of handwashing\n")
res <- d %>% group_by(HW3) %>%
  do(res=paste0(.$HW3[1],": ", unique(.$HW3_lab)))
res[[2]]

cat("\nHW4_lab, #Usual place for handwashing\n")
res <- d %>% group_by(HW4) %>%
  do(res=paste0(.$HW4[1],": ", unique(.$HW4_lab)))
res[[2]]

cat("\nHW5_lab, #Soap/other material available for washing hands\n")
res <- d %>% group_by(HW5) %>%
  do(res=paste0(.$HW5[1],": ", unique(.$HW5_lab)))
res[[2]]

cat("\nHW6_lab, #Hand washing material shown\n")
res <- d %>% group_by(HW6) %>%
  do(res=paste0(.$HW6[1],": ", unique(.$HW6_lab)))
res[[2]]











cat("\nWS11_lab, #sanitation type\n")
res <- d %>% group_by(WS11) %>%
  do(res=paste0(.$WS11[1],": ", unique(.$WS11_lab)))
res[[2]]

cat("\nWS12_lab, #Pit latrine or septic tank ever been emptied\n")
res <- d %>% group_by(WS12) %>%
  do(res=paste0(.$WS12[1],": ", unique(.$WS12_lab)))
res[[2]]

cat("\nWS13_lab, #Place the contents were emptied\n")
res <- d %>% group_by(WS13) %>%
  do(res=paste0(.$WS13[1],": ", unique(.$WS13_lab)))
res[[2]]

cat("\nWS14_lab, #Location of the toilet faciltity\n")
res <- d %>% group_by(WS14) %>%
  do(res=paste0(.$WS14[1],": ", unique(.$WS14_lab)))
res[[2]]

cat("\nWS15_lab, #Toilet facility shared")
res <- d %>% group_by(WS15) %>%
  do(res=paste0(.$WS15[1],": ", unique(.$WS15_lab)))
res[[2]]





cat("\nWS1_lab, #drinking water source\n")
res <- d %>% group_by(WS1) %>%
  do(res=paste0(.$WS1[1],": ", unique(.$WS1_lab)))
res[[2]]

cat("\nWS2_lab, #water secondary source\n")
res <- d %>% group_by(WS2) %>%
  do(res=paste0(.$WS2[1],": ", unique(.$WS2_lab)))
res[[2]]


cat("\nhelevel_lab, #education level\n")
res <- d %>% group_by(helevel) %>%
  do(res=paste0(.$helevel[1],": ", unique(.$helevel_lab)))
res[[2]]

cat("\narea_type_lab, #urban/rural\n")
res <- d %>% group_by(area_type) %>%
  do(res=paste0(.$area_type[1],": ", unique(.$area_type_lab)))
res[[2]]

cat("\nHC4_lab, #main material of floor\n")
res <- d %>% group_by(HC4) %>%
  do(res=paste0(.$HC4[1],": ", unique(.$HC4_lab)))
res[[2]]

cat("\nEU1_lab, #type of cookstove used\n")
res <- d %>% group_by(EU1) %>%
  do(res=paste0(.$EU1[1],": ", unique(.$EU1_lab)))
res[[2]]

cat("\nEU2_lab, #cookstove have chimney\n")
res <- d %>% group_by(EU2) %>%
  do(res=paste0(.$EU2[1],": ", unique(.$EU2_lab)))
res[[2]]

cat("\nEU3_lab, #cookstove have a fan - too sparse\n")
res <- d %>% group_by(EU3) %>%
  do(res=paste0(.$EU3[1],": ", unique(.$EU3_lab)))
res[[2]]


cat("\nHC17, #any animals, \n")
res <- d %>% group_by(HC17) %>%
  do(res=paste0(.$HC17[1],": ", unique(.$HC17_lab)))
res[[2]]

cat("\nHC5_lab, #roof material\n")
res <- d %>% group_by(HC5) %>%
  do(res=paste0(.$HC5[1],": ", unique(.$HC5_lab)))
res[[2]]

cat("\nHC6_lab, #wall material\n")
res <- d %>% group_by(HC6) %>%
  do(res=paste0(.$HC6[1],": ", unique(.$HC6_lab)))
res[[2]]

cat("\nEU4, #fuel\n")
res <- d %>% group_by(EU4) %>%
  do(res=paste0(.$EU4[1],": ", unique(.$EU4_lab)))
res[[2]]


sink()


