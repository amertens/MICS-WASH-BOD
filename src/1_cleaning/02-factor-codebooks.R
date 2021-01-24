rm(list=ls())
source("0-config.R")
library(washb)


d <- readRDS(here("data/compiled_raw_MICS_survey.rds"))


sink("codebooks/factor_levels.txt") 

#To recode:

cat("\nWS1_lab, #drinking water source\n")
res <- d %>% group_by(WS1) %>%
  do(res=paste0(.$WS1[1],": ", unique(.$WS1_lab)))
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

cat("\nEU4_lab, #type of energy source of cookstove\n")
res <- d %>% group_by(HC5) %>%
  do(res=paste0(.$HC5[1],": ", unique(.$HC5_lab)))
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


