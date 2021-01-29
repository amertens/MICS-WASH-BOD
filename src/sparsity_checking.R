

rm(list=ls())
source("0-config.R")


d <- readRDS(here("results/adjusted_RR.rds")) 

table(d$X)
d <- d %>% filter(X=="WASH", Y=="haz")

summary(d$coef)
head(d)

d <- d %>% filter(abs(coef)>.5)

head(d)


d <- readRDS(here("data/compiled_clean_MICS_survey.rds"))

table(d$country, d$WASH)

df <- d %>% filter(!is.na(WASH)) %>% group_by(country, WASH) %>% summarise(mean(haz, na.rm=T)) 
