

#NOTES
# What is the proper weighting of the regression functions?
# What is the proper WASH-less asset index to use?
# Is roof/floor/wall material and number of rooms already in the wealth index?
  
#Need to move BH to main folders
#need to check all cleaned surveys are mics6 and there are no MICS5
#Need to see if bh.sav is available online for downloaded surveys missing it

#BH datasets are much longer than other datasets... 
#need to check if longitudinal and I'm merging correctly. I'm just using birth order

#left_joining bh is increasing dataset size
#NOTE: need to look up line number and double check that I am merging datasets correctly



source("0-config.R")


#load clean data
dfull <- readRDS(here("data/compiled_clean_MICS_survey.rds"))

#subset to just BD to code out analysis
d <- dfull %>% filter(country=="Bangladesh")


#use survey package to calculate correctly-weighted means 

df <- d %>% filter(!is.na(wat_imp))
table(df$haz)

#Run linear regression for continious outcomes (what is the right clustering/weighting?)
#We will include random effects for households nested within sampling clusters to account for the survey design.
#weights for e-coli: ecpopweight_S or ecpopweight_H, while popweight for other exposures
df <- d %>% filter(!is.na(EC_risk_H))
df <- droplevels(df)
res<-lmer(haz~EC_risk_H + (1|clust_num),  data=df)
summary(res)


#Run modified poisson for binary outcomes
#https://rstudio-pubs-static.s3.amazonaws.com/5752_fc41dca85dd24539bc99868697de83d0.html
geeglm.log.poisson <- geeglm(formula = recex_dich ~ ageyrs + smokever + sex + race,
                             data    = restricted2,
                             weights = popweight
                             family  = poisson(link = "log"),
                             id      = seqno,
                             corstr  = "exchangeable")