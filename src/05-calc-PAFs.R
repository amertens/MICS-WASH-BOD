

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
d <- readRDS(here("data/compiled_clean_POC_survey.rds"))



Wvars <- c("educ",
           "mage",
           "aged",
           "sex",
           "birthord", 
           "rural",
           #"everbf", 
           "currbf",
           "nhh",
           "nchild5",
           "floor",
           "cookstove",
           "chimney",
           "fan",
           "fuel",
           "roof",
           "wall",
           "nroom_sleeping")


#just run for BD, and without clustering
d <- d %>% filter(country %in% c("Bangladesh"))
d <- droplevels(d)
d$clust_num <- 1:nrow(d)

res1 <- d %>% group_by(country) %>%
  do(mics_regression(d=.,
                     Y ="stunt",
                     X="EC_H",
                     W=Wvars,
                     weight = "ecpopweight_H",
                     clustid= "clust_num",
                     family="modified possion", calc_PAF=T, low_risk_level=0))
res1

#-------------------------------------------------
# Adjusted analysis
#-------------------------------------------------

res_adj <- res_adj_bin <- res_adj_cont <- NULL


#binary_outcomes
d <- d %>% filter(country %in% c("Bangladesh"))
d <- droplevels(d)


res_adj_bin<-NULL
for(i in c("stunt", "wast","diarrhea","ari")){
  res1 <- res2 <- res3 <- res4 <- res5 <- res6 <- NULL
  res1 <- d %>% group_by(country) %>%
    do(mics_regression(d=.,
                       Y =i,
                       X="EC_H",
                       W=Wvars,
                       weight = "ecpopweight_H",
                       clustid= "clust_num",
                       family="modified possion", calc_PAF=T, low_risk_level=0))
  res2 <- d %>% group_by(country) %>%
    do(mics_regression(d=.,
                       Y =i,
                       X="EC_S",
                       W=Wvars,
                       weight = "ecpopweight_S",
                       clustid= "clust_num",
                       family="modified possion", calc_PAF=T, low_risk_level=0))
  res3 <- d %>% group_by(country) %>%
    do(mics_regression(d=.,
                       Y =i,
                       X="san_imp",
                       W=Wvars,
                       weight = "popweight",
                       clustid= "clust_num",
                       family="modified possion", calc_PAF=T, low_risk_level=1))
  res4 <- d %>% group_by(country) %>%
    do(mics_regression(d=.,
                       Y =i,
                       X="wat_imp",
                       W=Wvars,
                       weight = "popweight",
                       clustid= "clust_num",
                       family="modified possion", calc_PAF=T, low_risk_level=1))
  res5 <- d %>% group_by(country) %>%
    do(mics_regression(d=.,
                       Y =i,
                       X="hyg_imp",
                       W=Wvars,
                       weight = "popweight",
                       clustid= "clust_num",
                       family="modified possion", calc_PAF=T, low_risk_level=1))
  res6 <- d %>% group_by(country) %>%
    do(mics_regression(d=.,
                       Y =i,
                       X="WASH",
                       W=Wvars,
                       weight = "popweight",
                       clustid= "clust_num",
                       family="modified possion", calc_PAF=T, low_risk_level=1))
  
  res_adj_bin <- bind_rows(res_adj_bin, res1, res2, res3, res4, res5, res6)
}

saveRDS(res_adj_bin, here("results/adjusted_PAFs.rds"))







