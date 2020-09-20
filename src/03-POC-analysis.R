

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



res <- mics_regression(d=d,
                Y ="stunt",
                X="EC_risk_H",
                W=NULL,
                weight = "ecpopweight_H",
                clustid= "clust_num",
                family="modified possion")
res


res <- mics_regression(d=d,
                       Y ="stunt",
                       X="EC_risk_H",
                       W="mage",
                       weight = "ecpopweight_H",
                       clustid= "clust_num",
                       family="modified possion")
res

d <- dfull %>% filter(country %in% c("Bangladesh", "Zimbabwe","PakistanPunjab"))




res <- d %>% group_by(country) %>%
              do(mics_regression(d=.,
                       Y ="stunt",
                       X="EC_risk_H",
                       W=c("mage","aged"),
                       weight = "ecpopweight_H",
                       clustid= "clust_num",
                       family="modified possion"))
res


