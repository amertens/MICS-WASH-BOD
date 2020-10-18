

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


                
d$clust_num <- paste0(d$clust_num, "-",d$HH_num)


#-------------------------------------------------
# Adjusted analysis
#-------------------------------------------------

res_adj <- res_adj_bin <- res_adj_cont <- NULL

d <- droplevels(d)




for(i in Wvars){
  cat(i, "\n")
  print(table(d[[i]]))
}



#binary_outcomes

res_adj_bin<-NULL
for(i in c("stunt", "wast","diarrhea","ari")){
  res1 <- res2 <- res3 <- res4 <- res5 <- res6 <- NULL
  print(i)
  res1 <- d %>% 
    do(mics_regression(d=.,
                       Y =i,
                       X="EC_H",
                       W=Wvars,
                       weight = "ecpopweight_H",
                       clustid= "clust_num",
                       family="modified possion", calc_PAF=FALSE))
  res2 <- d %>% 
    do(mics_regression(d=.,
                       Y =i,
                       X="EC_S",
                       W=Wvars,
                       weight = "ecpopweight_S",
                       clustid= "clust_num",
                       family="modified possion", calc_PAF=FALSE))
  res3 <- d %>% 
    do(mics_regression(d=.,
                       Y =i,
                       X="san_imp",
                       W=Wvars,
                       weight = "popweight",
                       clustid= "clust_num",
                       family="modified possion", calc_PAF=FALSE))
  res4 <- d %>% 
    do(mics_regression(d=.,
                       Y =i,
                       X="wat_imp",
                       W=Wvars,
                       weight = "popweight",
                       clustid= "clust_num",
                       family="modified possion", calc_PAF=FALSE))
  res5 <- d %>% 
    do(mics_regression(d=.,
                       Y =i,
                       X="hyg_imp",
                       W=Wvars,
                       weight = "popweight",
                       clustid= "clust_num",
                       family="modified possion", calc_PAF=FALSE))
  res6 <- d %>% 
    do(mics_regression(d=.,
                       Y =i,
                       X="WASH",
                       W=Wvars,
                       weight = "popweight",
                       clustid= "clust_num",
                       family="modified possion", calc_PAF=FALSE))
  
  res_adj_bin <- bind_rows(res_adj_bin, res1, res2, res3, res4, res5, res6)
}




#continious outcomes

res_adj_cont <- NULL
for(i in c("haz", "whz","waz")){
  res1 <- res2 <- res3 <- res4 <- res5 <- res6 <- NULL
  res1 <- d %>% 
    do(mics_regression(d=.,
                       Y =i,
                       X="EC_H",
                       W=Wvars,
                       weight = "ecpopweight_H",
                       clustid= "clust_num",
                       family="gaussian"))
  res2 <- d %>% 
    do(mics_regression(d=.,
                       Y =i,
                       X="EC_S",
                       W=Wvars,
                       weight = "ecpopweight_S",
                       clustid= "clust_num",
                       family="gaussian"))
  res3 <- d %>% 
    do(mics_regression(d=.,
                       Y =i,
                       X="san_imp",
                       W=Wvars,
                       weight = "popweight",
                       clustid= "clust_num",
                       family="gaussian"))
  res4 <- d %>% 
    do(mics_regression(d=.,
                       Y =i,
                       X="wat_imp",
                       W=Wvars,
                       weight = "popweight",
                       clustid= "clust_num",
                       family="gaussian"))
  res5 <- d %>% 
    do(mics_regression(d=.,
                       Y =i,
                       X="hyg_imp",
                       W=Wvars,
                       weight = "popweight",
                       clustid= "clust_num",
                       family="gaussian"))
  res6 <- d %>% 
    do(mics_regression(d=.,
                       Y =i,
                       X="WASH",
                       W=Wvars,
                       weight = "popweight",
                       clustid= "clust_num",
                       family="gaussian"))
  
  res_adj_cont <- bind_rows(res_adj_cont, res1, res2, res3, res4, res5, res6)
}




res_adj <- bind_rows(res_adj_bin, res_adj_cont)

saveRDS(res_adj, here("results/adjusted_1step_sens.rds"))

