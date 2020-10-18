

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

#subset to just POC countries
d <- dfull %>% filter(country %in% c("Bangladesh", "Zimbabwe","PakistanPunjab"))
d <- droplevels(d)

saveRDS(d, file=here("data/compiled_clean_POC_survey.rds"))

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

                Y ="haz"
                X="WASH"
                W=NULL
                weight = "popweight"
                clustid = "clust_num"
                family = "gaussian"

                
d$clust_num <- paste0(d$clust_num, "-",d$HH_num)
# d$clust_num <- 1:nrow(d)                

# res <- mics_regression(d=d,
#                 Y ="stunt",
#                 X="EC_H",
#                 W=NULL,
#                 weight = "ecpopweight_H",
#                 clustid= "clust_num",
#                 family="gaussian", calc_PAF=T, low_risk_level=1)
# res
# 
# # 
# # 
# # d <- d %>% filter(country %in% c("Bangladesh"))
# # 
# res1 <- d %>% group_by(country) %>%
#   do(mics_regression(d=.,
#                      Y ="stunt",
#                      X="EC_H",
#                      W=NULL,
#                      weight = "ecpopweight_H",
#                      clustid= "clust_num",
#                      family="modified possion", calc_PAF=T, low_risk_level=1))


#-------------------------------------------------
# Unadjusted analysis
#-------------------------------------------------


res_unadj_bin <- run_MICS_regressions(outcomes = c("stunt", "wast","diarrhea","ari"), family="modified possion", PAF=T, Wvars=NULL)
res_unadj_cont <- run_MICS_regressions(outcomes = c("haz", "whz"), family="gaussian", PAF=F, Wvars=NULL)


# res_unadj <- res_unadj_bin <- res_unadj_cont <- NULL
# 
# #binary_outcomes
# for(i in c("stunt", "wast","diarrhea","ari")){
#   res1 <- res2 <- res3 <- res4 <- res5 <- res6 <- res7 <- NULL
#   res1 <- d %>% group_by(country) %>%
#     do(mics_regression(d=.,
#                        Y =i,
#                        X="EC_H",
#                        W=NULL,
#                        weight = "ecpopweight_H",
#                        clustid= "clust_num",
#                        family="modified possion", calc_PAF=T, low_risk_level=0))
#   res2 <- d %>% group_by(country) %>%
#     do(mics_regression(d=.,
#                        Y =i,
#                        X="EC_S",
#                        W=NULL,
#                        weight = "ecpopweight_S",
#                        clustid= "clust_num",
#                        family="modified possion", calc_PAF=T, low_risk_level=0))
#   res3 <- d %>% group_by(country) %>%
#     do(mics_regression(d=.,
#                        Y =i,
#                        X="san_imp",
#                        W=NULL,
#                        weight = "popweight",
#                        clustid= "clust_num",
#                        family="modified possion", calc_PAF=T, low_risk_level=1))
#   res4 <- d %>% group_by(country) %>%
#     do(mics_regression(d=.,
#                        Y =i,
#                        X="wat_imp",
#                        W=NULL,
#                        weight = "popweight",
#                        clustid= "clust_num",
#                        family="modified possion", calc_PAF=T, low_risk_level=1))
#   res5 <- d %>% group_by(country) %>%
#     do(mics_regression(d=.,
#                        Y =i,
#                        X="hyg_imp",
#                        W=NULL,
#                        weight = "popweight",
#                        clustid= "clust_num",
#                        family="modified possion", calc_PAF=T, low_risk_level=1))
#   res6 <- d %>% group_by(country) %>%
#     do(mics_regression(d=.,
#                        Y =i,
#                        X="WASH",
#                        W=NULL,
#                        weight = "popweight",
#                        clustid= "clust_num",
#                        family="modified possion", calc_PAF=T, low_risk_level=1))
#   
#   res_unadj_bin <- bind_rows(res_unadj_bin, res1, res2, res3, res4, res5, res6)
# }
# 
# 
# 
# #continious outcomes
# for(i in c("haz", "whz","waz")){
#   res1 <- res2 <- res3 <- res4 <- res5 <- res6 <- NULL
#   res1 <- d %>% group_by(country) %>%
#     do(mics_regression(d=.,
#                        Y =i,
#                        X="EC_H",
#                        W=NULL,
#                        weight = "ecpopweight_H",
#                        clustid= "clust_num",
#                        family="gaussian"))
#   res2 <- d %>% group_by(country) %>%
#     do(mics_regression(d=.,
#                        Y =i,
#                        X="EC_S",
#                        W=NULL,
#                        weight = "ecpopweight_S",
#                        clustid= "clust_num",
#                        family="gaussian"))
#   res3 <- d %>% group_by(country) %>%
#     do(mics_regression(d=.,
#                        Y =i,
#                        X="san_imp",
#                        W=NULL,
#                        weight = "popweight",
#                        clustid= "clust_num",
#                        family="gaussian"))
#   res4 <- d %>% group_by(country) %>%
#     do(mics_regression(d=.,
#                        Y =i,
#                        X="wat_imp",
#                        W=NULL,
#                        weight = "popweight",
#                        clustid= "clust_num",
#                        family="gaussian"))
#   res5 <- d %>% group_by(country) %>%
#     do(mics_regression(d=.,
#                        Y =i,
#                        X="hyg_imp",
#                        W=NULL,
#                        weight = "popweight",
#                        clustid= "clust_num",
#                        family="gaussian"))
#   res6 <- d %>% group_by(country) %>%
#     do(mics_regression(d=.,
#                        Y =i,
#                        X="WASH",
#                        W=NULL,
#                        weight = "popweight",
#                        clustid= "clust_num",
#                        family="gaussian"))
#   
#   res_unadj_cont <- bind_rows(res_unadj_cont, res1, res2, res3, res4, res5, res6)
# }


res_unadj <- bind_rows(res_unadj_bin, res_unadj_cont)

saveRDS(res_unadj, here("results/unadjusted_RR.rds"))




#-------------------------------------------------
# Adjusted analysis
#-------------------------------------------------

res_adj <- res_adj_bin <- res_adj_cont <- NULL

d <- droplevels(d)
res_adj_bin <- run_MICS_regressions(outcomes = c("stunt", "wast","diarrhea","ari"), family="modified possion", PAF=T, Wvars=Wvars)
res_adj_cont <- run_MICS_regressions(outcomes = c("haz", "whz"), family="gaussian", PAF=F, Wvars=Wvars)



# 
# for(i in Wvars){
#   cat(i, "\n")
#   print(table(d[[i]]))
# }
# 
# 
# 
# #binary_outcomes
# 
# res_adj_bin<-NULL
# for(i in c("stunt", "wast","diarrhea","ari")){
#   res1 <- res2 <- res3 <- res4 <- res5 <- res6 <- res7 <- NULL
#   print(i)
#   res1 <- d %>% group_by(country) %>%
#     do(mics_regression(d=.,
#                        Y =i,
#                        X="EC_H",
#                        W=Wvars,
#                        weight = "ecpopweight_H",
#                        clustid= "clust_num",
#                        family="modified possion", calc_PAF=T, low_risk_level=0))
#   res2 <- d %>% group_by(country) %>%
#     do(mics_regression(d=.,
#                        Y =i,
#                        X="EC_S",
#                        W=Wvars,
#                        weight = "ecpopweight_S",
#                        clustid= "clust_num",
#                        family="modified possion", calc_PAF=T, low_risk_level=0))
#   res3 <- d %>% group_by(country) %>%
#     do(mics_regression(d=.,
#                        Y =i,
#                        X="san_imp",
#                        W=Wvars,
#                        weight = "popweight",
#                        clustid= "clust_num",
#                        family="modified possion", calc_PAF=T, low_risk_level=1))
#   res4 <- d %>% group_by(country) %>%
#     do(mics_regression(d=.,
#                        Y =i,
#                        X="wat_imp",
#                        W=Wvars,
#                        weight = "popweight",
#                        clustid= "clust_num",
#                        family="modified possion", calc_PAF=T, low_risk_level=1))
#   res5 <- d %>% group_by(country) %>%
#     do(mics_regression(d=.,
#                        Y =i,
#                        X="hyg_imp",
#                        W=Wvars,
#                        weight = "popweight",
#                        clustid= "clust_num",
#                        family="modified possion", calc_PAF=T, low_risk_level=1))
#   res6 <- d %>% group_by(country) %>%
#     do(mics_regression(d=.,
#                        Y =i,
#                        X="safely_manH20",
#                        W=Wvars,
#                        weight = "ecpopweight_H",
#                        clustid= "clust_num",
#                        family="modified possion", calc_PAF=T, low_risk_level=1))
#   res7 <- d %>% group_by(country) %>%
#     do(mics_regression(d=.,
#                        Y =i,
#                        X="WASH",
#                        W=Wvars,
#                        weight = "ecpopweight_H",
#                        clustid= "clust_num",
#                        family="modified possion", calc_PAF=T, low_risk_level=1))
#   
#   res_adj_bin <- bind_rows(res_adj_bin, res1, res2, res3, res4, res5, res6, res7)
# }

saveRDS(res_adj_bin, here("results/adjusted_bin.rds"))



#continious outcomes


# res_adj_cont <- NULL
# for(i in c("haz", "whz")){
#   res1 <- res2 <- res3 <- res4 <- res5 <- res6 <- res7 <- NULL
#   res1 <- d %>% group_by(country) %>%
#     do(mics_regression(d=.,
#                        Y =i,
#                        X="EC_H",
#                        W=Wvars,
#                        weight = "ecpopweight_H",
#                        clustid= "clust_num",
#                        family="gaussian"))
#   res2 <- d %>% group_by(country) %>%
#     do(mics_regression(d=.,
#                        Y =i,
#                        X="EC_S",
#                        W=Wvars,
#                        weight = "ecpopweight_S",
#                        clustid= "clust_num",
#                        family="gaussian"))
#   res3 <- d %>% group_by(country) %>%
#     do(mics_regression(d=.,
#                        Y =i,
#                        X="san_imp",
#                        W=Wvars,
#                        weight = "popweight",
#                        clustid= "clust_num",
#                        family="gaussian"))
#   res4 <- d %>% group_by(country) %>%
#     do(mics_regression(d=.,
#                        Y =i,
#                        X="wat_imp",
#                        W=Wvars,
#                        weight = "popweight",
#                        clustid= "clust_num",
#                        family="gaussian"))
#   res5 <- d %>% group_by(country) %>%
#     do(mics_regression(d=.,
#                        Y =i,
#                        X="hyg_imp",
#                        W=Wvars,
#                        weight = "popweight",
#                        clustid= "clust_num",
#                        family="gaussian"))
#   res6 <- d %>% group_by(country) %>%
#     do(mics_regression(d=.,
#                        Y =i,
#                        X="WASH",
#                        W=Wvars,
#                        weight = "popweight",
#                        clustid= "clust_num",
#                        family="gaussian"))
#   
#   res7 <- d %>% group_by(country) %>%
#     do(mics_regression(d=.,
#                        Y =i,
#                        X="safely_manH20",
#                        W=Wvars,
#                        weight = "popweight",
#                        clustid= "clust_num",
#                        family="gaussian"))
#   
#   res_adj_cont <- bind_rows(res_adj_cont, res1, res2, res3, res4, res5, res6, res7)
# }



saveRDS(res_adj_cont, here("results/adjusted_cont.rds"))

res_adj <- bind_rows(res_adj_bin, res_adj_cont)

saveRDS(res_adj, here("results/adjusted_RR.rds"))

