

source("0-config.R")


#load clean data
d <- readRDS(here("data/compiled_clean_POC_survey.rds"))



Wvars <- c("educ",
           "mage",
           "aged",
           "sex",
           "birthord", 
           #"rural", - drop rural as covariate
           "everbf", 
           "currbf",
           "nhh",
           "nchild5",
           "floor",
           "cookstove",
           "chimney",
           "fuel",
           "roof",
           "wall",
           "own_animals",
           "HHwealth_quart",
           "nroom_sleeping")



    

                
d$country <- paste0(d$country, "-",d$rural)


#-------------------------------------------------
# Adjusted analysis
#-------------------------------------------------

res_adj <- res_adj_bin <- res_adj_cont <- NULL

d <- droplevels(d)




#binary_outcomes


res_adj <- res_adj_bin <- res_adj_cont <- NULL

d <- droplevels(d)
res_adj_bin <- run_MICS_regressions(outcomes = c("stunt", "wast","diarrhea","ari"), family="modified possion", PAF=F, Wvars=Wvars)
res_adj_cont <- run_MICS_regressions(outcomes = c("haz", "whz"), family="gaussian", PAF=F, Wvars=Wvars)


res_adj <- bind_rows(res_adj_bin, res_adj_cont)

saveRDS(res_adj, here("results/adjusted_rural_subgroup.rds"))

