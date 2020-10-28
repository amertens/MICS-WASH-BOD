
source("0-config.R")


#load clean data (need to add E coli counts)
d <- readRDS(here("data/compiled_clean_POC_survey.rds"))

d <- d %>% filter(country=="Zimbabwe")


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



library(here)
library(tidyverse)
library(data.table)
library(sl3)
library(tmle3)
library(tmle3shift)
set.seed(429153)


# learners used for conditional expectation regression
lrn_mean <- Lrnr_mean$new()
lrn_fglm <- Lrnr_glm_fast$new()
lrn_xgb <- Lrnr_xgboost$new(nrounds = 200)
sl_lrn <- Lrnr_sl$new(
  learners = list(lrn_mean, lrn_fglm #, lrn_xgb
                  ),
  metalearner = Lrnr_nnls$new()
)


sl3_list_learners("density")



# learners used for conditional density regression (i.e., propensity score)
lrn_haldensify <- Lrnr_haldensify$new(
  n_bins = 5, grid_type = "equal_mass",
  lambda_seq = exp(seq(-1, -13, length = 5))
)
# lrn_rfcde <- Lrnr_rfcde$new(
#   n_trees = 1000, node_size = 5,
#   n_basis = 31, output_type = "observed"
# )
sl_lrn_dens <- Lrnr_sl$new(
  learners = list(lrn_haldensify),
  metalearner = Lrnr_solnp_density$new()
)

  
learner_list <- list(Y = sl_lrn, A = sl_lrn_dens)


# organize data and nodes for tmle3
head(d)

summary(d$EC_cfu_H)
node_list <- list(W = Wvars[1], A = "EC_cfu_H", Y = "diarrhea")





# initialize a tmle specification
tmle_spec <- tmle_shift(
  shift_val = 10,
  shift_fxn = shift_additive_bounded,
  shift_fxn_inv = shift_additive_bounded_inv
)




df <- d %>% filter(!is.na(EC_cfu_H), !is.na(diarrhea))
summary(df$EC_cfu_H)

tmle_fit <- tmle3(tmle_spec, df, node_list, learner_list)








