

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


# #load clean data (need to add E coli counts)
# d <- readRDS(here("data/compiled_clean_POC_survey.rds"))
# 
# 
# 
# Wvars <- c("educ",
#            "mage",
#            "aged",
#            "sex",
#            "birthord", 
#            "rural",
#            #"everbf", 
#            "currbf",
#            "nhh",
#            "nchild5",
#            "floor",
#            "cookstove",
#            "chimney",
#            "fan",
#            "fuel",
#            "roof",
#            "wall",
#            "nroom_sleeping")
# 
# 
#                 
# d$clust_num <- paste0(d$clust_num, "-",d$HH_num)



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
  learners = list(lrn_mean, lrn_fglm, lrn_xgb),
  metalearner = Lrnr_nnls$new()
)


sl3_list_learners("density")



# learners used for conditional density regression (i.e., propensity score)
lrn_haldensify <- Lrnr_haldensify$new(
  n_bins = 5, grid_type = "equal_mass",
  lambda_seq = exp(seq(-1, -13, length = 500))
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



# simulate simple data for tmle-shift sketch
n_obs <- 500 # number of observations
tx_mult <- 2 # multiplier for the effect of W = 1 on the treatment

## baseline covariates -- simple, binary
W <- replicate(2, rbinom(n_obs, 1, 0.5))

## create treatment based on baseline W
A <- rnorm(n_obs, mean = tx_mult * W, sd = 1)

## create outcome as a linear function of A, W + white noise
Y <- rbinom(n_obs, 1, prob = plogis(A + W))

# organize data and nodes for tmle3
data <- data.table(W, A, Y)
setnames(data, c("W1", "W2", "A", "Y"))
node_list <- list(W = c("W1", "W2"), A = "A", Y = "Y")
head(data)





# initialize a tmle specification
tmle_spec <- tmle_shift(
  shift_val = 0.5,
  shift_fxn = shift_additive_bounded,
  shift_fxn_inv = shift_additive_bounded_inv
)




tmle_fit <- tmle3(tmle_spec, data, node_list, learner_list)








