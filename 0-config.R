

library(tidyverse)
library(haven)
library(here)
library(ggeasy)
library(janitor)
library(viridis)
#library(washb)
library(survey)
library(geepack)
library(lme4)
library(caret)
library(janitor)
library(forcats)
library(metafor)
library(R.utils)
# library(tlverse)
# library(sl3)
try(library(washb))

#remotes::install_github("Larsvanderlaan/tmle3@firstChanges") #Lar's development version with survey weights
#library(tmle3)

library(mice) # Data imputation
library(miceadds)
library(purrr) # Flexible functional programming

#remotes::install_github("epix-project/mics")

source(here::here("functions/load_data_functions.R"))
source(here::here("functions/analysis_functions.R"))
source(here::here("functions/plot_functions.R"))
source(here::here("functions/analysis_wrapper_functions.R"))

data_dir <- NULL
if(dir.exists("C:/Users/andre/Dropbox/MICS-WASH-data/")){ 
  data_dir = "C:/Users/andre/Dropbox/MICS-WASH-data/"
}
if(dir.exists("C:/Users/anmol/OneDrive/Documents/GitHub/MICS-WASH-BOD/MICS-WASH-data/")){ 
  data_dir <- "C:/Users/anmol/OneDrive/Documents/GitHub/MICS-WASH-BOD/MICS-WASH-data/"
}

sandwichSE <- function(dat, fm, cluster){
  require(sandwich, quietly = TRUE)
  require(lmtest, quietly = TRUE)
  if (is.factor(cluster)) {
    cluster <- droplevels(cluster)
  }
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- fm$rank
  dfc <- (M/(M - 1)) * ((N - 1)/(N - K))
  uj <- apply(estfun(fm), 2, function(x) tapply(x, cluster, 
                                                sum))
  vcovCL <- dfc * sandwich(fm, meat = crossprod(uj)/N)
  return(vcovCL)
}


scaleFUN <- function(x) sprintf("%.2f", x)
scaleRR <- function(x) sprintf("%.1f", x)



#Adjustment covariates


Wvars <- c("educ",
           "mage",
           "aged",
           "sex",
           "birthord", 
           "rural",
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


