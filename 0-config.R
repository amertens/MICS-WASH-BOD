
rm(list=ls())

library(tidyverse)
library(haven)
library(here)
library(ggeasy)
library(janitor)
library(viridis)
library(washb)
library(survey)
library(geepack)
library(lme4)


#remotes::install_github("epix-project/mics")

source(here::here("functions/load_data_functions.R"))

data_dir = "C:/Users/andre/Dropbox/MICS-WASH-BOD/Data/"



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