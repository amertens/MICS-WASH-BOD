
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

