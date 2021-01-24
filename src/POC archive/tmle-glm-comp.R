
#Uncontaminated HH water, diarrhea, Bangladesh


source("0-config.R")




#load clean data
dfull <- readRDS(here("data/compiled_clean_POC_survey.rds"))

#subset to just POC countries
d <- dfull %>% filter(country %in% c("Bangladesh"))
d <- droplevels(d)


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






Y="diarrhea"
X="EC_H"
W=Wvars
weight = "ecpopweight_H"
clustid= "clust_num"
family="binomial"
glm=T
return_model=FALSE
  
  df <- data.frame(
    Y=subset(d, select=get(Y)),
    X=subset(d, select=get(X)),
    id=subset(d, select=get(clustid)),
    weight=subset(d, select=get(weight)),
    subset(d, select=W))
  
  varnames <- colnames(df)[1:4]
  colnames(df)[1:4] <- c("Y","X","id","weight")
  
  fullrows <- nrow(df)
  df <- df %>% filter(!is.na(Y))
  Yrows <- nrow(df)
  cat("Rows dropped due to missing outcome: ",fullrows - Yrows,"\n")
  df <- df %>% filter(!is.na(X))
  Xrows <- nrow(df)
  cat("Rows dropped due to missing exposure: ",Yrows -Xrows,"\n")
  df <- df[complete.cases(df),]
  cat("Rows dropped due to missing covariates: ",Xrows - nrow(df),"\n")
  
  df <- droplevels(df)
  
  Wscreen=NULL
  if(!is.null(W)){
    cat("\n-----------------------------------------\nPre-screening the adjustment covariates:\n-----------------------------------------\n")
    prescreen_family <- ifelse(family=="gaussian",family,"binomial")
    
    Wdf <- df %>% select(W)
    #drop covariates with near zero variance
    if(length(nearZeroVar(Wdf))>0){
      Wdf<-Wdf[,-nearZeroVar(Wdf)]
    }
    
    #prescreen
    suppressWarnings(Wscreen <- 
                       MICS_prescreen(Y = df$Y, 
                                      Ws = Wdf, 
                                      family = prescreen_family, 
                                      pval = 0.2, print = T))
    #select n/10 covariates if binary outcome
    if(family!="gaussian" & !is.null(Wscreen)){
      nY<-floor(min(table(df$Y, df$X))/10)
      if(length(Wscreen)>nY){
        Wscreen<-Wscreen[1:nY]
      }
    }
    
  }
  
  
  processed <- process_missing(data=df, node_list,  max_p_missing = 0.5)
  df_processed <- processed$data
  df_processed <- droplevels(df_processed)
  node_list <- processed$node_list
  
  
  
  df <- df_processed
  df$aged <- scale(df$aged)
  head(df)
  saveRDS(df, here("results/tmle_glm_example_data.rds"))
  
  
  
  library(tidyverse)
  library(tlverse)
  library(sl3)
  library(tmle3)
  
  #read in data
  df <- readRDS(here("results/tmle_glm_example_data.rds"))
  head(df)
  table(df$X, df$Y)

  
  #Set up TMLE
  set.seed(12345)
  #fit model
  
  #note, not useing ID or weights for simplicity, but real regression
  #and TMLE models will be weighted with clustered SE's
  node_list <- list(
    W="aged",
    #id="id",
    #weight="weight",
    A="X",
    Y="Y"
  )
  
  

  #Set up contrast
  RR_spec <- tmle_RR(
    baseline_level = levels(df$X)[1],
    contrast_level = levels(df$X)[2]
  )


  #Set up SL library
  lrnr_glm <- make_learner(Lrnr_glm)
  SL_list=list(lrnr_glm)
  ls_metalearner <- make_learner(Lrnr_solnp)
  sl_Y <- Lrnr_sl$new(learners = SL_list,
                      metalearner = ls_metalearner)
  sl_A <- Lrnr_sl$new(learners = SL_list,
                      metalearner = ls_metalearner)
  learner_list <- list(A = sl_A, Y = sl_Y)
  

  #fit tmle
  tmle_fit <- tmle3(RR_spec, df, node_list, learner_list)$summary
  tmle_fit
  
  #fit glm
  fit <- glm(Y ~ X + aged,family=poisson(link="log"), data=df)
  summary(fit)
  
  
     
     