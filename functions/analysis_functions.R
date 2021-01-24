

mics_regression <- function(d, Y, X, W, weight = "ecpopweight_H", clustid= "clust_num", family="modified possion", calc_PAF=FALSE, low_risk_level=0, return_model=FALSE){
  
  cat("\n",Y,", ",X," ",d$country[1],"\n")
  
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
  # df <- df[complete.cases(df),]
  # cat("Rows dropped due to missing covariates: ",Xrows - nrow(df),"\n")
  
  df <- droplevels(df)
  
  if(nrow(df)!=0){
  
  Wscreen=NULL
  if(!is.null(W)){
    #cat("\n-----------------------------------------\nPre-screening the adjustment covariates:\n-----------------------------------------\n")
    prescreen_family <- ifelse(family=="gaussian",family,"binomial")
    
    Wdf <- df %>% select(W)
    #drop covariates with near zero variance
     if(length(nearZeroVar(Wdf))>0){
            Wdf<-Wdf[,-nearZeroVar(Wdf)]
     }
    
    #prescreen
    suppressWarnings(try(Wscreen <- 
                       MICS_prescreen(Y = df$Y, 
                                       Ws = Wdf, 
                                       family = prescreen_family, 
                                       pval = 0.2, print = F)))
    #select n/10 covariates if binary outcome
    if(family!="gaussian" & !is.null(Wscreen)){
      nY<-floor(min(table(df$Y, df$X))/10)
      if(length(Wscreen)>nY){
        Wscreen<-Wscreen[1:nY]
      }
    }
   
    #Drop sparse factor levels
    if(!is.null(Wscreen)){
      Wdf <- df %>% subset(., select =c(Wscreen))
      Wdf <- as.data.frame(model.matrix(~., model.frame(~ ., Wdf, na.action=na.pass))[,-1]) 
      
      #scale and drop covariates with near zero variance
      pp_no_nzv <- preProcess(Wdf,
                              method = c("center", "scale", "YeoJohnson", "nzv"))
      pp_no_nzv
      Wdf<- predict(pp_no_nzv, newdata = Wdf)
    }else{
      Wdf=NULL
    }
    
    df <- bind_cols(df %>% subset(., select =c("Y","X","id", "weight")), Wdf)
  }
  
  #order by id for geeglm function
  df <- df %>% arrange(id)
  
  if(family=="gaussian"){
    # #model formula
    # f <- ifelse(is.null(Wscreen),
    #             "Y ~ X + (1|id)",
    #             paste0("Y ~ X + (1|id) + ", paste(colnames(Wdf), collapse = " + ")))
    # #fit model
    # fit <- lmer(as.formula(f),  data=df, weights = weight)
    # 
    # coef <- as.data.frame(t(summary(fit)$coefficients[2,]))
    # res <- data.frame(Y=varnames[1],
    #                   X=varnames[2],
    #                   coef=coef$Estimate,
    #                   se=coef$`Std. Error`,
    #                   tval=coef$`t value`)
    # 
    # #Calc 95%CI
    # res$ci.lb <- res$coef - 1.96*res$se
    # res$ci.ub <- res$coef + 1.96*res$se
    # 
    # #calculate pvalue by normal approximation
    # #https://www.r-bloggers.com/three-ways-to-get-parameter-specific-p-values-from-lmer/
    # res$pval <- 2 * (1 - pnorm(abs(res$tval)))
    # res$N<-nrow(df)
    # res$W <-ifelse(is.null(Wscreen), "unadjusted", paste(Wscreen, sep="", collapse=", "))
    
    
    if(length(unique(df$X))>1){
    
    #model formula
    f <- ifelse(is.null(Wscreen),
                "Y ~ X",
                paste0("Y ~ X  + ", paste(colnames(Wdf), collapse = " + ")))
    
    fit=NULL
    
    #fit <- mpreg(formula = as.formula(f), df = df, family="gaussian", vcv=FALSE)
    res <- mpreg(varnames=varnames, formula = as.formula(f), df = df, family="gaussian")
    
    res$N<-nrow(df)
    res$W <-ifelse(is.null(Wscreen), "unadjusted", paste(Wscreen, sep="", collapse=", "))
    }else{
      
      res <- data.frame(Y=varnames[1],
                        X=varnames[2],
                        coef=NA,
                        RR=NA,
                        se=NA,
                        Zval=NA,
                        pval=NA)
      
      res$N<-nrow(df)
      res$W <-ifelse(is.null(Wscreen), "unadjusted", paste(Wscreen, sep="", collapse=", "))
      
    }
    
  }
  if(family=="modified possion"){
    
    #check sparsity
    sparseN<-min(table(df$X, df$Y))
    if(sparseN>10 & min(dim(table(df$X, df$Y)))==2){
    
    
    #model formula
    f <- ifelse(is.null(Wscreen),
                "Y ~ X",
                paste0("Y ~ X + ", paste(colnames(Wdf), collapse = " + ")))

    fit=NULL
    res <- mpreg(varnames=varnames, formula = as.formula(f), df = df, family="modified possion", vcv=FALSE)
    
      
    #   #fit model
    # tryCatch( { fit <- withTimeout( { 
    #    geeglm(formula = as.formula(f),
    #                 data    = df,
    #                 weights = weight,
    #                 family  = poisson(link = "log"),
    #                 id      = id,
    #                 corstr  = "exchangeable") 
    #   },
    #   timeout = 120) },
    #   TimeoutException = function(ex) cat("Timed out\n"))
    # 
    #   coef <- as.data.frame(summary(fit)$coefficients[2,])
    #   res <- data.frame(Y=varnames[1],
    #                     X=varnames[2],
    #                     coef=coef$Estimate,
    #                     RR=exp(coef$Estimate),
    #                     se=coef$Std.err,
    #                     wald=coef$Wald,
    #                     pval=coef$`Pr(>|W|)`)
    #   
    #   #Calc 95%CI
    #   res$ci.lb <- exp(res$coef - 1.96*res$se)
    #   res$ci.ub <- exp(res$coef + 1.96*res$se)
    #   
    #   #calculate pvalue by normal approximation
    #   #https://www.r-bloggers.com/three-ways-to-get-parameter-specific-p-values-from-lmer/

    
      if(calc_PAF){
        

        paflist <- bootAR(fn=ARfun,fmla = as.formula(f),data=df,ID=df$id,strata=rep(1, nrow(df)),iter=10,dots=TRUE, low_risk_level=low_risk_level)
        res$PAF <- paflist$bootest[2]
        res$PAF.lb <- paflist$boot95lb[2]
        res$PAF.ub <- paflist$boot95ub[2]
        res$low_risk_level=low_risk_level
      }
      
      res$n<-sum(df$Y, na.rm=T)
      res$N<-nrow(df)
      res$W <-ifelse(is.null(Wscreen), "unadjusted", paste(Wscreen, sep="", collapse=", "))
 
    }else{
      res <- data.frame(Y=varnames[1],
                        X=varnames[2],
                        coef=NA,
                        RR=NA,
                        se=NA,
                        Zval=NA,
                        pval=NA)
      if(calc_PAF){
        res$PAF <-NA
        res$PAF.lb <- NA
        res$PAF.ub <- NA
      }
      
      res$n<-sum(df$Y, na.rm=T)
      res$N<-nrow(df)
      res$W <-ifelse(is.null(Wscreen), "unadjusted", paste(Wscreen, sep="", collapse=", "))
      
    }
    
  }  
  
  }else{
    res <- data.frame(Y=varnames[1],
                      X=varnames[2],
                      coef=NA,
                      RR=NA,
                      se=NA,
                      Zval=NA,
                      pval=NA,
                      n=0,
                      N=0)
    }
  
  if(return_model){
    if(calc_PAF){
      return(list(res=res, fit=fit, paflist=paflist)) 
    }else{
      return(list(res=res, fit=fit)) 
    }
  }else{
    return(res)
  }
}






mics_tmle <- function(d, Y, X, W, weight = "ecpopweight_H", clustid= "clust_num", family="binomial", glm=F, return_model=FALSE){
  
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
  
  
  
  
  #Set up TMLE
  set.seed(12345)
  #fit model
    node_list <- list(
      W=Wscreen,
      id="id",
      weight="weight",
      A="X",
      Y="Y"
    )
  
  
  processed <- process_missing(data=df, node_list,  max_p_missing = 0.5)
  df_processed <- processed$data
  df_processed <- droplevels(df_processed)
  node_list <- processed$node_list
  
  if(identical(node_list$W,character(0))){
    node_list$W <- NULL
  }
  
  ate_spec <- tmle_ATE(
    treatment_level = levels(df_processed$X)[2],
    control_level = levels(df_processed$X)[1]
  )
  RR_spec <- tmle_RR(
    baseline_level = levels(df_processed$X)[1],
    contrast_level = levels(df_processed$X)[2]
  )
  PAR_spec <- tmle_PAR(
    baseline_level = levels(df_processed$X)[1]
  )
  

  # choose base learners
  #We will include simple means, generalized linear models, generalized additive models, penalized regressions, and gradient boosting machines  in the machine-learning ensemble. 
  
  lrnr_mean <- make_learner(Lrnr_mean)
  lrnr_glm <- make_learner(Lrnr_glm)
  gbm_lrnr <- Lrnr_gbm$new()
  #lrnr_xgboost <- make_learner(Lrnr_xgboost)
  lrnr_glmnet <- Lrnr_glmnet$new()
  gam_lrnr <- Lrnr_gam$new()
  
  
  if(glm){
    SL_list=list(lrnr_glm)
  }else{
    SL_list=list(lrnr_mean, lrnr_glm, gam_lrnr, lrnr_glmnet, gbm_lrnr)
  }
  
  # define metalearners appropriate to data types
  ls_metalearner <- make_learner(Lrnr_solnp)
  sl_Y <- Lrnr_sl$new(learners = SL_list,
                      metalearner = ls_metalearner)
  sl_A <- Lrnr_sl$new(learners = SL_list,
                      metalearner = ls_metalearner)
 
  # ls_metalearner <- make_learner(Lrnr_nnls)
  # sl_Y <- sl_A <- make_learner(Lrnr_glm)
  learner_list <- list(A = sl_A, Y = sl_Y)
  
  #XXXXXXXXXXXXXXXXXXXXXXXXX
  # ADD SAMPLE WEIGHTs
  #XXXXXXXXXXXXXXXXXXXXXXXXX
  
  if(family=="gaussian"){
    tmle_fit <- tmle3(ate_spec, df_processed, node_list, learner_list)$summary
    
    res <- data.frame(Y=varnames[1],
                      X=varnames[2],
                      est=tmle_fit$tmle_est,
                      se=tmle_fit$se,
                      ci.lb = tmle_fit$lower,
                      ci.ub = tmle_fit$upper)
    res$N<-nrow(df_processed)
    
  }
  if(family=="binomial"){
    
    #check sparsity
    sparseN<-min(table(df$X, df$Y))
    if(sparseN>10){
      
    tmle_fit <- tmle3(RR_spec, df_processed, node_list, learner_list)$summary
    
  
    res <- data.frame(Y=varnames[1],
                      X=varnames[2],
                      RR=tmle_fit$psi_transformed[3],
                      est=tmle_fit$tmle_est[3],
                      se=tmle_fit$se[3],
                      ci.lb = tmle_fit$lower_transformed[3],
                      ci.ub = tmle_fit$upper_transformed[3])
    res$n<-sum(df_processed$Y, na.rm=T)
    res$N<-nrow(df_processed)   
    
    }else{
      res <- data.frame(Y=varnames[1],
                        X=varnames[2],
                        RR=NA,
                        est=NA,
                        se=NA,
                        ci.lb=NA,
                        ci.ub=NA)

      res$n<-sum(df_processed$Y, na.rm=T)
      res$N<-nrow(df_processed)
    }
    
  }  
  
  #calc PAR
  #PAR_fit <- tmle3(PAR_spec, df_processed, node_list, learner_list) #$summary
  
  
  
  res$W <-ifelse(is.null(Wscreen), "unadjusted", paste(Wscreen, sep="", collapse=", "))
  
  if(return_model){
    return(list(res=res, fit=fit))
  }else{
    return(res)
  }
}













#Prescreen function
MICS_prescreen<-function (Y, Ws, family = "gaussian", pval = 0.2, print = TRUE){
  require(lmtest)
  if (pval > 1 | pval < 0) {
    stop("P-value threshold not set between 0 and 1.")
  }
  Ws <- as.data.frame(Ws)
  dat <- data.frame(Ws, Y)
  nW <- ncol(Ws)
  LRp <- matrix(rep(NA, nW), nrow = nW, ncol = 1)
  rownames(LRp) <- names(Ws)
  colnames(LRp) <- "P-value"
    for (i in 1:nW) {
      dat$W <- dat[, i]
      df <- data.frame(Y=dat$Y, W=dat$W)
      df <- df[complete.cases(df), ]
      
      if (class(dat$W) == "factor" & dim(table(dat$W)) == 
          1) {
        fit1 <- fit0 <- glm(Y ~ 1, data = df, family = family)
      }
      else {
        fit1 <- glm(Y ~ W, data = df, family = family)
        fit0 <- glm(Y ~ 1, data = df, family = family)
      }
      LRp[i] <- lrtest(fit1, fit0)[2, 5]
    }
  
 
  p20 <- ifelse(LRp < pval, 1, 0)
  if (print == TRUE) {
    cat("\nLikelihood Ratio Test P-values:\n")
    print(round(LRp, 5))
    if (sum(p20) > 0) {
      LRps <- matrix(LRp[p20 == 1, ], ncol = 1)
      rownames(LRps) <- names(Ws)[p20 == 1]
      colnames(LRps) <- "P-value"
      LRps <- LRps %>% as.data.frame() %>% arrange(`P-value`)
      cat(paste("\n\nCovariates selected (P<", pval, 
                "):\n", sep = ""))
      print(LRps)
    }
    else {
      cat(paste("\nNo covariates were associated with the outcome at P<", 
                pval))
    }
  }
  if (sum(p20) > 0) {
    return(rownames(LRps))
  }else{
    return(NULL)
    
  }
}



# #use survey package to calculate correctly-weighted means 
# 
# df <- d %>% filter(!is.na(wat_imp))
# table(df$haz)


#Run linear regression for continious outcomes (what is the right clustering/weighting?)
#We will include random effects for households nested within sampling clusters to account for the survey design.
#weights for e-coli: ecpopweight_S or ecpopweight_H, while popweight for other exposures


#Other sources:
#https://stat.ethz.ch/pipermail/r-sig-mixed-models/2010q4/004700.html


#Run modified poisson for binary outcomes
#https://rstudio-pubs-static.s3.amazonaws.com/5752_fc41dca85dd24539bc99868697de83d0.html
#https://stats.stackexchange.com/questions/346662/poisson-approximation-of-a-binomial-model-with-random-effects-how-to-get-robus
#for clustered data, use cluster instead of individual as ID: http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.903.8133&rep=rep1&type=pdf


#other sources
# #Might need to use this package for robust estimation with mixed effects:
# #https://cran.r-project.org/web/packages/robustlmm/vignettes/rlmer.pdf
# #https://stats.stackexchange.com/questions/331840/how-to-calculate-sandwich-standard-errors-for-generalized-least-squares-models
# #shift estimate
# #https://tlverse.org/tmle3shift/







##############################################
# fit.rma
##############################################

# Documentation: fit.rma
# Usage: fit.rma(data,age,ni,xi,measure,nlab, method = "REML")
# Description: Take an input dataframe with each row summarizing the N's and cases from a single
#             cohort and then calculated the parameter of interest specified by measure, pooled
#             across all cohorts
# age (optional): variable level to filter the data by, selecting a specific age category


# Args/Options:
# data: a data frame with variables studyid, country, agecat, and outcome-specific summary measures for ni and xi
# ni: name of the variable that is the total count of observations
# xi:  name of the variable
# measure: fed into rma() function; character string indicating the type of data supplied to the function. "PLO" by default for logit transformed proportion
# nlab: optional label of the count of observations i.e. "children" to be appended to the formatted counts/
# method: fed into rma() function; haracter string specifying whether a fixed- ("FE") or a random/mixed-effects model ("REML") should be fitted.

# random effects function, save results nicely

fit.rma <- function(data, ni, xi = NULL, yi = NULL, vi = NULL, measure = "PLO", nlab = "", method = "REML", age=NULL) {
  if(!is.null(age)){
    data <- data[data$agecat==age,]
    data$age <- age
  }
  
  mode_continuous <- !is.null(yi) | !is.null(vi)
  mode_binary <- !is.null(xi)
  if (mode_binary & mode_continuous) stop("can only do binary or continuous")
  # check if measure=="PLO" - default for all parameters bounded between 0 and 1 (prevalence, cumulative incidence)
  # because then output needs back transformation
  if (measure == "PLO" & mode_binary) {
    # If only one row of the data, no need for pooling (single study, often seens when looping over agecats),
    # so just use the escalc() function that underlies RMA to calculate the parameter for the single study
    if (nrow(data) == 1) {
      fit <- NULL
      try(fit <- escalc(data = data, ni = data[[ni]], xi = data[[xi]], method = method, measure = measure, append = T))
      data <- fit
      data$se <- sqrt(data$vi)
      out <- data %>%
        ungroup() %>%
        mutate(nstudies = 1, nmeas = data[[ni]]) %>%
        mutate(
          est = plogis(yi),
          lb = plogis(yi - 1.96 * se),
          ub = plogis(yi + 1.96 * se),
          nmeas.f = paste0("N=", format(sum(data[[ni]]), big.mark = ",", scientific = FALSE), " ", nlab),
          nstudy.f = paste0("N=", nstudies, " studies"),
          method.used=method,
          ptest.f = sprintf("%0.0f", est)
        ) %>%
        subset(., select =c(nstudies, nmeas, est, se, lb, ub, nmeas.f, nstudy.f, method.used, agecat, ptest.f)) %>%
        as.tibble()
      rownames(out) <- NULL
      # If input is more than 1 row (multiple studies), pool across studies with rma() function from metafor package
    } else {
      # Check if 0 cases of the outcome
      # Use FE model if all 0 counts because no heterogeneity and rma.glmm fails
      if(!is.null(xi)){
        if (sum(data[[xi]]) == 0) method <- "FE"
      }
      fit <- NULL
      method_fit <- method
      try(fit <- rma(
        data = data,
        ni = data[[ni]],
        method = method,
        xi = data[[xi]],
        measure = measure
      ))
      if(is.null(fit)){try(fit <- rma(
        data = data,
        ni = data[[ni]],
        method = "ML",
        xi = data[[xi]],
        measure = measure
      ))
        method_fit <- "ML"
      }
      if(is.null(fit)){try(fit <- rma(
        data = data,
        ni = data[[ni]],
        method = "DL",
        xi = data[[xi]],
        measure = measure
      ))
        method_fit <- "DL"
      }
      # Create formatted dataset to return
      out <- data %>%
        ungroup() %>%
        summarise(
          nstudies = length(unique(paste0(studyid," ",country))),
          nmeas = sum(data[[ni]])
        ) %>%
        mutate(
          est = plogis(fit$beta),
          se = fit$se,
          lb = plogis(fit$beta - 1.96 * fit$se),
          ub = plogis(fit$beta + 1.96 * fit$se),
          nmeas.f = paste0("N=", format(sum(data[[ni]]), big.mark = ",", scientific = FALSE), " ", nlab),
          nstudy.f = paste0("N=", nstudies, " studies"),
          method.used=method_fit
        ) %>%
        as.tibble()
      rownames(out) <- NULL
    }
  } else {
    
    if(measure == "IR"){
      # If measure if IR for incidence rate, use `to="if0all"` argument to add .5 to all cells of the 2x2 table if one is 0 so rma() works
      to <- "if0all"
      fit <- NULL
      method_fit <- method
      try(fit <- rma(
        data = data,
        ti = data[[ni]],
        method = method,
        xi = data[[xi]],
        measure = measure,
        to=to
      ))
      if(is.null(fit)){try(fit <- rma(
        data = data,
        ti = data[[ni]],
        method = "ML",
        xi = data[[xi]],
        measure = measure
      ))
        method_fit <- "ML"
      }
      if(is.null(fit)){try(fit <- rma(
        data = data,
        ti = data[[ni]],
        method = "DL",
        xi = data[[xi]],
        measure = measure
      ))
        method_fit <- "DL"
      }
      
    }else{
      
      
      # If measure other than PLO or IR is chosen:
      to <- "only0"
      
      fit <- NULL
      method_fit <- method
      
      if (mode_binary) {
        try(fit <- rma(
          data = data,
          ni = data[[ni]],
          method = method,
          xi = data[[xi]],
          measure = measure,
          to=to
        ))
        if(is.null(fit)){try(fit <- rma(
          data = data,
          ni = data[[ni]],
          method = "ML",
          xi = data[[xi]],
          measure = measure
        ))
          method_fit <- "ML"
        }
        if(is.null(fit)){try(fit <- rma(
          data = data,
          ni = data[[ni]],
          method = "DL",
          xi = data[[xi]],
          measure = measure
        ))
          method_fit <- "DL"
        }
      }
      if (mode_continuous) {
        try(fit <- rma(
          data = data,
          mi = data[[yi]],
          sdi = sqrt(data[[vi]]),
          ni = data[[ni]],
          method = method,
          measure = "MN"
        ))
        if(is.null(fit)){try(fit <- rma(
          data = data,
          mi = data[[yi]],
          sdi = sqrt(data[[vi]]),
          ni = data[[ni]],
          method = "ML",
          measure = "MN"))
          method_fit <- "ML"
        }
        if(is.null(fit)){try(fit <- rma(
          data = data,
          mi = data[[yi]],
          sdi = sqrt(data[[vi]]),
          ni = data[[ni]],
          method = "DL",
          measure = "MN"))
          method_fit <- "DL"
        }
      }
    }
    out <- data %>%
      ungroup() %>%
      summarise(
        nstudies = length(unique(studyid)),
        nmeas = sum(data[[ni]])
      ) %>%
      mutate(
        est = fit$beta,
        se = fit$se,
        lb = fit$ci.lb,
        ub = fit$ci.ub,
        nmeas.f = paste0(
          "N=", format(sum(data[[ni]]), big.mark = ",", scientific = FALSE),
          " ", nlab
        ),
        nstudy.f = paste0("N=", nstudies, " studies"),
        method.used=method_fit
      )
    
  }
  return(out)
}

# it automatically figures out if you input continous data or binary data.
# to recover the old continuous functionality
# fit.cont.rma = fit.rma(data, yi, vi, ni, nlab, method, measure = "GEN")

# fit.rma.rec.cohort is also combined into fit.rma
# fit.rma.rec.cohort = fit.rma(data, ni, xi, measure = "PLO", nlab, method = "REML")

# the original fit.rma
# fit.rma(data, xi, ni, nlab, method, measure = "GEN")

##############################################
# fit.escalc
##############################################


# calc individual cohort parameters, variances, standard errors, and 95% CI from the rma() arguements
# Input:
# meas: PR for prevalence, CI for cumulative incidence, and IR for incidence rate
# PLO for log transformed prevalence (to prevent 95% CI outside 0 and 1).
# age (optional): variable level to filter the data by, selecting a specific age category

# Returns:
# Inputted dataframe with appended columns
# yi = outcome of interest
# vi = variance of outcome
# se = standard error
# ci.lb = lower bound of 95% confidence interval
# ci.ub = upper bound of 95% confidence interval

fit.escalc <- function(data, ni, xi = NULL, yi = NULL, vi = NULL, measure, method = "REML", age=NULL) {
  if(!is.null(age)){data <- data[data$agecat==age,]}
  
  mode_continuous <- !is.null(yi) | !is.null(vi)
  mode_binary <- !is.null(xi)
  if (mode_binary & mode_continuous) stop("can only do binary or continuous")
  if(measure == "IR"){
    data <- escalc(data = data, ti = data[[ni]], xi = data[[xi]], method = method, measure = measure, append = TRUE)
  }
  if (mode_binary & measure!="IR") {
    data <- escalc(data = data, ni = data[[ni]], xi = data[[xi]], method = method, measure = measure, append = TRUE)
  }
  if (mode_continuous) {
    data <- data.frame(data, escalc(data = data, ni = data[[ni]], mi = data[[yi]], sdi = sqrt(data[[vi]]), method = method, measure = "MN"))
  }
  if (measure == "PLO") {
    data$se <- sqrt(data$vi)
    data$ci.lb <- plogis(data$yi - 1.96 * data$se)
    data$ci.ub <- plogis(data$yi + 1.96 * data$se)
    data$yi <- plogis(data$yi)
  } else {
    
    data$se <- sqrt(data$vi)
    data$ci.lb <- data$yi - 1.96 * data$se
    data$ci.ub <- data$yi + 1.96 * data$se
  }
  return(data)
}
# it automatically figures out if you input continous data or binary data.
# to recover the old continuous functionality
# fit.escalc.cont = fit.escalc(data, yi, vi, measure = "GEN", method = "REML")




poolRR <- function(d, method="REML"){
  Ncountries <- d %>% ungroup() %>% summarise(N=n())
  
    fit<-NULL
    try(fit<-rma(yi=coef, sei=se, data=d, method=method, measure="RR"))
    if(method=="REML"){
      if(is.null(fit)){try(fit<-rma(yi=coef, sei=se, data=d, method="ML", measure="RR"))}
      if(is.null(fit)){try(fit<-rma(yi=coef, sei=se, data=d, method="DL", measure="RR"))}
      if(is.null(fit)){try(fit<-rma(yi=coef, sei=se, data=d, method="HE", measure="RR"))}
    }
    if(is.null(fit)){
      est<-data.frame(logRR.psi=NA, logSE=NA, RR=NA, RR.CI1=NA,  RR.CI2=NA)
    }else{
      
      est<-data.frame(fit$b, fit$se)
      colnames(est)<-c("logRR.psi","logSE")
      
      est$RR<-exp(est$logRR)
      est$RR.CI1<-exp(est$logRR - 1.96 * est$logSE)
      est$RR.CI2<-exp(est$logRR + 1.96 * est$logSE)
      
      est$Ncountries <- Ncountries$N
    }
  
  return(est)
}




pool.cont <- function(d, method="REML"){
  nstudies <- d %>% summarise(N=n())
  
  #cat("Var: ", d$intervention_variable[1], " level: ",d$intervention_level[1] ," age: ", d$agecat[1] , "nstudies: ", nstudies$N, "\n")
    
    fit<-NULL
    try(fit<-rma(yi=coef, sei=se, data=d, method=method, measure="GEN"))
    if(method=="REML"){
      if(is.null(fit)){try(fit<-rma(yi=coef, sei=se, data=d, method="ML", measure="GEN"))}
      if(is.null(fit)){try(fit<-rma(yi=coef, sei=se, data=d, method="DL", measure="GEN"))}
      if(is.null(fit)){try(fit<-rma(yi=coef, sei=se, data=d, method="HE", measure="GEN"))}
    }
    if(is.null(fit)){fit <- data.frame(b=NA, ci.lb=NA, ci.ub=NA)}
    est<-data.frame(fit$b, fit$ci.lb, fit$ci.ub)
    colnames(est)<-c("ATE","CI1","CI2")
    est$Nstudies <- nstudies$N

  rownames(est) <- NULL
  return(est)
}

















ARfun <- function(fmla,data,low_risk_level) {
  # a function to estimate a marginally adjusted attributable risk for two different parameters
  # a population attributable risk (PAR) comparing the empirical distribution vs. none exposed: P(Y|A,W) - P(Y|A=0,W)
  # and a population attributable fraction (PAF), which is the PAR / P(Y|A,W)
  #
  #
  # See Muller, C. J. & MacLehose, R. F. Estimating predicted probabilities from logistic regression: 
  #     different methods correspond to different target populations. Int J Epidemiol, 2014, 43, 962-970
  #     for a tutorial on marginal standardization for binary outcomes
  # See Greenland, Sander, and Karsten Drescher. Maximum likelihood estimation of the attributable fraction 
  #     from logistic models." Biometrics (1993): 865-872.
  #     for the relevant methods underlying the approach.
  # arguments:
  # fmla : a model formula for glm
  # data : data.frame that includes all of the variables in the formula, and the population used for estimation
  #         note: data must include "bodycontact" and all variables specified in the formula (fmla)
  
  data$X <- ifelse(data$X==low_risk_level, 1, 0)
  
  # --------------------------------------
  # Fit log-linear model used to estimate
  # counterfactuals.
  # --------------------------------------
  id <- data$id
  weight <- data$weight
  regfit <- glm(as.formula(fmla), family=poisson(link="log"), data=data, weights = weight)
  # regfit <- geeglm(formula = as.formula(fmla),
  #                  data    = data,
  #                  weights = weight,
  #                  family  = poisson(link = "log"),
  #                  id      = id,
  #                  corstr  = "exchangeable") 
  # 
  # --------------------------------------
  # create counterfactual datasets
  # and predicted P(Y|A,W)
  # --------------------------------------
  # Empirical Probability (adjusted for W)
  pY <- predict(regfit,type="response")
  
  # Counterfactual: everybody has improved WASH characteristic
  cf1 <- data
  cf1$X  <- 1
  pY1 <- predict(regfit,newdata=cf1,type="response")
  
  # Counterfactual: nobody has improved WASH characteristic
  cf0 <- data
  cf0$X  <- 0
  pY0 <- predict(regfit,newdata=cf0,type="response")
  
  
  # --------------------------------------
  # calculate marginally standardized means
  # for the empirical and counterfactual 
  # distributions of exposure
  # --------------------------------------
  mu  <- mean(pY)
  mu1 <- mean(pY1)
  mu0 <- mean(pY0)
  
  # --------------------------------------
  # estimate the parameters of interest
  # PAR: P(Y|A,  W) - P(Y|A=1,W)
  # PAF: 100% * [P(Y|A,  W) - P(Y|A=1,W)] / P(Y|A,  W)
  # --------------------------------------
  #if(low_risk_level==1){
    thetaPAR <- mean(pY-pY1)
    thetaPAF <- 100*( thetaPAR/mean(pY) )    
  # }else{
  #   thetaPAR <- mean(pY-pY0)
  #   thetaPAF <- 100*( thetaPAR/mean(pY) )
  # }

  
  # --------------------------------------
  # return marginal means and 
  # parameters of interest as a list
  # --------------------------------------
  list(thetaPAR=thetaPAR,thetaPAF=thetaPAF,mu=mu,mu1=mu1,mu0=mu0)
}



















#-------------------------------------------
# Stratified bootstrap resampling function
# Samples n_i households from stratum i,
# where stratum in this study is beach cohort
# 
# This function is tailored to store multiple
# statistics generated by the ARswimex and AR35cfu 
# functions above.  
#-------------------------------------------


bootAR <- function(fn=ARfun,fmla,data,ID,strata,iter,low_risk_level,dots=TRUE) {
  # fn    : an Attributable risk function to call
  # fmla  : a model formula for glm, which is passed to fn
  # data  : data.frame that includes all of the variables in the formula, and the population used for estimation
  #         which is passed to fn.
  # ID    : ID for resampling (e.g., household ID required b/c of repeated, potentially correlated obs w/in HH)
  # strata: stratifying variable 
  # iter  : number of bootstrap iterations	
  # dots  : logical. print dots for iterations
  if(length(ID)!=nrow(data) | length(strata)!=nrow(data)) {
    print("data, ID, and strata args must be the same length")
    break
  }
  
  # add ID variables to data frame for convenience (+ removes empty beach strata for some subgroup analyses)
  data$ID <- ID
  data$strata <- factor(strata)
  
  if(dots) cat("\n\n\nBootstrap Iterations (",iter,") \n----|--- 1 ---|--- 2 ---|--- 3 ---|--- 4 ---| --- 5 \n",sep="") 
  start.time <- Sys.time()
  
  # create an empty matrix to store bootstrap results
  # corresponding to the PAR and PAF parameters of interest + 3 marginal means
  stats <- matrix(NA,ncol=5,nrow=iter)
  colnames(stats) <- c("thetaPAR","thetaPAF","PY","PY1","PY0")
  
  # Create the bootstrap samples of size n_i, within each stratum i
  IDlist <- as.list(tapply(data$ID,data$strata,function(x) unique(x)))
  bs <- matrix(NA,nrow=length(unique(data$ID)),ncol=iter)
  k <- 0
  for(i in 1:dim(IDlist)) {
    Nids <- length(IDlist[[i]])
    j <- k+1
    k <- j-1+Nids
    bs[j:k,] <- sample(IDlist[[i]],Nids*iter,replace=TRUE)
  }
  
  # calculate statistics for each bootstrap sample
  # call the appropriate AR function (specified in the fn argument), depending on the exposure of interest
  for (bb in 1:iter) {
    bd <- merge(data.frame(ID=bs[,bb]),data,by="ID",all.x=TRUE)
    stats[bb,] <- unlist( tryCatch( do.call(fn,args=list(fmla=fmla,data=bd, low_risk_level=low_risk_level)), error=function(e) rep(NA,5) )  )
    if(dots) cat(".",sep="")
    if(dots && bb %% 50 == 0) cat(bb,"\n")
  }
  if(dots) cat("\n Bootstrap Run Time:",round(difftime(Sys.time(),start.time,units="mins"),3)," Minutes \n\n")
  
  # if there were any bootstrap samples where the model failed to converge 
  # for any reason, caught by tryCatch(), then report that.
  # also triage any estimates that are clearly from a failed model that sort-of converged
  # (due to model instability from sparse data in a bootstrap sample)
  stats[(stats[,1]>10 | stats[,1]< -10 | stats[,3]>10 | stats[,3]< -10), ] <- NA
  Nna <- sum(ifelse(is.na(stats[,1])==TRUE,1,0))
  if(Nna>0) {
    cat("\n   Warning: ",Nna," bootstrap replicates failed to converge to sensible estimates \n")
    cat("\n   Bootstrap estimates are based on the remaining ",iter-Nna," replicates\n\n")
  }
  
  # calculate the mean and percentile 95% confidence intervals for the statistics
  bootest  <- apply(stats,2,function(x) mean(x,na.rm=TRUE))
  boot95lb <- apply(stats,2,function(x) quantile(x,prob=0.025,na.rm=TRUE))
  boot95ub <- apply(stats,2,function(x) quantile(x,prob=0.975,na.rm=TRUE))
  
  
  # print the results
  cat("\n Bootstrap Estimates:\n\n Means    :", paste(sprintf("%1.4f",bootest)),"\n Lower 95%:", paste(sprintf("%1.4f",boot95lb)),"\n Upper 95%:",paste(sprintf("%1.4f",boot95ub)),"\n")
  
  list(bootest=bootest,boot95lb=boot95lb,boot95ub=boot95ub,stats=stats)
}
















# --------------------------------------
# Convenience wrapper function to run 
# modified Poisson models and obtain 
# robust SEs (clusterd on hhid)
# this is the work horse of all the 
# regressions run in this analysis
# --------------------------------------
# mpreg <- function(formula, df, family, vcv=FALSE) {
#   # modified Poisson regression formula
#   # dataaset used to fit the model	
#   if(family=="gaussian"){
#     fit <- glm(as.formula(formula),family="gaussian", weights = df$weight, data=df)
#   }else{
#     fit <- glm(as.formula(formula),family=poisson(link="log"), weights = df$weight, data=df)
#   }
#   vcovCL <- cl(df=df,fm=fit,cluster=df$id)
#   rfit <- coeftest(fit, vcovCL)
#   print(summary(fit))
#   cat("\n\nRobust, Sandwich Standard Errors Account for Clustering:\n")
#   print(rfit) 
#   if(vcv==FALSE) {
#     return(rfit)
#   } else {
#     list(fit=rfit,vcovCL=vcovCL)
#   }
# }
mpreg <- function(varnames, formula, df, family, vcv=FALSE) {
  meth <- make.method(df)
  pred <- make.predictorMatrix(df)
  
  imp <-mice(df, 
             meth = meth, 
             pred = pred, 
             print = FALSE, 
             m = 10, 
             maxit = 6)
  
    # modified Poisson regression formula
  # dataaset used to fit the model	
  if(family=="gaussian"){
    #fit <- glm(as.formula(formula),family="gaussian", weights = df$weight, data=df)
    # fit_imp <- imp %>%
    #   with(glm(as.formula(f),family=poisson(link="log"), weights = imp$weight))
    
    datlist <- miceadds::mids2datlist( imp )
    # linear regression with cluster robust standard errors
    mod <- lapply(datlist, FUN = function(x){glm.cluster( data=x ,         
                                                                       formula=formula,  
                                                                       cluster = x$id,
                                                                       weight = x$weight,
                                                                       family = family)}) 
    
  }else{
    #fit <- glm(as.formula(formula),family=poisson(link="log"), weights = df$weight, data=df)
    # fit_imp <- imp %>%
    #   with(glm(formula = as.formula(f), family=poisson(link="log"), weights = imp$weight))
    datlist <- miceadds::mids2datlist( imp )
    # linear regression with cluster robust standard errors
    
        mod <- list()
        for(i in 1:length(datlist)){
         
          df <- datlist[[i]]
          Cluster = df$id
          Weights = df$weight

          mod_temp <- glm.cluster( data=df ,         
                       formula=formula,  
                       cluster = Cluster,
                       weight = Weights,
                       family = poisson(link="log"))
          
          mod[[i]] <-  mod_temp
        }
        
    
  }
  
  # extract parameters and covariance matrix
  betas <- lapply( mod , FUN = function(rr){ coef(rr) } )
  vars <- lapply( mod , FUN = function(rr){ vcov(rr) } )
  # conduct statistical inference
  invisible(fit <- summary(pool_mi( qhat = betas, u = vars )))
  
  coef <- as.data.frame(fit[2,])
  res <- data.frame(Y=varnames[1],
                    X=varnames[2],
                    coef=coef$results,
                    RR=exp(coef$results),
                    se=coef$se,
                    pval=coef$p)
  
  #Calc 95%CI
  if(family!="gaussian"){
    res$ci.lb <- exp(res$coef - 1.96*res$se)
    res$ci.ub <- exp(res$coef + 1.96*res$se)
  }
  
  return(res)
  # print(summary(fit))
  # cat("\n\nRobust, Sandwich Standard Errors Account for Clustering:\n")
  # print(rfit) 
  # if(vcv==FALSE) {
  #   return(rfit)
  # } else {
  #   list(fit=rfit,vcovCL=vcovCL)
  # }
}



# cl   <- function(df,fm, cluster){
#   # df: data used to fit the model
#   # fm : model fit (object)
#   # cluster : vector of cluster IDs
#   require(sandwich, quietly = TRUE)
#   require(lmtest, quietly = TRUE)
#   M <- length(unique(cluster))
#   N <- length(cluster)
#   K <- fm$rank
#   dfc <- (M/(M-1))*((N-1)/(N-K))
#   uj  <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum));
#   vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)
#   return(vcovCL)
# }





glm.cluster <- function (data, formula, cluster, weight, family){
  
  mod <- stats::glm(data = data, formula = formula, weights = weight, family = family)
  
  vcov2 <- miceadds:::lm_cluster_compute_vcov(mod = mod, cluster = cluster, 
                                              data = data)
  
  res <- list(glm_res = mod, vcov = vcov2)
  class(res) <- "glm.cluster"
  return(res)
}











assetPCA<-function(ret,  reorder=F ){
  
  print(ret$country[1])
  
  #create level for for missingness
  ret$missing <- rowSums(is.na(ret))
  ret <- ret %>% filter(missing < 29) %>% subset(., select = -c(missing))
  
  #Select assets
  ret<-ret %>% ungroup() %>% as.data.frame(.) 
  id<-subset(ret, select=c("country","clust_num","HH_num")) #drop id's
  ret<-ret[,which(!(colnames(ret) %in% c("country","clust_num","HH_num")))]


  # for(i in 4:ncol(df)){
  #   df[,i] <- as.numeric(df[,i])
  #   df[is.na(df[,i]),i] <- median(df[,i], na.rm=T)
  # }
  
  for(i in 1:ncol(ret)){
    ret[which(ret[,i]=="9"),i] <- NA
    #print(table(ret[,i]))
    if(length(unique(ret[,i]==2))){
      miss <- which(is.na(ret[,i]))
      ret[,i] <- ifelse(ret[,i]==2,0,1)
      ret[miss,i] <- NA
    }
  }
  
  for(i in 1:ncol(ret)){
    ret[,i]<-as.character(ret[,i])
    ret[is.na(ret[,i]),i] <- "miss"
    ret[,i]<-as.factor(ret[,i])
  }
  
  #Remove columns with almost no variance
  if(length(nearZeroVar(ret))>0){
    ret<-ret[,-nearZeroVar(ret)]
  }
  

  #Convert factors into indicators
  ret<-droplevels(ret)
  gc()
  ret<-design_matrix(ret)
  gc()

  #Remove columns with almost no variance
  if(length(nearZeroVar(ret))>0){
    ret<-ret[,-nearZeroVar(ret)]
  }
  
  

  ## Convert the data into matrix ##
  ret <- as.matrix(ret)
  
  
  ##Computing the principal component using eigenvalue decomposition ##
  princ.return <- princomp(ret) 
  
  
  ## To get the first principal component in a variable ##
  load <- loadings(princ.return)[,1]   
  
  pr.cp <- ret %*% load  ## Matrix multiplication of the input data with the loading for the 1st PC gives us the 1st PC in matrix form. 
  
  ret <- as.data.frame(ret)
  ret$HHwealth <- as.numeric(pr.cp[,1]) ## Gives us the 1st PC in numeric form in pr.
  
  #Create 4-level household weath index
  quartiles<-quantile(ret$HHwealth, probs=seq(0, 1, 0.25))
  print(quartiles)
  ret<-as.data.frame(ret)
  ret$HHwealth_quart<-rep(1, nrow(ret))
  ret$HHwealth_quart[ret$HHwealth>=quartiles[2]]<-2
  ret$HHwealth_quart[ret$HHwealth>=quartiles[3]]<-3
  ret$HHwealth_quart[ret$HHwealth>=quartiles[4]]<-4
  table(ret$HHwealth_quart)
  ret$HHwealth_quart<-factor(ret$HHwealth_quart)
  
  if(reorder==T){
    levels(ret$HHwealth_quart)<-c("WealthQ4","WealthQ3","WealthQ2","WealthQ1")
    ret$HHwealth_quart<-factor(ret$HHwealth_quart, levels=c("WealthQ1", "WealthQ2","WealthQ3","WealthQ4"))
  }else{
    levels(ret$HHwealth_quart)<-c("WealthQ1", "WealthQ2","WealthQ3","WealthQ4")
  }
  
  #Table assets by pca quartile to identify wealth/poverty levels
  d<-data.frame(id, ret)
  wealth.tab <- d %>% subset(., select=-c(country,clust_num,HH_num)) %>%
    group_by(HHwealth_quart) %>%
    summarise_all(funs(mean)) %>% as.data.frame()
  print(wealth.tab)
  
  #Save just the wealth data
  d <- d %>% subset(select=c(country, clust_num, HH_num, HHwealth, HHwealth_quart))

  return(d)
}
