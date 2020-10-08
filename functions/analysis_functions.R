

mics_regression <- function(d, Y, X, W, weight = "ecpopweight_H", clustid= "clust_num", family="modified possion", return_model=FALSE){
  
  cat(Y,", ",X,"\n")
  
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
    if(family!="gaussian"){
      nY<-floor(min(table(df$Y, df$X))/10)
      if(length(Wscreen)>nY){
        Wscreen<-Wscreen[1:nY]
      }
    }
   
    #Drop sparse factor levels
    Wdf <- df %>% subset(., select =c(Wscreen))
    Wdf <- as.data.frame(model.matrix(~., Wdf)[,-1]) 
    
    #scale anddrop covariates with near zero variance
    pp_no_nzv <- preProcess(Wdf,
                            method = c("center", "scale", "YeoJohnson", "nzv"))
    pp_no_nzv
    Wdf<- predict(pp_no_nzv, newdata = Wdf)
    
    # if(length(nearZeroVar(Wdf))>0){
    #   Wdf<-Wdf[,-nearZeroVar(Wdf)]
    # }
    df <- bind_cols(df %>% subset(., select =c("Y","X","id", "weight")), Wdf)
  }
  
  #order by id for geeglm function
  df <- df %>% arrange(id)
  
  if(family=="gaussian"){
    #model formula
    f <- ifelse(is.null(Wscreen),
                "Y ~ X + (1|id)",
                paste0("Y ~ X + (1|id) + ", paste(colnames(Wdf), collapse = " + ")))
    #fit model
    fit <- lmer(as.formula(f),  data=df, weights = weight)
    coef <- as.data.frame(t(summary(fit)$coefficients[2,]))
    res <- data.frame(Y=varnames[1],
                      X=varnames[2],
                      coef=coef$Estimate,
                      se=coef$`Std. Error`,
                      tval=coef$`t value`)
    
    #Calc 95%CI
    res$ci.lb <- res$coef - 1.96*res$se
    res$ci.ub <- res$coef + 1.96*res$se
    
    #calculate pvalue by normal approximation
    #https://www.r-bloggers.com/three-ways-to-get-parameter-specific-p-values-from-lmer/
    res$pval <- 2 * (1 - pnorm(abs(res$tval)))
    res$N<-nrow(df)
    
    
  }
  if(family=="modified possion"){
    #model formula
    f <- ifelse(is.null(Wscreen),
                "Y ~ X",
                paste0("Y ~ X + ", paste(colnames(Wdf), collapse = " + ")))
    id <- df$id
    weight <- df$weight
    fit=NULL
    
      
      #fit model
    tryCatch( { fit <- withTimeout( { 
       geeglm(formula = as.formula(f),
                    data    = df,
                    weights = weight,
                    family  = poisson(link = "log"),
                    id      = id,
                    corstr  = "exchangeable") 
      },
      timeout = 120) },
      TimeoutException = function(ex) cat("Timed out\n"))

      coef <- as.data.frame(summary(fit)$coefficients[2,])
      res <- data.frame(Y=varnames[1],
                        X=varnames[2],
                        coef=coef$Estimate,
                        RR=exp(coef$Estimate),
                        se=coef$Std.err,
                        wald=coef$Wald,
                        pval=coef$`Pr(>|W|)`)
      
      #Calc 95%CI
      res$ci.lb <- exp(res$coef - 1.96*res$se)
      res$ci.ub <- exp(res$coef + 1.96*res$se)
      
      #calculate pvalue by normal approximation
      #https://www.r-bloggers.com/three-ways-to-get-parameter-specific-p-values-from-lmer/
      res$n<-sum(df$Y, na.rm=T)
      res$N<-nrow(df)      
      
 
    
  }  
  
  
  res$W <-ifelse(is.null(Wscreen), "unadjusted", Wscreen)
  
  if(return_model){
    return(list(res=res, fit=fit))
  }else{
    return(res)
  }
}






mics_tmle <- function(d, Y, X, W, weight = "ecpopweight_H", clustid= "clust_num", family="binomial", return_model=FALSE){
  
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
    if(family!="gaussian"){
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
  
  df_processed$X <- factor(df_processed$X, levels=c("0","1"))
  ate_spec <- tmle_ATE(
    treatment_level = levels(df_processed$X)[2],
    control_level = levels(df_processed$X)[1]
  )
  
  
  # choose base learners
  lrnr_mean <- make_learner(Lrnr_mean)
  lrnr_glm <- make_learner(Lrnr_glm)
  lrnr_xgboost <- make_learner(Lrnr_xgboost)
  
  # define metalearners appropriate to data types
  ls_metalearner <- make_learner(Lrnr_solnp)
  sl_Y <- Lrnr_sl$new(learners = list(lrnr_mean, lrnr_glm, lrnr_xgboost),
                      metalearner = ls_metalearner)
  sl_A <- Lrnr_sl$new(learners = list(lrnr_mean, lrnr_glm, lrnr_xgboost),
                      metalearner = ls_metalearner)
 
  # ls_metalearner <- make_learner(Lrnr_nnls)
  # sl_Y <- sl_A <- make_learner(Lrnr_glm)
  learner_list <- list(A = sl_A, Y = sl_Y)
  tmle_fit <- tmle3(ate_spec, df_processed, node_list, learner_list)
  
  
  
  #XXXXXXXXXXXXXXXXXXXXXXXXX
  # ADD ID AND SAMPLE WEIGHTs
  #XXXXXXXXXXXXXXXXXXXXXXXXX
  
  
  
  
  
  if(family=="gaussian"){
  
    
    res <- data.frame(Y=varnames[1],
                      X=varnames[2],
                      coef=tmle_fit$tmle_est,
                      se=tmle_fit$se,
                      ci.lb = lower,
                      ci.ub = upper)
    res$N<-nrow(df_processed)
    
  }
  if(family=="binomial"){
    #model formula
    f <- ifelse(is.null(Wscreen),
                "Y ~ X",
                paste0("Y ~ X + ", paste(Wscreen, collapse = " + ")))
    id <- df$id
    weight <- df$weight
    fit=NULL
    
    
    #fit model
    tryCatch( { fit <- withTimeout( { 
      geeglm(formula = as.formula(f),
             data    = df,
             weights = weight,
             family  = poisson(link = "log"),
             id      = id,
             corstr  = "exchangeable") 
    },
    timeout = 120) },
    TimeoutException = function(ex) cat("Timed out\n"))
    
    coef <- as.data.frame(summary(fit)$coefficients[2,])
    res <- data.frame(Y=varnames[1],
                      X=varnames[2],
                      coef=coef$Estimate,
                      se=coef$Std.err,
                      wald=coef$Wald,
                      pval=coef$`Pr(>|W|)`)
    
    #Calc 95%CI
    res$ci.lb <- res$coef - 1.96*res$se
    res$ci.ub <- res$coef + 1.96*res$se
    
    #calculate pvalue by normal approximation
    #https://www.r-bloggers.com/three-ways-to-get-parameter-specific-p-values-from-lmer/
    res$n<-sum(df$Y, na.rm=T)
    res$N<-nrow(df)      
    
    
    
  }  
  
  
  res$W <-ifelse(is.null(Wscreen), "unadjusted", Wscreen)
  
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
  dat <- dat[complete.cases(dat), ]
  nW <- ncol(Ws)
  LRp <- matrix(rep(NA, nW), nrow = nW, ncol = 1)
  rownames(LRp) <- names(Ws)
  colnames(LRp) <- "P-value"
    for (i in 1:nW) {
      dat$W <- dat[, i]
      if (class(dat$W) == "factor" & dim(table(dat$W)) == 
          1) {
        fit1 <- fit0 <- glm(Y ~ 1, data = dat, family = family)
      }
      else {
        fit1 <- glm(Y ~ W, data = dat, family = family)
        fit0 <- glm(Y ~ 1, data = dat, family = family)
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
  return(rownames(LRps))
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


##############################################
# run_rma
##############################################


# run fit.rma wrapper across a list of ages
# Input:
# data: 
# n_name:
# label:
# method:

# Returns:
# est:
# lb:
# ub:
# agecat:
# ptest.f:
# label:

run_rma <- function(data, n_name, x_name, label, method) {
  
  # create age list
  agelist <- as.list(levels(data$agecat))
  
  # apply fit.rma across age list
  res.list <- lapply(agelist, function(x)
    fit.rma(
      data = data, ni = n_name, xi = x_name, age = x, measure = "PLO", nlab = "children",
      method = method
    ))
  
  # unlist output
  res <- as.data.frame(do.call(rbind, res.list))
  
  # tidy up output
  res$est <- as.numeric(res$est)
  res$lb <- as.numeric(res$lb)
  res$ub <- as.numeric(res$ub)
  res <- res %>% mutate(est = est * 100, lb = lb * 100, ub = ub * 100)
  res$agecat <- factor(res$agecat, levels = levels(data$agecat))
  res$ptest.f <- sprintf("%0.0f", res$est)
  res$label <- label
  
  return(res)
}

##############################################
# run_rma_agem
##############################################


# run fit.rma wrapper across a list of ages for age in months
# Input:
# data: 
# n_name:
# label:
# method:

# Returns:
# est:
# lb:
# ub:
# agecat:
# ptest.f:
# label:

run_rma_agem <- function(data, n_name, x_name, label, method) {
  
  # create age list
  agelist <- as.list(levels(data$agem))
  
  # apply fit.rma across age list
  res.list <- lapply(agelist, function(x)
    fit.rma(
      data = data %>% filter(agem == x),
      ni = n_name, xi = x_name, measure = "PLO", nlab = "children",
      method = method
    ))
  
  # unlist output
  res <- as.data.frame(do.call(rbind, res.list))
  
  # tidy up output
  res[, 3] <- as.numeric(res[, 3])
  res[, 5] <- as.numeric(res[, 5])
  res[, 6] <- as.numeric(res[, 6])
  res <- res %>% mutate(est = est * 100, lb = lb * 100, ub = ub * 100)
  res$agem <- factor(levels(data$agem), levels = levels(data$agem))
  res$ptest.f <- sprintf("%0.0f", res$est)
  res$label <- label
  
  res <- res %>% select(label, agem, nstudies, nmeas, everything())
  
  return(res)
}


##############################################
# run_rma_agem
##############################################

# runs a ttest across agecats
# Input
# data: 
# y:
# levels:
# ref:
# comp:

# Returns
# pval < 0.05:
# pval < 0.01:
# pval < 0.001:

ki.ttest <- function(data, y, levels, ref, comp) {
  pval <- NULL
  for (i in 1:length(comp)) {
    colnames(data)[colnames(data) == y] <- "y"
    colnames(data)[colnames(data) == levels] <- "levels"
    datasub <- data[data$levels %in% c(ref, comp[i]), ]
    pval <- rbind(pval, t.test(y ~ levels, data = datasub)$p.value)
  }
  pval <- data.frame(comp = comp, pval)
  colnames(pval)[2] <- "pval"
  pval$pvalcat <- ""
  pval$pvalcat[pval$pval < 0.05] <- "*"
  pval$pvalcat[pval$pval < 0.01] <- "**"
  pval$pvalcat[pval$pval < 0.001] <- "***"
  return(pval)
}





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

