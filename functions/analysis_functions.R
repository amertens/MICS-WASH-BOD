

mics_regression <- function(d, Y, X, W, weight = "ecpopweight_H", clustid= "clust_num", family="modified possion", return_model=FALSE){
  
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
  
  Wscreen=NULL
  if(!is.null(W)){
    cat("\n-----------------------------------------\nPre-screening the adjustment covariates:\n-----------------------------------------\n")
    prescreen_family <- ifelse(family=="gaussian",family,"binomial")
    suppressWarnings(Wscreen <- 
                       washb_prescreen(Y = df$Y, 
                                       Ws = df %>% select(W), 
                                       family = prescreen_family, 
                                       pval = 0.2, print = T))
  }
  
  if(family=="gaussian"){
    #model formula
    f <- ifelse(is.null(Wscreen),
                "Y ~ X + (1|id)",
                paste0("Y ~ X + (1|id) + ", paste(Wscreen, collapse = " + ")))
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
                paste0("Y ~ X + ", paste(Wscreen, collapse = " + ")))
    
    #fit model
    fit <- geeglm(formula = as.formula(f),
                  data    = df,
                  weights = weight,
                  family  = poisson(link = "log"),
                  id      = id,
                  corstr  = "exchangeable")   
    
    
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
