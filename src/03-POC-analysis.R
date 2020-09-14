

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


#load clean data
dfull <- readRDS(here("data/compiled_clean_MICS_survey.rds"))

#subset to just BD to code out analysis
d <- dfull %>% filter(country=="Bangladesh")



d$ecpopweight_H <- as.numeric(d$ecpopweight_H)




#use survey package to calculate correctly-weighted means 

df <- d %>% filter(!is.na(wat_imp))
table(df$haz)


#Run linear regression for continious outcomes (what is the right clustering/weighting?)
#We will include random effects for households nested within sampling clusters to account for the survey design.
#weights for e-coli: ecpopweight_S or ecpopweight_H, while popweight for other exposures
df <- d %>% filter(!is.na(EC_risk_H))
df <- droplevels(df)
res <- lmer(haz~factor(EC_risk_H) + (1|clust_num) + (1|HH_num),  data=df, weights = ecpopweight_H)
summary(res)


#Other sources:
#https://stat.ethz.ch/pipermail/r-sig-mixed-models/2010q4/004700.html


#Run modified poisson for binary outcomes
#https://rstudio-pubs-static.s3.amazonaws.com/5752_fc41dca85dd24539bc99868697de83d0.html
#https://stats.stackexchange.com/questions/346662/poisson-approximation-of-a-binomial-model-with-random-effects-how-to-get-robus
#for clustered data, use cluster instead of individual as ID: http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.903.8133&rep=rep1&type=pdf
geeglm.log.poisson <- geeglm(formula = stunt ~ factor(EC_risk_H),
                             data    = df,
                             weights = ecpopweight_H,
                             family  = poisson(link = "log"),
                             id      = clust_num,
                             corstr  = "exchangeable")
summary(geeglm.log.poisson)
exp(0.01414471)




#MICS regression function
Y ="stunt"
X="EC_risk_H"
W=NULL
weight = "ecpopweight_H"
clustid= "clust_num"
family="modified possion"

mics_regression <- function(d, Y, X, W, weight = "ecpopweight_H", clustid= "clust_num", family="modified possion"){
  
  df <- data.frame(
    Y=subset(d, select=get(Y)),
    X=subset(d, select=get(X)),
    id=subset(d, select=get(clustid)),
    weight=subset(d, select=get(weight)),
    W=subset(d, select=W))
  
  varnames <- colnames(df)[1:4]
  colnames(df)[1:4] <- c("Y","X","id","weight")
  
  fullrows <- nrow(df)
  df <- df[complete.cases(df),]
  cat("Rows dropped due to missingness: ",fullrows - nrow(df),"\n")
  
  if(family=="gaussian"){
    fit <- lmer(Y~X + (1|id),  data=df, weights = weight)
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
    fit <- geeglm(formula = Y ~ X,
                                 data    = df,
                                 weights = weight,
                                 family  = poisson(link = "log"),
                                 id      = id,
                                 corstr  = "exchangeable")    
  }  
  
  return(list(res=resdf, fit=fit))
}


summary(geeglm.log.poisson)




# res<-glm(formula = stunt ~ EC_risk_H,
#        data    = df,
#        weights = ecpopweight_H,
#        family  = poisson(link = "log"))
# summary(res)
# 
# 
# #Might need to use this package for robust estimation with mixed effects:
# #https://cran.r-project.org/web/packages/robustlmm/vignettes/rlmer.pdf
# 
# 
# df <- d %>% filter(!is.na(EC_risk_H))
# df <- droplevels(df)
# df$seqno <- 1:nrow(df)
# res <- glmer(stunt~factor(EC_risk_H) + (1|clust_num) + (1|HH_num), family=poisson(link = "log"),  data=df, weights = ecpopweight_H)
# summary(res)
# vcovCL <- sandwichSE(dat = df, fm = res, cluster = df$seqno)
# 
# 
# 
# 
# #fit <- glm(stunt~factor(EC_risk_H), family=poisson(link = "log"),  data=df, weights = ecpopweight_H)
# 
# 
# 
# #https://stats.stackexchange.com/questions/331840/how-to-calculate-sandwich-standard-errors-for-generalized-least-squares-models
# library(nlme)
# fm1 <- gls(follicles ~ sin(2*pi*Time) + cos(2*pi*Time), Ovary,
#            correlation = corAR1(form = ~ 1 | Mare))
# 
# X <- model.matrix(fm1, data=Ovary)
# Mares <- Ovary$Mare
# 
# W <- lapply(lapply(levels(Mares), getVarCov, obj=fm1), solve)
# Xsplit <- lapply(split(as.data.frame(X), Mares), as.matrix)
# Resids <- split(residuals(fm1), Mares)
# 
# Scores <- mapply(function(x, w, r) {
#   t(x)%*%w%*%r}, Xsplit, W, Resids)
# rowSums(Scores)
# 
# 
# #this forms the basis of generating the model based variance-covariance matrix for the trend.
# 
# xwx <- Reduce(`+`, mapply(function(x,w) t(x)%*%w%*%x, Xsplit, W, SIMPLIFY=F))
# solve(xwx)
# vcov(fm1)
# 
# 
# 
# #Now the sandwich is obtained by using the residuals as plug-in estimators and obtaining the output:
#   
#   Amat <- xwx
# Bmat <- mapply(function(x, w, r) t(x)%*%w%*%diag(c(r)^2)%*%w%*%x, Xsplit, W, Resids, SIMPLIFY=F)
# Bmat <- Reduce(`+`, Bmat)
# 
# SandCov <- solve(Amat) %*% Bmat %*% solve(Amat)
# 
# 
# 
# 
# 
# #shift estimate
# #https://tlverse.org/tmle3shift/
