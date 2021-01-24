source("0-config.R")


#load clean data
dfull <- readRDS(here("data/compiled_clean_MICS_survey.rds"))

#subset to just POC countries
#d <- dfull %>% filter(country %in% c("Bangladesh", "Zimbabwe","PakistanPunjab"))
d <- dfull %>% filter(country %in% c("PakistanPunjab"))
#d <- dfull %>% filter(country %in% c("Bangladesh"))




d <- droplevels(d)

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
           "fan",
           "fuel",
           "roof",
           "wall",
           "nroom_sleeping")



res <-   mics_regression(d=d,
                       Y ="stunt",
                       X="EC_H",
                       W=Wvars,
                       weight = "ecpopweight_H",
                       clustid= "clust_num",
                       family="modified possion")


Y ="stunt"
X="EC_H"
W=Wvars
weight = "ecpopweight_H"
clustid= "clust_num"
family="modified possion"




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



  Wscreenfull<-Wscreen
  
  Wscreen<-Wscreenfull[c(1:10,12)]


  #model formula
  f <- ifelse(is.null(Wscreen),
              "Y ~ X",
              paste0("Y ~ X + ", paste(Wscreen, collapse = " + ")))
  id <- df$id
  weight <- df$weight
  
  #fit model
  fit <- geeglm(formula = as.formula(f),
                data    = df,
                weights = weight,
                family  = poisson(link = "log"),
                id      = id,
                corstr  = "exchangeable")   
  summary(fit)
  
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



res$W <-ifelse(is.null(Wscreen), "unadjusted", Wscreen)
