

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

#subset to just POC countries
d <- dfull %>% filter(country %in% c("Bangladesh", "Zimbabwe","PakistanPunjab"))
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

for(i in Wvars){
  cat(i,": ",levels(d[[i]]),"\n")
}

d$hhid <- paste0(d$clust_num, "-",d$HH_num)

d=d %>% filter(country=="Bangladesh") %>% droplevels(.)
Y ="stunt"
X="WASH"
W=Wvars
weight = "popweight"
clustid= "clust_num"
family="modified possion"
# 
# res1 <- d %>% group_by(country) %>%
#   do(mics_regression(d=.,
#                      Y ="stunt",
#                      X="EC_H",
#                      W=NULL,
#                      weight = "ecpopweight_H",
#                      clustid= "clust_num",
#                      family="modified possion"))


table(d$floor)
table(d$chimney)

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
    if(family!="gaussian"){
      nY<-floor(min(table(df$Y, df$X))/10)
      if(length(Wscreen)>nY){
        Wscreen<-Wscreen[1:nY]
      }
    }
    
  }
  
  
save(df, Wscreen, file=here("temp/temp.Rdata"))  


load(here("temp/temp.Rdata"))  


Wdf <- df %>% subset(., select =c(Wscreen))
Wdf <- as.data.frame(model.matrix(~., Wdf)[,-1]) 

#scale anddrop covariates with near zero variance
pp_no_nzv <- preProcess(Wdf,
                        method = c("center", "scale", "YeoJohnson", "nzv"))
pp_no_nzv
Wdf<- predict(pp_no_nzv, newdata = Wdf)

# 
# if(length(nearZeroVar(Wdf))>0){
#   Wdf<-Wdf[,-nearZeroVar(Wdf)]
# }



df <- bind_cols(df %>% subset(., select =c("Y","X","id", "weight")), Wdf)
df <- df %>% arrange(id)
#quantile(df2$weight, c(0.1, 0.9))


#  tmp <- cor(temp)
#  tmp[upper.tri(tmp)] <- 0
#  diag(tmp) <- 0
# # Above two commands can be replaced with 
# # tmp[!lower.tri(tmp)] <- 0
# #
#  data.new <- data[,!apply(tmp,2,function(x) any(x > 0.99))]
#  head(data.new)


  #if(family=="modified possion"){
    #model formula
    f <- ifelse(is.null(Wscreen),
                "Y ~ X",
                paste0("Y ~ X + ", paste(colnames(Wdf), collapse = " + ")))
    id <- df$id
    weight <- df$weight
    fit=NULL
    

    

        #fit model
 fit <- geeglm(formula = as.formula(f),
             data    = df,
             weights = weight,
             family  = poisson(link = "log"),
             id      = id,
             corstr  = "exchangeable") 
 summary(fit)$coefficients[2,1:2]
 
 fit <- geeglm(formula = as.formula(f),
               data    = df,
               weights = weight,
               family  = poisson(link = "log"),
               id      = 1:length(id),
               corstr  = "exchangeable") 
 summary(fit)$coefficients[2,1:2]
 
 fit <- geeglm(formula = as.formula(f),
               data    = df,
               #weights = weight,
               family  = poisson(link = "log"),
               id      = id,
               corstr  = "exchangeable") 
 summary(fit)$coefficients[2,1:2]
 
 #fit model
 fit <- glm(formula = as.formula(f),
               data    = df,
               weights = weight,
               family  = poisson(link = "log"))
 summary(fit)$coefficients[2,1:2]
 coeftest(fit, vcov = sandwich)
 
 
 
 
 
 
  
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
    
    
    

 





  f <- ifelse(is.null(Wscreen),
              "Y ~ X + (1|id)",
              paste0("Y ~ X + (1|id) + ", paste(colnames(Wdf), collapse = " + ")))
  
  fit <- glmer(as.formula(f),  data=df, weights = weight, family = "binomial")
summary(fit)




