#diarrhea ,  WASH_noEC   Zimbabwe 

outcomes = c("diarrhea")
family="modified possion"
Wvars=Wvars

d <- d %>% filter(country=="Zimbabwe")

 Y ="diarrhea"
                       X="WASH_noEC"
                       W=Wvars
                       weight = "ecpopweight_H"
                       clustid= "clust_num"
                       family=family
 calc_PAF=TRUE
 low_risk_level="Uncontaminated"
 
 
 # res8 <- d %>% group_by(country) %>%
 #   do(mics_regression(d=.,
 #                      Y ="diarrhea",
 #                      X="WASH_noEC",
 #                      W=Wvars,
 #                      weight = "popweight",
 #                      clustid= "clust_num",
 #                      family=family, calc_PAF=T, low_risk_level="Improved"))
 
 Y ="diarrhea"
 X="WASH_noEC"
 W=Wvars
 weight = "popweight"
 clustid= "clust_num"
 family=family
 calc_PAF=T
 low_risk_level="Improved"
 return_model=FALSE
   
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
   

     Wscreen=NULL
     Wdf = NULL
     if(!is.null(W)){
       #cat("\n-----------------------------------------\nPre-screening the adjustment covariates:\n-----------------------------------------\n")
       prescreen_family <- ifelse(family=="gaussian",family,"binomial")
       
       try(Wdf <- df %>% select(W))
       #drop covariates with near zero variance
       if(length(nearZeroVar(Wdf))>0){
         try(Wdf<-Wdf[,-nearZeroVar(Wdf)])
       }
       
       #prescreen
       suppressWarnings(try(Wscreen <- 
                              MICS_prescreen(Y = df$Y, 
                                             Ws = Wdf, 
                                             family = prescreen_family, 
                                             pval = 0.2, print = F)))
       #select n/10 covariates if binary outcome
       if(family!="gaussian" & !is.null(Wscreen)){
         nY<-floor(min(table(df$Y))/10) -1 #minus one because 10 variables needed to estimate coef. of X
         if(nY>=1){
           if(length(Wscreen)>nY){
             Wscreen<-Wscreen[1:nY]
           }        
         }else{
           Wscreen=NULL
         }
       }
       
       #Drop sparse factor levels
       if(!is.null(Wscreen)){
         Wdf = NULL
         try(Wdf <- df %>% subset(., select =c(Wscreen)))
         try(Wdf <- as.data.frame(model.matrix(~., model.frame(~ ., Wdf, na.action=na.pass))[,-1]))
         
         #scale and drop covariates with near zero variance
         try(pp_no_nzv <- preProcess(Wdf, method = c("center", "scale", "YeoJohnson", "nzv")))
         
         try(Wdf<- predict(pp_no_nzv, newdata = Wdf))
         try(Wdf<- data.frame(Wdf))
       }else{
         Wdf=NULL
       }
       
       df <- bind_cols(df %>% subset(., select =c("Y","X","id", "weight")), Wdf)
     }
     
     #order by id for geeglm function
     df <- df %>% arrange(id)

       
         #model formula
         f <- ifelse(is.null(Wdf),
                     "Y ~ X",
                     paste0("Y ~ X + ", paste(colnames(Wdf), collapse = " + ")))
         
         fit=NULL
         res <- mpreg(varnames=varnames, formula = as.formula(f), df = df, family="modified possion", vcv=FALSE)
         
 

 
 fn=ARfun
 fmla = as.formula(f)
 data=df
 ID=df$id
 strata=rep(1, nrow(df))
 iter=50
 dots=FALSE
 low_risk_level=low_risk_level
 
 
 
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

 
 
 
 
 
 
 
 
 
 
 iter=1
 
 data=bd

 
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
   
   #multiple imputation - one imputed dataset per bootstrap
   meth <- make.method(data)
   pred <- make.predictorMatrix(data)
   
   set.seed(12345)
   df <-mice(data, 
               meth = meth, 
               pred = pred, 
               print = FALSE, 
               m = 1, 
               maxit = 6)
   
   datlist <- miceadds::mids2datlist( df )
   
   
   table(is.na(data))
   data <- datlist[[1]]
   table(is.na(data2))
   
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