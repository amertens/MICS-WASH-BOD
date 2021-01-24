
  adj <- ifelse(is.null(Wvars),"unadj","adj")
  
  fullres <- NULL 
  
  outcomes = c("stunt", "wast","diarrhea","ari")
  family="modified possion"
  PAF=T
  Wvars=Wvars
  
    #print(i)
    # res1 <- d %>% group_by(country) %>%
    #   do(mics_regression(d=.,
    #                      Y ="stunt",
    #                      X="EC_H",
    #                      W=Wvars,
    #                      weight = "ecpopweight_H",
    #                      clustid= "clust_num",
    #                      family=family, calc_PAF=PAF, low_risk_level="Uncontaminated"))

    Y ="stunt"
    X="EC_H"
    W=Wvars
    weight = "ecpopweight_H"
    clustid= "clust_num"
    family=family
    calc_PAF=PAF
    low_risk_level="Uncontaminated"
   return_model=FALSE
      
      cat(Y,", ",X,"\n")
      
      df <- data.frame(
        Y=subset(d, select=get(Y)),
        X=subset(d, select=get(X)),
        id=subset(d, select=get(clustid)),
        weight=subset(d, select=get(weight)),
        subset(d, select=W))
      
      #artificially add missingness
      for(i in 5:ncol(df)){
        df[sample(nrow(df), floor(nrow(df)/10)),i] <- NA
      }
      table(is.na(df))
      
      varnames <- c(Y,X, "clustid","weight")
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
      if(!is.null(W)){
        cat("\n-----------------------------------------\nPre-screening the adjustment covariates:\n-----------------------------------------\n")
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
                                              pval = 0.2, print = T)))
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
        
        
        
        #model formula
        f <- ifelse(is.null(Wscreen),
                    "Y ~ X",
                    paste0("Y ~ X  + ", paste(colnames(Wdf), collapse = " + ")))

        res <- mpreg(formula = as.formula(f), df = df, family="gaussian")
        
        # coef <- as.data.frame(t(fit[2,]))
        # res <- data.frame(Y=varnames[1],
        #                   X=varnames[2],
        #                   coef=coef$Estimate,
        #                   se=coef$`Std. Error`,
        #                   Zval=coef$`z value`,
        #                   pval=coef$`Pr(>|z|)`)
        # 
        # #Calc 95%CI
        # res$ci.lb <- res$coef - 1.96*res$se
        # res$ci.ub <- res$coef + 1.96*res$se
        
        res$N<-nrow(df)
        res$W <-ifelse(is.null(Wscreen), "unadjusted", paste(Wscreen, sep="", collapse=", "))
        
        
      }
      if(family=="modified possion"){
        
        #check sparsity
        sparseN<-min(table(df$X, df$Y))
        if(sparseN>10){
          
          
          #model formula
          f <- ifelse(is.null(Wscreen),
                      "Y ~ X",
                      paste0("Y ~ X + ", paste(colnames(Wdf), collapse = " + ")))
          # id <- df$id
          # weight <- df$weight
          # fit=NULL
          
          
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
          
          
          res <- mpreg(varnames=varnames, formula = as.formula(f), df = df, family="modified possion", vcv=FALSE)
          # fit <- mpreg(formula = as.formula(f), df = df, family="modified possion", vcv=FALSE)
          # coef <- as.data.frame(t(fit[2,]))
          # res <- data.frame(Y=varnames[1],
          #                   X=varnames[2],
          #                   coef=coef$Estimate,
          #                   RR=exp(coef$Estimate),
          #                   se=coef$`Std. Error`,
          #                   Zval=coef$`z value`,
          #                   pval=coef$`Pr(>|z|)`)
          # 
          #   #Calc 95%CI
          #   res$ci.lb <- exp(res$coef - 1.96*res$se)
          #   res$ci.ub <- exp(res$coef + 1.96*res$se)
          
          
          if(calc_PAF){
            paflist <- bootAR(fn=ARfun,fmla = as.formula(f),data=df,ID=df$id,strata=rep(1, nrow(df)),iter=100,dots=TRUE, low_risk_level=low_risk_level)
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
      
      