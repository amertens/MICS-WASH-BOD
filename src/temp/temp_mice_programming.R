
rm(list=ls())
source("0-config.R")

d <- readRDS(here("data/compiled_clean_POC_survey.rds"))



saveRDS(d, file=here("data/compiled_clean_POC_survey.rds"))

Y ="stunt"
X="WASH"
W=NULL
weight = "popweight"
clustid = "clust_num"
family = "modified possion"
calc_PAF=T
low_risk_level="Improved"
return_model=FALSE


# d$clust_num <- paste0(d$clust_num, "-",d$HH_num)
# 
# 
# d <- droplevels(d)
# outcomes = c("stunt", "wast","diarrhea","ari")
# family="modified possion"
# PAF=T
# Wvars=Wvars
# 
#   adj <- ifelse(is.null(Wvars),"unadj","adj")
#   
#   i= outcomes[1]
#     print(i)
#     d <- d %>% filter(country=="Bangladesh")
#     
#                          Y =i
#                          X="EC_H"
#                          W=Wvars
#                          weight = "ecpopweight_H"
#                          clustid= "clust_num"
#                          family=family
#                          calc_PAF=PAF
#                          low_risk_level="Uncontaminated"
#           return_model=FALSE
#   
#   cat(Y,", ",X,"\n")
#   
#   df <- data.frame(
#     Y=subset(d, select=get(Y)),
#     X=subset(d, select=get(X)),
#     id=subset(d, select=get(clustid)),
#     weight=subset(d, select=get(weight)),
#     subset(d, select=W))
#   
#   varnames <- colnames(df)[1:4]
#   colnames(df)[1:4] <- c("Y","X","id","weight")
#   
#   fullrows <- nrow(df)
#   df <- df %>% filter(!is.na(Y))
#   Yrows <- nrow(df)
#   cat("Rows dropped due to missing outcome: ",fullrows - Yrows,"\n")
#   df <- df %>% filter(!is.na(X))
#   Xrows <- nrow(df)
#   cat("Rows dropped due to missing exposure: ",Yrows -Xrows,"\n")
#   df <- df[complete.cases(df),]
#   cat("Rows dropped due to missing covariates: ",Xrows - nrow(df),"\n")
#   
#   df <- droplevels(df)
#   
#   Wscreen=NULL
#   if(!is.null(W)){
#     cat("\n-----------------------------------------\nPre-screening the adjustment covariates:\n-----------------------------------------\n")
#     prescreen_family <- ifelse(family=="gaussian",family,"binomial")
#     
#     Wdf <- df %>% select(W)
#     #drop covariates with near zero variance
#     if(length(nearZeroVar(Wdf))>0){
#       Wdf<-Wdf[,-nearZeroVar(Wdf)]
#     }
#     
#     #prescreen
#     suppressWarnings(try(Wscreen <- 
#                            MICS_prescreen(Y = df$Y, 
#                                           Ws = Wdf, 
#                                           family = prescreen_family, 
#                                           pval = 0.2, print = T)))
#     #select n/10 covariates if binary outcome
#     if(family!="gaussian" & !is.null(Wscreen)){
#       nY<-floor(min(table(df$Y, df$X))/10)
#       if(length(Wscreen)>nY){
#         Wscreen<-Wscreen[1:nY]
#       }
#     }
#     
#     #Drop sparse factor levels
#     if(!is.null(Wscreen)){
#       Wdf <- df %>% subset(., select =c(Wscreen))
#       Wdf <- as.data.frame(model.matrix(~., Wdf)[,-1]) 
#       
#       #scale and drop covariates with near zero variance
#       pp_no_nzv <- preProcess(Wdf,
#                               method = c("center", "scale", "YeoJohnson", "nzv"))
#       pp_no_nzv
#       Wdf<- predict(pp_no_nzv, newdata = Wdf)
#     }else{
#       Wdf=NULL
#     }
#     
#     df <- bind_cols(df %>% subset(., select =c("Y","X","id", "weight")), Wdf)
#     df <- df[complete.cases(df),]
#   }
#   
#   #order by id for geeglm function
#   df <- df %>% arrange(id)
#   
#   if(family=="gaussian"){
#     # #model formula
#     # f <- ifelse(is.null(Wscreen),
#     #             "Y ~ X + (1|id)",
#     #             paste0("Y ~ X + (1|id) + ", paste(colnames(Wdf), collapse = " + ")))
#     # #fit model
#     # fit <- lmer(as.formula(f),  data=df, weights = weight)
#     # 
#     # coef <- as.data.frame(t(summary(fit)$coefficients[2,]))
#     # res <- data.frame(Y=varnames[1],
#     #                   X=varnames[2],
#     #                   coef=coef$Estimate,
#     #                   se=coef$`Std. Error`,
#     #                   tval=coef$`t value`)
#     # 
#     # #Calc 95%CI
#     # res$ci.lb <- res$coef - 1.96*res$se
#     # res$ci.ub <- res$coef + 1.96*res$se
#     # 
#     # #calculate pvalue by normal approximation
#     # #https://www.r-bloggers.com/three-ways-to-get-parameter-specific-p-values-from-lmer/
#     # res$pval <- 2 * (1 - pnorm(abs(res$tval)))
#     # res$N<-nrow(df)
#     # res$W <-ifelse(is.null(Wscreen), "unadjusted", paste(Wscreen, sep="", collapse=", "))
#     
#     
#     
#     #model formula
#     f <- ifelse(is.null(Wscreen),
#                 "Y ~ X",
#                 paste0("Y ~ X  + ", paste(colnames(Wdf), collapse = " + ")))
#     fit <- mpreg(formula = as.formula(f), df = df, family="gaussian", vcv=FALSE)
#     
#     coef <- as.data.frame(t(fit[2,]))
#     res <- data.frame(Y=varnames[1],
#                       X=varnames[2],
#                       coef=coef$Estimate,
#                       se=coef$`Std. Error`,
#                       Zval=coef$`z value`,
#                       pval=coef$`Pr(>|z|)`)
#     
#     #Calc 95%CI
#     res$ci.lb <- res$coef - 1.96*res$se
#     res$ci.ub <- res$coef + 1.96*res$se
#     
#     res$N<-nrow(df)
#     res$W <-ifelse(is.null(Wscreen), "unadjusted", paste(Wscreen, sep="", collapse=", "))
#     
#     
#   }
#   if(family=="modified possion"){
#     
#     #check sparsity
#     sparseN<-min(table(df$X, df$Y))
#     if(sparseN>10){
#       
#       
#       #model formula
#       f <- ifelse(is.null(Wscreen),
#                   "Y ~ X",
#                   paste0("Y ~ X + ", paste(colnames(Wdf), collapse = " + ")))
#       # id <- df$id
#       # weight <- df$weight
#       fit=NULL
#       
#       
#       #   #fit model
#       # tryCatch( { fit <- withTimeout( { 
#       #    geeglm(formula = as.formula(f),
#       #                 data    = df,
#       #                 weights = weight,
#       #                 family  = poisson(link = "log"),
#       #                 id      = id,
#       #                 corstr  = "exchangeable") 
#       #   },
#       #   timeout = 120) },
#       #   TimeoutException = function(ex) cat("Timed out\n"))
#       # 
#       #   coef <- as.data.frame(summary(fit)$coefficients[2,])
#       #   res <- data.frame(Y=varnames[1],
#       #                     X=varnames[2],
#       #                     coef=coef$Estimate,
#       #                     RR=exp(coef$Estimate),
#       #                     se=coef$Std.err,
#       #                     wald=coef$Wald,
#       #                     pval=coef$`Pr(>|W|)`)
#       #   
#       #   #Calc 95%CI
#       #   res$ci.lb <- exp(res$coef - 1.96*res$se)
#       #   res$ci.ub <- exp(res$coef + 1.96*res$se)
#       #   
#       #   #calculate pvalue by normal approximation
#       #   #https://www.r-bloggers.com/three-ways-to-get-parameter-specific-p-values-from-lmer/
#       
# 
#       #complete case model fitting:      
#       fit <- mpreg(formula = as.formula(f), df = df, family="modified possion", vcv=FALSE)
#       coef <- as.data.frame(t(fit[2,]))
#       res <- data.frame(Y=varnames[1],
#                         X=varnames[2],
#                         coef=coef$Estimate,
#                         RR=exp(coef$Estimate),
#                         se=coef$`Std. Error`,
#                         Zval=coef$`z value`,
#                         pval=coef$`Pr(>|z|)`)
#       
#       #Calc 95%CI
#       res$ci.lb <- exp(res$coef - 1.96*res$se)
#       res$ci.ub <- exp(res$coef + 1.96*res$se)
# 
# #---------------------------------------------------------------------------
# # Save test data
# #---------------------------------------------------------------------------      
# save(list=ls(), file=here("results/temp_mice_data"))
# 
#       
#       
#---------------------------------------------------------------------------
# Mice fitting
#---------------------------------------------------------------------------   
      
load(file=here("results/temp_mice_data"))
      
meth <- make.method(df)
pred <- make.predictorMatrix(df)

imp <-mice(df, 
             meth = meth, 
              pred = pred, 
              print = FALSE, 
              m = 10, 
             maxit = 6)

# fit <- mpreg(formula = as.formula(f), df = df, family="modified possion", vcv=FALSE)
#       
# fit1.lm <- imp %>%
#   with(lm(formula = as.formula(f)))
#       
# est1.lm <- pool(fit1.lm)
# est1.lm

fit1.glm <- imp %>%
  with(glm(formula = as.formula(f), family=poisson(link="log"), weights = imp$weight))

est1.glm <- pool(fit1.glm)
est1.glm
  

  # # dataaset used to fit the model	
  # if(family=="gaussian"){
  #   fit <- glm(as.formula(formula),family="gaussian", weights = df$weight, data=df)
  # }else{

#resources for clustered SE's with multiple imputation
#http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.903.8133&rep=rep1&type=pdf
#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5588607/
#https://stackoverflow.com/questions/43942417/r-clustered-robust-standard-errors-using-miceadds-lm-cluster-error-with-subse
#https://cran.r-project.org/web/packages/miceadds/miceadds.pdf


datlist <- miceadds::mids2datlist( imp )
# linear regression with cluster robust standard errors
mod <- lapply(datlist, FUN = function(data){miceadds::glm.cluster( data=df ,         
                                                                  formula=as.formula(f),  
                                                                  cluster = df$id,
                                                                  weights = df$weight)}  )

# extract parameters and covariance matrix
betas <- lapply( mod , FUN = function(rr){ coef(rr) } )
vars <- lapply( mod , FUN = function(rr){ vcov(rr) } )
# conduct statistical inference
fit <- summary(pool_mi( qhat = betas, u = vars ))

      coef <- as.data.frame(fit[2,])
      res <- data.frame(Y=varnames[1],
                        X=varnames[2],
                        coef=coef$results,
                        RR=exp(coef$results),
                        se=coef$se,
                        pval=coef$p)

      #Calc 95%CI
      res$ci.lb <- exp(res$coef - 1.96*res$se)
      res$ci.ub <- exp(res$coef + 1.96*res$se)


res
