#https://www.gerkovink.com/miceVignettes/Combining_inferences/Combining_inferences.html
library(mice) # Data imputation
library(dplyr) # Data manipulation
library(magrittr) # Flexible piping in R
library(purrr) # Flexible functional programming
set.seed(123)

meth <- make.method(boys)
meth["bmi"] <- "~ I(wgt / (hgt / 100)^2)"
#Then we remove bmi as a predictor for hgt and wgt to avoid circularity (bmi feeding back into hgt and wgt.
                                                                        
pred <- make.predictorMatrix(boys)
pred[c("hgt", "wgt"), "bmi"] <- 0
pred
imp <-mice(boys, 
           meth = meth, 
           pred = pred, 
           print = FALSE, 
           m = 10, 
           maxit = 6)

fit1.lm <- imp %>%
  with(lm(age ~ wgt + hgt))

est1.lm <- pool(fit1.lm)
est1.lm

summary(est1.lm)

glm.cluster(data, formula, cluster, weights=NULL, subset=NULL, family="binomial" )


glm.ro


library(mice)
library(miceadds)
library(estimatr)

# imputation of the dataset: use six imputations
data(data.ma01)
dat <- data.ma01[, -c(1)] # note I keep idschool in data
imp <- mice::mice( dat , maxit = 3, m = 6)
datlist <- miceadds::mids2datlist(imp)

# linear regression with cluster robust standard errors and weights
mod <- lapply(
  datlist, 
  function (dat) {
    estimatr::lm_robust(read ~ paredu + female, dat, clusters = idschool, weights = studwgt)
  }
)

# note that you can use the `se_type` argument of lm_robust() 
# to change the vcov estimation

# extract parameters and covariance matrix
betas <- lapply(mod, coef)
vars <- lapply(mod, vcov)
# conduct statistical inference
summary(pool_mi( qhat = betas, u = vars ))









data(data.ma01)
# imputation of the dataset: use six imputations
dat <- data.ma01[ , - c(1:2) ]
imp <- mice::mice( dat , maxit=3 , m=6 )
datlist <- miceadds::mids2datlist( imp )
# linear regression with cluster robust standard errors
mod <- lapply(datlist, FUN = function(data){miceadds::lm.cluster( data=data ,         
                                                                  formula=read ~ paredu+ female ,  cluster = data.ma01$idschool )}  )

# extract parameters and covariance matrix
betas <- lapply( mod , FUN = function(rr){ coef(rr) } )
vars <- lapply( mod , FUN = function(rr){ vcov(rr) } )
# conduct statistical inference
summary(pool_mi( qhat = betas, u = vars ))

#Example that breaks with weights:
mod <- lapply(datlist, FUN = function(data){miceadds::lm.cluster( data=data ,         
                                                                    formula=read ~ paredu+ female ,  cluster = data.ma01$idschool,
                                                                    weights=data.ma01$studwgt)}  )





