

source("0-config.R")
library(pifpaf)

d <- readRDS(here("data/compiled_clean_MICS_survey.rds")) %>% filter(country=="Bangladesh")
res <- readRDS(here("results/pooled_results.rds")) %>% filter(country=="Bangladesh")

#X <- data.frame(Exposure = d$EC_risk_S, contrast = NA) %>% filter(!is.na(Exposure))
res <- res %>% filter(X=="EC_risk_S", Y=="Stunting", adjusted==1)
#X <- data.frame(Exposure = d$EC_risk_S, contrast = NA) %>% filter(!is.na(Exposure))

X <- data.frame(Exposure = d$EC_risk_S) %>% filter(!is.na(Exposure))

X$Exposure <- fct_recode(X$Exposure, "Low risk" = "1", "Moderate risk" = "2", "High risk" = "3", "Very high risk" = "4",)
head(X)

thetahat <- res$est
thetahat <- c(1.4, 1.3, 1.2)
thetavar <- diag(((log(res$est)-log(res$ci.lb))/1.96)^2)

#Categorical relative risk function
rr <- function(X, theta){
  
  #Create return vector with default risk of 1
  r_risk <- rep(1, nrow(X))
  
  #Assign categorical relative risk
  # for(i in 1:length(thetahat)){
  #   r_risk[which(X[,"Exposure"] == X[i,"Contrast"])] <- thetahat[i]
  # }
  r_risk[which(X[,"Exposure"] == "Moderate risk")]      <- thetahat[1]
  r_risk[which(X[,"Exposure"] == "High risk")]  <- thetahat[2]
  r_risk[which(X[,"Exposure"] == "Very high risk")]       <- thetahat[3]
  
  return(r_risk)
}

paf.confidence(X, thetahat, rr, thetavar, check_rr = FALSE)



#Example 1: Exponential Relative Risk
#--------------------------------------------
set.seed(18427)
X        <- data.frame(Exposure = rnorm(100,3,1))
thetahat <- 0.32
thetavar <- 0.02
rr       <- function(X, theta){exp(theta*X)}

#Using bootstrap method
paf.confidence(X, thetahat, rr, thetavar)

## Not run: 
#Same example with loglinear method
paf.confidence(X, thetahat, rr, thetavar, confidence_method = "loglinear")

#Same example with linear method (usually the widest and least precise)
paf.confidence(X, thetahat, rr, thetavar, confidence_method = "linear")

#Same example with inverse method 
paf.confidence(X, thetahat, rr, thetavar, confidence_method = "inverse")

#Same example with one2one method 
#assume 99% ci of theta is [0.27, 0.35]
paf.confidence(X, thetahat, rr, thetalow = 0.27, thetaup = 0.35, 
               confidence_method = "one2one", confidence_theta = 99)

#Example 2: Linear Relative Risk with weighted sample
#--------------------------------------------
set.seed(18427)
X                   <- data.frame(Exposure = rbeta(100,3,1))
weights             <- runif(100)
normalized_weights  <- weights/sum(weights)
thetahat            <- 0.17
thetavar            <- 0.01
rr                  <- function(X, theta){theta*X^2 + 1}
paf.confidence(X, thetahat, rr, thetavar, weights = normalized_weights)

#Change the confidence level and paf method
paf.confidence(X, thetahat, rr,  thetavar, weights = normalized_weights, 
               method = "kernel", confidence = 90)


#Example 3: Multivariate Linear Relative Risk
#--------------------------------------------
set.seed(18427)
X1       <- rnorm(100,4,1)
X2       <- rnorm(100,2,0.4)
thetahat <- c(0.12, 0.03)
thetavar <- diag(c(0.01, 0.02))

#But the approximate method crashes due to operator
Xmean <- data.frame(Exposure = mean(X1), 
                    Covariate = mean(X2))
Xvar  <- var(cbind(X1, X2))

#When creating relative risks avoid using the $ operator
#as it doesn't work under approximate method of PAF
rr_not    <- function(X, theta){
  exp(theta[1]*X$Exposure + theta[2]*X$Covariate)
}
rr_better <- function(X, theta){
  exp(theta[1]*X[,"Exposure"] + theta[2]*X[,"Covariate"])
}

paf.confidence(Xmean, thetahat, rr_better, thetavar,
               method = "approximate", Xvar = Xvar)

## End(Not run)
## Not run: 
#Warning: $ operator in rr definitions don't work in approximate
paf.confidence(Xmean, thetahat, rr_not, thetavar,
               method = "approximate", Xvar = Xvar)

## End(Not run)

## Not run: 
#Example 4: Categorical Relative Risk & Exposure
#--------------------------------------------
set.seed(18427)
mysample  <- sample(c("Normal","Overweight","Obese"), 100, 
                    replace = TRUE, prob = c(0.4, 0.1, 0.5))
X        <- data.frame(Exposure = mysample)

thetahat <- c(1, 1.2, 1.5)
thetavar <- diag(c(0.1, 0.2, 0.3))

#Categorical relative risk function
rr <- function(X, theta){
  
  #Create return vector with default risk of 1
  r_risk <- rep(1, nrow(X))
  
  #Assign categorical relative risk
  r_risk[which(X[,"Exposure"] == "Normal")]      <- thetahat[1]
  r_risk[which(X[,"Exposure"] == "Overweight")]  <- thetahat[2]
  r_risk[which(X[,"Exposure"] == "Obese")]       <- thetahat[3]
  
  return(r_risk)
}

paf.confidence(X, thetahat, rr, thetavar, check_rr = FALSE)


#Example 5: Continuous Exposure and Categorical Relative Risk
#------------------------------------------------------------------
set.seed(18427)

#Assume we have BMI from a sample
BMI          <- data.frame(Exposure = rlnorm(100, 3.1, sdlog = 0.1))

#Theoretical minimum risk exposure is at 20kg/m^2 in borderline "Normal" category
BMI_adjusted <- BMI - 20

thetahat <- c(Malnourished = 2.2, Normal = 1, Overweight = 1.8, 
              Obese = 2.5)
thetavar <- diag(c(0.1, 0.2, 0.2, 0.1))
rr       <- function(X, theta){
  
  #Create return vector with default risk of 1
  r_risk <- rep(1, nrow(X))
  
  #Assign categorical relative risk
  r_risk[which(X[,"Exposure"] < 0)]             <- theta[1] #Malnourished
  r_risk[intersect(which(X[,"Exposure"] >= 0), 
                   which(X[,"Exposure"] < 5))]  <- theta[2] #Normal
  r_risk[intersect(which(X[,"Exposure"] >= 5), 
                   which(X[,"Exposure"] < 10))] <- theta[3] #Overweight
  r_risk[which(X[,"Exposure"] >= 10)]           <- theta[4] #Obese
  
  return(r_risk)
}

paf.confidence(BMI_adjusted, thetahat, rr, thetavar, check_exposure = FALSE)

#Example 6: Bivariate exposure and rr ("classical PAF")
#------------------------------------------------------------------
set.seed(18427)
mysample  <- sample(c("Exposed","Unexposed"), 1000, 
                    replace = TRUE, prob = c(0.1, 0.9))
X         <- data.frame(Exposure = mysample)
theta     <- c("Exposed" = 2.5, "Unexposed" = 1.2)  
thetavar  <- matrix(c(0.04, 0.02, 0.02, 0.03), ncol = 2)
rr        <- function(X, theta){
  
  #Create relative risk function
  r_risk <- rep(1, nrow(X))
  
  #Assign values of relative risk
  r_risk[which(X[,"Exposure"] == "Unexposed")] <- theta["Unexposed"]
  r_risk[which(X[,"Exposure"] == "Exposed")]   <- theta["Exposed"]
  
  return(r_risk)
}    

paf.confidence(X, theta, rr, thetavar)

#Example 7: Continuous exposure, several covariates
#------------------------------------------------------------------
X <- data.frame(Exposure = rbeta(100, 2, 3),
                Age      = runif(100, 20, 100),
                Sex      = sample(c("M","F"), 100, replace = TRUE),
                BMI      = rlnorm(100, 3.2, 0.2))
thetahat <- c(-0.1, 0.05, 0.2, -0.4, 0.3, 0.1)

#Create variance of theta
almostvar <- matrix(runif(6^2), ncol = 6)
thetavar  <- t(almostvar) %*% almostvar
rr <- function(X, theta){
  #Create risk vector
  Risk    <- rep(1, nrow(X))
  
  #Identify subpopulations
  males   <- which(X[,"Sex"] == "M")
  females <- which(X[,"Sex"] == "F")
  
  #Calculate population specific rr
  Risk[males] <- theta[1]*X[males,"Exposure"] + 
    theta[2]*X[males,"Age"]^2 + 
    theta[3]*X[males,"BMI"]/2 
  
  Risk[females] <- theta[4]*X[females,"Exposure"] + 
    theta[5]*X[females,"Age"]^2 + 
    theta[6]*X[females,"BMI"]/2 
  
  return(Risk)
}

paf.confidence(X, thetahat, rr, thetavar)

## End(Not run)
