

rm(list=ls())
source("0-config.R")



#load clean data
d <- readRDS(here("results/adjusted_RR.rds"))
head(d)


calculate_paf_and_95ci <- function(prevalence, rr, se_log_rr) {
  # Calculate PAF
  paf <- prevalence * (rr - 1) / (prevalence * (rr - 1) + 1)
  
  # Calculate the derivative of PAF with respect to log(RR)
  d_paf_d_log_rr <- prevalence * (1 - prevalence) * exp(log(rr)) / (1 + prevalence * (rr - 1))^2
  
  # Calculate the variance of PAF
  var_paf <- (d_paf_d_log_rr^2) * (se_log_rr^2)
  
  # Calculate the standard error of PAF
  se_paf <- sqrt(var_paf)
  
  # Calculate the 95% CI for PAF
  ci_lower_paf <- paf - 1.96 * se_paf
  ci_upper_paf <- paf + 1.96 * se_paf
  
  return(list(PAF = paf, CI_lower = ci_lower_paf, CI_upper = ci_upper_paf))
}

# Example usage
prevalence <- 0.30  # Example prevalence
rr <- 1.5           # Example relative risk
se_log_rr <- 0.1    # Example standard error of log(RR)

# Calculate PAF and its 95% CI
d$PAF=d$PAF.ci.lb=d$PAF.ci.ub=NA
for(i in 1:nrow(d)){
  res=data.frame(PAF=NA, PAF.ci.lb=NA, PAF.ci.ub=NA)
  res <- calculate_paf_and_95ci(prevalence=0.3, rr=d$RR[i], se_log_rr=d$se[i])
  d$PAF[i] = res$PAF
  d$PAF.ci.lb[i]=res$CI_lower
  d$PAF.ci.ub[i]=res$CI_upper
}

summary(d$PAF)
summary(d$PAF.ci.lb)

# 
# res_adj_bin <- run_MICS_regressions(outcomes = c("stunt","diarrhea"), family="modified possion", PAF=T, Wvars=Wvars)
# res_adj_bin <- res_adj_bin %>% mutate(adjusted=1)
# 
# 
# saveRDS(res_adj_bin, here("results/adj_PAFs.rds"))
# 
# res_paf <- res_adj_bin
# 
# saveRDS(res_paf, here("results/prim_PAFs.rds"))
# 


# Define the PAF function
paf_function <- function(pe, rr) {
  pe * (rr - 1) / (pe * (rr - 1) + 1)
}

# Partial derivatives of PAF with respect to PE and RR
d_paf_d_pe <- function(pe, rr) {
  (rr - 1) / (pe * (rr - 1) + 1)^2
}
d_paf_d_rr <- function(pe, rr) {
  pe / (pe * (rr - 1) + 1)^2
}

# Example data
pe <- 0.3        # Prevalence
se_pe <- 0.05    # Standard Error of PE
rr <- 1.5        # Relative Risk
se_rr <- 0.1     # Standard Error of RR

# Calculate PAF
paf <- paf_function(pe, rr)

# Calculate Variances of PE and RR
var_pe <- se_pe^2
var_rr <- se_rr^2

# Apply Delta Method
var_paf <- (d_paf_d_pe(pe, rr)^2 * var_pe) + (d_paf_d_rr(pe, rr)^2 * var_rr)
se_paf <- sqrt(var_paf)

# Calculate 95% CI for PAF
ci_lower <- paf - 1.96 * se_paf
ci_upper <- paf + 1.96 * se_paf

# Results
list(PAF = paf, CI_lower = ci_lower, CI_upper = ci_upper)
