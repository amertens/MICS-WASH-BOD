


# #saveRDS(res1, file=here(paste0("results/individual_estimates/",i,"_EC_H_",adj,".rds")))
# 
# 
# outcomes=c("stunt", "wast","diarrhea","ari")
# family="modified possion"


run_MICS_regressions <- function(outcomes, family, PAF, Wvars){
  
  adj <- ifelse(is.null(Wvars),"unadj","adj")
  
  fullres <- NULL 
 

for(i in outcomes){
  res1 <- res2 <- res3 <- res4 <- res5 <- res6 <- res7 <- res8 <- NULL
  print(i)
  res1 <- d %>% group_by(country) %>%
    do(mics_regression(d=.,
                       Y =i,
                       X="EC_H",
                       W=Wvars,
                       weight = "ecpopweight_H",
                       clustid= "clust_num",
                       family=family, calc_PAF=PAF, low_risk_level="Uncontaminated"))
  saveRDS(res1, file=here(paste0("results/individual_estimates/",i,"_EC_H_",adj,".rds")))
  
  res2 <- d %>% group_by(country) %>%
    do(mics_regression(d=.,
                       Y =i,
                       X="EC_S",
                       W=Wvars,
                       weight = "ecpopweight_S",
                       clustid= "clust_num",
                       family=family, calc_PAF=PAF, low_risk_level="Uncontaminated"))
  saveRDS(res2, file=here(paste0("results/individual_estimates/",i,"_EC_S_",adj,".rds")))
  
  res3 <- d %>% group_by(country) %>%
    do(mics_regression(d=.,
                       Y =i,
                       X="san_imp",
                       W=Wvars,
                       weight = "popweight",
                       clustid= "clust_num",
                       family=family, calc_PAF=PAF, low_risk_level="Improved"))
  saveRDS(res3, file=here(paste0("results/individual_estimates/",i,"_san_imp_",adj,".rds")))
  
  res4 <- d %>% group_by(country) %>%
    do(mics_regression(d=.,
                       Y =i,
                       X="wat_imp",
                       W=Wvars,
                       weight = "popweight",
                       clustid= "clust_num",
                       family=family, calc_PAF=PAF, low_risk_level="Improved"))
  saveRDS(res4, file=here(paste0("results/individual_estimates/",i,"_wat_imp_",adj,".rds")))
  
  res5 <- d %>% group_by(country) %>%
    do(mics_regression(d=.,
                       Y =i,
                       X="hyg_imp",
                       W=Wvars,
                       weight = "popweight",
                       clustid= "clust_num",
                       family=family, calc_PAF=PAF, low_risk_level="Improved"))
  saveRDS(res5, file=here(paste0("results/individual_estimates/",i,"_hyg_imp_",adj,".rds")))
  
  res6 <- d %>% group_by(country) %>%
    do(mics_regression(d=.,
                       Y =i,
                       X="safely_manH20",
                       W=Wvars,
                       weight = "ecpopweight_H",
                       clustid= "clust_num",
                       family=family, calc_PAF=PAF, low_risk_level="Safe"))
  saveRDS(res6, file=here(paste0("results/individual_estimates/",i,"_safely_manH20_",adj,".rds")))
  
  res7 <- d %>% group_by(country) %>%
    do(mics_regression(d=.,
                       Y =i,
                       X="WASH",
                       W=Wvars,
                       weight = "ecpopweight_H",
                       clustid= "clust_num",
                       family=family, calc_PAF=PAF, low_risk_level="Improved"))
  saveRDS(res7, file=here(paste0("results/individual_estimates/",i,"_WASH_",adj,".rds")))
  
  res8 <- d %>% group_by(country) %>%
    do(mics_regression(d=.,
                       Y =i,
                       X="WASH_noEC",
                       W=Wvars,
                       weight = "popweight",
                       clustid= "clust_num",
                       family=family, calc_PAF=PAF, low_risk_level="Improved"))
  saveRDS(res8, file=here(paste0("results/individual_estimates/",i,"_WASH_",adj,".rds")))
  

  
  
  fullres <- bind_rows(fullres, res1, res2, res3, res4, res5, res6, res7, res8)
}
 
  return(fullres) 
}






mics_multinomial_regression <- function(d, Y, X, W, weight = "ecpopweight_H", clustid= "clust_num", family="modified possion", calc_PAF=FALSE, low_risk_level=0, return_model=FALSE){
  fullres <-res1 <- res2 <- res3 <- res4 <- res5 <- res6 <- res7 <- NULL
  Xname=X
  colnames(d)[which(colnames(d)==X)] <- "Xvar"
  
  d <- d %>% filter(!is.na(Xvar)) 
  
  X_levels = levels(d$Xvar)
  print(X_levels)
  nlevels=length(X_levels)
  
  for(i in 1:(nlevels-1)){
    df <- d %>% filter(Xvar %in% X_levels[c(i, nlevels)])
    df$Xvar <- ifelse(df$Xvar==X_levels[i],1,0) 
    ref=X_levels[nlevels]
    contrast=X_levels[i]

    
    res <- df %>% group_by(country) %>%
      do(mics_regression(d=.,
                         Y =Y,
                         X="Xvar",
                         W=W,
                         weight = weight,
                         clustid= clustid,
                         family=family, calc_PAF=calc_PAF, low_risk_level=low_risk_level))
    res$X=Xname
    res$ref=ref
    res$contrast=contrast
    
    fullres <- bind_rows(fullres, res)
    
  }
  
  
  return(fullres)
}







run_mics_multinomial_regressions <- function(outcomes, family, PAF, Wvars){
  
  adj <- ifelse(is.null(Wvars),"unadj","adj")
  
  fullres <- NULL 
  
  
  for(i in outcomes){
    res1 <- res2 <- res3 <- res4 <- res5 <- res6 <- res7 <- NULL
    print(i)
    res1 <- d %>% group_by(country) %>%
      do(mics_multinomial_regression(d=.,
                              Y =i,
                              X="EC_risk_H",
                              W=Wvars,
                              weight = "ecpopweight_H",
                              clustid= "clust_num",
                              family=family, calc_PAF=PAF, low_risk_level=0))
    #saveRDS(res1, file=here(paste0("results/individual_estimates/",i,"_EC_H_",adj,".rds")))
    
    res2 <- d %>% group_by(country) %>%
      do(mics_multinomial_regression(d=.,
                              Y =i,
                              X="EC_risk_S",
                              W=Wvars,
                              weight = "ecpopweight_S",
                              clustid= "clust_num",
                              family=family, calc_PAF=PAF, low_risk_level=0))
    #saveRDS(res2, file=here(paste0("results/individual_estimates/",i,"_EC_S_",adj,".rds")))
    
    res3 <- d %>% group_by(country) %>%
      do(mics_multinomial_regression(d=.,
                              Y =i,
                              X="san_imp_cat",
                              W=Wvars,
                              weight = "popweight",
                              clustid= "clust_num",
                              family=family, calc_PAF=PAF, low_risk_level=1))
    #saveRDS(res3, file=here(paste0("results/individual_estimates/",i,"_san_imp_",adj,".rds")))
    
    res4 <- d %>% group_by(country) %>%
      do(mics_multinomial_regression(d=.,
                              Y =i,
                              X="wat_imp_cat",
                              W=Wvars,
                              weight = "popweight",
                              clustid= "clust_num",
                              family=family, calc_PAF=PAF, low_risk_level=1))
    #saveRDS(res4, file=here(paste0("results/individual_estimates/",i,"_wat_imp_",adj,".rds")))
    
    res5 <- d %>% group_by(country) %>%
      do(mics_multinomial_regression(d=.,
                              Y =i,
                              X="hyg_imp_cat",
                              W=Wvars,
                              weight = "popweight",
                              clustid= "clust_num",
                              family=family, calc_PAF=PAF, low_risk_level=1))
    #saveRDS(res5, file=here(paste0("results/individual_estimates/",i,"_hyg_imp_",adj,".rds")))
    
    
    fullres <- bind_rows(fullres, res1, res2, res3, res4, res5)
  }
  
  return(fullres) 
}





run_mics_tmle <- function(outcomes, family, PAF=NULL, glm=F, Wvars){
  
  adj <- ifelse(is.null(Wvars),"unadj","adj")
  
  fullres <- NULL 
  
  
  for(i in outcomes){
    res1 <- res2 <- res3 <- res4 <- res5 <- res6 <- res7 <- res8 <- NULL
    print(i)
    res1 <- d %>% group_by(country) %>%
      do(mics_tmle(d=.,
                   Y =i,
                   X="EC_H",
                   W=Wvars,
                   glm=glm,
                   weight = "ecpopweight_H",
                   clustid= "clust_num",
                   family=family))
    #saveRDS(res1, file=here(paste0("results/individual_estimates/",i,"_EC_H_",adj,".rds")))
    
    res2 <- d %>% group_by(country) %>%
      do(mics_tmle(d=.,
                   Y =i,
                   X="EC_S",
                   W=Wvars,
                   glm=glm,
                   weight = "ecpopweight_S",
                   clustid= "clust_num",
                   family=family))
    #saveRDS(res2, file=here(paste0("results/individual_estimates/",i,"_EC_S_",adj,".rds")))
    
    res3 <- d %>% group_by(country) %>%
      do(mics_tmle(d=.,
                   Y =i,
                   X="san_imp",
                   W=Wvars,
                   glm=glm,
                   weight = "popweight",
                   clustid= "clust_num",
                   family=family))
    #saveRDS(res3, file=here(paste0("results/individual_estimates/",i,"_san_imp_",adj,".rds")))
    
    res4 <- d %>% group_by(country) %>%
      do(mics_tmle(d=.,
                   Y =i,
                   X="wat_imp",
                   W=Wvars,
                   glm=glm,
                   weight = "popweight",
                   clustid= "clust_num",
                   family=family))
    #saveRDS(res4, file=here(paste0("results/individual_estimates/",i,"_wat_imp_",adj,".rds")))
    
    res5 <- d %>% group_by(country) %>%
      do(mics_tmle(d=.,
                   Y =i,
                   X="hyg_imp",
                   W=Wvars,
                   glm=glm,
                   weight = "popweight",
                   clustid= "clust_num",
                   family=family))
    #saveRDS(res5, file=here(paste0("results/individual_estimates/",i,"_hyg_imp_",adj,".rds")))
    
    res6 <- d %>% group_by(country) %>%
      do(mics_tmle(d=.,
                   Y =i,
                   X="safely_manH20",
                   W=Wvars,
                   glm=glm,
                   weight = "ecpopweight_H",
                   clustid= "clust_num",
                   family=family))
    #saveRDS(res6, file=here(paste0("results/individual_estimates/",i,"_safely_manH20_",adj,".rds")))
    
    res7 <- d %>% group_by(country) %>%
      do(mics_tmle(d=.,
                   Y =i,
                   X="WASH",
                   W=Wvars,
                   glm=glm,
                   weight = "ecpopweight_H",
                   clustid= "clust_num",
                   family=family))
    #saveRDS(res7, file=here(paste0("results/individual_estimates/",i,"_WASH_",adj,".rds")))
    
    res8 <- d %>% group_by(country) %>%
      do(mics_tmle(d=.,
                   Y =i,
                   X="WASH_noEC",
                   W=Wvars,
                   glm=glm,
                   weight = "popweight",
                   clustid= "clust_num",
                   family=family))
    #saveRDS(res7, file=here(paste0("results/individual_estimates/",i,"_WASH_",adj,".rds")))
    
    
    
    
    fullres <- bind_rows(fullres, res1, res2, res3, res4, res5, res6, res7, res8)
  }
  
  return(fullres) 
}



