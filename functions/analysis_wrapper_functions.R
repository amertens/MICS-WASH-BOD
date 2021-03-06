

# #saveRDS(res1, file=here(paste0("results/individual_estimates/",i,"_EC_H_",adj,".rds")))
# 
# 
# outcomes=c("stunt", "wast","diarrhea","ari")
# family="modified possion"


run_MICS_regressions <- function(outcomes, family, PAF, Wvars, save.data=F){
  
  adj <- ifelse(is.null(Wvars),"unadj","adj")
  
  fullres <- NULL 
 

for(i in outcomes){
  res1 <- res2 <- res3 <- res4 <- res5 <- res6 <- res7 <- res8 <- res9 <- NULL
  print(i)
  res1 <- d %>% group_by(country) %>%
    do(mics_regression(d=.,
                       Y =i,
                       X="EC_H",
                       W=Wvars,
                       weight = "ecpopweight_H",
                       clustid= "clust_num",
                       family=family, calc_PAF=PAF, low_risk_level="Uncontaminated"))
  if(save.data) saveRDS(res1, file=here(paste0("results/individual_estimates/",i,"_EC_H_",adj,".rds")))
  
  res2 <- d %>% group_by(country) %>%
    do(mics_regression(d=.,
                       Y =i,
                       X="EC_S",
                       W=Wvars,
                       weight = "ecpopweight_S",
                       clustid= "clust_num",
                       family=family, calc_PAF=PAF, low_risk_level="Uncontaminated"))
  if(save.data) saveRDS(res2, file=here(paste0("results/individual_estimates/",i,"_EC_S_",adj,".rds")))
  
  res3 <- d %>% group_by(country) %>%
    do(mics_regression(d=.,
                       Y =i,
                       X="san_imp",
                       W=Wvars,
                       weight = "popweight",
                       clustid= "clust_num",
                       family=family, calc_PAF=PAF, low_risk_level="Improved"))
  if(save.data) saveRDS(res3, file=here(paste0("results/individual_estimates/",i,"_san_imp_",adj,".rds")))
  
  res4 <- d %>% group_by(country) %>%
    do(mics_regression(d=.,
                       Y =i,
                       X="wat_imp",
                       W=Wvars,
                       weight = "popweight",
                       clustid= "clust_num",
                       family=family, calc_PAF=PAF, low_risk_level="Improved"))
  if(save.data) saveRDS(res4, file=here(paste0("results/individual_estimates/",i,"_wat_imp_",adj,".rds")))
  
  res5 <- d %>% group_by(country) %>%
    do(mics_regression(d=.,
                       Y =i,
                       X="hyg_imp",
                       W=Wvars,
                       weight = "popweight",
                       clustid= "clust_num",
                       family=family, calc_PAF=PAF, low_risk_level="Improved"))
  if(save.data) saveRDS(res5, file=here(paste0("results/individual_estimates/",i,"_hyg_imp_",adj,".rds")))
  
  res6 <- d %>% group_by(country) %>%
    do(mics_regression(d=.,
                       Y =i,
                       X="safely_manH20",
                       W=Wvars,
                       weight = "ecpopweight_H",
                       clustid= "clust_num",
                       family=family, calc_PAF=PAF, low_risk_level="Safe"))
  if(save.data) saveRDS(res6, file=here(paste0("results/individual_estimates/",i,"_safely_manH20_",adj,".rds")))
  
  res7 <- d %>% group_by(country) %>%
    do(mics_regression(d=.,
                       Y =i,
                       X="WASH",
                       W=Wvars,
                       weight = "ecpopweight_H",
                       clustid= "clust_num",
                       family=family, calc_PAF=PAF, low_risk_level="Improved"))
  if(save.data) saveRDS(res7, file=here(paste0("results/individual_estimates/",i,"_WASH_",adj,".rds")))
  
  res8 <- d %>% group_by(country) %>%
    do(mics_regression(d=.,
                       Y =i,
                       X="WASH_noEC",
                       W=Wvars,
                       weight = "popweight",
                       clustid= "clust_num",
                       family=family, calc_PAF=PAF, low_risk_level="Improved"))
  if(save.data) saveRDS(res8, file=here(paste0("results/individual_estimates/",i,"_WASH_noEC_",adj,".rds")))
  
  res9 <- d %>% group_by(country) %>%
    do(mics_regression(d=.,
                       Y =i,
                       X="piped_san",
                       W=Wvars,
                       weight = "popweight",
                       clustid= "clust_num",
                       family=family, calc_PAF=PAF, low_risk_level="Piped"))
  if(save.data) saveRDS(res8, file=here(paste0("results/individual_estimates/",i,"_pipedSan_",adj,".rds")))
  

  
  
  fullres <- bind_rows(fullres, res1, res2, res3, res4, res5, res6, res7, res8, res9)
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
  
  set.seed(12345)
  
  for(i in outcomes){
    res1 <- res2 <- res3 <- res4 <- res5 <- res6 <- res7 <- NULL
    print(i)
    res1 <- d %>% group_by(country) %>%
      do(mics_regression(d=.,
                                     Y =i,
                                     X="EC_risk_H",
                                     W=Wvars,
                                     weight = "ecpopweight_H",
                                     clustid= "clust_num",
                                     family=family, calc_PAF=PAF, low_risk_level="1"))
    #saveRDS(res1, file=here(paste0("results/individual_estimates/",i,"_EC_H_",adj,".rds")))
    
    res2 <- d %>% group_by(country) %>%
      do(mics_regression(d=.,
                                     Y =i,
                                     X="EC_risk_S",
                                     W=Wvars,
                                     weight = "ecpopweight_S",
                                     clustid= "clust_num",
                                     family=family, calc_PAF=PAF, low_risk_level="1"))
    #saveRDS(res2, file=here(paste0("results/individual_estimates/",i,"_EC_S_",adj,".rds")))
    
    res3 <- d %>% group_by(country) %>%
      do(mics_regression(d=.,
                                     Y =i,
                                     X="san_imp_cat",
                                     W=Wvars,
                                     weight = "popweight",
                                     clustid= "clust_num",
                                     family=family, calc_PAF=PAF, low_risk_level="High coverage"))
    #saveRDS(res3, file=here(paste0("results/individual_estimates/",i,"_san_imp_",adj,".rds")))
    
    res4 <- d %>% group_by(country) %>%
      do(mics_regression(d=.,
                                     Y =i,
                                     X="wat_imp_cat",
                                     W=Wvars,
                                     weight = "popweight",
                                     clustid= "clust_num",
                                     family=family, calc_PAF=PAF, low_risk_level="Basic"))
    #saveRDS(res4, file=here(paste0("results/individual_estimates/",i,"_wat_imp_",adj,".rds")))
    
    res5 <- d %>% group_by(country) %>%
      do(mics_regression(d=.,
                                     Y =i,
                                     X="hyg_imp_cat",
                                     W=Wvars,
                                     weight = "popweight",
                                     clustid= "clust_num",
                                     family=family, calc_PAF=PAF, low_risk_level="Basic"))
    #saveRDS(res5, file=here(paste0("results/individual_estimates/",i,"_hyg_imp_",adj,".rds")))
    
    res6 <- d %>% group_by(country) %>%
      do(mics_regression(d=.,
                         Y =i,
                         X="san_imp_cat2",
                         W=Wvars,
                         weight = "popweight",
                         clustid= "clust_num",
                         family=family, calc_PAF=PAF, low_risk_level="Safely managed"))
    
    fullres <- bind_rows(fullres, res1, res2, res3, res4, res5, res6)
  }
  
  return(fullres) 
}

run_mics_multinomial_regressions_sens <- function(outcomes, family, PAF, Wvars){
  
  adj <- ifelse(is.null(Wvars),"unadj","adj")
  
  fullres <- NULL 
  
  
  for(i in outcomes){
    res1 <- res2 <- res3 <- res4 <- res5 <- NULL
    print(i)
  
    
    res3 <- d %>% group_by(country) %>%
      do(mics_regression(d=.,
                         Y =i,
                         X="san_imp_cat",
                         W=Wvars,
                         weight = "popweight",
                         clustid= "clust_num",
                         family=family, calc_PAF=PAF, low_risk_level="High coverage"))
    #saveRDS(res3, file=here(paste0("results/individual_estimates/",i,"_san_imp_",adj,".rds")))
    
    res4 <- d %>% group_by(country) %>%
      do(mics_regression(d=.,
                         Y =i,
                         X="wat_imp_cat",
                         W=Wvars,
                         weight = "popweight",
                         clustid= "clust_num",
                         family=family, calc_PAF=PAF, low_risk_level="Basic"))
    #saveRDS(res4, file=here(paste0("results/individual_estimates/",i,"_wat_imp_",adj,".rds")))
   
    fullres <- bind_rows(fullres, res1, res2, res3, res4, res5)
  }
  
  return(fullres) 
}



run_mics_multinomial_regressions_old <- function(outcomes, family, PAF, Wvars){
  
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
    
    # res3 <- d %>% group_by(country) %>%
    #   do(mics_tmle(d=.,
    #                Y =i,
    #                X="san_imp",
    #                W=Wvars,
    #                glm=glm,
    #                weight = "popweight",
    #                clustid= "clust_num",
    #                family=family))
    # #saveRDS(res3, file=here(paste0("results/individual_estimates/",i,"_san_imp_",adj,".rds")))
    # 
    # res4 <- d %>% group_by(country) %>%
    #   do(mics_tmle(d=.,
    #                Y =i,
    #                X="wat_imp",
    #                W=Wvars,
    #                glm=glm,
    #                weight = "popweight",
    #                clustid= "clust_num",
    #                family=family))
    # #saveRDS(res4, file=here(paste0("results/individual_estimates/",i,"_wat_imp_",adj,".rds")))
    # 
    # res5 <- d %>% group_by(country) %>%
    #   do(mics_tmle(d=.,
    #                Y =i,
    #                X="hyg_imp",
    #                W=Wvars,
    #                glm=glm,
    #                weight = "popweight",
    #                clustid= "clust_num",
    #                family=family))
    # #saveRDS(res5, file=here(paste0("results/individual_estimates/",i,"_hyg_imp_",adj,".rds")))
    # 
    # res6 <- d %>% group_by(country) %>%
    #   do(mics_tmle(d=.,
    #                Y =i,
    #                X="safely_manH20",
    #                W=Wvars,
    #                glm=glm,
    #                weight = "ecpopweight_H",
    #                clustid= "clust_num",
    #                family=family))
    # #saveRDS(res6, file=here(paste0("results/individual_estimates/",i,"_safely_manH20_",adj,".rds")))
    # 
    # res7 <- d %>% group_by(country) %>%
    #   do(mics_tmle(d=.,
    #                Y =i,
    #                X="WASH",
    #                W=Wvars,
    #                glm=glm,
    #                weight = "ecpopweight_H",
    #                clustid= "clust_num",
    #                family=family))
    # #saveRDS(res7, file=here(paste0("results/individual_estimates/",i,"_WASH_",adj,".rds")))
    # 
    # res8 <- d %>% group_by(country) %>%
    #   do(mics_tmle(d=.,
    #                Y =i,
    #                X="WASH_noEC",
    #                W=Wvars,
    #                glm=glm,
    #                weight = "popweight",
    #                clustid= "clust_num",
    #                family=family))
    # #saveRDS(res7, file=here(paste0("results/individual_estimates/",i,"_WASH_",adj,".rds")))
    
    
    
    
    fullres <- bind_rows(fullres, res1, res2, res3, res4, res5, res6, res7, res8)
  }
  
  return(fullres) 
}










run_MICS_regressions_secondary <- function(outcomes, family, PAF, Wvars){
  
  adj <- ifelse(is.null(Wvars),"unadj","adj")
  
  fullres <- NULL 
  
  set.seed(12345)
  
  for(i in outcomes){
    res1 <- res2 <- res3 <- res4 <- res5 <- res6 <- res7 <- NULL
    print(i)
    res1 <- d %>% group_by(country) %>%
      do(mics_regression(d=.,
                         Y =i,
                         X="Piped_san_cat",
                         W=Wvars,
                         weight = "popweight",
                         clustid= "clust_num",
                         family=family, calc_PAF=PAF, low_risk_level="Sewered"))
    
    res2 <- d %>% group_by(country) %>%
      do(mics_regression(d=.,
                         Y =i,
                         X="san_coverage",
                         W=Wvars,
                         weight = "popweight",
                         clustid= "clust_num",
                         family=family, calc_PAF=PAF, low_risk_level="High coverage"))
    
    res3 <- d %>% group_by(country) %>%
      do(mics_regression(d=.,
                         Y =i,
                         X="imp_off_prem_V_unimp",
                         W=Wvars,
                         weight = "popweight",
                         clustid= "clust_num",
                         family=family, calc_PAF=PAF, low_risk_level="Improved, off premise"))
    
    res4 <- d %>% group_by(country) %>%
      do(mics_regression(d=.,
                         Y =i,
                         X="imp_on_prem_V_imp_off_prem",
                         W=Wvars,
                         weight = "popweight",
                         clustid= "clust_num",
                         family=family, calc_PAF=PAF, low_risk_level="Improved, on premise"))
    
    res5 <- d %>% group_by(country) %>%
      do(mics_regression(d=.,
                         Y =i,
                         X="imp_on_prem_HQ_V_imp_on_prem_LQ",
                         W=Wvars,
                         weight = "ecpopweight_H",
                         clustid= "clust_num",
                         family=family, calc_PAF=PAF, low_risk_level="Improved, on premise, uncontaminated"))
    
    res6 <- d %>% group_by(country) %>%
      do(mics_regression(d=.,
                         Y =i,
                         X="imp_on_prem_sufficient_V_imp_on_prem_insufficient",
                         W=Wvars,
                         weight = "popweight",
                         clustid= "clust_num",
                         family=family, calc_PAF=PAF, low_risk_level="Improved, on premise, sufficient"))
    
    res7 <- d %>% group_by(country) %>%
      do(mics_regression(d=.,
                         Y =i,
                         X="imp_on_prem_sufficient_HQ_V_imp_on_prem_insufficient_LQ",
                         W=Wvars,
                         weight = "ecpopweight_H",
                         clustid= "clust_num",
                         family=family, calc_PAF=PAF, low_risk_level="Improved, on premise, HQ, sufficient"))
    
    res8 <- d %>% group_by(country) %>%
      do(mics_regression(d=.,
                         Y =i,
                         X="imp_on_prem_sufficient_HQ_V_imp_on_prem_insufficient_HQ",
                         W=Wvars,
                         weight = "ecpopweight_H",
                         clustid= "clust_num",
                         family=family, calc_PAF=PAF, low_risk_level="Improved, on premise, HQ, sufficient"))

    res9 <- d %>% group_by(country) %>%
      do(mics_regression(d=.,
                         Y =i,
                         X="imp_on_prem_sufficient_LQ_V_imp_on_prem_insufficient_LQ",
                         W=Wvars,
                         weight = "ecpopweight_H",
                         clustid= "clust_num",
                         family=family, calc_PAF=PAF, low_risk_level="Improved, on premise, LQ, sufficient"))
    
    
    
    fullres <- bind_rows(fullres, res1, res2, res3, res4, res5, res6, res7, res8, res9)
  }
  
  return(fullres) 
}




run_MICS_regressions_POU <- function(outcomes, family, PAF, Wvars){
  
  adj <- ifelse(is.null(Wvars),"unadj","adj")
  
  fullres <- NULL 
  
  set.seed(12345)
  
  for(i in outcomes){
    res1 <- res2 <- res3 <- res4 <- NULL
    print(i)
    res1 <- d %>% group_by(country) %>%
      do(mics_regression(d=.,
                         Y =i,
                         X="POU_anytreatment",
                         W=Wvars,
                         weight = "popweight",
                         clustid= "clust_num",
                         family=family, calc_PAF=PAF, low_risk_level="Treated"))
    
    res2 <- d %>% group_by(country) %>%
      do(mics_regression(d=.,
                         Y =i,
                         X="POU_chlorine",
                         W=Wvars,
                         weight = "popweight",
                         clustid= "clust_num",
                         family=family, calc_PAF=PAF, low_risk_level="Treated"))
    
    res3 <- d %>% group_by(country) %>%
      do(mics_regression(d=.,
                         Y =i,
                         X="POU_filter",
                         W=Wvars,
                         weight = "popweight",
                         clustid= "clust_num",
                         family=family, calc_PAF=PAF, low_risk_level="Treated"))
    
    res4 <- d %>% group_by(country) %>%
      do(mics_regression(d=.,
                         Y =i,
                         X="POU_solar",
                         W=Wvars,
                         weight = "popweight",
                         clustid= "clust_num",
                         family=family, calc_PAF=PAF, low_risk_level="Treated"))
    
    fullres <- bind_rows(fullres, res1, res2, res3, res4)
  }
  
  return(fullres) 
}






