
rm(list=ls())
source("0-config.R")

d <- readRDS(here("results/pooled_results.rds")) %>% 
   filter(!is.na(ci.lb),
          analysis %in% c("primary-multi", "primary", "secondary", "FE"))


levels(d$Y)
unique(d$X)
d$X <- factor(d$X, levels =c("EC_risk_H", "EC_risk_S", "wat_imp_cat", "san_imp_cat","hyg_imp_cat",
                             "EC_H","EC_S",  "wat_imp", "san_imp", "hyg_imp", "WASH", "safely_manH20", "WASH_noEC",
                             "piped_san" ,"san_imp_cat2",
                             "Piped_san_cat",
                             "san_coverage",
                             "imp_off_prem_V_unimp",     
                             "imp_on_prem_V_imp_off_prem",
                             "imp_on_prem_HQ_V_imp_on_prem_LQ",
                             "imp_on_prem_sufficient_V_imp_on_prem_insufficient",  
                             "imp_on_prem_sufficient_HQ_V_imp_on_prem_insufficient_LQ"))
levels(d$X)

i=levels(d$Y)[7]
j=levels(d$X)[6]
j="san_imp_cat"

plist <- list()

#Add pooled FE to multinomial...

for(i in levels(d$Y)){
  for(j in levels(d$X)){
   df <- d %>% filter(adjusted==1, analysis %in% c("primary","primary-multi","secondary","FE"), Y==i, X==j) %>% 
      droplevels(.) 
   
   if(nrow(df)>0){
   
   Xlabel <- df$Xlab2[1]  
   reference <- df$ref[1]  
   
   if(df$binary[1]==1){
    p <-  ggplot(df, aes(y=est, x=countrylab, color=region)) +
      geom_point(aes(shape=sig)) + 
      geom_linerange(aes(ymin=ci.lb, ymax=ci.ub )) +
      scale_color_manual(values=tableau11[c(1, 8:2,9,10,11)]) +
      geom_hline(yintercept = 1) +
      geom_vline(xintercept = 2.5, linetype="dashed") +
      scale_y_continuous(trans='log10') +
       scale_shape_manual(values=c(19,13), guide=FALSE) +
      coord_flip() + 
      xlab("") + ylab(paste0("Relative Risk (ref=",reference,")"))
   }else{
     p <-  ggplot(df, aes(y=est, x=countrylab, color=region)) +
       geom_point(aes(shape=sig)) + 
       geom_linerange(aes(ymin=ci.lb, ymax=ci.ub )) +
       scale_color_manual(values=tableau11[c(1, 8:2,9,10,11)]) +
        scale_shape_manual(values=c(19,13), guide=FALSE) +
       #scale_x_discrete(labels= levels(df$countrylab)) +
       geom_hline(yintercept = 0) +
       geom_vline(xintercept = 2.5, linetype="dashed") +
       coord_flip() +
       xlab("") + ylab(paste0("Mean difference (ref=",reference,")"))  
   }
   
   # if(df$multinomial[1]==1){
   #   p <- p + facet_wrap(~contrast, scales="fixed") + 
   #     ggtitle(paste0("Outcome: ",i, "\nExposure: ", Xlabel))
   # }else{
   #   p <- p + ggtitle(paste0("Outcome: ",i, "\nExposure: ", Xlabel))
   # }
   
   if(df$multinomial[1]==1){
      p <- p + facet_wrap(~contrast, scales="fixed") + 
         ggtitle(paste0("Exposure: ", Xlabel))
   }else{
      p <- p + ggtitle(paste0("Exposure: ", Xlabel))
   }
   
    plist[[length(plist)+1]] <- p
    names(plist)[length(plist)] <- paste0(i, ".",j)
   }
  } 
}
# 
# plist[[1]]
# names(plist)
# length(plist)
# 
 plist$Diarrhea.piped_san



#-------------------------------------------------------------
# save figures
#-------------------------------------------------------------

saveRDS(plist, file=here("figures/forest_figure_objects.rds"))






 


