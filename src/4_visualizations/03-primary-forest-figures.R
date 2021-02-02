
rm(list=ls())
source("0-config.R")

d <- readRDS(here("results/pooled_results.rds"))



unique(d$Y)
unique(d$X)
table(d$X, d$Y)

unique(d$analysis)


i=unique(d$Y)[1]
j=unique(d$X)[1]

plist <- list()

#Add pooled FE to multinomial...

for(i in unique(d$Y)){
  for(j in unique(d$X)){
   df <- d %>% filter(adjusted==1, analysis %in% c("primary","primary-multi","FE"), Y==i, X==j) %>% 
      droplevels(.) 
   
   if(nrow(df)>0){
   
   Xlabel <- df$Xlab2[1]  
   reference <- df$ref[1]  
   
   if(df$binary[1]==1){
    p <-  ggplot(df, aes(y=est, x=countrylab, color=region)) +
      geom_point() + 
      geom_linerange(aes(ymin=ci.lb, ymax=ci.ub )) +
      scale_color_manual(values=tableau11[c(1, 8:2,9,10,11)]) +
      geom_hline(yintercept = 1) +
      geom_vline(xintercept = 2.5, linetype="dashed") +
      scale_y_continuous(trans='log10') +
      #scale_x_discrete(labels= levels(df$countrylab)) +
      #scale_y_continuous(breaks=c(0.25, 0.5,1, 2, 4, 8), trans='log10', labels=scaleFUN) +
      coord_flip() + 
      xlab("") + ylab(paste0("Relative Risk (ref=",reference,")"))
   }else{
     p <-  ggplot(df, aes(y=est, x=countrylab, color=region)) +
       geom_point() + 
       geom_linerange(aes(ymin=ci.lb, ymax=ci.ub )) +
       scale_color_manual(values=tableau11[c(1, 8:2,9,10,11)]) +
       #scale_x_discrete(labels= levels(df$countrylab)) +
       geom_hline(yintercept = 0) +
       geom_vline(xintercept = 2.5, linetype="dashed") +
       coord_flip() +
       xlab("") + ylab(paste0("Mean difference (ref=",reference,")"))  
   }
   
   if(df$multinomial[1]==1){
     p <- p + facet_wrap(~contrast, scales="fixed") + 
       ggtitle(paste0("Outcome: ",i, "\nExposure: ", Xlabel))
   }else{
     p <- p + ggtitle(paste0("Outcome: ",i, "\nExposure: ", Xlabel))
   }
   
    plist[[length(plist)+1]] <- p
    names(plist)[length(plist)] <- paste0(i, ".",j)
   }
  } 
}

plist[[1]]
names(plist)
length(plist)

plist$WHZ.san_imp_cat



#-------------------------------------------------------------
# save figures
#-------------------------------------------------------------

saveRDS(plist, file=here("figures/forest_figure_objects.rds"))





 


