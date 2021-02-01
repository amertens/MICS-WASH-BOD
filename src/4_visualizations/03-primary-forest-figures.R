
source("0-config.R")

d <- readRDS(here("results/pooled_results.rds"))



unique(d$Y)
unique(d$X)
table(d$X, d$Y)

unique(d$analysis)


i=unique(d$Y)[1]
j=unique(d$X)[9]

plist <- list()

#Add pooled FE to multinomial...

for(i in unique(d$Y)){
  for(j in unique(d$X)){
   df <- d %>% filter(adjusted==1, analysis %in% c("primary","primary-multi","FE"), Y==i, X==j) %>% 
      droplevels(.) 
   
   if(nrow(df)>0){
   
   Xlabel <- df$Xlab[1]  
   reference <- df$ref[1]  
   
   if(df$binary[1]==1){
    p <-  ggplot(df, aes(y=est, x=countrylab, color=region)) +
      geom_point() + 
      geom_linerange(aes(ymin=ci.lb, ymax=ci.ub )) +
      scale_color_manual(values=tableau11[c(1, 8:2,9,10,11)]) +
      geom_hline(yintercept = 1) +
      geom_vline(xintercept = 2.5, linetype="dashed") +
      scale_y_continuous(breaks=c(0.25, 0.5,1, 2, 4, 8), trans='log10', labels=scaleFUN) +
      coord_flip() + 
      xlab("Country") + ylab(paste0("Relative Risk (ref=",reference,")"))
   }else{
     p <-  ggplot(df, aes(y=est, x=countrylab, color=region)) +
       geom_point() + 
       geom_linerange(aes(ymin=ci.lb, ymax=ci.ub )) +
       scale_color_manual(values=tableau11[c(1, 8:2,9,10,11)]) +
       geom_hline(yintercept = 1) +
       geom_vline(xintercept = 2.5, linetype="dashed") +
       coord_flip() +
       xlab("Country") + ylab(paste0("Mean difference (ref=",reference,")"))  
   }
   
   if(df$multinomial[1]==1){
     p <- p + facet_wrap(~contrast, scales="free") + 
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

plist$Mortality.WASH

p_prim_forest_diar <- d %>% filter(adjusted==1, binary==1, analysis %in% c("primary","FE"), Y=="Diarrhea", exposure_type=="HH") %>% 
  droplevels(.) %>%
  ggplot(., aes(y=est, x=countrylab, color=region)) +
  facet_wrap(~Xlab, ncol=1, scales="free") +
  geom_point() + 
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub )) +
  scale_color_manual(values=tableau11) +
  geom_hline(yintercept = 1) +
  geom_vline(xintercept = 2.5, linetype="dashed") +
  scale_y_continuous(breaks=c(0.25, 0.5,1, 2, 4, 8), trans='log10', labels=scaleFUN) +
  coord_flip() +
  xlab("Country") + ylab("Relative Risk")

p_prim_forest_mort <- d %>% filter( analysis=="mortality") %>% 
  droplevels(.) %>%
  ggplot(., aes(y=est, x=country, color=country)) +
  facet_wrap(~X) +
  geom_point() + 
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub )) +
  scale_color_manual(values=tableau11[1:4]) +
  geom_hline(yintercept = 1) +
  scale_y_continuous(breaks=c(0.25, 0.5,1, 2, 4, 8), trans='log10', labels=scaleFUN) +
  coord_flip() +
  xlab("Country") + ylab("Relative Risk")


p_prim_forest_secondary_outcomes <- d %>% filter(adjusted==1, binary==1, analysis=="primary", Y!="Diarrhea") %>% 
  droplevels(.) %>%
  ggplot(., aes(y=est, x=country, color=country)) +
  facet_grid(Y~X, switch = "y") +
  geom_point() + 
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub )) +
  scale_color_manual(values=tableau11[1:4]) +
  geom_hline(yintercept = 1) +
  scale_y_continuous(breaks=c(0.25, 0.5,1, 2, 4, 8), trans='log10', labels=scaleFUN) +
  coord_flip() +
  xlab("Country") + ylab("Relative Risk") +
  theme(strip.background = element_blank(),
        axis.text.y = element_text(size=8, hjust = 1),
        strip.text.x = element_text(size=8, face = "bold"),
        strip.text.y = element_text(size=8, angle = 180, face = "bold"),
        strip.placement = "outside",
        axis.text.x = element_text(size=10, vjust = 0.5),
        legend.box.background = element_rect(colour = "black"), 
        title = element_text(margin=margin(0,0,-10,0)))




#-------------------------------------------------------------
# RR's multinomial
#-------------------------------------------------------------

#Note: make the facet labels on the left sife and add the reference to the facet labels
#Make sure changes to put low-risk level as reference


p_multi_forest_diar <- d %>% filter(adjusted==1, binary==1, analysis=="primary-multi",  multinomial==1, Y=="Diarrhea") %>% 
  droplevels(.) %>%
  mutate(X=factor(X, levels = rev(levels(X)))) %>%
  arrange(X) %>%
  mutate(Xref=paste0(X,"\n(Ref.: ",ref,")"),
         Xref=factor(Xref, levels = unique(Xref))) %>%
  ggplot(., aes(y=est, x=contrast, group=country, color=country)) +
  #facet_grid(Xref~Y, scale="free_y", switch = "y") +
  facet_wrap(~Xref, scale="free_y") +
  geom_point(position = position_dodge(0.6)) + 
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub ), position = position_dodge(0.6)) +
  scale_color_manual(values=tableau11) +
  geom_hline(yintercept = 1) +
  scale_y_continuous(breaks=c(0.25, 0.5,1, 2, 4, 8), trans='log10', labels=scaleFUN) +
  coord_flip() +
  xlab("") + ylab("Relative Risk") +
  theme(strip.background = element_blank(),
        legend.position="bottom",
        legend.title = element_blank(),
        axis.text.y = element_text(size=8, hjust = 1),
        strip.text.x = element_text(size=8, face = "bold"),
        strip.text.y = element_text(size=8, angle = 180, face = "bold"),
        # strip.placement = "outside",
         axis.text.x = element_text(size=10, vjust = 0.5),
        # #panel.spacing = unit(0, "lines"),
        #legend.box.background = element_rect(colour = "black"), 
        title = element_text(margin=margin(0,0,-10,0)))



p_multi_forest_secondary_outcomes <- d %>% filter(adjusted==1, binary==1, analysis=="primary-multi", Y!="Diarrhea",  multinomial==1) %>% 
  droplevels(.) %>%
  mutate(X=factor(X, levels = rev(levels(X)))) %>%
  arrange(X) %>%
  mutate(Xref=paste0(X,"\n(Ref.: ",ref,")"),
         Xref=factor(Xref, levels = unique(Xref))) %>%
  ggplot(., aes(y=est, x=contrast, group=country, color=country)) +
  facet_grid(Xref~Y, scale="free_y", switch = "y") +
  geom_point(position = position_dodge(0.6)) + 
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub ), position = position_dodge(0.6)) +
  scale_color_manual(values=tableau11) +
  geom_hline(yintercept = 1) +
  scale_y_continuous(breaks=c(0.25, 0.5,1, 2, 4, 8), trans='log10', labels=scaleFUN) +
  coord_flip() +
  xlab("") + ylab("Relative Risk") +
  theme(strip.background = element_blank(),
        legend.position="bottom",
        axis.text.y = element_text(size=8, hjust = 1),
        strip.text.x = element_text(size=8, face = "bold"),
        strip.text.y = element_text(size=8, angle = 180, face = "bold"),
        strip.placement = "outside",
        legend.title = element_blank(),
        axis.text.x = element_text(size=10, vjust = 0.5),
        #panel.spacing = unit(0, "lines"),
        #legend.box.background = element_rect(colour = "black"), 
        title = element_text(margin=margin(0,0,-10,0)))


#-------------------------------------------------------------
# Z-score differences
#-------------------------------------------------------------


p_prim_Zscore_forest <- d %>% filter(adjusted==1, binary==0, analysis=="primary") %>% 
  droplevels(.) %>%
  ggplot(., aes(y=est, x=country, color=country)) +
  facet_grid(Y~X, switch = "y") +
  geom_point() + 
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub )) +
  scale_color_manual(values=tableau11[1:4]) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  xlab("Country and outcome") + ylab("Z-score difference") +
  theme(strip.background = element_blank(),
        axis.text.y = element_text(size=8, hjust = 1),
        strip.text.x = element_text(size=8, face = "bold"),
        strip.text.y = element_text(size=8, angle = 180, face = "bold"),
        strip.placement = "outside",
        axis.text.x = element_text(size=10, vjust = 0.5),
        legend.box.background = element_rect(colour = "black"), 
        title = element_text(margin=margin(0,0,-10,0)))



#-------------------------------------------------------------
# Z-score differences - multinomial
#-------------------------------------------------------------



p_multi_Zscore_forest <- d %>% filter(adjusted==1, binary==0, analysis=="primary-multi", multinomial==1) %>% 
  droplevels(.) %>%
  mutate(X=factor(X, levels = rev(levels(X)))) %>%
  arrange(X) %>%
  mutate(Xref=paste0(X,"\n(Ref.: ",ref,")"),
         Xref=factor(Xref, levels = unique(Xref))) %>%
  ggplot(., aes(y=est, x=contrast, group=country, color=country)) +
  facet_grid(Xref~Y, scale="free_y", switch = "y") +
  geom_point(position = position_dodge(0.6)) + 
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub ), position = position_dodge(0.6)) +
  scale_color_manual(values=tableau11) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  xlab("") + ylab("Z-score difference") +
  theme(strip.background = element_blank(),
        legend.position="bottom",
        axis.text.y = element_text(size=8, hjust = 1),
        strip.text.x = element_text(size=8, face = "bold"),
        strip.text.y = element_text(size=8, angle = 180, face = "bold"),
        strip.placement = "outside",
        legend.title = element_blank(),
        axis.text.x = element_text(size=10, vjust = 0.5),
        #panel.spacing = unit(0, "lines"),
        #legend.box.background = element_rect(colour = "black"), 
        title = element_text(margin=margin(0,0,-10,0)))





#-------------------------------------------------------------
#-compare TMLE forest plots
#-------------------------------------------------------------

p_tmle_forest <- d %>% filter(adjusted==1, binary==1, analysis=="tmle") %>% 
  droplevels(.) %>%
  ggplot(., aes(y=est, x=country, color=country)) +
  facet_grid(Y~X) +
  geom_point() + 
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub )) +
  scale_color_manual(values=tableau11[1:4]) +
  geom_hline(yintercept = 1) +
  scale_y_continuous(breaks=c(0.25, 0.5,1, 2, 4, 8), trans='log10', labels=scaleFUN) +
  coord_flip() +
  xlab("Country") + ylab("Relative Risk")



#-------------------------------------------------------------
# save figures
#-------------------------------------------------------------



save(p_prim_pooled, p_prim_forest_diar, p_prim_forest_secondary_outcomes,
     p_multi_pooled, p_multi_forest_diar, p_multi_forest_secondary_outcomes,
     p_prim_Zscore_pooled, p_prim_Zscore_forest,
     p_multi_Zscore_pooled, p_multi_Zscore_forest,
     pPAF_pos, p_prim_forest_mort,
     p_unadj_comp_RR, p_unadj_comp_diff,
     p_tmle_comp_RR, p_tmle_glm_comp_RR, p_rural_pooled,
     p_1step_comp_RR, p_CC_comp_RR, p_FE_comp_RR, p_clustid_comp_RR,
     file=here("figures/figure_objects.Rdata"))





 


