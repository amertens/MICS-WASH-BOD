
source("0-config.R")

d <- readRDS(here("results/pooled_results.rds"))

#fix so unimproved in Y axis and (ref: Improved) in X-axis

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Primary figures
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


#-------------------------------------------------------------
# RR's single increase
#-------------------------------------------------------------


#To do: make labeling of Y axis clearer, add in mortality
p_prim_pooled_HH <- d %>% filter(ref!=contrast, adjusted==1, binary==1, analysis=="primary", country=="Pooled - RE", exposure_type=="HH") %>% 
  droplevels(.) %>%
  #mutate(=factor(X, levels = rev(levels(X)))) %>%
  ggplot(., aes(y=est, x=Xlab),color="black") +
  facet_grid(~Y) +
  geom_point() + 
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub )) +
  #scale_color_manual(values=tableau10) +
  geom_hline(yintercept = 1) +
  #scale_y_continuous(breaks=c(0.25, 0.5,1, 1.1, 1.5, 2, 4, 8), trans='log10', labels=scaleFUN) +
  scale_y_continuous(trans='log10') +
  coord_flip() +
  xlab("WASH Characteristic") + ylab("Relative Risk (ref: Improved)")
p_prim_pooled_HH


p_prim_pooled_WQ <- d %>% filter(ref!=contrast, adjusted==1, binary==1, analysis=="primary", country=="Pooled - RE", exposure_type=="WQ") %>% 
  droplevels(.) %>%
  #mutate(=factor(X, levels = rev(levels(X)))) %>%
  ggplot(., aes(y=est, x=Xlab),color="black") +
  facet_grid(~Y) +
  geom_point() + 
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub )) +
  #scale_color_manual(values=tableau10) +
  geom_hline(yintercept = 1) +
  #scale_y_continuous(breaks=c(0.25, 0.5,1, 1.1, 1.5, 2, 4, 8), trans='log10', labels=scaleFUN) +
  scale_y_continuous(trans='log10') +
  coord_flip() +
  xlab("WASH Characteristic") + ylab("Relative Risk (ref: Improved)")
p_prim_pooled_WQ







#-------------------------------------------------------------
# RR's multinomial
#-------------------------------------------------------------

#Note: make the facet labels on the left sife and add the reference to the facet labels
#Make sure changes to put low-risk level as reference

p_multi_pooled_HH <- d %>% filter(adjusted==1, binary==1, analysis=="primary-multi", country=="Pooled - RE", exposure_type=="HH") %>% 
  droplevels(.) %>%
  arrange(Xlab) %>%
  ggplot(., aes(y=est, x=contrast),color="black") +
  facet_grid(Xlab~Y, scale="free_y", switch = "y") +
  geom_point() + 
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub )) +
  geom_text(aes(label=reflab), nudge_y=.2, size = 3) +
  geom_hline(yintercept = 1) +
  scale_y_continuous(breaks=c(0.25, 0.5,1, 2, 4, 8), trans='log10', labels=scaleFUN) +
  coord_flip() +
  xlab("") + ylab("Relative Risk")+
  theme(strip.background = element_blank(),
        axis.text.y = element_text(size=8, hjust = 1),
        strip.text.x = element_text(size=8, face = "bold"),
        strip.text.y = element_text(size=8, angle = 180, face = "bold"),
        strip.placement = "outside",
        axis.text.x = element_text(size=10, vjust = 0.5),
        legend.box.background = element_rect(colour = "black"), 
        title = element_text(margin=margin(0,0,-10,0)))

p_multi_pooled_WQ <- d %>% filter(adjusted==1, binary==1, analysis=="primary-multi", country=="Pooled - RE", exposure_type=="WQ") %>% 
  droplevels(.) %>%
  arrange(Xlab) %>%
  ggplot(., aes(y=est, x=contrast),color="black") +
  facet_grid(Xlab~Y, scale="free_y", switch = "y") +
  geom_point() + 
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub )) +
  geom_text(aes(label=reflab), nudge_y=.2, size = 3) +
  geom_hline(yintercept = 1) +
  scale_y_continuous(breaks=c(0.25, 0.5,1, 2, 4, 8), trans='log10', labels=scaleFUN) +
  coord_flip() +
  xlab("") + ylab("Relative Risk")+
  theme(strip.background = element_blank(),
        axis.text.y = element_text(size=8, hjust = 1),
        strip.text.x = element_text(size=8, face = "bold"),
        strip.text.y = element_text(size=8, angle = 180, face = "bold"),
        strip.placement = "outside",
        axis.text.x = element_text(size=10, vjust = 0.5),
        legend.box.background = element_rect(colour = "black"), 
        title = element_text(margin=margin(0,0,-10,0)))


#-------------------------------------------------------------
# Z-score differences
#-------------------------------------------------------------
p_prim_Zscore_pooled_HH <- d %>% filter(ref!=contrast, adjusted==1, binary==0, analysis=="primary", country=="Pooled - RE", exposure_type=="HH") %>% 
  droplevels(.) %>%
  ggplot(., aes(y=est, x=Xlab),color="black") +
  facet_grid(~Y) +
  geom_point() + 
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub )) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  xlab("WASH Characteristic") + ylab("Z-score difference (ref: Improved)")

p_prim_Zscore_pooled_WQ <- d %>% filter(ref!=contrast, adjusted==1, binary==0, analysis=="primary", country=="Pooled - RE", exposure_type=="WQ") %>% 
  droplevels(.) %>%
  ggplot(., aes(y=est, x=Xlab),color="black") +
  facet_grid(~Y) +
  geom_point() + 
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub )) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  xlab("WASH Characteristic") + ylab("Z-score difference (ref: Improved)")





#-------------------------------------------------------------
# Z-score differences - multinomial
#-------------------------------------------------------------

#missing categories- where is WASH and safely managed, etc.

p_multi_Zscore_pooled_HH <- d %>% filter(adjusted==1, binary==0, analysis=="primary-multi", country=="Pooled - RE", multinomial==1, exposure_type=="HH") %>% 
  droplevels(.) %>%
  arrange(Xlab) %>%
  ggplot(., aes(y=est, x=contrast),color="black") +
  facet_grid(Xlab~Y, scale="free_y", switch = "y") +
  geom_point() + 
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub )) +
  geom_text(aes(label=reflab), nudge_y=.1, size = 3) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  xlab("") + ylab("Z-score difference")+
  theme(strip.background = element_blank(),
        axis.text.y = element_text(size=8, hjust = 1),
        strip.text.x = element_text(size=8, face = "bold"),
        strip.text.y = element_text(size=8, angle = 180, face = "bold"),
        strip.placement = "outside",
        axis.text.x = element_text(size=10, vjust = 0.5),
        legend.box.background = element_rect(colour = "black"), 
        title = element_text(margin=margin(0,0,-10,0)))

p_multi_Zscore_pooled_WQ <- d %>% filter(adjusted==1, binary==0, analysis=="primary-multi", country=="Pooled - RE", multinomial==1, exposure_type=="WQ") %>% 
  droplevels(.) %>%
  arrange(Xlab) %>%
  ggplot(., aes(y=est, x=contrast),color="black") +
  facet_grid(Xlab~Y, scale="free_y", switch = "y") +
  geom_point() + 
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub )) +
  geom_text(aes(label=reflab), nudge_y=.05, size = 3) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  xlab("") + ylab("Z-score difference")+
  theme(strip.background = element_blank(),
        axis.text.y = element_text(size=8, hjust = 1),
        strip.text.x = element_text(size=8, face = "bold"),
        strip.text.y = element_text(size=8, angle = 180, face = "bold"),
        strip.placement = "outside",
        axis.text.x = element_text(size=10, vjust = 0.5),
        legend.box.background = element_rect(colour = "black"), 
        title = element_text(margin=margin(0,0,-10,0)))


#-------------------------------------------------------------
# save figures
#-------------------------------------------------------------

save(list = ls(pattern="p_"), file=here("figures/primary_figure_objects.Rdata"))





 


