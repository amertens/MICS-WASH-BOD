
rm(list=ls())
source("0-config.R")

d <- readRDS(here("results/pooled_results.rds")) %>% filter(!is.na(region), region!="pooled") %>%
  rename(Region=region) %>%
  mutate(Region = factor(Region, levels=rev(c("WCA", "ESA", "MENA", "SA", "EAP", "LAC", "ECA"))))


# set colors
dd.col <- rev(tableau10[1:7])
names(dd.col)  <- levels(d$Region)


#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Regional figures
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

# East Asia and the Pacific EAP
# Eastern and Southern Africa ESA
# Europe and Central Asia ECA
# Latin America and Caribbean LAC
# Middle East and North Africa MENA
# South Asia SA
# West and Central Africa WCA

#only includes estimates from at least 2 countries

#-------------------------------------------------------------
# RR's single increase
#-------------------------------------------------------------


p_Region_prim_pooled_HH <- d %>% filter(ref!=contrast, adjusted==1, binary==1, analysis=="region", country=="Pooled - RE", exposure_type=="HH") %>% 
  droplevels(.) %>%
  ggplot(., aes(y=est, x=Xlab, group=Region, color=Region)) +
  facet_grid(~Y) +
  geom_point(position=position_dodge(0.5)) + 
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub), position=position_dodge(0.5)) +
  scale_color_manual(values=dd.col) +
  geom_hline(yintercept = 1) +
  scale_y_continuous(breaks=c(0.5, 0.7, 1,1.4, 2, 3, 4), trans='log10') +
  #scale_y_continuous(trans='log10') +
  coord_flip(ylim=c(0.5, 2.5)) +
  xlab("WASH Characteristic") + ylab("Relative Risk (ref: Improved)") + 
  theme(legend.position = "bottom") + guides(colour = guide_legend(reverse = TRUE, nrow = 1))
p_Region_prim_pooled_HH


p_Region_prim_pooled_WQ <- d %>% filter(ref!=contrast, adjusted==1, binary==1, analysis=="region", country=="Pooled - RE", exposure_type=="WQ") %>% 
  droplevels(.) %>%
  #mutate(=factor(X, levels = rev(levels(X)))) %>%
  ggplot(., aes(y=est, x=Xlab, group=Region, color=Region)) +
  facet_grid(~Y) +
  geom_point(position=position_dodge(0.5)) + 
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub), position=position_dodge(0.5)) +
  scale_color_manual(values=dd.col) +
  geom_hline(yintercept = 1) +
  scale_y_continuous(breaks=c(0.5, 0.7, 1,1.4, 2, 3, 4, 6), trans='log10') +
  coord_flip(ylim=c(0.5, 6)) +
  xlab("WASH Characteristic") + ylab("Relative Risk (ref: Improved)")+ 
  theme(legend.position = "bottom") + guides(colour = guide_legend(reverse = TRUE, nrow = 1))
p_Region_prim_pooled_WQ







#-------------------------------------------------------------
# RR's multinomial
#-------------------------------------------------------------

#Note: make the facet labels on the left sife and add the reference to the facet labels
#Make sure changes to put low-risk level as reference

p_Region_multi_pooled_HH <- d %>% filter(adjusted==1, binary==1, analysis=="region-multi", country=="Pooled - RE", exposure_type=="HH") %>% 
  droplevels(.) %>%
  arrange(Xlab) %>%
  ggplot(., aes(y=est, x=contrast, group=Region, color=Region)) +
  facet_grid(Xlab~Y, scale="free_y", switch = "y") +
  geom_point(position=position_dodge(0.5)) + 
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub), position=position_dodge(0.5)) +
  geom_text(aes(label=reflab), nudge_y=.2, size = 3) +
  geom_hline(yintercept = 1) +
  scale_y_continuous(breaks=c(0.25, 0.5,1, 2, 4, 8), trans='log10') +
  coord_flip(ylim=c(0.25, 3.5)) +
  scale_color_manual(values=dd.col) +
  xlab("") + ylab("Relative Risk")+
  theme(strip.background = element_blank(),
        axis.text.y = element_text(size=8, hjust = 1),
        strip.text.x = element_text(size=8, face = "bold"),
        strip.text.y = element_text(size=8, angle = 180, face = "bold"),
        strip.placement = "outside",
        axis.text.x = element_text(size=10, vjust = 0.5),
        legend.box.background = element_rect(colour = "black"), 
        title = element_text(margin=margin(0,0,-10,0)))+ 
  theme(legend.position = "bottom") + guides(colour = guide_legend(reverse = TRUE, nrow = 1))

p_Region_multi_pooled_WQ <- d %>% filter(adjusted==1, binary==1, analysis=="region-multi", country=="Pooled - RE", exposure_type=="WQ") %>% 
  droplevels(.) %>%
  arrange(Xlab) %>%
  ggplot(., aes(y=est, x=contrast, group=Region, color=Region)) +
  facet_grid(Xlab~Y, scale="free_y", switch = "y") +
  geom_point(position=position_dodge(0.5)) + 
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub), position=position_dodge(0.5)) +
  geom_text(aes(label=reflab), nudge_y=.2, size = 3) +
  geom_hline(yintercept = 1) +
  scale_y_continuous(breaks=c(0.25, 0.5,1, 2, 4, 8), trans='log10') +
  coord_flip(ylim=c(0.4, 3)) +
  scale_color_manual(values=dd.col) +
  xlab("") + ylab("Relative Risk")+
  theme(strip.background = element_blank(),
        axis.text.y = element_text(size=8, hjust = 1),
        strip.text.x = element_text(size=8, face = "bold"),
        strip.text.y = element_text(size=8, angle = 180, face = "bold"),
        strip.placement = "outside",
        axis.text.x = element_text(size=10, vjust = 0.5),
        legend.box.background = element_rect(colour = "black"), 
        title = element_text(margin=margin(0,0,-10,0)))+ 
  theme(legend.position = "bottom") + guides(colour = guide_legend(reverse = TRUE, nrow = 1))


#-------------------------------------------------------------
# Z-score differences
#-------------------------------------------------------------
p_Region_prim_Zscore_pooled_HH <- d %>% filter(ref!=contrast, adjusted==1, binary==0, analysis=="region", country=="Pooled - RE", exposure_type=="HH") %>% 
  droplevels(.) %>%
  ggplot(., aes(y=est, x=Xlab, group=Region, color=Region)) +
  facet_grid(~Y) +
  geom_point(position=position_dodge(0.5)) + 
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub), position=position_dodge(0.5)) +
  geom_hline(yintercept = 0) +
  coord_flip(ylim=c(-0.5, 0.25)) +
  scale_color_manual(values=dd.col) +
  xlab("WASH Characteristic") + ylab("Z-score difference (ref: Improved)")+ 
  theme(legend.position = "bottom") + guides(colour = guide_legend(reverse = TRUE, nrow = 1))

p_Region_prim_Zscore_pooled_WQ <- d %>% filter(ref!=contrast, adjusted==1, binary==0, analysis=="region", country=="Pooled - RE", exposure_type=="WQ") %>% 
  droplevels(.) %>%
  ggplot(., aes(y=est, x=Xlab, group=Region, color=Region)) +
  facet_grid(~Y) +
  geom_point(position=position_dodge(0.5)) + 
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub), position=position_dodge(0.5)) +
  geom_hline(yintercept = 0) +
  coord_flip(ylim=c(-1, 0.25)) +
  scale_color_manual(values=dd.col) +
  xlab("WASH Characteristic") + ylab("Z-score difference (ref: Improved)")+ 
  theme(legend.position = "bottom") + guides(colour = guide_legend(reverse = TRUE, nrow = 1))





#-------------------------------------------------------------
# Z-score differences - multinomial
#-------------------------------------------------------------

#missing categories- where is WASH and safely managed, etc.

p_Region_multi_Zscore_pooled_HH <- d %>% filter(adjusted==1, binary==0, analysis=="region-multi", country=="Pooled - RE", multinomial==1, exposure_type=="HH") %>% 
  droplevels(.) %>%
  arrange(Xlab) %>%
  ggplot(., aes(y=est, x=contrast, group=Region, color=Region)) +
  facet_grid(Xlab~Y, scale="free_y", switch = "y") +
  geom_point(position=position_dodge(0.5)) + 
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub), position=position_dodge(0.5)) +
  geom_text(aes(label=reflab), nudge_y=.1, size = 3) +
  geom_hline(yintercept = 0) +
  coord_flip(ylim=c(-1, 0.75)) +
  xlab("") + ylab("Z-score difference")+
  scale_color_manual(values=dd.col) +
  theme(strip.background = element_blank(),
        axis.text.y = element_text(size=8, hjust = 1),
        strip.text.x = element_text(size=8, face = "bold"),
        strip.text.y = element_text(size=8, angle = 180, face = "bold"),
        strip.placement = "outside",
        axis.text.x = element_text(size=10, vjust = 0.5),
        legend.box.background = element_rect(colour = "black"), 
        title = element_text(margin=margin(0,0,-10,0)))+ 
  theme(legend.position = "bottom") + guides(colour = guide_legend(reverse = TRUE, nrow = 1))

p_Region_multi_Zscore_pooled_WQ <- d %>% filter(adjusted==1, binary==0, analysis=="region-multi", country=="Pooled - RE", multinomial==1, exposure_type=="WQ") %>% 
  droplevels(.) %>%
  arrange(Xlab) %>%
  ggplot(., aes(y=est, x=contrast, group=Region, color=Region)) +
  facet_grid(Xlab~Y, scale="free_y", switch = "y") +
  geom_point(position=position_dodge(0.5)) + 
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub), position=position_dodge(0.5)) +
  geom_text(aes(label=reflab), nudge_y=.05, size = 3) +
  geom_hline(yintercept = 0) +
  coord_flip(ylim=c(-0.5, 0.5)) +
  scale_color_manual(values=dd.col) +
  xlab("") + ylab("Z-score difference")+
  theme(strip.background = element_blank(),
        axis.text.y = element_text(size=8, hjust = 1),
        strip.text.x = element_text(size=8, face = "bold"),
        strip.text.y = element_text(size=8, angle = 180, face = "bold"),
        strip.placement = "outside",
        axis.text.x = element_text(size=10, vjust = 0.5),
        legend.box.background = element_rect(colour = "black"), 
        title = element_text(margin=margin(0,0,-10,0)))+ 
  theme(legend.position = "bottom") + guides(colour = guide_legend(reverse = TRUE, nrow = 1))


#-------------------------------------------------------------
# save figures
#-------------------------------------------------------------

save(list = ls(pattern="p_Region_"), file=here("figures/Region_figure_objects.Rdata"))

ls(pattern="p_Region_")



 


