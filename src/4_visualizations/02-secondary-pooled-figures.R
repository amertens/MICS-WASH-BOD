

rm(list=ls())
source("0-config.R")

d <- readRDS(here("results/pooled_results.rds"))

#fix so unimproved in Y axis and (ref: Improved) in X-axis

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# secondary figures
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

d %>% filter(X=="safely_manH20",ref!=contrast, adjusted==1, binary==1, analysis=="secondary", country=="Pooled - RE", exposure_type=="WQ") %>% 
  droplevels(.) 
df<- d %>% filter(X=="safely_manH20", binary==1, analysis=="secondary", country=="Pooled - RE", exposure_type=="WQ") %>% 
  droplevels(.) 
#-------------------------------------------------------------
# RR's single increase
#-------------------------------------------------------------

d <- d %>% filter(analysis=="secondary")
unique(d$X)

#Reorder factors
d <- d %>% mutate(
  contrast=factor(contrast, levels=rev(
    c("Piped","Non-piped","Safe","Safely managed","Unsafe","Improved","Uncontaminated",
      "Contaminated","Low risk","Moderate risk", "High risk",  "Very high risk",  
      "Surface water", "Unimproved",
        "Continuous",  "Basic", "Limited",  "No facility", "None", 
      "High coverage",
      "Basic, non-sewer","Sewered", 
      "Improved, off premise",
      "Improved, on premise, contaminated","Improved, on premise, insufficient",
      "Improved, on premise, LQ, insufficient", "Improved, on premise",
      "Improved, on premise, uncontaminated","Improved, on premise, sufficient",
      "Improved, on premise, HQ, sufficient"  ))))


p_sec_pooled <- d %>% filter(!(X %in% c("Piped_san_cat","san_coverage" )),ref!=contrast, adjusted==1, binary==1, analysis=="secondary", country=="Pooled - RE") %>% 
  droplevels(.) %>%
  #mutate(=factor(X, levels = rev(levels(X)))) %>%
  ggplot(., aes(y=est, x=Xlab),color="black") +
  facet_grid(~Y) +
  geom_point(aes(shape=sig), size=2) + 
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub )) +
  #scale_color_manual(values=c("black","blue")) +
  scale_shape_manual(values=c(19,13), guide=FALSE) +
  geom_hline(yintercept = 1) +
  #scale_y_continuous(breaks=c(0.25, 0.5,1, 1.1, 1.5, 2, 4, 8), trans='log10', labels=scaleFUN) +
  scale_y_continuous(trans='log10') +
  coord_flip() +
  xlab("Sanitation Characteristic") + ylab("Relative Risk (ref: Improved)")
p_sec_pooled


p_sec_multi_pooled <- d %>% filter(X %in% c("Piped_san_cat","san_coverage" ), adjusted==1, binary==1, analysis=="secondary", country=="Pooled - RE") %>% 
  droplevels(.) %>%
  arrange(Xlab) %>%
  ggplot(., aes(y=est, x=contrast),color="black") +
  facet_grid(Xlab~Y, scale="free_y", switch = "y") +
  geom_point(aes(shape=sig), size=2) + 
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub )) +
  geom_text(aes(label=reflab), nudge_y=-.015, size = 3) +
  geom_hline(yintercept = 1) +
  scale_shape_manual(values=c(19,13), guide=FALSE) +
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
p_sec_multi_pooled

#-------------------------------------------------------------
# save figures
#-------------------------------------------------------------

save(list = ls(pattern="p_"), file=here("figures/secondary_figure_objects.Rdata"))

