
rm(list=ls())
source("0-config.R")

d <- readRDS(here("results/pooled_results.rds")) %>% filter(!is.na(ci.lb))



#-------------------------------------------------------------
#-compare unadjusted to adjusted estimates
#-------------------------------------------------------------

#RR
#p_prim_pooled_HH <- d %>% filter(adjusted==1, binary==1, analysis=="primary", country=="Pooled - RE", exposure_type=="HH") %>% 
  
p_unadj_comp_RR <- d %>% filter(binary==1, analysis=="primary", country=="Pooled - RE") %>% 
  droplevels(.) %>%
  mutate(#X=factor(X, levels = rev(levels(X))), 
         adjusted=factor(adjusted, levels=c("0","1"), labels = c("Unadjusted","Adjusted"))) %>%
  ggplot(., aes(y=est, x=Xlab, group=adjusted, color=adjusted)) +
  facet_grid(~Y) +
  geom_point(aes(shape=sig), position = position_dodge(0.6)) + 
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub), position = position_dodge(0.6)) +
  scale_color_manual(values=tableau10[c(10,4)], guide = guide_legend(reverse = TRUE)) +
  geom_hline(yintercept = 1) +
  scale_y_continuous(breaks=c(0.25, 0.5,1, 2, 4, 8), trans='log10', labels=scaleFUN) +
  coord_flip() +  scale_shape_manual(values=c(19,13)) +
  xlab("WASH Characteristic reference level") + ylab("Relative Risk") + theme(legend.title = element_blank(), legend.position = "right")

#continuous
p_unadj_comp_diff <- d %>% filter(binary==0, analysis=="primary", country=="Pooled - RE") %>% 
  droplevels(.) %>%
  mutate(adjusted=factor(adjusted, levels=c("0","1"), labels = c("Unadjusted","Adjusted"))) %>%
  ggplot(., aes(y=est, x=Xlab, group=adjusted, color=adjusted)) +
  facet_grid(~Y) +
  geom_point(aes(shape=sig), position = position_dodge(0.6)) + 
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub), position = position_dodge(0.6)) +
  scale_color_manual(values=tableau10[c(10,4)], guide = guide_legend(reverse = TRUE)) +
  geom_hline(yintercept = 0) +
  coord_flip() +  scale_shape_manual(values=c(19,13)) +
  xlab("WASH Characteristic reference level") + ylab("Z-score difference") + theme(legend.title = element_blank(), legend.position = "right")

#-------------------------------------------------------------
#-compare TMLE to primary estimates
#-------------------------------------------------------------

#RR
p_tmle_comp_RR <- d %>% filter(Y=="Diarrhea", X %in% c("EC_H","EC_S"), binary==1, adjusted==1, analysis=="primary"|analysis=="tmle", country=="Pooled - RE") %>% 
  droplevels(.) %>%
  mutate(analysis=factor(analysis, levels=c("primary","tmle"), labels = c("GLM","TMLE"))) %>%
  ggplot(., aes(y=est, x=Xlab, group=analysis, color=analysis)) +
  geom_point(aes(shape=sig), position = position_dodge(0.6)) + 
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub), position = position_dodge(0.6)) +
  scale_color_manual(values=tableau10[c(10,4)], guide = guide_legend(reverse = TRUE)) +
  geom_hline(yintercept = 1) +
  scale_y_continuous(breaks=c(0.25, 0.5,1, 2, 4, 8), trans='log10', labels=scaleFUN) +
  coord_flip() +  scale_shape_manual(values=c(19,13)) +
  xlab("WASH Characteristic reference level") + ylab("Relative Risk") + theme(legend.title = element_blank(), legend.position = "right")

#diff
p_tmle_comp_diff <- d %>% filter(Y=="HAZ", X %in% c("EC_H","EC_S"), binary==0, adjusted==1, analysis=="primary"|analysis=="tmle", country=="Pooled - RE") %>% 
  droplevels(.) %>%
  mutate(analysis=factor(analysis, levels=c("primary","tmle"), labels = c("GLM","TMLE"))) %>%
  ggplot(., aes(y=est, x=Xlab, group=analysis, color=analysis)) +
  geom_point(aes(shape=sig), position = position_dodge(0.6)) + 
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub), position = position_dodge(0.6)) +
  scale_color_manual(values=tableau10[c(10,4)], guide = guide_legend(reverse = TRUE)) +
  geom_hline(yintercept = 0) +
  coord_flip() +  scale_shape_manual(values=c(19,13)) +
  xlab("WASH Characteristic reference level") + ylab("Z-score difference") + theme(legend.title = element_blank(), legend.position = "right")


#Forest plot

p_tmle_comp_RR_forest <- d %>% filter(Y=="Diarrhea", X %in% c("EC_H","EC_S"), binary==1, adjusted==1, analysis=="primary"|analysis=="tmle") %>% 
  droplevels(.) %>%
  mutate(analysis=factor(analysis, levels=c("primary","tmle"), labels = c("GLM","TMLE"))) %>%
  ggplot(., aes(y=est, x=country, group=analysis, color=analysis)) +
  facet_wrap(~Xlab) +
  geom_point(aes(shape=sig), position = position_dodge(0.6)) + 
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub), position = position_dodge(0.6)) +
  scale_color_manual(values=tableau10[c(10,4)], guide = guide_legend(reverse = TRUE)) +
  geom_hline(yintercept = 1) +
  scale_y_continuous(breaks=c(0.25, 0.5,1, 2, 4, 8), trans='log10', labels=scaleFUN) +
  coord_flip() +  scale_shape_manual(values=c(19,13)) +
  xlab("WASH Characteristic reference level") + ylab("Relative Risk") + theme(legend.title = element_blank(), legend.position = "right")


p_tmle_comp_diff_forest <- d %>% filter(Y=="HAZ", X %in% c("EC_H","EC_S"), binary==0, adjusted==1, analysis=="primary"|analysis=="tmle") %>% 
  droplevels(.) %>%
  mutate(analysis=factor(analysis, levels=c("primary","tmle"), labels = c("GLM","TMLE"))) %>%
  ggplot(., aes(y=est, x=country, group=analysis, color=analysis)) +
  facet_wrap(~Xlab) +
  geom_point(aes(shape=sig), position = position_dodge(0.6)) + 
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub), position = position_dodge(0.6)) +
  scale_color_manual(values=tableau10[c(10,4)], guide = guide_legend(reverse = TRUE)) +
  geom_hline(yintercept = 0) +
  coord_flip() +  scale_shape_manual(values=c(19,13)) +
  xlab("WASH Characteristic reference level") + ylab("Relative Risk") + theme(legend.title = element_blank(), legend.position = "right")


#RE vs. FE comparison
p_FE_comp_RR <- d %>% filter(binary==1, adjusted==1, multinomial==0, analysis %in% c("primary","FE"), country %in% c("Pooled - RE","Pooled - FE")) %>% 
  droplevels(.) %>%
  # mutate(#X=factor(X, levels = rev(levels(X))), 
  #   adjusted=factor(adjusted, levels=c("0","1"), labels = c("Unadjusted","Adjusted"))) %>%
  ggplot(., aes(y=est, x=Xlab, group=country, color=country)) +
  facet_grid(~Y) +
  geom_point(aes(shape=sig), position = position_dodge(0.6)) + 
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub), position = position_dodge(0.6)) +
  scale_color_manual(values=tableau10[c(10,4)], guide = guide_legend(reverse = TRUE)) +
  geom_hline(yintercept = 1) +
  scale_y_continuous(breaks=c(0.25, 0.5,1, 2, 4, 8), trans='log10', labels=scaleFUN) +
  coord_flip() +  scale_shape_manual(values=c(19,13)) +
  xlab("WASH Characteristic reference level") + ylab("Relative Risk") + theme(legend.title = element_blank(), legend.position = "right")


#-------------------------------------------------------------
# save figures
#-------------------------------------------------------------

save(list = ls(pattern="p_"), file=here("figures/sensitivity_figure_objects.Rdata"))


ls(pattern="p_")


 


