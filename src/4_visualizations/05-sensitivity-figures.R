
source("0-config.R")

d <- readRDS(here("results/pooled_POC_results.rds"))



#-------------------------------------------------------------
#-compare unadjusted to adjusted estimates
#-------------------------------------------------------------

#RR
p_unadj_comp_RR <- d %>% filter(binary==1, analysis=="primary", country=="Pooled") %>% 
  droplevels(.) %>%
  mutate(X=factor(X, levels = rev(levels(X))), 
         adjusted=factor(adjusted, levels=c("0","1"), labels = c("Unadjusted","Adjusted"))) %>%
  ggplot(., aes(y=est, x=X, group=adjusted, color=adjusted)) +
  facet_grid(~Y) +
  geom_point(position = position_dodge(0.6)) + 
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub), position = position_dodge(0.6)) +
  scale_color_manual(values=tableau10[c(10,4)], guide = guide_legend(reverse = TRUE)) +
  geom_hline(yintercept = 1) +
  scale_y_continuous(breaks=c(0.25, 0.5,1, 2, 4, 8), trans='log10', labels=scaleFUN) +
  coord_flip() +
  xlab("WASH Characteristic reference level") + ylab("Relative Risk") + theme(legend.title = element_blank(), legend.position = "right")

#continuous
p_unadj_comp_diff <- d %>% filter(binary==0, analysis=="primary", country=="Pooled") %>% 
  droplevels(.) %>%
  mutate(X=factor(X, levels = rev(levels(X))), 
         adjusted=factor(adjusted, levels=c("0","1"), labels = c("Unadjusted","Adjusted"))) %>%
  ggplot(., aes(y=est, x=X, group=adjusted, color=adjusted)) +
  facet_grid(~Y) +
  geom_point(position = position_dodge(0.6)) + 
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub), position = position_dodge(0.6)) +
  scale_color_manual(values=tableau10[c(10,4)], guide = guide_legend(reverse = TRUE)) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  xlab("WASH Characteristic reference level") + ylab("Z-score difference") + theme(legend.title = element_blank(), legend.position = "right")

#-------------------------------------------------------------
#-compare TMLE to primary estimates
#-------------------------------------------------------------

#RR
p_tmle_comp_RR <- d %>% filter(binary==1, adjusted==1, analysis=="primary"|analysis=="tmle", country=="Pooled") %>% 
  droplevels(.) %>%
  mutate(X=factor(X, levels = rev(levels(X))), 
         analysis=factor(analysis, levels=c("primary","tmle"), labels = c("GLM","TMLE"))) %>%
  ggplot(., aes(y=est, x=X, group=analysis, color=analysis)) +
  facet_grid(~Y) +
  geom_point(position = position_dodge(0.6)) + 
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub), position = position_dodge(0.6)) +
  scale_color_manual(values=tableau10[c(10,4)], guide = guide_legend(reverse = TRUE)) +
  geom_hline(yintercept = 1) +
  scale_y_continuous(breaks=c(0.25, 0.5,1, 2, 4, 8), trans='log10', labels=scaleFUN) +
  coord_flip() +
  xlab("WASH Characteristic reference level") + ylab("Relative Risk") + theme(legend.title = element_blank(), legend.position = "right")


p_tmle_glm_comp_RR <- d %>% filter(binary==1, adjusted==1, analysis=="primary"|analysis=="tmle-glm", country=="Pooled") %>% 
  droplevels(.) %>%
  mutate(X=factor(X, levels = rev(levels(X))), 
         analysis=factor(analysis, levels=c("primary","tmle-glm"), labels = c("GLM","TMLE fit\nwith glm"))) %>%
  ggplot(., aes(y=est, x=X, group=analysis, color=analysis)) +
  facet_grid(~Y) +
  geom_point(position = position_dodge(0.6)) + 
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub), position = position_dodge(0.6)) +
  scale_color_manual(values=tableau10[c(10,4)], guide = guide_legend(reverse = TRUE)) +
  geom_hline(yintercept = 1) +
  scale_y_continuous(breaks=c(0.25, 0.5,1, 2, 4, 8), trans='log10', labels=scaleFUN) +
  coord_flip() +
  xlab("WASH Characteristic reference level") + ylab("Relative Risk") + theme(legend.title = element_blank(), legend.position = "right")



#TEMP
p_tmle_glm_comp_RR <- d %>% filter(binary==1, adjusted==1, analysis=="primary"|analysis=="tmle-glm", country=="Bangladesh") %>% 
  droplevels(.) %>%
  mutate(X=factor(X, levels = rev(levels(X))), 
         analysis=factor(analysis, levels=c("primary","tmle-glm"), labels = c("GLM","TMLE fit\nwith glm"))) %>%
  ggplot(., aes(y=est, x=X, group=analysis, color=analysis)) +
  facet_grid(~Y) +
  geom_point(position = position_dodge(0.6)) + 
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub), position = position_dodge(0.6)) +
  scale_color_manual(values=tableau10[c(10,4)], guide = guide_legend(reverse = TRUE)) +
  geom_hline(yintercept = 1) +
  scale_y_continuous(breaks=c(0.25, 0.5,1, 2, 4, 8), trans='log10', labels=scaleFUN) +
  coord_flip() +
  xlab("WASH Characteristic reference level") + ylab("Relative Risk") + theme(legend.title = element_blank(), legend.position = "right")


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
#-compare 1-step to primary pooled estimates
#-------------------------------------------------------------

#RR
p_1step_comp_RR <- d %>% filter(binary==1, adjusted==1, analysis=="FE"|analysis=="1step", country=="Pooled") %>% 
  droplevels(.) %>%
  mutate(X=factor(X, levels = rev(levels(X))), 
         analysis=factor(analysis, levels=c("FE","1step"), labels = c("2-step","1-step"))) %>%
  ggplot(., aes(y=est, x=X, group=analysis, color=analysis)) +
  facet_grid(~Y) +
  geom_point(position = position_dodge(0.6)) + 
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub), position = position_dodge(0.6)) +
  scale_color_manual(values=tableau10[c(10,4)], guide = guide_legend(reverse = TRUE)) +
  geom_hline(yintercept = 1) +
  scale_y_continuous(breaks=c(0.25, 0.5,1, 2, 4, 8), trans='log10', labels=scaleFUN) +
  coord_flip() +
  xlab("WASH Characteristic reference level") + ylab("Relative Risk") + theme(legend.title = element_blank(), legend.position = "right")



#-------------------------------------------------------------
#-compare CC to primary pooled estimates
#-------------------------------------------------------------
p_CC_comp_RR <- d %>% filter(binary==1, adjusted==1, analysis=="primary"|analysis=="CC", country=="Pooled") %>% 
  droplevels(.) %>%
  mutate(X=factor(X, levels = rev(levels(X))), 
         analysis=factor(analysis, levels=c("primary","CC"), labels = c("Imputation","Complete Case"))) %>%
  ggplot(., aes(y=est, x=X, group=analysis, color=analysis)) +
  facet_grid(~Y) +
  geom_point(position = position_dodge(0.6)) + 
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub), position = position_dodge(0.6)) +
  scale_color_manual(values=tableau10[c(10,4)], guide = guide_legend(reverse = TRUE)) +
  geom_hline(yintercept = 1) +
  scale_y_continuous(breaks=c(0.25, 0.5,1, 2, 4, 8), trans='log10', labels=scaleFUN) +
  coord_flip() +
  xlab("WASH Characteristic reference level") + ylab("Relative Risk") + theme(legend.title = element_blank(), legend.position = "right")


#-------------------------------------------------------------
#-FE versus RE
#-------------------------------------------------------------
p_FE_comp_RR <- d %>% filter(binary==1, adjusted==1, multinomial==0, analysis=="primary"|analysis=="FE", country=="Pooled") %>% 
  droplevels(.) %>%
  mutate(X=factor(X, levels = rev(levels(X))), 
         analysis=factor(analysis, levels=c("primary","FE"), labels = c("Random-effects","Fixed-effects"))) %>%
  ggplot(., aes(y=est, x=X, group=analysis, color=analysis)) +
  facet_grid(~Y) +
  geom_point(position = position_dodge(0.6)) + 
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub), position = position_dodge(0.6)) +
  scale_color_manual(values=tableau10[c(10,4)], guide = guide_legend(reverse = TRUE)) +
  geom_hline(yintercept = 1) +
  scale_y_continuous(breaks=c(0.25, 0.5,1, 2, 4, 8), trans='log10', labels=scaleFUN) +
  coord_flip() +
  xlab("WASH Characteristic reference level") + ylab("Relative Risk") + theme(legend.title = element_blank(), legend.position = "right")


#-------------------------------------------------------------
# ckustid sense
#-------------------------------------------------------------
p_clustid_comp_RR <- d %>% filter(binary==1, adjusted==1, multinomial==0, analysis=="primary"|analysis=="clustid", country=="Pakistan") %>% 
  droplevels(.) %>%
  mutate(X=factor(X, levels = rev(levels(X))), 
         analysis=factor(analysis, levels=c("primary","clustid"), labels = c("HH-ID","Cluster-ID"))) %>%
  ggplot(., aes(y=est, x=X, group=analysis, color=analysis)) +
  facet_grid(~Y) +
  geom_point(position = position_dodge(0.6)) + 
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub), position = position_dodge(0.6)) +
  scale_color_manual(values=tableau10[c(10,4)], guide = guide_legend(reverse = TRUE)) +
  geom_hline(yintercept = 1) +
  scale_y_continuous(breaks=c(0.25, 0.5,1, 2, 4, 8), trans='log10', labels=scaleFUN) +
  coord_flip() +
  xlab("WASH Characteristic reference level") + ylab("Relative Risk") + theme(legend.title = element_blank(), legend.position = "right")





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





 


