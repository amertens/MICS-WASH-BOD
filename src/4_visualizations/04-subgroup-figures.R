
source("0-config.R")

d <- readRDS(here("results/pooled_results.rds"))


#-------------------------------------------------------------
# subgroup figure
#-------------------------------------------------------------
# Why only Urban?

#diarrhea
p_rural_pooled <- d %>% filter(adjusted==1, binary==1, Y=="Diarrhea", analysis=="rural", country=="Pooled") %>%
  droplevels(.) %>%
  mutate(X=factor(X, levels = rev(levels(X))), 
         subgroup =factor(subgroup , levels=c("Rural","Urban"))) %>% 
  ggplot(., aes(y=est, x=X, group=subgroup , color=subgroup )) +
  facet_grid(~country) +
  geom_point(position = position_dodge(0.6)) + 
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub), position = position_dodge(0.6)) +
  scale_color_manual(values=tableau10[c(10,4)], guide = guide_legend(reverse = TRUE)) +
  geom_hline(yintercept = 1) +
  scale_y_continuous(breaks=c(0.25, 0.5,1, 2, 4, 8), trans='log10', labels=scaleFUN) +
  coord_flip() +
  xlab("WASH Characteristic reference level") + ylab("Relative Risk")  +
  theme(legend.title = element_blank(), legend.position = "right")

#stunting
p_rural_pooled <- d %>% filter(adjusted==1, binary==1, Y=="Stunt", analysis=="rural", country!="Pooled") %>%
  droplevels(.) %>%
  mutate(X=factor(X, levels = rev(levels(X))), 
         subgroup =factor(subgroup , levels=c("Rural","Urban"))) %>% 
  ggplot(., aes(y=est, x=X, group=subgroup , color=subgroup )) +
  facet_grid(~country) +
  geom_point(position = position_dodge(0.6)) + 
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub), position = position_dodge(0.6)) +
  scale_color_manual(values=tableau10[c(10,4)], guide = guide_legend(reverse = TRUE)) +
  geom_hline(yintercept = 1) +
  scale_y_continuous(breaks=c(0.25, 0.5,1, 2, 4, 8), trans='log10', labels=scaleFUN) +
  coord_flip() +
  xlab("WASH Characteristic reference level") + ylab("Relative Risk")  +
  theme(legend.title = element_blank(), legend.position = "right")


#-------------------------------------------------------------
# subgroup forest figure
#-------------------------------------------------------------
p_rural_forest<- d %>% filter(adjusted==1, binary==1, Y=="Diarrhea", analysis=="rural", country!="Pooled") %>%
  droplevels(.) %>%
  mutate(X=factor(X, levels = rev(levels(X))), 
         subgroup =factor(subgroup , levels=c("Rural","Urban"))) %>% 
  ggplot(., aes(y=est, x=X, group=subgroup , color=subgroup )) +
  facet_grid(~country) +
  geom_point(position = position_dodge(0.6)) + 
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub), position = position_dodge(0.6)) +
  scale_color_manual(values=tableau10[c(10,4)], guide = guide_legend(reverse = TRUE)) +
  geom_hline(yintercept = 1) +
  scale_y_continuous(breaks=c(0.25, 0.5,1, 2, 4, 8), trans='log10', labels=scaleFUN) +
  coord_flip() +
  xlab("WASH Characteristic reference level") + ylab("Relative Risk")  +
  theme(legend.title = element_blank(), legend.position = "right")

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





 


