
source("0-config.R")

d <- readRDS(here("results/pooled_results.rds")) %>% filter(!is.na(ci.lb))

#mark if both


#-------------------------------------------------------------
# subgroup figure
#-------------------------------------------------------------
# Why only Urban?
d %>% filter( analysis=="rural", country=="Pooled - RE") 

#binary
p_rural_pooled_bin <- d %>% filter(adjusted==1, binary==1, analysis=="rural", country=="Pooled - RE") %>%
  droplevels(.) %>%
  mutate(subgroup =factor(subgroup , levels=c("Rural","Urban"))) %>% 
  ggplot(., aes(y=est, x=Xlab, group=subgroup , color=subgroup )) +
  facet_wrap(~Y, scales="free") +
  geom_point(position = position_dodge(0.6)) + 
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub), position = position_dodge(0.6)) +
  scale_color_manual(values=tableau10[c(10,4)], guide = guide_legend(reverse = TRUE)) +
  geom_hline(yintercept = 1) +
  scale_y_continuous(breaks=c(0.25, 0.5,1, 2, 4, 8), trans='log10', labels=scaleFUN) +
  coord_flip() +
  xlab("WASH Characteristic reference level") + ylab("Relative Risk")  +
  theme(legend.title = element_blank(), legend.position = "right")

#note no "improved WASH, no contamination" for rarer wasting/ARI outcomes


p_rural_pooled_cont <- d %>% filter(adjusted==1, binary==0, analysis=="rural", country=="Pooled - RE") %>%
  droplevels(.) %>%
  mutate(subgroup =factor(subgroup , levels=c("Rural","Urban"))) %>% 
  ggplot(., aes(y=est, x=Xlab, group=subgroup , color=subgroup )) +
  facet_wrap(~Y, scales="free") +
  geom_point(position = position_dodge(0.6)) + 
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub), position = position_dodge(0.6)) +
  scale_color_manual(values=tableau10[c(10,4)], guide = guide_legend(reverse = TRUE)) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  xlab("WASH Characteristic reference level") + ylab("Relative Risk")  +
  theme(legend.title = element_blank(), legend.position = "right")


#-------------------------------------------------------------
# save figures
#-------------------------------------------------------------

save(list = ls(pattern="p_"), file=here("figures/subgroup_figure_objects.Rdata"))

ls(pattern="p_")





 


