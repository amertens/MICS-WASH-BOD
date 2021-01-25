
source("0-config.R")

d <- readRDS(here("results/pooled_POC_results.rds"))
dPAF <- readRDS(here("results/paf_results.rds"))
dPAF_sig <- readRDS(here("results/paf_sig_results.rds"))
dPAF_pos <- readRDS(here("results/paf_pos_results.rds"))

#Compare boostrapped PAF's to hand-calculated PAF's
df <- readRDS(here("data/compiled_clean_POC_survey.rds")) %>% filter(country=="Bangladesh")
table(df$san_imp, df$stunt)


#Clean data for primary figure
d <- d %>% 
  mutate(
    multinomial = ifelse(X %in% c("EC_risk_H", "EC_risk_S", "wat_imp_cat", "san_imp_cat", "hyg_imp_cat"),1,0),
    Y=case_when(
    Y=="stunt" ~ "Stunting",
    Y=="wast" ~ "Wasting",
    Y=="diarrhea" ~ "Diarrhea",
    Y=="ari" ~ "ARI",
    Y=="haz" ~ "HAZ",
    Y=="whz" ~ "WHZ",
    Y=="mort" ~ "Mortality"
      ),
    Y=factor(Y, levels=c("Diarrhea", "Stunting", "Wasting", "ARI", "HAZ", "WHZ","Mortality")),
    country=case_when(
      country=="pooled" ~ "Pooled",
      country=="PakistanPunjab" ~ "Pakistan",
      country==country ~ country
    ),
    country=factor(country, levels=rev(c("Bangladesh", "Pakistan", "Zimbabwe", "Pooled"))),
    X = case_when(X=="EC_H" ~ "Uncontaminated\nHH water", 
                     X=="EC_S" ~ "Uncontaminated\nsource water", 
                     X=="san_imp" ~ "Improved\nsanitation", 
                     X=="wat_imp" ~ "Improved\nwater supply", 
                     X=="hyg_imp" ~ "Improved\nhygiene", 
                     X=="WASH" ~ "Improved WASH,\nno contamination",
                     X=="WASH_noEC" ~ "Improved\nWASH",
                     X=="safely_manH20" ~ "Safely managed\ndrinking water",
                  X=="EC_risk_H" ~ "HH water\ncontamination", 
                  X=="EC_risk_S" ~ "Source water\ncontamination", 
                  X=="san_imp_cat" ~ "Sanitation\ncategory", 
                  X=="wat_imp_cat" ~ "Water supply\ncategory", 
                  X=="hyg_imp_cat" ~ "Hygiene\ncategory"),
   X=factor(X, levels = c(
                          "Improved\nwater supply", 
                          "Improved\nsanitation", 
                          "Improved\nhygiene", 
                          "Improved\nWASH",
                          "Uncontaminated\nHH water", 
                          "Uncontaminated\nsource water", 
                          "Safely managed\ndrinking water",
                          "Improved WASH,\nno contamination",
                          "HH water\ncontamination", 
                          "Source water\ncontamination", 
                          "Sanitation\ncategory", 
                          "Water supply\ncategory", 
                          "Hygiene\ncategory")),
   contrast = case_when(
     contrast=="1" ~ "Unimproved",
     contrast=="2" ~ "Moderate risk",
     contrast=="3" ~ "High risk", 
     contrast=="4" ~ "Very high risk", 
     contrast==contrast ~ contrast 
   ),
   contrast=factor(contrast, levels=rev(c("Moderate risk", "High risk",  "Very high risk",   "Basic", "Limited",  "No facility", "None",  "Unimproved", "Surface water"))),
   ref = case_when(
     ref=="0" ~ "Improved",
     ref=="1" ~ "Low risk",
     ref==ref ~ ref 
   ))

table(d$X)
table(d$Y)


dPAF_pos  <- dPAF_pos  %>% 
  mutate(
    multinomial = ifelse(X %in% c("EC_risk_H", "EC_risk_S", "wat_imp_cat", "san_imp_cat", "hyg_imp_cat"),1,0),
    Y=case_when(
      Y=="stunt" ~ "Stunting",
      Y=="wast" ~ "Wasting",
      Y=="diarrhea" ~ "Diarrhea",
      Y=="ari" ~ "ARI",
      Y=="haz" ~ "HAZ",
      Y=="whz" ~ "WHZ"
    ),
    Y=factor(Y, levels=c("Diarrhea", "Stunting", "Wasting", "ARI", "HAZ", "WHZ")),
    country=case_when(
      country=="PakistanPunjab" ~ "Pakistan",
      country==country ~ country
    ),
    country=factor(country, levels=rev(c("Bangladesh", "Pakistan", "Zimbabwe", "Pooled"))),
    X = case_when(X=="EC_H" ~ "Uncontaminated\nHH water", 
                  X=="EC_S" ~ "Uncontaminated\nsource water", 
                  X=="san_imp" ~ "Improved\nsanitation", 
                  X=="wat_imp" ~ "Improved\nwater supply", 
                  X=="hyg_imp" ~ "Improved\nhygiene", 
                  X=="WASH" ~ "Improved WASH,\nno contamination",
                  X=="WASH_noEC" ~ "Improved\nWASH",
                  X=="safely_manH20" ~ "Safely managed\ndrinking water",
                  X=="EC_risk_H" ~ "HH water\ncontamination level", 
                  X=="EC_risk_S" ~ "Source water\ncontamination level", 
                  X=="san_imp_cat" ~ "Sanitation\ncategory", 
                  X=="wat_imp_cat" ~ "Water supply\ncategory", 
                  X=="hyg_imp_cat" ~ "Hygiene\ncategory"),
    X=factor(X, levels = c(
      "Improved\nwater supply", 
      "Improved\nsanitation", 
      "Improved\nhygiene", 
      "Improved\nWASH",
      "Uncontaminated\nHH water", 
      "Uncontaminated\nsource water", 
      "Safely managed\ndrinking water",
      "Improved WASH,\nno contamination",
      "HH water\ncontamination level", 
      "Source water\ncontamination level", 
      "Sanitation\ncategory", 
      "Water supply\ncategory", 
      "Hygiene\ncategory")))




#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Primary figures
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


#-------------------------------------------------------------
# subgroup figure
#-------------------------------------------------------------
#Note: fix so the binary outcomes run

# p_rural_pooled <- d %>% filter(adjusted==1, binary==1, analysis=="rural", country=="Pooled") %>%
#   droplevels(.) %>%
#   mutate(X=factor(X, levels = rev(levels(X))), 
#          subgroup =factor(subgroup , levels=c("Rural","Urban"))) %>% 
#   ggplot(., aes(y=est, x=X, group=subgroup , color=subgroup )) +
#   facet_grid(~Y) +
#   geom_point(position = position_dodge(0.6)) + 
#   geom_linerange(aes(ymin=ci.lb, ymax=ci.ub), position = position_dodge(0.6)) +
#   scale_color_manual(values=tableau10[c(10,4)], guide = guide_legend(reverse = TRUE)) +
#   #scale_color_manual(values=tableau10) +
#   geom_hline(yintercept = 1) +
#   scale_y_continuous(breaks=c(0.25, 0.5,1, 2, 4, 8), trans='log10', labels=scaleFUN) +
#   coord_flip() +
#   xlab("WASH Characteristic reference level") + ylab("Relative Risk")  +
#   theme(legend.title = element_blank(), legend.position = "right")


p_rural_pooled <- d %>% filter(adjusted==1, binary==1, Y=="Diarrhea", analysis=="rural", country!="Pooled") %>%
  droplevels(.) %>%
  mutate(X=factor(X, levels = rev(levels(X))), 
         subgroup =factor(subgroup , levels=c("Rural","Urban"))) %>% 
  ggplot(., aes(y=est, x=X, group=subgroup , color=subgroup )) +
  facet_grid(~country) +
  geom_point(position = position_dodge(0.6)) + 
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub), position = position_dodge(0.6)) +
  scale_color_manual(values=tableau10[c(10,4)], guide = guide_legend(reverse = TRUE)) +
  #scale_color_manual(values=tableau10) +
  geom_hline(yintercept = 1) +
  scale_y_continuous(breaks=c(0.25, 0.5,1, 2, 4, 8), trans='log10', labels=scaleFUN) +
  coord_flip() +
  xlab("WASH Characteristic reference level") + ylab("Relative Risk")  +
  theme(legend.title = element_blank(), legend.position = "right")


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





 

