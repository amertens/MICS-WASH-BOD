
source("0-config.R")

d <- readRDS(here("results/pooled_POC_results.rds"))
dtmle <- readRDS(here("results/pooled_POC_tmle_results.rds"))
dFE <- readRDS(here("results/pooled_POC_results_FE.rds"))
dPAF <- readRDS(here("results/paf_results.rds"))



#drop sparse levels
#d <- d %>% filter(n >50 | country=="pooled") %>% filter(Y!="waz")

table(d$X)
table(d$Y)

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
    Y=="whz" ~ "WHZ"
      ),
    Y=factor(Y, levels=c("Diarrhea", "Stunting", "Wasting", "ARI", "HAZ", "WHZ")),
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
                     X=="WASH" ~ "Improved WASH\nwithout contamination",
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
                          "Improved WASH\nwithout contamination",
                          "HH water\ncontamination level", 
                          "Source water\ncontamination level", 
                          "Sanitation\ncategory", 
                          "Water supply\ncategory", 
                          "Hygiene\ncategory")))

table(d$X)
table(d$Y)




#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Primary figures
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


#-------------------------------------------------------------
# RR's single increase
#-------------------------------------------------------------
p_prim_pooled <- d %>% filter(adjusted==1, binary==1, analysis=="primary", country=="Pooled") %>% 
  droplevels(.) %>%
  mutate(X=factor(X, levels = rev(levels(X)))) %>%
  ggplot(., aes(y=est, x=X),color="black") +
  facet_grid(~Y) +
  geom_point() + 
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub )) +
  #scale_color_manual(values=tableau10) +
  geom_hline(yintercept = 1) +
  scale_y_continuous(breaks=c(0.25, 0.5,1, 2, 4, 8), trans='log10', labels=scaleFUN) +
  coord_flip() +
  xlab("Outcome") + ylab("Relative Risk")


p_prim_forest <- d %>% filter(adjusted==1, binary==1, analysis=="primary") %>% 
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
# RR's multinomial
#-------------------------------------------------------------
d %>% filter(adjusted==1, binary==1, analysis=="primary-multi", country=="Pooled", multinomial==1) %>% 
  droplevels(.) %>%
  mutate(X=factor(X, levels = rev(levels(X)))) %>%
  ggplot(., aes(y=est, x=X),color="black") +
  facet_grid(~Y) +
  geom_point() + 
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub )) +
  #scale_color_manual(values=tableau10) +
  geom_hline(yintercept = 1) +
  scale_y_continuous(breaks=c(0.25, 0.5,1, 2, 4, 8), trans='log10', labels=scaleFUN) +
  coord_flip() +
  xlab("Outcome") + ylab("Relative Risk")


d %>% filter(adjusted==1, binary==1, analysis=="primary-multi", multinomial==1) %>% 
  droplevels(.) %>%
  ggplot(., aes(y=est, x=country, color=country)) +
  facet_grid(Y~X) +
  geom_point() + 
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub )) +
  scale_color_manual(values=tableau11[1:4]) +
  geom_hline(yintercept = 1) +
  scale_y_continuous(breaks=c(0.25, 0.5,1, 2, 4, 8), trans='log10', labels=scaleFUN) +
  coord_flip() +
  xlab("Outcome") + ylab("Relative Risk")



#-------------------------------------------------------------
# Z-score differences
#-------------------------------------------------------------
p_prim_Zscore_pooled <- d %>% filter(adjusted==1, binary==0, analysis=="primary", country=="Pooled") %>% 
  droplevels(.) %>%
  mutate(X=factor(X, levels = rev(levels(X)))) %>%
  ggplot(., aes(y=est, x=X),color="black") +
  facet_grid(~Y) +
  geom_point() + 
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub )) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  xlab("Outcome") + ylab("Z-score difference")

p_prim_Zscore_pooled


p_prim_Zscore_forest <- d %>% filter(adjusted==1, binary==0, analysis=="primary") %>% 
  droplevels(.) %>%
  ggplot(., aes(y=est, x=country, color=country)) +
  facet_grid(Y~X) +
  geom_point() + 
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub )) +
  scale_color_manual(values=tableau11[1:4]) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  xlab("Country") + ylab("Z-score difference")

p_prim_Zscore_forest

#-------------------------------------------------------------
# PAF ranking
#-------------------------------------------------------------

#Note: why are so many missing?

dPAF %>% filter(W!="unadjusted") %>%
  ggplot(., aes(y=PAF, x=country, color=country)) +
  facet_grid(Y~X) +
  geom_point() + 
  geom_linerange(aes(ymin=PAF.lb, ymax=PAF.ub )) +
  scale_color_manual(values=tableau10[1:3]) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  xlab("Outcome") + ylab("Population Attributable Fraction")



#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#Secondary figures
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX



#-------------------------------------------------------------
# subgroup figure
#-------------------------------------------------------------
#Note: fix so the binary outcomes run

# p_prim_pooled <- d %>% filter(adjusted==1, binary==1, analysis=="rural", country=="Pooled") %>% 
#   droplevels(.) %>%
#   mutate(X=factor(X, levels = rev(levels(X)))) %>%
#   ggplot(., aes(y=est, x=X),color="black") +
#   facet_grid(~Y) +
#   geom_point() + 
#   geom_linerange(aes(ymin=ci.lb, ymax=ci.ub )) +
#   #scale_color_manual(values=tableau10) +
#   geom_hline(yintercept = 1) +
#   scale_y_continuous(breaks=c(0.25, 0.5,1, 2, 4, 8), trans='log10', labels=scaleFUN) +
#   coord_flip() +
#   xlab("Outcome") + ylab("Relative Risk")

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
  geom_point(position = position_dodge(0.5)) + 
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub), position = position_dodge(0.5)) +
  scale_color_manual(values=tableau10[c(10,4)], guide = guide_legend(reverse = TRUE)) +
  geom_hline(yintercept = 1) +
  scale_y_continuous(breaks=c(0.25, 0.5,1, 2, 4, 8), trans='log10', labels=scaleFUN) +
  coord_flip() +
  xlab("Outcome") + ylab("Relative Risk") + theme(legend.title = element_blank(), legend.position = "right")

#continuous
p_unadj_comp_diff <- d %>% filter(binary==0, analysis=="primary", country=="Pooled") %>% 
  droplevels(.) %>%
  mutate(X=factor(X, levels = rev(levels(X))), 
         adjusted=factor(adjusted, levels=c("0","1"), labels = c("Unadjusted","Adjusted"))) %>%
  ggplot(., aes(y=est, x=X, group=adjusted, color=adjusted)) +
  facet_grid(~Y) +
  geom_point(position = position_dodge(0.5)) + 
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub), position = position_dodge(0.5)) +
  scale_color_manual(values=tableau10[c(10,4)], guide = guide_legend(reverse = TRUE)) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  xlab("Outcome") + ylab("Z-score difference") + theme(legend.title = element_blank(), legend.position = "right")

#-------------------------------------------------------------
#-compare TMLE to primary estimates
#-------------------------------------------------------------

#RR
p_tmle_comp_RR <- d %>% filter(binary==1, analysis=="primary", country=="Pooled") %>% 
  droplevels(.) %>%
  mutate(X=factor(X, levels = rev(levels(X))), 
         adjusted=factor(adjusted, levels=c("0","1"), labels = c("Unadjusted","Adjusted"))) %>%
  ggplot(., aes(y=est, x=X, group=adjusted, color=adjusted)) +
  facet_grid(~Y) +
  geom_point(position = position_dodge(0.5)) + 
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub), position = position_dodge(0.5)) +
  scale_color_manual(values=tableau10[c(10,4)], guide = guide_legend(reverse = TRUE)) +
  geom_hline(yintercept = 1) +
  scale_y_continuous(breaks=c(0.25, 0.5,1, 2, 4, 8), trans='log10', labels=scaleFUN) +
  coord_flip() +
  xlab("Outcome") + ylab("Relative Risk") + theme(legend.title = element_blank(), legend.position = "right")


#-------------------------------------------------------------
#-compare 1-step to primary pooled estimates
#-------------------------------------------------------------

#RR

#-------------------------------------------------------------
#-compare CC to primary pooled estimates
#-------------------------------------------------------------


#-------------------------------------------------------------
#-FE versus RE
#-------------------------------------------------------------


#-------------------------------------------------------------
# sparsity heatmap - updated
#-------------------------------------------------------------









head(d)

d %>% filter(country=="Bangladesh", adjusted==0, binary==1) %>% 
  ggplot(., aes(y=est, x=X)) +
    facet_wrap(~Y) +
    geom_point() + 
    geom_linerange(aes(ymin=ci.lb, ymax=ci.ub )) +
    geom_hline(yintercept = 1) +
    scale_y_continuous(breaks=c(0.25, 0.5,1, 2, 4, 8), trans='log10', labels=scaleFUN) +
    coord_flip() 



d %>% filter(country=="Bangladesh", adjusted==1, binary==1) %>% 
  ggplot(., aes(y=est, x=X)) +
  facet_wrap(~Y) +
  geom_point() + 
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub )) +
  geom_hline(yintercept = 1) +
  scale_y_continuous(breaks=c(0.25, 0.5,1, 2, 4, 8), trans='log10', labels=scaleFUN) +
  coord_flip() +
  xlab("Outcome") + ylab("Relative Risk")




dtmle %>% filter(country=="Bangladesh", adjusted==1, binary==1) %>% 
  ggplot(., aes(y=est, x=X)) +
  facet_wrap(~Y) +
  geom_point() + 
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub )) +
  geom_hline(yintercept = 1) +
  scale_y_continuous(breaks=c(0.25, 0.5,1, 2, 4, 8), trans='log10', labels=scaleFUN) +
  coord_flip() +
  xlab("Outcome") + ylab("Relative Risk")



dtmle %>% filter(country=="pooled", adjusted==1, binary==1) %>% 
  filter(!(Y=="ari" & X=="WASH")) %>%
  droplevels(.) %>%
  ggplot(., aes(y=est, x=X)) +
  facet_wrap(~Y) +
  geom_point() + 
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub )) +
  geom_hline(yintercept = 1) +
  scale_y_continuous(breaks=c(0.25, 0.5,1, 2, 4, 8), trans='log10', labels=scaleFUN) +
  coord_flip() +
  xlab("Outcome") + ylab("Relative Risk")








