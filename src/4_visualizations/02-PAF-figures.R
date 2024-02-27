
rm(list=ls())
source("0-config.R")

dpool <- readRDS(here("results/pooled_PAFs.rds"))
dPAF <- readRDS(here("results/prim_PAFs.rds"))
# dPAF <- dPAF %>% filter(adjusted==1, !is.na(PAF)) %>%
#   subset(., select=c(country, Y, X,RR,  pval,PAF, PAF.lb, PAF.ub, ci.lb, ci.ub))
head(dPAF)

# #get just PAF's from significant RR's
# dPAF_sig <- dPAF %>% filter(!is.na(PAF), ci.lb>1) 
# 
# #get just PAF's from RR's > 1
# dPAF_pos <- dPAF %>% filter(!is.na(PAF),  RR>1) 


dPAF  <- dPAF  %>% 
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



dpool  <- dpool  %>% 
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





#-------------------------------------------------------------
# PAF ranking
#-------------------------------------------------------------

# pPAF_sig <- dPAF_sig %>% filter(W!="unadjusted") %>%
#   ggplot(., aes(y=PAF, x=country, color=country)) +
#   facet_grid(Y~X) +
#   geom_point() + 
#   geom_linerange(aes(ymin=PAF.lb, ymax=PAF.ub )) +
#   scale_color_manual(values=tableau10[1:3]) +
#   geom_hline(yintercept = 0) +
#   coord_flip() +
#   xlab("WASH Characteristic reference level") + ylab("Population Attributable Fraction")
# 
# 
# pPAF <- dPAF %>% filter(W!="unadjusted", Y=="diarrhea") %>%
#   ggplot(., aes(y=PAF, x=country, color=country)) +
#   facet_wrap(~X) +
#   geom_point() + 
#   geom_linerange(aes(ymin=PAF.lb, ymax=PAF.ub )) +
#   scale_color_manual(values=tableau10[1:3]) +
#   geom_hline(yintercept = 0) +
#   coord_flip() +
#   xlab("WASH Characteristic reference level") + ylab("Population Attributable Fraction")
# pPAF
# 




p_pooled_PAF <- dpool %>% filter( X!="Improved WASH,\nno contamination") %>% droplevels(.) %>%
  ggplot(., aes(y=PAF, x=X)) +
  facet_wrap(~Y) +
  geom_point() + 
  geom_linerange(aes(ymin=PAF.lb, ymax=PAF.ub )) +
  coord_flip() +
  geom_hline(yintercept = 0) +
  xlab("Country and outcome") + ylab("Population Attributable Fraction") +
  theme(strip.background = element_blank(),
        legend.position="none",
        axis.text.y = element_text(size=8, hjust = 1),
        strip.text.x = element_text(size=8, face = "bold"),
        strip.text.y = element_text(size=8, angle = 180, face = "bold"),
        strip.placement = "outside",
        legend.title = element_blank(),
        axis.text.x = element_text(size=10, vjust = 0.5),
        title = element_text(margin=margin(0,0,-10,0)))


p_PAF <- dPAF %>% filter( X!="Improved WASH,\nno contamination") %>% droplevels(.) %>%
  ggplot(., aes(y=PAF, x=country, color=country)) +
  facet_grid(Y~X, switch = "y", scales="free") +
  geom_point() + 
  geom_linerange(aes(ymin=PAF.lb, ymax=PAF.ub )) +
  #scale_color_manual(values=tableau10[1:3]) +
  geom_hline(yintercept = 0) +
  #coord_flip(ylim=c(-100,100)) +
  coord_flip() +
  xlab("Country and outcome") + ylab("Population Attributable Fraction") +
  theme(strip.background = element_blank(),
        legend.position="none",
        axis.text.y = element_text(size=8, hjust = 1),
        strip.text.x = element_text(size=8, face = "bold"),
        strip.text.y = element_text(size=8, angle = 180, face = "bold"),
        strip.placement = "outside",
        legend.title = element_blank(),
        axis.text.x = element_text(size=10, vjust = 0.5),
        title = element_text(margin=margin(0,0,-10,0)))


save(list = ls(pattern="p_"), file=here("figures/PAF_figure_objects.Rdata"))

ls(pattern="p_")
