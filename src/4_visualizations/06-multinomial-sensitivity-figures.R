
rm(list=ls())
source("0-config.R")

#Load estimats with Basic as reference level
basic <- readRDS(here("results/pooled_results.rds")) %>% 
  filter(analysis=="sens-multi", country=="Pooled - RE", binary==1)

#Load primary estimates, and repool only when all levels present
prim <- readRDS(here("results/pooled_results.rds")) %>% 
  filter(analysis=="primary-multi", country=="Pooled - RE", binary==1) %>% mutate(analysis="All estimates")


RR_multi_adj <- readRDS(here("results/adjusted_mult_RR.rds")) %>% mutate(analysis="Only estimates from\ncountries with all levels") %>%
  filter(Y %in% c("ari", "diarrhea", "stunt", "wast", "mort"))
unique(RR_multi_adj$X)
d <- RR_multi_adj %>% filter(!is.na(ci.lb)) %>% group_by(country, analysis, Y, X, ref) %>% mutate(N=n()) %>%
  filter(X %in% c("EC_risk_H", "EC_risk_S") & N==3 | 
           X %in% c("hyg_imp_cat") & N==2 | 
           X %in% c("san_imp_cat", "wat_imp_cat") & N==4)
table(d$N)



RMAest_bin <- d %>% mutate(est=RR) %>% group_by(analysis, Y, X, ref, contrast) %>%
  do(poolRR(.)) %>% as.data.frame() %>%
  rename(est=RR, ci.lb=RR.CI1, ci.ub=RR.CI2)  %>%
  subset(., select =c(analysis, Y, X, ref, contrast, est,  ci.lb, ci.ub)) %>%
  mutate(country="pooled", binary=1)


#Clean data for figures
df <- RMAest_bin %>% 
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
    Xlab = case_when(X=="EC_H" ~ "Contaminated\nHH water", 
                     X=="EC_S" ~ "Contaminated\nsource water", 
                     X=="san_imp" ~ "Unimproved\nsanitation", 
                     X=="wat_imp" ~ "Unimproved\nwater supply", 
                     X=="hyg_imp" ~ "Unimproved\nhygiene", 
                     X=="WASH" ~ "Not improved WASH\nwith no contamination",
                     X=="WASH_noEC" ~ "Unimproved\nWASH",
                     X=="safely_manH20" ~ "Unsafely managed\ndrinking water",
                     X=="EC_risk_H" ~ "HH water\ncontamination", 
                     X=="EC_risk_S" ~ "Source water\ncontamination", 
                     X=="san_imp_cat" ~ "Sanitation\ncategory", 
                     X=="wat_imp_cat" ~ "Water supply\ncategory", 
                     X=="hyg_imp_cat" ~ "Hygiene\ncategory"),
    Xlab=factor(Xlab, levels = rev(c(
      "Unimproved\nwater supply", 
      "Unimproved\nsanitation", 
      "Unimproved\nhygiene", 
      "Unimproved\nWASH",
      "Contaminated\nHH water", 
      "Contaminated\nsource water", 
      "Unsafely managed\ndrinking water",
      "Not improved WASH\nwith no contamination",
      "HH water\ncontamination", 
      "Source water\ncontamination", 
      "Sanitation\ncategory", 
      "Water supply\ncategory", 
      "Hygiene\ncategory"))),
    Xlab2 = case_when(X=="EC_H" ~ "Contaminated HH water", 
                      X=="EC_S" ~ "Contaminated source water", 
                      X=="san_imp" ~ "Unimproved sanitation", 
                      X=="wat_imp" ~ "Unimproved water supply", 
                      X=="hyg_imp" ~ "Unimproved hygiene", 
                      X=="WASH" ~ "Not improved WASH with no contamination",
                      X=="WASH_noEC" ~ "Unimproved WASH",
                      X=="safely_manH20" ~ "Unsafely managed drinking water",
                      X=="EC_risk_H" ~ "HH water contamination", 
                      X=="EC_risk_S" ~ "Source water contamination", 
                      X=="san_imp_cat" ~ "Sanitation category", 
                      X=="wat_imp_cat" ~ "Water supply category", 
                      X=="hyg_imp_cat" ~ "Hygiene category"),
    Xlab2=factor(Xlab2, levels = rev(c(
      "Unimproved water supply", 
      "Unimproved sanitation", 
      "Unimproved hygiene", 
      "Unimproved WASH",
      "Contaminated HH water", 
      "Contaminated source water", 
      "Unsafely managed drinking water",
      "Not improved WASH with no contamination",
      "HH water contamination", 
      "Source water contamination", 
      "Sanitation category", 
      "Water supply category", 
      "Hygiene category"))),
    contrast = case_when(
      contrast=="0" & !grepl("EC",X)~ "Improved",
      contrast=="1" & !grepl("EC",X)~ "Unimproved",
      contrast=="0" & grepl("EC",X) ~ "Uncontaminated",
      contrast=="1" & grepl("EC",X) ~ "Contaminated",
      contrast=="1" & grepl("EC_risk",X) ~ "Low risk",
      contrast=="2" ~ "Moderate risk",
      contrast=="3" ~ "High risk", 
      contrast=="4" ~ "Very high risk", 
      contrast==contrast ~ contrast 
    ),
    contrast=factor(contrast, levels=rev(c("Unimproved","Improved","Uncontaminated","Contaminated","Low risk","Moderate risk", "High risk",  "Very high risk",  "High coverage", "Continuous",  "Basic", "Limited",  "No facility", "None",   "Surface water"))),
    ref = case_when(
      ref=="0" & !grepl("EC",X)~ "Improved",
      ref=="0" & grepl("EC",X) ~ "Uncontaminated",
      ref=="1" & grepl("EC_risk",X) ~ "Low risk",
      ref==ref ~ ref 
    ),
    exposure_type = ifelse(X %in% c("EC_H","EC_S","WASH", "safely_manH20", "EC_risk_H", "EC_risk_S"),
                           "WQ","HH"
    )) %>%
  mutate(country="Pooled - RE", adjusted=1)


plotdf <- bind_rows(prim, df) %>% 
  mutate(   contrast=factor(contrast, levels=rev(c("Improved","Uncontaminated","Contaminated","Low risk","Moderate risk", "High risk",  "Very high risk",  "High coverage", "Continuous",  "Basic", "Limited",  "No facility", "None",   "Surface water", "Unimproved"))),
            analysis= factor(analysis, levels=rev(c("All estimates","Only estimates from\ncountries with all levels"))))

table(plotdf$analysis)


#mark significant estimates
plotdf <- plotdf %>% mutate(sig = factor(case_when(
  Y %in% c("HAZ","WHZ") & ((ci.lb<0 & ci.ub<0) | (ci.lb>0 & ci.ub>0)) ~ 1,
  Y %in% c("HAZ","WHZ") & ((ci.lb<0 & ci.ub>0)) ~ 0,
  !(Y %in% c("HAZ","WHZ")) & ((ci.lb<1 & ci.ub<1) | (ci.lb>1 & ci.ub>1)) ~ 1,
  !(Y %in% c("HAZ","WHZ")) & ((ci.lb<1 & ci.ub>1)) ~ 0), levels=c("0","1"), labels=c("Not sig.","Sig.")))
plotdf$sig[is.na(plotdf$sig)] <- "Not sig."

#Temp drop mortality
plotdf <- plotdf %>% filter(Y!="Mortality")


p_multi_pooled_HH_sens <- plotdf %>% filter(adjusted==1, binary ==1, analysis %in% c("All estimates","Only estimates from\ncountries with all levels"), country=="Pooled - RE", exposure_type=="HH") %>% 
  droplevels(.) %>%
  arrange(Xlab) %>%
  ggplot(., aes(y=est, x=contrast, group=analysis, color=analysis),color="black") +
  facet_grid(Xlab~Y, scale="free_y", switch = "y") +
  geom_point(aes(shape=sig), position = position_dodge(0.6)) + 
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub), position = position_dodge(0.6)) +
  geom_text(aes(label=reflab), nudge_y=.2, size = 3) +
  geom_hline(yintercept = 1) +
  scale_y_continuous(breaks=c(0.25, 0.5,1, 2, 4, 8), trans='log10', labels=scaleFUN) +
  scale_color_manual(values=tableau10[c(10,4)], guide = guide_legend(reverse = TRUE)) +
  coord_flip() + scale_shape_manual(values=c(19,13), guide=FALSE) +
  xlab("") + ylab("Relative Risk")+
  theme(strip.background = element_blank(),
        axis.text.y = element_text(size=8, hjust = 1),
        strip.text.x = element_text(size=8, face = "bold"),
        strip.text.y = element_text(size=8, angle = 180, face = "bold"),
        strip.placement = "outside",
        axis.text.x = element_text(size=10, vjust = 0.5),
        legend.box.background = element_rect(colour = "black"), 
        title = element_text(margin=margin(0,0,-10,0))) + 
  theme(legend.title = element_blank(), legend.position = "bottom")


p_multi_pooled_WQ_sens <- plotdf %>% filter(adjusted==1, binary ==1, analysis %in% c("All estimates","Only estimates from\ncountries with all levels"), country=="Pooled - RE", exposure_type=="WQ") %>%
  droplevels(.) %>%
  arrange(Xlab) %>%
  ggplot(., aes(y=est, x=contrast, group=analysis, color=analysis),color="black") +
  facet_grid(Xlab~Y, scale="free_y", switch = "y") +
  geom_point(aes(shape=sig), position = position_dodge(0.6)) +
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub), position = position_dodge(0.6)) +
  geom_text(aes(label=reflab), nudge_y=.2, size = 3) +
  geom_hline(yintercept = 1) +
  scale_y_continuous(breaks=c(0.25, 0.5,1, 2, 4, 8), trans='log10', labels=scaleFUN) +
  scale_color_manual(values=tableau10[c(10,4)], guide = guide_legend(reverse = TRUE)) +
  coord_flip() + scale_shape_manual(values=c(19,13), guide=FALSE) +
  xlab("") + ylab("Relative Risk")+
  theme(strip.background = element_blank(),
        axis.text.y = element_text(size=8, hjust = 1),
        strip.text.x = element_text(size=8, face = "bold"),
        strip.text.y = element_text(size=8, angle = 180, face = "bold"),
        strip.placement = "outside",
        axis.text.x = element_text(size=10, vjust = 0.5),
        legend.box.background = element_rect(colour = "black"),
        title = element_text(margin=margin(0,0,-10,0))) +
  theme(legend.title = element_blank(), legend.position = "bottom")







#Need Basc reference category for water
p_basic <- basic %>% 
  droplevels(.) %>%
  arrange(Xlab) %>%
  ggplot(., aes(y=est, x=contrast),color="black") +
  facet_grid(Xlab~Y, scale="free_y", switch = "y") +
  geom_point(aes(shape=sig), position = position_dodge(0.6)) + 
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub), position = position_dodge(0.6)) +
  geom_text(aes(label=reflab), nudge_y=.1, size = 3) +
  geom_hline(yintercept = 1) +
  scale_y_continuous(breaks=c(0.25, 0.5,1, 2, 4, 8), trans='log10', labels=scaleFUN) +
  scale_color_manual(values=tableau10[c(10,4)], guide = guide_legend(reverse = TRUE)) +
  coord_flip() + scale_shape_manual(values=c(19,13), guide=FALSE) +
  xlab("") + ylab("Relative Risk")+
  theme(strip.background = element_blank(),
        axis.text.y = element_text(size=8, hjust = 1),
        strip.text.x = element_text(size=8, face = "bold"),
        strip.text.y = element_text(size=8, angle = 180, face = "bold"),
        strip.placement = "outside",
        axis.text.x = element_text(size=10, vjust = 0.5),
        legend.box.background = element_rect(colour = "black"), 
        title = element_text(margin=margin(0,0,-10,0))) 
p_basic

#-------------------------------------------------------------
# save figures
#-------------------------------------------------------------

save(list = ls(pattern="p_"), file=here("figures/mult_sensitivity_figure_objects.Rdata"))
ls(pattern="p_")



 


