
source("0-config.R")

d <- readRDS(here("results/pooled_POC_results.rds"))
dtmle <- readRDS(here("results/pooled_POC_tmle_results.rds"))

#drop sparse levels
d <- d %>% filter(n >50 | country=="pooled") %>% filter(Y!="waz")

#Clean data for primary figure
d <- d %>% 
  mutate(Y=case_when(
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
    country=factor(country, levels=c("Bangladesh", "Pakistan", "Zimbabwe", "Pooled")),
    X = case_when(X=="EC_H" ~ "Uncontaminated\nHH water", 
                     X=="EC_S" ~ "Uncontaminated\nsource water", 
                     X=="san_imp" ~ "Improved\nsanitation", 
                     X=="wat_imp" ~ "Improved\nwater supply", 
                     X=="hyg_imp" ~ "Improved\nhygiene", 
                     X=="WASH" ~ "Improved\nWASH"
                    # X=="safely_manH20" ~ "Safely managed\ndrinking water")
    ),
   X=factor(X, levels = c("Uncontaminated\nHH water", 
                          "Uncontaminated\nsource water", 
                          "Improved\nwater supply", 
                          "Improved\nsanitation", 
                          "Improved\nhygiene", 
                          "Improved\nWASH")) 
  )




#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Primary figures
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


#-------------------------------------------------------------
# RR's single increase
#-------------------------------------------------------------
d %>% filter(adjusted==1, binary==1, analysis=="primary") %>% 
  droplevels(.) %>%
  ggplot(., aes(y=est, x=Y, color=Y)) +
  facet_grid(X~country) +
  geom_point() + 
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub )) +
  scale_color_manual(values=tableau10[4:1]) +
  geom_hline(yintercept = 1) +
  scale_y_continuous(breaks=c(0.25, 0.5,1, 2, 4, 8), trans='log10', labels=scaleFUN) +
  coord_flip() +
  xlab("Outcome") + ylab("Relative Risk")

#-------------------------------------------------------------
# RR's multinomial
#-------------------------------------------------------------

#-------------------------------------------------------------
# Z-score differences
#-------------------------------------------------------------

#-------------------------------------------------------------
# PAF ranking
#-------------------------------------------------------------




#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#Secondary figures
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX



#-------------------------------------------------------------
# subgroup figure
#-------------------------------------------------------------


#-------------------------------------------------------------
#-compare unadjusted to adjusted estimates
#-------------------------------------------------------------

#RR

#continuous

#-------------------------------------------------------------
#-compare TMLE to primary estimates
#-------------------------------------------------------------

#RR

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








