


rm(list=ls())
source("0-config.R")
# load(here("results/adjusted_gams.Rdata"))
# 
# p <- ggplot(simul_plot_df) + 
#   geom_ribbon(aes(x = X, ymin = lwrS, ymax = uprS, group=Xvar , color=Xvar), alpha = 0.5) +
#   geom_path(aes(x = X, y = lwrS, group=Xvar , color=Xvar), color = "blue") +
#   geom_path(aes(x = X, y = uprS, group=Xvar , color=Xvar), color = "red") +
#   geom_path(aes(x = X, y = fit, group=Xvar , color=Xvar), color = "black") + 
#   facet_wrap(~Yvar, scales="free_y", ncol=1) 
#   
# print(p)

d <- readRDS(here("data/compiled_clean_MICS_survey.rds"))

d$EC_cfu_H[d$EC_cfu_H>101] <-NA
d$EC_cfu_S[d$EC_cfu_S>101] <-NA


# p<- ggplot(d, aes(x=EC_cfu_H, y=haz)) + geom_smooth(se=F) + geom_point(alpha=0.1) +
#   scale_x_continuous(trans='log10') 
#   
# print(p)
# 
# p2<- ggplot(d, aes(x=EC_cfu_H, y=haz)) + geom_smooth(se=F)
#   scale_x_continuous(trans='log10') 
# 
# print(p2)
# 
# 
# p3<- ggplot(d, aes(x=EC_cfu_H, y=haz)) + geom_smooth(se=F) + geom_point(alpha=0.1) +
#   coord_cartesian(ylim=c(-2,0)) +
#   scale_x_continuous(trans='log10') 
# 
# print(p3)


#Lots of 101 and 998
#101

#Figure out WQ26 and WQ27


#Why does EC_cfu_H==0 for all 4 categories of EC_risk_H?

#make plot of just 4 splines for the haz/whz and H/S combinations as a group
d1 <- d %>% filter(!is.na(EC_cfu_H) & !is.na(haz)) %>% select(EC_cfu_H, haz) %>% rename(ec=EC_cfu_H, Z=haz) %>% mutate(anthro="Outcome: HAZ", group="Household E. coli concentration")
d2 <- d %>% filter(!is.na(EC_cfu_H) & !is.na(whz)) %>% select(EC_cfu_H, whz) %>% rename(ec=EC_cfu_H, Z=whz) %>% mutate(anthro="Outcome: WHZ", group="Household E. coli concentration")
d3 <- d %>% filter(!is.na(EC_cfu_S) & !is.na(haz)) %>% select(EC_cfu_S, haz) %>% rename(ec=EC_cfu_S, Z=haz) %>% mutate(anthro="Outcome: HAZ", group="Source E. coli concentration")
d4 <- d %>% filter(!is.na(EC_cfu_S) & !is.na(whz)) %>% select(EC_cfu_S, whz) %>% rename(ec=EC_cfu_S, Z=whz) %>% mutate(anthro="Outcome: WHZ", group="Source E. coli concentration")

df <- bind_rows(d1, d2, d3, d4)
head(df)

p4<- ggplot(df, aes(x=ec, y=Z, group=group, color=group)) + geom_smooth() +
  #coord_cartesian(ylim=c(-2,0)) +
  facet_wrap(~anthro, scales="free_y", ncol=1) +
  scale_x_continuous(trans='log10') + theme(legend.position = "bottom", legend.title = element_blank()) + 
  ylab("Child Z-score") + xlab("Log E. coli concentrations")

print(p4)


saveRDS(p4, file=here("figures/ec-conc-figure-object.RDS"))
