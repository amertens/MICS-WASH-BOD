
source("0-config.R")

d <- readRDS(here("results/pooled_POC_results.rds"))
dtmle <- readRDS(here("results/pooled_POC_tmle_results.rds"))


head(d)

d %>% filter(country=="Bangladesh", adjusted==0, binary==1) %>% 
  ggplot(., aes(y=est, x=X)) +
    facet_wrap(~Y) +
    geom_point() + 
    geom_linerange(aes(ymin=ci.lb, ymax=ci.ub )) +
    geom_hline(yintercept = 1) +
    scale_y_continuous(breaks=c(0.25, 0.5,1, 2, 4, 8), trans='log10', labels=scaleFUN) +
    coord_flip() +



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








