
source("0-config.R")

d <- readRDS(here("results/pooled_POC_results.rds"))


head(d)

d %>% filter(country=="Bangladesh", adjusted==0, binary==0) %>% 
  ggplot(., aes(y=est, x=X)) +
    facet_wrap(~Y) +
    geom_point() + 
    geom_linerange(aes(ymin=ci.lb, ymax=ci.ub )) +
    scale_y_continuous(breaks=c(0.25, 0.5,1, 2, 4, 8), trans='log10', labels=scaleFUN)
    coord_flip() 







