
source("0-config.R")

d <- readRDS(here("results/unadjusted_RR.rds"))


head(d)

ggplot(d, aes(y=coef)) +
  geom_point() + 
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub ))







