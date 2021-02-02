


source(here::here("0-config.R"))

d <- readRDS(here("data/compiled_clean_MICS_survey.rds"))
d <- d %>% filter()
head(d)
colnames(d)

d$EC_cfu_H[d$EC_cfu_H>101] <-NA

d$EC_cfu_H[d$EC_cfu_H=101] <- 200


p<- ggplot(d, aes(x=EC_cfu_H, y=haz)) + geom_smooth(se=F) + geom_point(alpha=0.1)
print(p)


#Lots of 101 and 998
#101

#Figure out WQ26 and WQ27


#Why does EC_cfu_H==0 for all 4 categories of EC_risk_H?