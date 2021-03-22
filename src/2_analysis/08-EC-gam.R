

rm(list=ls())
source("0-config.R")

#load clean data
d <- readRDS(here("data/compiled_clean_MICS_survey.rds"))

d <- d %>% rename(EC_cfu_H=EC_100_H, EC_cfu_S=EC_100_S)

d$EC_cfu_H[d$EC_cfu_H>101] <-NA
d$EC_cfu_S[d$EC_cfu_S>101] <-NA

d$clusterid <- d$clust_num

Wvars <- c("country",Wvars)

#continuous
res_haz_s <- fit_RE_gam(d=d, X="EC_cfu_S", Y="haz",  W=Wvars)
#res_whz_s <- fit_RE_gam(d=d, X="EC_cfu_S", Y="whz",  W=Wvars)
res_haz_h <- fit_RE_gam(d=d, X="EC_cfu_H", Y="haz",  W=Wvars)
#res_whz_h <- fit_RE_gam(d=d, X="EC_cfu_H", Y="whz",  W=Wvars)


#binary
res_diar_s <- fit_RE_gam(d=d, X="EC_cfu_S", Y="diarrhea",  W=Wvars, family="binomial")
res_diar_H <- fit_RE_gam(d=d, X="EC_cfu_H", Y="diarrhea",  W=Wvars, family="binomial")

simul_plot_haz_s  <- gam_simul_CI(res_haz_s$fit, res_haz_s$dat, xlab="", ylab="", title="")
simul_plot_haz_h  <- gam_simul_CI(res_haz_h$fit, res_haz_h$dat, xlab="", ylab="", title="")
simul_plot_diar_s  <- gam_simul_CI(res_diar_s$fit, res_diar_s$dat, xlab="", ylab="", title="")
simul_plot_diar_h  <- gam_simul_CI(res_diar_H$fit, res_diar_H$dat, xlab="", ylab="", title="")

simul_plot_df <- bind_rows(
  data.frame(Xvar="EC_cfu_S", Yvar="haz", simul_plot_haz_s$pred),
  data.frame(Xvar="EC_cfu_H", Yvar="haz", simul_plot_haz_h$pred),
  data.frame(Xvar="EC_cfu_S", Yvar="diarrhea", simul_plot_diar_s$pred),
  data.frame(Xvar="EC_cfu_H", Yvar="diarrhea", simul_plot_diar_h$pred)
)

save(simul_plot_df, res_haz_s, res_haz_h, res_diar_s, res_diar_H, file=here("results/adjusted_gams.Rdata"))


