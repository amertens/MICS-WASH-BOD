



source(here::here("0-config.R"))
library(viridis)

d <- readRDS(here("data/compiled_clean_POC_survey.rds"))
d$mort <- as.numeric(d$mort)

sparse_tab <-function(X,Y){
 res <- d %>%
    tabyl(!!as.name(X), !!as.name(Y), country, show_na = FALSE) %>%
    bind_rows(., .id="country") 
  colnames(res)[2] = "Xlevel"
  res$X=X
  res$Y=Y
  res
}



Xvars <- c("san_imp_cat", "wat_imp_cat", "hyg_imp_cat", "EC_risk_H", "EC_risk_S", "WASH", "WASH_noEC")

#Xvars <- c("san_imp", "wat_imp", "hyg_imp", "EC_H", "EC_S", "WASH", "WASH_noEC")

Yvars <- c("diarrhea", "stunt", "wast", "ari", "mort")
fullres <- NULL
for(i in Xvars){
  for(j in Yvars){
    res <- sparse_tab(X=i, Y=j)  
    fullres <- bind_rows(fullres, res)
  }
}


##Fill missing Exposure-Outcome  combinations as NA so they appear as grey 
plotdf <- fullres  %>% 
  tidyr::expand(country, X, Y) %>%
  left_join(fullres, by = c("country", "X", "Y")) %>%
  ungroup()

plotdf <- plotdf %>% subset(., select =c(country, `0`, `1`, X, Y, Xlevel))%>%
  rowwise() %>% mutate(min_cell = min(`0`, `1`, na.rm=T)) %>%
  group_by(country, X, Y) %>% summarize(N_na = sum(is.na(min_cell)), min_cell=min(min_cell)) %>%
  mutate(
    #X=factor(X, levels = c("EC_H", "EC_S", "safely_manH20", "wat_imp", "san_imp", "hyg_imp", "WASH", "WASH_noEC")),
    X=factor(X, levels = c("EC_risk_H", "EC_risk_S", "safely_manH20", "wat_imp_cat", "san_imp_cat", "hyg_imp_cat", "WASH", "WASH_noEC")),
    Y=factor(Y, levels = rev(c("diarrhea", "stunt", "wast", "ari", "mort"))),
    Xcat = case_when(X=="EC_H" ~ "Uncontaminated\nHH water", 
                     X=="EC_S" ~ "Uncontaminated\nsource water", 
                     X=="san_imp" ~ "Improved\nsanitation", 
                     X=="wat_imp" ~ "Improved\nwater supply", 
                     X=="hyg_imp" ~ "Improved\nhygiene", 
                     X=="WASH_noEC" ~ "Improved\nWASH",
                     X=="WASH" ~ "Improved WASH\nfree of contamination",
                     X=="safely_manH20" ~ "Safely managed\ndrinking water",
                    X=="EC_risk_H" ~ "HH water\ncontamination level", 
                    X=="EC_risk_S" ~ "Source water\ncontamination level", 
                    X=="san_imp_cat" ~ "Sanitation\ncategory", 
                    X=="wat_imp_cat" ~ "Water supply\ncategory", 
                    X=="hyg_imp_cat" ~ "Hygiene category"),
    Ycat = case_when(
      Y=="diarrhea" ~ "Diarrhea", 
      Y=="stunt" ~ "Stunted", 
      Y=="wast" ~ "Wasted", 
      Y=="ari" ~ "Acute respiratory infection",
      Y=="mort" ~ "Mortality"
    )
  ) %>%
  arrange(Y, X, N_na) %>%
  mutate(`Exposure-Outcome combination`=paste0(X,"-",Y),
         country = factor(country, levels = unique(country)),
         `Exposure-Outcome combination`=factor(`Exposure-Outcome combination`, levels=unique(`Exposure-Outcome combination`)),
         Xcat=factor(Xcat, levels=unique(Xcat)), Ycat=factor(Ycat, levels=unique(Ycat)))

plotdf$N_cat <- factor(cut(plotdf$min_cell, c(0, 10,25,50,100,200,10000), right=F, labels = c("<10","10-25","25-50","50-100","100-200",">200")))

plotdf$N_cat <- addNA(plotdf$N_cat)
table(plotdf$N_cat)

head(plotdf)

#Set plot colors
viridis_cols = c(viridis(
  n = length(levels(plotdf$N_cat)) - 1,
  alpha = 1,
  begin = 0,
  end = 0.8,
  direction = -1,
  option = "C"
),
"grey90")

  
  
p <- ggplot(plotdf,aes(x=country, y=Ycat, fill=N_cat)) +
  geom_tile(aes(fill=N_cat),size=0.25, color="grey80") +
  labs(x="Country",y="Exposure-Outcome combination") +
  scale_fill_manual(na.value="grey90", 
                    guide=guide_legend(title="N of\nsmallest\ncell",title.vjust = 1,
                                       label.position="bottom",label.hjust=0.1,nrow=1),
                    values = viridis_cols)   +
  facet_grid(Xcat ~ ., scales = "free", space="free") +
  scale_x_discrete(expand=c(0,0))+
  scale_y_discrete(expand=c(0,0))+
  theme_grey(base_size=10) +
  theme(
    #aspect.ratio = 1,
    legend.title=element_text(color="grey20",size=8),
    legend.margin = margin(grid::unit(0.1,"cm")),
    legend.text=element_text(colour="grey20",size=7,face="bold"),
    legend.key.height=grid::unit(0.2,"cm"),
    legend.key.width=grid::unit(1,"cm"),
    legend.position = "right",
    axis.text.x=element_text(size=8,colour="grey20",angle=45,hjust=1),
    axis.text.y=element_text(size=8,vjust = 0.2,colour="grey20"),
    axis.ticks=element_line(size=0.4),
    plot.title=element_text(colour="grey20",hjust=0,size=12,face="bold"),
    strip.text.x = element_text(size=10),
    strip.text.y = element_text(angle=0,size=10),
    plot.background=element_blank(),
    panel.border=element_blank(),
    strip.background = element_blank(),
    panel.background=element_rect(fill="grey80", colour="grey80"),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank()
  ) +
  guides(fill = guide_legend("Rarest cell\nnumber", ncol=1)) 

p

# save plot 
ggsave(p, file=paste0(here::here(), "/figures/fig-sparsity-heatmap-POC.png"), height=10, width=5)





