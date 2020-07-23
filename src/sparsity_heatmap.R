



source(here::here("0-config.R"))

dfull <- readRDS(here("data/compiled_raw_MICS_survey.rds"))

d <- dfull %>% mutate(
  EC_risk_H = 
    case_when(
      EC_risk_H_1 ==100 ~ 1,   
      EC_risk_H_2 ==100 ~ 2,   
      EC_risk_H_3 ==100 ~ 3,   
      EC_risk_H_4 ==100 ~ 4,
      TRUE ~ NA_real_
    ),
  EC_risk_S = 
    case_when(
      EC_risk_S_1 ==100 ~ 1,   
      EC_risk_S_2 ==100 ~ 2,   
      EC_risk_S_3 ==100 ~ 3,   
      EC_risk_S_4 ==100 ~ 4,
      TRUE ~ NA_real_   
    ),
  stunt = 1*(HAZ2 < -2 ),
  wast = 1*(WHZ2 < -2 )
)

table(d$EC_risk_S)
table(d$EC_risk_H)
table(is.na(d$EC_risk_S))
table(is.na(d$EC_risk_H))
prop.table(table(is.na(d$EC_risk_S)))
prop.table(table(is.na(d$EC_risk_H)))

#code handwashing
#Only households where handwashing facility was observed by the interviewer (HW1=1, 2, 3) 
#and households with no handwashing facility (HW1=4) are included in the denominator of 
#the indicator (HW1=5, 6, and 9 [if any] are excluded). Households with water at handwashing 
#facility (HW2=1) and soap or other cleansing agent at handwashing facility 
#(HW7=A or B) are included in the numerator.

#Denominators are obtained by weighting the number of households by the total number of 
#household members (HH48).
table(d$HW1)
table(d$HW2)
table(d$HW7A)
table(d$HW7B)
table(d$HW7C)

d$hyg_imp <- ifelse(d$HW2==1 & (d$HW7A=="A"|d$HW7B=="B"), 1, 0)
d$hyg_imp[(d$HW2==9 | !(d$HW1 %in% c(1,2,3,4)))] <- NA
table(d$hyg_imp)
prop.table(table(d$hyg_imp))

#Code most-improved WASH
d$WASH <- ifelse(d$san_imp==1 & d$wat_imp==1 & d$EC_risk_H==4, 1, 0)

#code any contamination
d$EC_S <- ifelse(d$EC_risk_S==1, 1, 0)
d$EC_S[is.na(d$EC_risk_S)] <- NA
d$EC_H <- ifelse(d$EC_risk_H==1, 1, 0)
d$EC_H[is.na(d$EC_risk_H)] <- NA

#Code safely managed
d$safely_manH20 <- ifelse(d$EC_H==1 & d$wat_imp==1, 1, 0)
d$safely_manH20[is.na(d$EC_H)|is.na(d$wat_imp)] <- NA
table(d$safely_manH20)
table(d$country, d$safely_manH20)


#Rename variables
d <- d %>% rename(
  diarrhea=CA1,
  fever=CA14,
  cough=CA16,
  diff_breath=CA17,
  congestion=CA18,
  resp_healthcare=CA20
) 

d <- d %>% subset(., select = c(country, san_imp, wat_imp, hyg_imp, san_cat, safely_manH20, EC_S, EC_H, EC_risk_S, EC_risk_H, WASH, diarrhea, fever, cough, resp_healthcare, diff_breath, congestion, stunt, wast))

head(d)

#ARI symptoms
table(d$diff_breath)
table(d$congestion)
table(d$cough)

table(d$cough, d$congestion)

#number of children with symptoms of ARI: 
#Those who had an illness with a cough (CA16=1), 
#accompanied by a rapid or difficult breathing (CA17=1) 
#and whose symptoms were due to a problem in the chest, 
#or both a problem in the chest and a blocked nose (CA18=1 or 3).

# d$ari <- ifelse(d$diff_breath==1 & d$congestion %in% c(1,3) & d$cough==1, 1, 0)
# d$ari[(d$diff_breath==8 | d$diff_breath==9) &
#       (d$congestion==8 | d$congestion==9) &
#       (d$cough==8 | d$cough==9)] <- NA
# table(d$ari)
# table(d$country, d$ari)


#(cough OR rapid/difficult breathing) AND problem in chest
d$ari <- ifelse(d$congestion %in% c(1,3) & (d$diff_breath==1 | d$cough==1), 1, 0)
d$ari[(d$diff_breath==8 | d$diff_breath==9) &
        (d$congestion==8 | d$congestion==9) &
        (d$cough==8 | d$cough==9)] <- NA
table(d$ari)
table(d$country, d$ari)


df1<- d %>%
  tabyl(san_imp, diarrhea, country, show_na = FALSE) %>%
  bind_rows(., .id="country") %>%
  mutate(X="san_imp", Y="diarrhea") %>% mutate(Xlevel=as.character(san_imp))

df2<- d %>%
  tabyl(wat_imp, diarrhea, country, show_na = FALSE) %>%
  bind_rows(., .id="country") %>%
  mutate(X="wat_imp", Y="diarrhea") %>% mutate(Xlevel=as.character(wat_imp))

df3<- d %>%
  tabyl(hyg_imp, diarrhea, country, show_na = FALSE) %>%
  bind_rows(., .id="country") %>%
  mutate(X="hyg_imp", Y="diarrhea") %>% mutate(Xlevel=as.character(hyg_imp))

df4<- d %>%
  tabyl(EC_S, diarrhea, country, show_na = FALSE) %>%
  bind_rows(., .id="country") %>%
  mutate(X="EC_S", Y="diarrhea") %>% mutate(Xlevel=as.character(EC_S))

df5<- d %>%
  tabyl(EC_H, diarrhea, country, show_na = FALSE) %>%
  bind_rows(., .id="country") %>%
  mutate(X="EC_H", Y="diarrhea") %>% mutate(Xlevel=as.character(EC_H))

df6<- d %>%
  tabyl(WASH, diarrhea, country, show_na = FALSE) %>%
  bind_rows(., .id="country") %>%
  mutate(X="WASH", Y="diarrhea") %>% mutate(Xlevel=as.character(WASH))

df7<- d %>%
  tabyl(safely_manH20, diarrhea, country, show_na = FALSE) %>%
  bind_rows(., .id="country") %>%
  mutate(X="safely_manH20", Y="diarrhea") %>% mutate(Xlevel=as.character(safely_manH20))

diar <- bind_rows(df1, df2, df3, df4, df5, df6, df7)


df1<- d %>%
  tabyl(san_imp, resp_healthcare, country, show_na = FALSE) %>%
  bind_rows(., .id="country") %>%
  mutate(X="san_imp", Y="resp_healthcare") %>% mutate(Xlevel=as.character(san_imp))

df2<- d %>%
  tabyl(wat_imp, resp_healthcare, country, show_na = FALSE) %>%
  bind_rows(., .id="country") %>%
  mutate(X="wat_imp", Y="resp_healthcare") %>% mutate(Xlevel=as.character(wat_imp))

df3<- d %>%
  tabyl(hyg_imp, resp_healthcare, country, show_na = FALSE) %>%
  bind_rows(., .id="country") %>%
  mutate(X="hyg_imp", Y="resp_healthcare") %>% mutate(Xlevel=as.character(hyg_imp))

df4<- d %>%
  tabyl(EC_S, resp_healthcare, country, show_na = FALSE) %>%
  bind_rows(., .id="country") %>%
  mutate(X="EC_S", Y="resp_healthcare") %>% mutate(Xlevel=as.character(EC_S))

df5<- d %>%
  tabyl(EC_H, resp_healthcare, country, show_na = FALSE) %>%
  bind_rows(., .id="country") %>%
  mutate(X="EC_H", Y="resp_healthcare") %>% mutate(Xlevel=as.character(EC_H))

df6<- d %>%
  tabyl(WASH, resp_healthcare, country, show_na = FALSE) %>%
  bind_rows(., .id="country") %>%
  mutate(X="WASH", Y="resp_healthcare") %>% mutate(Xlevel=as.character(WASH))

df7<- d %>%
  tabyl(safely_manH20, resp_healthcare, country, show_na = FALSE) %>%
  bind_rows(., .id="country") %>%
  mutate(X="safely_manH20", Y="resp_healthcare") %>% mutate(Xlevel=as.character(safely_manH20))

resp_healthcare <- bind_rows(df1, df2, df3, df4, df5, df6, df7)



df1<- d %>%
  tabyl(san_imp, ari, country, show_na = FALSE) %>%
  bind_rows(., .id="country") %>%
  mutate(X="san_imp", Y="ari") %>% mutate(Xlevel=as.character(san_imp))

df2<- d %>%
  tabyl(wat_imp, ari, country, show_na = FALSE) %>%
  bind_rows(., .id="country") %>%
  mutate(X="wat_imp", Y="ari") %>% mutate(Xlevel=as.character(wat_imp))

df3<- d %>%
  tabyl(hyg_imp, ari, country, show_na = FALSE) %>%
  bind_rows(., .id="country") %>%
  mutate(X="hyg_imp", Y="ari") %>% mutate(Xlevel=as.character(hyg_imp))

df4<- d %>%
  tabyl(EC_S, ari, country, show_na = FALSE) %>%
  bind_rows(., .id="country") %>%
  mutate(X="EC_S", Y="ari") %>% mutate(Xlevel=as.character(EC_S))

df5<- d %>%
  tabyl(EC_H, ari, country, show_na = FALSE) %>%
  bind_rows(., .id="country") %>%
  mutate(X="EC_H", Y="ari") %>% mutate(Xlevel=as.character(EC_H))

df6<- d %>%
  tabyl(WASH, ari, country, show_na = FALSE) %>%
  bind_rows(., .id="country") %>%
  mutate(X="WASH", Y="ari") %>% mutate(Xlevel=as.character(WASH))

df7<- d %>%
  tabyl(safely_manH20, ari, country, show_na = FALSE) %>%
  bind_rows(., .id="country") %>%
  mutate(X="safely_manH20", Y="ari") %>% mutate(Xlevel=as.character(safely_manH20))

ari <- bind_rows(df1, df2, df3, df4, df5, df6, df7)




df1<- d %>%
  tabyl(san_imp, stunt, country, show_na = FALSE) %>%
  bind_rows(., .id="country") %>%
  mutate(X="san_imp", Y="stunt") %>% mutate(Xlevel=as.character(san_imp))

df2<- d %>%
  tabyl(wat_imp, stunt, country, show_na = FALSE) %>%
  bind_rows(., .id="country") %>%
  mutate(X="wat_imp", Y="stunt") %>% mutate(Xlevel=as.character(wat_imp))

df3<- d %>%
  tabyl(hyg_imp, stunt, country, show_na = FALSE) %>%
  bind_rows(., .id="country") %>%
  mutate(X="hyg_imp", Y="stunt") %>% mutate(Xlevel=as.character(hyg_imp))

df4<- d %>%
  tabyl(EC_S, stunt, country, show_na = FALSE) %>%
  bind_rows(., .id="country") %>%
  mutate(X="EC_S", Y="stunt") %>% mutate(Xlevel=as.character(EC_S))

df5<- d %>%
  tabyl(EC_H, stunt, country, show_na = FALSE) %>%
  bind_rows(., .id="country") %>%
  mutate(X="EC_H", Y="stunt") %>% mutate(Xlevel=as.character(EC_H))

df6<- d %>%
  tabyl(WASH, stunt, country, show_na = FALSE) %>%
  bind_rows(., .id="country") %>%
  mutate(X="WASH", Y="stunt") %>% mutate(Xlevel=as.character(WASH))

df7<- d %>%
  tabyl(safely_manH20, stunt, country, show_na = FALSE) %>%
  bind_rows(., .id="country") %>%
  mutate(X="safely_manH20", Y="stunt") %>% mutate(Xlevel=as.character(safely_manH20))

stunt <- bind_rows(df1, df2, df3, df4, df5, df6, df7)


df1<- d %>%
  tabyl(san_imp, wast, country, show_na = FALSE) %>%
  bind_rows(., .id="country") %>%
  mutate(X="san_imp", Y="wast") %>% mutate(Xlevel=as.character(san_imp))

df2<- d %>%
  tabyl(wat_imp, wast, country, show_na = FALSE) %>%
  bind_rows(., .id="country") %>%
  mutate(X="wat_imp", Y="wast") %>% mutate(Xlevel=as.character(wat_imp))

df3<- d %>%
  tabyl(hyg_imp, wast, country, show_na = FALSE) %>%
  bind_rows(., .id="country") %>%
  mutate(X="hyg_imp", Y="wast") %>% mutate(Xlevel=as.character(hyg_imp))

df4<- d %>%
  tabyl(EC_S, wast, country, show_na = FALSE) %>%
  bind_rows(., .id="country") %>%
  mutate(X="EC_S", Y="wast") %>% mutate(Xlevel=as.character(EC_S))

df5<- d %>%
  tabyl(EC_H, wast, country, show_na = FALSE) %>%
  bind_rows(., .id="country") %>%
  mutate(X="EC_H", Y="wast") %>% mutate(Xlevel=as.character(EC_H))

df6<- d %>%
  tabyl(WASH, wast, country, show_na = FALSE) %>%
  bind_rows(., .id="country") %>%
  mutate(X="WASH", Y="wast") %>% mutate(Xlevel=as.character(WASH))

df7<- d %>%
  tabyl(safely_manH20, wast, country, show_na = FALSE) %>%
  bind_rows(., .id="country") %>%
  mutate(X="safely_manH20", Y="wast") %>% mutate(Xlevel=as.character(safely_manH20))

wast <- bind_rows(df1, df2, df3, df4, df5, df6, df7)


plotdf <- bind_rows(diar, ari, stunt, wast)

##Fill missing Exposure-Outcome  combinations as NA so they appear as grey 
plotdf <- plotdf  %>% 
  tidyr::expand(country, X, Y) %>%
  left_join(plotdf, by = c("country", "X", "Y")) %>%
  ungroup()

plotdf <- plotdf %>% subset(., select =c(country, `0`, `1`, `2`, X, Y, Xlevel))%>%
  rowwise() %>% mutate(min_cell = min(`0`, `1`, `2`, na.rm=T)) %>%
  group_by(country, X, Y) %>% summarize(N_na = sum(is.na(min_cell)), min_cell=min(min_cell)) %>%
  mutate(
    X=factor(X, levels = c("EC_H", "EC_S", "safely_manH20", "wat_imp", "san_imp", "hyg_imp", "WASH")),
    Y=factor(Y, levels = rev(c("diarrhea", "stunt", "wast", "ari"))),
    Xcat = case_when(X=="EC_H" ~ "Uncontaminated\nHH water", 
                     X=="EC_S" ~ "Uncontaminated\nsource water", 
                     X=="san_imp" ~ "Improved\nsanitation", 
                     X=="wat_imp" ~ "Improved\nwater supply", 
                     X=="hyg_imp" ~ "Improved\nhygiene", 
                     X=="WASH" ~ "Improved\nWASH",
                     X=="safely_manH20" ~ "Safely managed\ndrinking water"),
    Ycat = case_when(
      Y=="diarrhea" ~ "Diarrhea", 
      Y=="stunt" ~ "Stunted", 
      Y=="wast" ~ "Wasted", 
      Y=="ari" ~ "Acute respiratory infection"
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
ggsave(p, file=paste0(here::here(), "/figures/fig-sparsity-heatmap.png"), height=10, width=10)





