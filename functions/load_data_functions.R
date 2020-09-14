
#load from data path
data_path = function(path){
  paste0(data_dir, path)
}

#extract stata labels
makeVlist <- function(dta) { 
  labels <- sapply(dta, function(x) attr(x, "label"))
  labs <- tibble(name = names(labels),
                 label = labels)
  labs$label<- as.character(labs$label)
  return(labs)
}


load_MICS_dataset <- function(country){
  path=paste0(country,"/",country,"_cleaned.dta")
  ch_path=paste0(country,"/ch.sav")
  bh_path=paste0(country,"/bh.sav")
  d <- read_dta(data_path(path)) %>%
    rename(HH.LN=HH26B)
  #load and merge child health
  ch <- read_sav(data_path(ch_path))
  ch <- ch %>% rename(clust_num=HH1, HH_num=HH2, HH.LN=LN, childLN=UF3)
  #subset to relevant CH variables
  if(!is.null(ch$CAGED)){
    ch <- ch %>% subset(., select = c(clust_num, HH_num, HH.LN, childLN, HL4, CAGED, BD2, BD3, HAZ2, WAZ2, WHZ2, HAZFLAG, WAZFLAG, WHZFLAG, CA1,CA14,CA16,CA17,CA18,CA20
    ))

    
  }else{
    ch <- ch %>% subset(., select = c(clust_num, HH_num, HH.LN, childLN, HL4, CAGE, BD2, BD3, HAZ2, WAZ2, WHZ2, HAZFLAG, WAZFLAG, WHZFLAG, CA1,CA14,CA16,CA17,CA18,CA20)) %>%
      mutate(CAGED=CAGE*30.4167) %>%
      subset(., select = -c(CAGE))
  }
  try(bh <- read_sav(data_path(bh_path)))
  try(bh <- bh  %>% rename(clust_num=HH1, HH_num=HH2, HH.LN=LN, childLN=BH8))
  #subset to relevant BH variables
  try(bh <- bh %>% subset(., select = c(clust_num, HH_num, HH.LN, childLN, brthord))) 
  
  #lab<-makeVlist(d)
  #write.csv(lab, here::here(paste0("codebooks/",country,"_vars.csv")))
  
  dim(ch)
  dim(d)
  d2 <- full_join(ch, d, by = c("clust_num","HH_num"))

  
  table(is.na(bh$brthord))
  try(df <- left_join(d2, bh, by = c("clust_num","HH_num","childLN")))
  dim(df)
  table(is.na(df$brthord))
  
  df <- data.frame(df, country= country)
  df <- df %>%
    mutate_all(as.character)
  return(df)
}







