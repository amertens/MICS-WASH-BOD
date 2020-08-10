
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
    rename(LN=HH26B)
  #load and merge child health
  ch <- read_sav(data_path(ch_path))
  ch <- ch %>% subset(., select = -c(LN)) %>% rename(clust_num=HH1, HH_num=HH2, LN=UF3)
  #subset to relevant CH variables
  if(!is.null(ch$CAGED)){
    ch <- ch %>% subset(., select = c(clust_num, HH_num, LN, HL4, CAGED, BD2, BD3, HAZ2, WAZ2, WHZ2))
  }else{
    ch <- ch %>% subset(., select = c(clust_num, HH_num, LN, HL4, CAGE, BD2, BD3, HAZ2, WAZ2, WHZ2)) %>%
      mutate(CAGED=CAGE*30.4167) %>%
      subset(., select = -c(CAGE))
  }
  try(bh <- read_sav(data_path(bh_path)))
  try(bh <- bh %>% subset(., select = -c(LN)) %>% rename(clust_num=HH1, HH_num=HH2, LN=BH8))
  #subset to relevant BH variables
  try(bh <- bh %>% subset(., select = c(clust_num, HH_num, LN, brthord))) 
  
  #lab<-makeVlist(d)
  #write.csv(lab, here::here(paste0("codebooks/",country,"_vars.csv")))
  
  dim(ch)
  dim(d)
  d <- full_join(ch, d, by = c("clust_num","HH_num","LN"))
  try(df <- left_join(d, bh, by = c("clust_num","HH_num","LN")))
  dim(d)
  d <- data.frame(d, country= country)
  d <- d %>%
    mutate_all(as.character)
  return(d)
}







