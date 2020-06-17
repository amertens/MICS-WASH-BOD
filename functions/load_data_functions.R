
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
  d <- read_dta(data_path(path))
  #load and merge child health
  ch <- read_sav(data_path(ch_path))
  ch <- ch %>% rename(clust_num=HH1, HH_num=HH2)
  
  lab<-makeVlist(d)
  write.csv(lab, here::here(paste0("codebooks/",country,"_vars.csv")))
  
  dim(ch)
  dim(d)
  d <- full_join(ch, d, by = c("clust_num","HH_num"))
  dim(d)
  d <- data.frame(d, country= country)
  d <- d %>%
    mutate_all(as.character)
  return(d)
}







