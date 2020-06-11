
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
  d <- read_dta(data_path(path))
  lab<-makeVlist(d)
  write.csv(lab, here::here(paste0("codebooks/",country,"_vars.csv")))
  d <- data.frame(d, country= country)
  return(d)
}







