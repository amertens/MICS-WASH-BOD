
source("0-config.R")

d1 <- read_dta(data_path("Bangladesh/Bangladesh_cleaned.dta"))
d1 <- as_factor(d1)




lab<-makeVlist(d1)
write.csv(lab, here("codebooks/BD_vars.csv"))

#get labels
labs<-str(d1)

head(d1)


d2 <- read_dta(data_path("Congo/Congo_cleaned.dta"))

head(d2)


colnames(d1)
colnames(d2)


