

rm(list=ls())
source("0-config.R")

d <- readRDS(here("data/compiled_intermediate_MICS_survey.rds"))


#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Clean covariate missingness and collapse factor levels:
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

# #factor to clean variables to easier to recode levels
# clean_factor = function(x){
#   x<-iconv(x, "UTF-8", "UTF-8",sub='')
#   x <- tolower(as.character(x))
#   x <- gsub(" ","",x)
#   x <- gsub("-",".",x)
#   x <- gsub("/",".",x)
#   x <- gsub(",",".",x)
#   x <- gsub("\\'",".",x)
#   x <- gsub("\\'",".",x)
#   x <- gsub("\\(",".",x)
#   x <- gsub("\\)",".",x)
#   x <- gsub("\\+",".plus",x)
#   x <- fct_explicit_na(x, "missing")
#   print(paste(unique(x), collapse = ' = "",\n') %>% cat())
#   return(x)
# }

#----------------------------------------------------------------------
# education
#----------------------------------------------------------------------
#d$educ<-clean_factor(d$educ)
unique(d$educ)

d$educ <- recode(d$educ, 
                 "0" = "none",
                 "1" =  "primary",
                 "2" = "secondary", 
                 "3" = "higher",
                 "4" = "higher",
                 "5" = "higher",
                 "6" = "higher",
                 "9" = "missing",
                 "99" = "missing",
                 .default = "missing")

d$educ <- factor(d$educ, levels = c("none","primary","secondary", "higher","missing"))

table(d$educ)
prop.table(table(d$educ))

#----------------------------------------------------------------------
# floor
#----------------------------------------------------------------------
#d$floor <- clean_factor(d$floor)

#coded from https://dhsprogram.com/pubs/pdf/AS61/AS61.pdf
unique(d$floor)



"improved"
 "missing"
"unimproved"

d$floor <- recode(d$floor, 
                  "11" = "unimproved",
                  "12" = "unimproved",
                  "13" = "unimproved",
                  "21" = "rudimentary",
                  "22" = "rudimentary",
                  "23" = "rudimentary",
                  "24" = "rudimentary",
                  "25" = "rudimentary",
                  "26" = "rudimentary",
                  "31" = "improved",
                  "32" = "improved",
                  "33" = "improved",
                  "34" = "improved",
                  "35" = "improved",
                  "36" = "improved",
                  "37" = "improved",
                  "38" = "improved",
                  "39" = "improved",
                  "96" = "missing",
                  "98" = "missing",
                  "99" = "missing",
                  .default = "missing")

d$floor <- factor(d$floor, levels = c("unimproved","rudimentary","improved","missing"))

table(d$floor)

#----------------------------------------------------------------------
# cookstove
#----------------------------------------------------------------------
unique(d$cookstove)

#d$cookstove <- clean_factor(d$cookstove)
d$cookstove <- recode(d$cookstove, 
                      "1" = "improved",
                      "10" = "improved",
                      "2" = "improved",
                      "3" = "improved",
                      "4" = "improved",
                      "5" = "improved",
                      "6" = "improved",
                      "7" = "unimproved",
                      "8" = "unimproved",
                      "9" = "unimproved",
                      "96" = "missing",
                      "97" = "missing",
                      "99" = "missing",
                      .default = "missing")
d$cookstove <- factor(d$cookstove, levels = c("unimproved","improved","missing"))

table(d$cookstove)


#----------------------------------------------------------------------
# roof
#----------------------------------------------------------------------
unique(d$roof)

#d$roof <- clean_factor(d$roof)
d$roof <- recode(d$roof, 
                 "11" = "unimproved",
                 "12" = "unimproved",
                 "13" = "unimproved",
                 "14" = "unimproved",
                 "21" = "unimproved",
                 "23" = "unimproved",
                 "24" = "unimproved",
                 "25" = "unimproved",
                 "26" = "unimproved",
                 "31" = "improved",
                 "32" = "improved",
                 "33" = "improved",
                 "34" = "improved",
                 "35" = "improved",
                 "36" = "improved",
                 "37" = "improved",
                 "38" = "improved",
                 "96" = "missing",
                 "99" = "missing",
                 .default = "missing")
d$roof <- factor(d$roof, levels = c("unimproved","improved","missing"))

table(d$roof)

#----------------------------------------------------------------------
# chimney, fan
#----------------------------------------------------------------------
unique(d$chimney)

d$chimney <- fct_explicit_na(d$chimney, "missing")
#d$fan <- fct_explicit_na(d$fan, "missing")


d <- d %>% mutate(
  chimney = case_when(
    chimney=="missing" ~ "missing",  
    chimney=="2" ~ "no",  
    chimney=="1" ~ "yes",  
    chimney=="8" ~ "missing",  
    chimney=="9" ~ "missing"
  ))


table(d$chimney)

#----------------------------------------------------------------------
# wall
#----------------------------------------------------------------------

#Note: wall material variable varies between studies
#some of these are electricity

table(d$wall)
unique(d$wall)

#d$wall <- clean_factor(d$wall)
d$wall <- recode(d$wall, 
                 "11" = "natural",
                 "12" = "natural",
                 "13" = "natural",
                 "14" = "natural",
                 "15" = "natural",
                 "16" = "natural",
                 "21" = "rudimentary",
                 "22" = "rudimentary",
                 "23" = "rudimentary",
                 "24" = "rudimentary",
                 "25" = "rudimentary",
                 "26" = "rudimentary",
                 "27" = "rudimentary",
                 "28" = "rudimentary",
                 "29" = "rudimentary",
                 "31" = "finished",
                 "32" = "finished",
                 "33" = "finished",
                 "34" = "finished",
                 "35" = "finished",
                 "36" = "finished",
                 "37" = "finished",
                 "38" = "finished",
                 "39" = "finished",
                 "40" = "finished",
                 "41" = "finished",
                 "96" = "missing",
                 "99" = "missing",
                 .default = "missing")
d$roof <- factor(d$roof, levels = c("natural","rudimentary","finished","missing"))

table(d$wall)


#----------------------------------------------------------------------
# fuel
#----------------------------------------------------------------------

unique(d$fuel)

#https://dhsprogram.com/data/Guide-to-DHS-Statistics/Cooking_Fuel.htm
#Solid fuels include coal/lignite, charcoal, wood, straw/shrub/grass, agricultural crops, and animal dung. 
#Clean fuels include electricity, liquefied petroleum gas (LPG), natural gas, and biogas.

#d$fuel <- clean_factor(d$fuel)
d$fuel <- recode(d$fuel, 
                 "10" ="solid",
                 "11" ="solid",
                 "4" ="solid",
                 "6" ="solid",
                 "7" ="solid",
                 "8" ="solid",
                 "9" ="solid",
                 "1" ="clean",
                 "12" ="clean",
                 "2" ="clean",
                 "3" ="clean",
                 "5" ="clean",
                 "missing" = "95",
                 "missing" = "96",
                 "missing" = "99",
                 .default = "missing")
d$fuel <- factor(d$fuel, levels = c("solid","clean","missing"))

table(d$fuel)


#----------------------------------------------------------------------
# other covariates
#----------------------------------------------------------------------

table(d$sex)

table(d$aged==9999)
d$aged[d$aged==9999] <- NA
table(1*is.na(d$aged),is.na(d$haz)) #age is not missing for any anthro measurement

summary(d$mage)

table(d$birthord)
class(d$birthord)

table(d$rural)
d$rural <- as.factor(d$rural)
#d$fan <- as.factor(d$fan)
d$chimney <- as.factor(d$chimney)

table(d$everbf)
table(d$currbf)
table(is.na(d$everbf))
table(is.na(d$currbf))
d$everbf <- fct_explicit_na(d$everbf, "missing")
d$currbf <- fct_explicit_na(d$currbf, "missing")


table(d$own_animals)
table(is.na(d$own_animals))
d$own_animals <- fct_explicit_na(d$own_animals, "missing")

table(d$everbf)
table(d$currbf)
table(d$everbf, d$currbf)
cor(as.numeric(d$everbf), as.numeric(d$currbf))


d$nroom_sleeping <- as.numeric(d$nroom_sleeping)
d$birthord <- as.numeric(d$birthord)


saveRDS(d, here("data/compiled_clean_MICS_survey.rds"))
