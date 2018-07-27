
library("reshape2")
library(tidyverse)
library(lubridate)
longitu_test <- read.csv("testdata/longitu test_less_less.csv",stringsAsFactors = FALSE)

#add time variable (should be define by investigator, and by defaut, it is the order of the same ID)
dat<- longitu_test %>% group_by(patient.code) %>% mutate(time = row_number()) 
ttt <- FuzzyClean(dat)

#information per patient, and should be the same for each patient. 
#surpose know which variable is per patient 
# frist 30 row. 
PerPatient <- ttt[,c(1:7)]
# check if the data is cosistant within each patients. 
PerPatient %>% group_by(patient.code) %>% unique %>% nrow

PerPatient %>% group_by(patient.code) %>% unique -> m
unique(PerPatient$patient.code) %>% length
m[duplicated(m$patient.code),] %>% View

# patient charatrisisc doesn't match, please check
m[duplicated(m$patient.code),1] -> dupid
m[m$patient.code %in% dupid$patient.code,] %>% View

# to do the next step will ignore and pretent that they are the same. 

# PerPatient will work as base then merge the rest of it
pervisit <- ttt[,-c(2:7)]
nvisit <- max(pervisit$time)
# make max time colmune in total 
colnumbers <- ncol(pervisit) - 2
total_ncol <- nvisit * colnumbers 
colnames(pervisit)[c(2:colnumbers+1)]

# add columne name to the dataset

assuming <- ttt[ttt$time == 1,]
Dwide <- assuming
for (i in (1:10)){
  # need to fix the name 
  tmpd <- pervisit[pervisit$time == i,]
  colnames(tmpd)[-1] <- paste("Time",i,colnames(tmpd)[-1], sep = ".")
  Dwide <- left_join(Dwide, tmpd[-ncol(tmpd),], by = "patient.code")
}

colnames(Dwide)

long to wide
wide to long  
multiple layers, 

w <- reshape(pervisit,
             idvar = "patient.code",
             timevar = "time",
             direction = "wide")
