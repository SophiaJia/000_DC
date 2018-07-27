library(tidyverse)
library(magrittr)
library(janitor)
library(stringdist)
library(lubridate)


CheckRecord <- function(Dat){
  
  nodup = duplicated(Org)  %>% table %>% length() == 1
  if (nodup){
    print("There is no duplicated records")
  }else{
    unique(Dat)
    print("Duplicates moved out")
  }
}

colnames(Org)

# check duplicates 
# check unwanted rows
CheckRecord(Org)
# Basic demo 
# age , gender, ECOG, Charlson index, histology, grade, t stage, m stage, n stage, Ki67, 

dicList <- matrix(list(), 17, 6)
dicList[1,1][[1]] <- c("age")


IsNumeric function 

select_if(x, IsNumeric)





outList <-
  
  dic <- c("AGE", "GENDER", "")  

Cname <- colnames(Org) %>% toupper
amatch(Cname %>% toupper,dic)

colm <- matrix(list(), length(Cname), 6)
colm[,1] <- Cname

# age 
grep("AGE", Cname, value = TRUE)
Org[,grep("AGE", Cname)]

apply(Org[,grep("AGE", Cname)],2,parse_number)     

apply(Org,2,all.is.numeric)     

all.is.numeric(Org)


sapply(Org,mode) %>% table
sapply(Org,class) %>% table
sapply(D,mode) %>% table
sapply(D,class) %>% table
tmp <- sapply(Org,parse_number) %>% as.tibble()
tmp %>% select_if(MissLt)

MissLt <- function(x, ratio = 0.5){
  sum(is.na(x))/length(x) < ratio
}


colnames(tmp)[apply(tmp, 2, MissLt)] 


## convert number columns to numbers, and remember to exclude date. 


colnames(tmp)[sapply(Org, function(x) !all(is.na(as.Date(as.character(x),format = c("%d/%m/%Y", "%d-%m-%Y", "%Y/%m/%d", "%Y-%m-%d")))))]


IS.Number <- function(x, ratio = 0.5){
  y = parse_number(x)
  sum(is.na(y))/length(y) < ratio
}

AS.Number <- function()

IS.Date  <- function(x, addformat = NULL ){
  format = c("%m/%d/%Y", "%m-%d-%Y", "%Y/%m/%d", "%Y-%m-%d")
  y <- as.Date(as.character(x),format= format)
  MissLt(y,ratio = 0.99)
}


If IS.Date, then parse Data. 



fixDate<- function(Data){
  Data %>% mutate_if(IS.Date, mdy)
  #last.warning %>% names
}

ttttmp <- fixDate(Org)


IS.Date(D$Adjuvant.chemotherapy)

y <- as.Date(as.character(Org$date.of.relapse),format= format)

sum(is.na(y))/length(y)
AS.Date <- function

IS.Factor 

inherits
inherits(Org$date.of.relapse, 'Date')


fixDate

sapply(ttttmp, class) %>% table

########################################
IS.Number <- function(x, ratio = 0.5){
  y = parse_number(x)
  sum(is.na(y))/length(y) < ratio
}

AS.Number<- function(Data){
  Data %>% mutate_if(IS.Number|!IS.Date, parse_number)
  #last.warning %>% names
}

tmpp <- AS.Number(ttttmp)




read.table("H:/NCDBPUF_Mesothel.0.2013.2.dat", 
           header=TRUE, skip=3)


Ntmp <- readLines("H:/NCDBPUF_Mesothel.0.2013.2.dat")



