## 2018-05-02 demo 

library(tidyverse)
library(magrittr)
library(janitor)
library(stringdist)
library(lubridate)
library(Survout)
# input: 

Org <- read.csv("testdata/sd test_bicky.csv",stringsAsFactors = FALSE)

##input ##############
Inlist <- c("Race","Histologic.type","Age.at.diagnosis", "ECOG.Score","Charlson.Risk.Index", "Number.of.medications.at.diagnosis","Histologic.grade","KI.67...",
            "T", "M","N","Overall.Pathological.overall.staging..TNM.","Overall.clincal.staging..TNM.","No.of.hospitalizations")
Race <- c("BLACK", "WHITE","OTHER", "ASIAN")
Histology <-c("DUCTAL", "LOBULAR","MIXED", "OTHERS")
age_hard <- c(18, 100)
zero2inf <- c(0,Inf)
ecgo <-c(0,5)
Charlson <- c(0, 50)
KI.67<-c(0,100)
TNM <- c(0,5)

m <- matrix(list(), length(Inlist), 6)
colnames(m) <- c("Var","Type","Scale","Range.hard","Range.soft","other")
m[,1] <- Inlist

m[1,3][[1]] <- Race
m[2,3][[1]] <- Histology

m[3,4][[1]] <- age_hard
m[4,4][[1]] <- ecgo
m[5,4][[1]] <- Charlson
m[6,4][[1]] <- zero2inf 
m[8,4][[1]] <- KI.67
m[13,4][[1]] <- TNM
##########################################
# Data clean
NewDat <- AutoClean(Dat = Org, rule = m,  outfile = NULL)
NewDat2 <- FuzzyClean(Org)

# Table one
TheTable1(NewDat)



aggregate





CheckRecord <- function(Dat){
  
  nodup = duplicated(Dat)  %>% table %>% length() == 1
  if (nodup){
    print("There is no duplicate records")
  }else{
    print("Duplicates moved out")
    unique(Dat)
  }
}


IDstring <- ("Recond", "ID","MRN","S")



# identical   # 18 
duplicated(Org)  %>% table
D_ident <- CheckRecord(Org)

# different content , same ID. 





# different ID number , same contents
anyDuplicated(Org) 

duplicated(Org, incomparables = "Record.ID")

duplicated(Org, incomparables = FALSE, MARGIN = 1)




Reduce <- function(x, D, id, tar, summ){ D. <- D[which(D[,id]==x),]
o <- which( D.[,tar]==summary(D.[,tar])[summ] )
if( is.na(o[1]) ){ out <- D.[1,]; out[,tar] <- summary(D.[,tar])[summ]
} else{ out <- D.[o[1],] }
return( out ) } 

ReduceRows <- function(D, id, tar, summ){ 
  i <- 1; out <- Reduce( unique(D[,id])[i], D, id, tar, summ )
  for(i in 2:length(unique(D[,id]))){ out <- rbind(out, Reduce( unique(D[,id])[i], D, id, tar, summ) ) }
  return(out)
}





