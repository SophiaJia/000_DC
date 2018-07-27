#demo
library(tidyverse)
library(magrittr)
library(janitor)
library(stringdist)
library(lubridate)

Org <- read.csv("H:/Projects/P_Haddad, Abdo/triple negative Breast Cancer_bicky/org/Updated list_TNBC_Neo & Adj for Sophia.csv", stringsAsFactors = FALSE, na.string = TRUE)
sapply(Org, class)%>% table 

# Date
D <- fixDate(Org)
sapply(D, class)%>% table
colnames(D)[sapply(D, class) == "Date"]

#Number
options(warn=-1) 
D1 <- AS.Number(D)
sapply(D1, class)%>% table
summary(D1[sapply(D1, class) == "numeric"])
sapply(D1[sapply(D1, class) == "character"], table)





