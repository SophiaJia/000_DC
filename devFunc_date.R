## Date is not in the same order 
#mdy or #ydm 

library(lubridate)
library(tidyverse)

path = "testdata/eg_date.xlsx"  #gdata is the best, XLConnect is the second 
path2 = "testdata/eg1.csv"

require(gdata)
d1 = read.xls (path, sheet = 1, header = TRUE)
d2 = read.csv(path2)

x = d1$Lesion.PreGKRS
IS.Date(x)
WhichDF(x)
y = fixDate_col(x)

## get which one is failed to parse,

format = c( "%m-%d-%Y","%B %d, %Y","%B/%d/%Y","%d %b %y","%d.%m.%Y","%Y/%m/%d" ,"%Y-%m-%d","%m/%d/%Y")

multidate <- function(data, formats){
  # assign the last date format.  this takes longer time. 
  a<-list()
  for(i in 1:length(formats)){
    a[[i]]<- as.Date(data,format=formats[i])
    a[[1]][!is.na(a[[i]])]<-a[[i]][!is.na(a[[i]])]
  }
  a[[1]]
}

multidate(d1$Lesion.PreGKRS,format)
multidate(d2$Date.of.last.f.u,format)

# the shortest length is when year is two digit. and if it is not date of birth , it should be over 2000



# the easiest way to detect if it is date variable 
   ## maybe use as.date
   ## regurlar expression. 
# why check first, why not trun it in to date directly  because there are different format. 
# need better solution. 

as.Date(d1$Lesion.PreGKRS, format)

fixDate_col(d1$Lesion.PreGKRS)


############
# show dates that is failed to parse. 

d1 
tt1 <- fixDate(d1)
datalist = list()
j = 1
for(i in c(3,5,6,7,9,10)){
  tmp = d1[is.na(tt1[,i]) &!is.na(d1[,i])&(d1[,i] != ""),]
  if(!is.null(tmp) & length(tmp[,1]> 0)){
  datalist[[j]] = tmp
  j = j + 1
  }
}


big_data = do.call(dplyr::union, datalist)







