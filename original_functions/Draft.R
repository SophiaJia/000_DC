D <- New[colnames(New)[sapply(New, class) == "Date"]]
D2 <- cbind(`ID` = New$patient.code,D)
di <- D2
n = ncol(di)

for (i in 2:n){
  varname  <- paste0("col",i)
di <-
  di %>% mutate(!!varname := 0)
}

for (i in 2:(n-1)){
    varname1  <- paste0("col",i)
    varname2  <- paste0("col",i+1)
    di[[varname1]][di[,i] > di[,i+1]] <- 1
    di[[varname2]][di[,i] > di[,i+1]] <- 1
}

t <- (di %>% select(starts_with("col")) %>% rowSums > 0 )
do <- di[t,]




