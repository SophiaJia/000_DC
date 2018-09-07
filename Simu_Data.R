## create a simulated dateset 
library(readr)
eg_long <- read_csv("testdata/eg_long.csv")

# ID , text, number, date (regular)
Dtest <- eg_long[,c(1,2,3,19,14,27)] 

# mix text with number 
Dtest$tn <- paste(Dtest$Gender, Dtest$KPS)

# parciel mix test with number 
Dtest$tnp <- ifelse(Dtest$KPS == 90,Dtest$KPS, Dtest$tn) 

# randomly missing tnp
Dtest$tnpr <- ifelse(Dtest$KPS == 80, NA, 
                     ifelse(Dtest$KPS == 50, Dtest$Gender, Dtest$tnp)) 

save(Dtest,file = "Dtest.RData")
