# 0531 demo

D <- read.csv("testdata/ex_scRub_cat.csv",stringsAsFactors = FALSE)
D2 <- FuzzyClean(D)

D_cat <- D[sapply(D2, mode) == "character"]

allLevel <- function(x){
  x  %>% table %>% names %>% paste(., collapse="; ") 
} 
ta <- cbind(
  `Variable Name`  = colnames(D_cat),
  `Levels` = apply(D_cat, 2, allLevel))
rownames(ta) <- NULL
ta

D_cat  %>% table %>% names
