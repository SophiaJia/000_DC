## Gender fix function 

gender_fix <- function(x){
   ifelse(grepl("F", toupper(x)), "Female","Male")
}