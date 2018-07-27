CheckRecord <- function(Dat){
  
  nodup = duplicated(Org)  %>% table %>% length() == 1
  if (nodup){
    print("There is no duplicated records")
  }else{
    unique(Dat)
    print("Duplicates moved out")
  }
}
