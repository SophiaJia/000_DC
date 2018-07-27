## longtinutail data furnctions 

long2wide <- function(data, ID = NULL, var_base = NULL, var_vist = NULL){
  
  if(is.null(ID)){
    warning("Missing ID ")
    break
  }
  
  if(is.null(var_base)){
    warning("Baseline Variable Index: var_base")
    break
  }
  
  # add visit times 
  in_id <- which(colnames(data) == ID)
  ttt <- data %>% group_by_at(in_id) %>% mutate(Imakethistime = row_number()) 
  
  # get baseline variable which is the same for each patients 
  PerPatient <- ttt[,var_base]
  
  # visit variable which is different per visit 
  pervisit <- ttt[,c(ID, colnames(ttt)[!colnames(ttt) %in% var_base])]
  
  # transform
  nvisit <- max(pervisit$Imakethistime)
  Dwide <- unique(PerPatient)
  for (i in (1:nvisit)){
    # need to fix the name 
    tmpd <- pervisit[pervisit$Imakethistime == i,]
    colnames(tmpd)[-1] <- paste("Time",i,colnames(tmpd)[-1], sep = ".")
    Dwide <- left_join(Dwide, tmpd[-ncol(tmpd),], by = ID)
  }
  Dwide
}

#what <- long2wide(longitu_test, ID = "patient.code", var_base = longitu_test[,c(1:7)] %>%  colnames())
#long2wide(longitu_test,  var_base = longitu_test[,c(1:7)] %>%  colnames())

wide_test <- read.csv("testdata/eg_wide.csv",stringsAsFactors = FALSE)
wide2long <- function(data, ID = NULL, var_base = NULL, timename = NA, nvisit = NA){
  
  key <- array()
  long <- tibble()
  in_id <- which(colnames(data) == ID)
  PerPatient <- data[,var_base]
  
  for (i in (1:nvisit)){
    # need to fix the name 
    key[i] <- paste(timename, i, sep = ".")
    tmpd   <- data %>% select(c(starts_with(key[i])))
    tmpd   <- tmpd %>% mutate(time_m = i)
    tmpd   <- bind_cols(data[ID],tmpd)
    colnames(tmpd) <- gsub(key[i],"",colnames(tmpd))
    
    tmpd2 <- left_join(PerPatient,tmpd , by = ID)
    # should be the same for each visit. 
    long <- bind_rows(long,tmpd2)
  }
  #long <- long[order(long[,in_id] %>% as.vector),] 
  long <- arrange(long,long[[in_id]])
  long

}
what2 <- wide2long(wide_test, ID = "patient.code", var_base = wide_test[,(1:6)] %>%  colnames(), timename = "Time", nvisit = 4)

long_test <- read.csv("testdata/eg_long.csv",stringsAsFactors = FALSE)
