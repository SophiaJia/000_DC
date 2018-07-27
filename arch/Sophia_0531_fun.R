
## helper ####
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

CheckRecord <- function(Dat){
  
  nodup = duplicated(Org)  %>% table %>% length() == 1
  if (nodup){
    print("There is no duplicate records")
  }else{
    unique(Dat)
    print("Duplicates moved out")
  }
}


MissLt <- function(x, ratio = 0.5){
  sum(is.na(x))/length(x) < ratio
}

## Date ################################################
IS.Date  <- function(x, addformat = NULL, exactformat = NULL){
  if (is.null(exactformat)){
    format = c("%m/%d/%Y", "%m-%d-%Y","%Y/%m/%d" ,"%Y-%m-%d", addformat) # order doesn't matter , as long as the expression matches. 
    y <- as.Date(as.character(x),format= format)
    MissLt(y,ratio = 1-(1/length(y)))
  }
  else{
    y <- as.Date(as.character(x),format= exactformat) 
    MissLt(y,ratio = 1-(1/length(y)))
  }
}

# NumOfWarning <- function(last.warning){
#   if(is.null(last.warning)){nwar = 0} else
#     if(!identical((last.warning %>% names %>% agrep("All",.)), integer(0))){ nwar = Inf} else
#     {(nwar = last.warning %>% names %>% parse_number())}
#   nwar
# }
# example 
sapply(Org, IS.Date) %>% table
names(Org)[sapply(Org, IS.Date)] 

WhichDF <-function(x, returnall = FALSE){
  # return which data format of the data variable 
  if(IS.Date(x)){
    options(warn=-1)
    nwar1 <- sum(is.na(mdy(x)))
    nwar2 <- sum(is.na(dmy(x)))
    nwar3 <- sum(is.na(ydm(x)))
    nwar4 <- sum(is.na(ymd(x)))
    order <- c(nwar1, nwar2, nwar3, nwar4)
    options(warn=0)
    index <- which(order == min(order))
    rightformat <- c("mdy", "dmy","ydm","ymd")[index]
    if(length(rightformat) == 1){
      rightformat
    }else
      if (length(rightformat) > 1 & returnall == FALSE){
        #warning(" More than two format were find and only return the first one, use retrunal = TRUE to see all the format. ")
        rightformat[1]
      }else
        if (length(rightformat) > 1 & returnall == TRUE){
          rightformat
        }
  }
  else {"other"}
}



IS.mdy <- function(x){
  WhichDF(x) == "mdy"
}
IS.dmy <- function(x){
  WhichDF(x) == "dmy"
}
IS.ymd <- function(x){
  WhichDF(x) == "ymd"
}
IS.ydm <- function(x){
  WhichDF(x) == "ydm"
}
fixDate<- function(Data){
  # check worning massage for each kind ?  mdy and dmy exclusive, and combine wiht ymd
  Data %>% 
    mutate_if(IS.mdy, mdy) %>%
    mutate_if(IS.dmy, dmy) %>%
    mutate_if(IS.ydm, ydm) %>%
    mutate_if(IS.ymd, ymd) 
}

# no later than Sys.Data()

## Number ################################################
options(warn=0)

IS.Number <- function(x, ratio = 0.0001){
  #options(warn=-1)  #how to turn off the warning inside of the function 
  y = parse_number(x)
  x.missing <- sum(is.na(x))
  y.missing <- sum(is.na(y))
  if(ratio == 1){
    (x.missing == y.missing) & ! IS.Date(x) 
  } else {
    (y.missing/length(x)-x.missing/length(x) < ratio)& ! IS.Date(x) 
  }
}

IS.Number2 <- function(x, ratio = 0.5){
  #options(warn=-1)  #how to turn off the warning inside of the function 
  y = parse_number(x)
  x.missing <- sum(is.na(x))
  y.missing <- sum(is.na(y))
  if(ratio == 1){
    (x.missing == y.missing) & ! IS.Date(x) 
  } else {
    (y.missing/length(x)-x.missing/length(x) < ratio)& ! IS.Date(x) 
  }
}

AS.Number <- function(Data){
  Data %>% 
    mutate_if(IS.Number2, parse_number)
}

Is.int2fac <- function(x){
  ((table(x) %>% length) < 6 & is.numeric(x))
}

As.int2fac <- function(Data){
  Data %>% 
    mutate_if(Is.int2fac, as.factor)
}
# sapply(D1, class) %>% table


## Factor ################################################
# redcap only check vs uncheck 
# D2 <-D1 %>% 
#   mutate_if(is.character,funs(replace(., . == "Checked","Yes")) ) %>% 
#   mutate_if(is.character,funs(replace(., . == "Unchecked","No")) )

# fix number and data before char
fixchar <- function(x){
  x %>% 
    mutate_if(is.character, toupper)%>%
    mutate_if(is.character,trimws) %>% 
    mutate_if(is.character,funs(replace(., . == "",NA))) %>%
    mutate_if(is.character,funs(replace(., . == "UNKNOWN",NA))) %>%
    mutate_if(is.character,funs(replace(., . == "NA",NA))) 
}

# # how many levels 
# D3 <- fixchar(D2)
# lev <- sapply(D3[sapply(D3, class) == "character"], table)
# lev2<- mapply(length,lev)
# levelgt7 <- lev2[lev2 > 7]
# levelgt10 <- lev2[lev2 > 10]
# leveleq2 <- lev2[lev2 == 2] %>% names # this variables no need to change. 
# leveleq1 <- lev2[lev2 == 1] # output this factor has only one level.

Is.integer <- function(x){
  # if the reminer of x/1 is O then it is an intager 
  # if all missing is intager than the colume is intager, otherwise is numeric
  if(is.numeric(x)){
    sum(x%%1==0,na.rm = TRUE) == sum(!is.na(x))
  }else{
    FALSE
  }
}

As.integer <- function(Data){
  Data %>% 
    mutate_if(Is.integer, as.integer)
}


## functions ################################################
# fixDate<- function(Data){
#   Data %>% mutate_if(grepl("date", colnames(.)), mdy)
#   #last.warning %>% names
# }
fixScale <- function(x, ScaleType){
  Dtmp  <- adist(x, ScaleType) ## all
  colnames(Dtmp) <- ScaleType
  rownames(Dtmp) <- x
  i <- apply(Dtmp, 1, which.min)
  tmp <- data.frame(rawtext = x, coded = ScaleType[i])
  tmp
}
fixScale2 <- function(varb, ScaleType,maxDist=100){
  
  Dtmp <- sapply(varb, function(x) amatch(x,ScaleType, maxDist=maxDist)) # Inf = all, or specify a number
  ScaleType[Dtmp]
}

fixrange <- function(x, range_hard, range_soft = NULL){
  y = x %>% parse_number()
  y[y <= range_hard[1]&!is.na(y)] <- NA
  y[y >= range_hard[2]&!is.na(y)] <- NA
  
  if (!is.null(range_soft)){
    y[y <= range_soft[1]&!is.na(y)] <- NA
    y[y >= range_soft[2]&!is.na(y)] <- NA
    
  }
  y
}
reporttmp <- function(Org_data, New_data, f = "tmp.xlsx"){
  f = f
  OrgName = names(Org_data)
  NewName = names(New_data)
  
  # freq table 
  OrgTable = vector("list", length = length(OrgName))
  NewTable = vector("list", length = length(NewName))
  
  #columes which is not date type will be in the freq table 
  ColIndex <- c(1:length(OrgName))[sapply(New_data, class) != "Date"]
  
  names(OrgTable) = OrgName
  names(NewTable) = NewName
  
  for (i in ColIndex) {
    D1 = tabyl(as.character(Org_data[[i]]), sort = T)[1:3]
    names(D1)[1] = "value"
    OrgTable[[i]] = D1
    
    D2 = tabyl(as.character(New_data[[i]]), sort = T)[1:3]
    names(D2)[1] = "value"
    NewTable[[i]] = D2
    
  }
  
  d = bind_cols(Org_data, New_data)
  d = d[order(names(d))]
  (modS = seq(2, length(d), 2))
  I = d
  I[] = 0
  for (j in modS) {
    if (class(d[[j]]) == "Date"){
      #d[[j]] = as.character(d[[j]])  
      I[j] = (( str_length(d[[j-1]]) >0) & is.na(d[[j]]))
    }
    else (I[j] = (as.character(d[[j - 1]]) %>% toupper %>% str_trim != as.character(d[[j]])%>% toupper %>% str_trim))
  }
  hs1 = createStyle(fgFill = "#DCE6F1", halign = "CENTER", 
                    textDecoration = "bold")
  redStyle = createStyle(fontColour = "#FF0000")
  wb <- createWorkbook()
  sht = "Suggestions"
  addWorksheet(wb, sht)
  writeData(wb, sht, d, startRow = 1, headerStyle = hs1)
  for (i in 1:length(modS)) addStyle(wb, sht, style = redStyle, 
                                     rows = 1 + which(I[[modS[i]]]), cols = modS[i])
  setColWidths(wb, sht, cols = 1:(dim(d)[2]), widths = "auto")
  freezePane(wb, sht, firstRow = TRUE, firstCol = TRUE)
  for (i in ColIndex) {
    sht = str_c(substrRight(OrgName[i],30))
    print(sht)
    addWorksheet(wb, sht)
    writeData(wb, sht, "Original/Dirty", startRow = 1, startCol = 1)
    writeData(wb, sht, OrgTable[[OrgName[i]]], startRow = 2, headerStyle = hs1)
    setColWidths(wb, sht, cols = 1:(dim(OrgTable[[i]])[2]), widths = "auto")
    writeData(wb, sht, "Scrubbed/Clean", startRow = 1, startCol = 5)
    writeData(wb, sht, NewTable[[NewName[i]]], startRow = 2, startCol = 5, 
              headerStyle = hs1)
    setColWidths(wb, sht, cols = 5:(5 + (dim(NewTable[[NewName[i]]])[2])), 
                 widths = "auto")
    freezePane(wb, sht, firstActiveRow = 3)
  }
  saveWorkbook(wb, file = f, overwrite = TRUE)
}

FuzzyClean <- function(Dat){
  Dat %>% 
    fixDate() %>%
    AS.Number() %>%
    fixchar()%>% 
    As.int2fac %>% 
    As.integer
}


AutoClean <- function(Dat , rule , outfile = "Report.xlsx"){
  
  NewDat1 <- FuzzyClean(Dat)
  NewDat2 <- NewDat1
  
  rule[,2] <- sapply(NewDat1[unlist(m[,1])],mode)
  
  for(i in 1: length(rule[,2])){
    if (rule[i,2] == "character" & !is.null(rule[i,3][[1]]) ){
      NewDat2[rule[i,1][[1]]] <- fixScale2(NewDat1[[rule[i,1][[1]]]] %>% toupper %>% str_trim, rule[i,3][[1]])
    }
    
    if (rule[i,2] == "numeric" & !is.null(rule[i,4][[1]]) ){
      NewDat2[rule[i,1][[1]]] <- fixrange(NewDat1[[rule[i,1][[1]]]], rule[i,4][[1]])
    }
  }
  
  if(!is.null(outfile)){
    reporttmp(Dat, NewDat2, outfile)
  }
  
  # the following is for analysis use, will not included in the output
  # after checking the range of integer as numeric columne, if the integar is less than 5 levels then save as factor
  
  NewDat2
}

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

#what2 <- wide2long(wide_test, ID = "patient.code", var_base = wide_test[,(1:6)] %>%  colnames(), timename = "Time", nvisit = 4)




#Warning messages: (where is this warnings?)
# 1: In rbind(names(probs), probs_f) :
#   number of columns of result is not a multiple of vector length (arg 1)
# 2: In rbind(names(probs), probs_f) :
#   number of columns of result is not a multiple of vector length (arg 1)