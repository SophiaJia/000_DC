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


WhichDF <- function(x, returnall = FALSE){
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
    if (length(rightformat) > 1 & returnall == FALSE){
      warning(" More than two format were find and only return the first one, use retrunal = TRUE to see all the format. ")
    }else
      if (length(rightformat) > 1 & returnall == TRUE){
        rightformat
      }
    rightformat[1]
    
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
    mutate_if(sapply(., IS.mdy), mdy) %>%
    mutate_if(sapply(., IS.dmy), dmy) %>%
    mutate_if(sapply(., IS.ydm), ydm) %>%
    mutate_if(sapply(., IS.ymd), ymd) 
}

# no later than Sys.Data()

