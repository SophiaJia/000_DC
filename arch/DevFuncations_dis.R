### check missing
MissLt <- function(x, ratio = 0.5){
  sum(is.na(x))/length(x) < ratio
}

### Date: 
IS.Date  <- function(x, addformat = NULL ){
  format = c("%m/%d/%Y", "%m-%d-%Y", "%Y/%m/%d", "%Y-%m-%d")  #If there is any other format.  
  y <- as.Date(as.character(x),format= format)  # OK to use as. Date, if there is a single raw has the data format consider as date variable
  MissLt(y,ratio = 1-(1/length(y)))
}
# example: which col is the data type 
  sapply(Org, IS.Date) %>% table
  names(Org)[sapply(Org, IS.Date)]

# next step comver the date type to the same/right format

NumOfWarning <- function(last.warning){
  if(is.null(last.warning)){nwar = 0} else
    if(!identical((last.warning %>% names %>% agrep("All",.)), integer(0))){ nwar = Inf} else
    {(nwar = last.warning %>% names %>% parse_number())}
  nwar
}

WhichDF <- function(x, returnall = FALSE){
  # return which data format of the data variable 
  if(IS.Date(x)){
    options(warn = -1)
    assign("last.warning", NULL, envir = baseenv())
    mdy(x)
    nwar1 <- NumOfWarning(last.warning)
    
    assign("last.warning", NULL, envir = baseenv())
    dmy(x)
    nwar2 <- NumOfWarning(last.warning)
    
    assign("last.warning", NULL, envir = baseenv())
    ydm(x)
    nwar3 <- NumOfWarning(last.warning)
    
    assign("last.warning", NULL, envir = baseenv())
    ymd(x)
    nwar4 <- NumOfWarning(last.warning)
    options(warn = 0)
    order <- c(nwar1, nwar2, nwar3, nwar4)
    index <- which(order == min(order))
    rightformat <- c("mdy", "dmy","ydm","ymd")[index]
    if (length(rightformat) > 1&returnall == FALSE){
      warning(" More than two format were find and only return the first one, use retrunal = TRUE to see all the format. ")
    }else
      if (length(rightformat) > 1&returnall == TRUE){
        rightformat
      }
    rightformat[1]
    
  }
  else {NA}
}


