MissLt <- function(x, ratio = 0.5){
  sum(is.na(x))/length(x) < ratio
}

# if the char colnume is a number
IS.Number <- function(x, ratio = 0.5){
  y = parse_number(x)
  sum(is.na(y))/length(y) < ratio
}

IS.Date  <- function(x, addformat = NULL ){
  format = c("%m/%d/%Y", "%m-%d-%Y", "%Y/%m/%d", "%Y-%m-%d")
  y <- as.Date(as.character(x),format= format)
  MissLt(y,ratio = 0.99)
}




