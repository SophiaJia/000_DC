MissLt <- function(x, ratio = 0.5){
  sum(is.na(x))/length(x) < ratio
}

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
    mutate_if(sapply(., IS.Number2), parse_number)
}
