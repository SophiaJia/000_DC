library(readr)
eg_long <- read_csv("testdata/eg_long.csv")


##try [[//d]] pertentional number 


text_filter_cell <- function(x){
  #input : string
  #output : text, number, mix
  #date variable consider as mix 
  options(warn=-1)
  y = "Nothing"
  if(is.na(x)) y = NA #csv file can recognize NA 
  else if(!grepl("[^A-Za-z]", x)) y = "text"
  else if (!is.na(as.numeric(x))) y = "number"
  else if(grepl("\\d",x)) y = "Mnumber"
  y
}

#testing
letters <- "abc"
numbers <- "12.3"
mix <- "b1dd"
tmp <- eg_long$WBRT.dose
tmp2 <- paste(eg_long$WBRT.dose, eg_long$Pathology)


text_filter_cell(letters)
text_filter_cell(numbers)
text_filter_cell(mix)
text_filter_cell(tmp2[7])
text_filter_cell("32;")
text_filter_cell(";")
text_filter_cell("3t")
text_filter_cell("1.1.1.1.1")

### text a column
text_filter_col <- function(x){
  #input : a column
  #output, check the percentage of text, number, mix in this column
  c1 <- table(sapply(x,text_filter_cell))
  c2 <- as.matrix(c1) %>% t
  
  if("number" %in% names(c1)){
    # the column is numeric 
    y = parse_number(x)
  }else if("text" %in% names(c1)){
    # the column is text
    y = fixchar_col(x)
  }else{
    # consider as number unless noted
    y = parse_number(x)
  }
  y
}












