text_filter_cell <- function(x){
  #input : string
  #output : text, number, mix
  options(warn=-1)
  y = "mix"
  if(!grepl("[^A-Za-z]", x)) y = "text"
  else if (!is.na(as.numeric(x))) y = "number"
  y
}

#testing
letters <- "abc"
numbers <- "12.3"
mix <- "b1dd"

text_filter_cell(letters)
text_filter_cell(numbers)
text_filter_cell(mix)

### text a column
text_filter_col <- function(x){
  #input : a column
  #output, text, number mix



}





