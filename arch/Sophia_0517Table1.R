TheTable1 <-function(Data, con_var = NULL, cat_var = NULL, output = NULL){
  # input require Data, con variable and cat variable are optional 
  # output Table 1, output to excel is optional
  
  if (is.null(con_var)){
    con_var   <- colnames(Data)[sapply(Data, class) == "numeric"]
    #con_names <- colnames(NewDat)[sapply(NewDat, class) == "numeric"]
  }
  
  if(is.null(cat_var)){
    cat_var_f <- colnames(Data)[sapply(Data, class) == "factor"]
    cat_var_char   <- colnames(Data)[sapply(Data, class) == "character"]
    #cat_names <- colnames(NewDat)[sapply(NewDat, class) == "character"]
    cat_var <- c(cat_var_f, cat_var_char)
  }
  
  all_var <- c(con_var, cat_var)
  
  table1tmp <- CreateTableOne(vars = all_var, factorVars = cat_var, data = Data, includeNA = F) 
  X1=print(table1tmp, quote = FALSE, noSpaces = TRUE, showAllLevels = TRUE)
  Variable=row.names(X1)
  Variable[1]="Total number of cases"
  d=data.frame(Variable,X1)
  if(!is.null(output)){
    p2excel(datastable = d, filename = output)
  }else{
    d
  }
}

