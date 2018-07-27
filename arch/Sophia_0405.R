library(scRub) 
library(openxlsx) 
library(dplyr)
library(tidyverse)
library(lubridate)
library(magrittr)
library(editrules)

Org <- read.csv("scRubTestData.csv", stringsAsFactors = FALSE, na.string = TRUE) 
Org[!is.na(Org$MRN %>% parse_number()),] -> Org
#sapply(Org, class)%>% table 

# select a list of factors
Inlist <- c("MRN", 
            "X1st.cancer.date","X2nd..myeloid..cancer.date", "sample.date","XRT.date","date.last.f.u","date.death", 
            "date.to.calc..OS", "date.TMN.1st.dx","date.TMN.2nd.dx", 
            "X1st.cancer.type", "X2nd..myeloid..cancer.type", "heme.cancer.in.FH.", "age.1st.ca.dx", "Gender", "IPSS.R.value", "age.myeloid.ca.dx"
            )

Org_trim <- Org %>% select(Inlist)
write.csv(Org_trim , "ex_scRub_cat.csv")
Dat <- as.tibble(Org_trim)
#sapply(Dat, class)%>% table # all character
# colclass <- sapply(Dat, class)
# name_total <- colnames(Dat)
# (name_char   <- colclass[colclass == "character"] %>% names)
# (name_int    <- colclass[colclass == "integer"]   %>% names)
# (name_logical<- colclass[colclass == "logical"]   %>% names)
# (name_num    <- colclass[colclass == "numeric"]   %>% names)

# Rules
CancerType <- c("BLADDER", "LUNG","PANCREATIC","BREAST","LYMPHOMA","PROSTATE","COLON","MELANOMA","MYELOMA","THYROID","KIDNEY","ORAL","UTERINE","SKIN","CERVICAL","RECTAL","NHL","HL","LEUKEMIA",
                "ANAL","PERITONEAL","TESTICULAR","N/A",
                "LARYNGEAL","CARCINOMA","ENDOMETRIAL","LIVER","ESOPHAGEAL","OVARIAN",
                "TONGUE","TONSILLAR","OSTEOSARCOMA","LEIOMYOSARCOMA","CROHN","PAROTID","PTLD","CLL","UNKNOWN")
High_low <-  c("VERY HIGH", "HIGH","INTERMEDIATE","LOW","VERY LOW","","UNKNOWN","NA")
YES_NO   <-  c("YES","NO","UNKNOWN","","0","1","NA") 
Gender   <-  c("Female","Male", "F","M")
range_soft <- c(18, 100)
range_hard <- c(0, 150)

## Rule Matrix ###### 
m <- matrix(list(), 17, 6)
colnames(m) <- c("Var","Type","Scale","Range.hard","Range.soft","other")
m[,1] <- Inlist
m[,2] <- c("num",rep("Date",9),rep("char",3),"num","char","char","num")
m[11,3][[1]] <- CancerType
m[16,3][[1]] <- High_low
m[13,3][[1]] <- YES_NO
m[15,3][[1]] <- Gender
m[14,4][[1]] <- range_hard
m[17,4][[1]] <- range_hard
m[14,5][[1]] <- range_soft
m[17,5][[1]] <- range_soft
#Check_uncheck --> yes/no 
########
m

tt <- adist("Dat$X1st.cancer.type %>% toupper()" , CancerType)
rownames(tt) <- Dat$X1st.cancer.type
colnames(tt) <- CancerType



### functions
fixDate<- function(Data){
  Data %>% mutate_if(grepl("date", colnames(.)), mdy)
  #last.warning %>% names
}
fixScale <- function(x, ScaleType){
  Dtmp <- adist(x, ScaleType)
  colnames(Dtmp) <- ScaleType
  rownames(Dtmp) <- x
  i <- apply(Dtmp, 1, which.min)
  tmp <- data.frame(rawtext = x, coded = ScaleType[i])
  tmp
}
fixrange <- function(x, range_hard, range_soft = NULL){
  y = x %>% parse_number()
  y[y < range_hard[1]&!is.na(y)] <- NA
  y[y > range_hard[2]&!is.na(y)] <- NA
  
  if (!is.null(range_soft)){
    y[y < range_soft[1]&!is.na(y)] <- NA
    y[y > range_soft[2]&!is.na(y)] <- NA
    
  }
  y
}
reporttmp <- function(Org_data, New_data, f = "scRubOut.xlsx"){
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
      d[[j]] = as.character(d[[j]])  
      I[j] = ((d[[j-1]] == "") != d[[j]] %>% is.na())
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
    sht = str_c(OrgName[i])
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


## test1 ##############
NewDat1 <- 
  fixDate(Dat) %>% 
  mutate(X1st.cancer.type = fixScale(Dat$X1st.cancer.type, CancerType)[,2]) %>%
  mutate(Gender = fixSex(Gender)) %>%
  mutate(IPSS.R.value = fixScale(IPSS.R.value, ScaleType)[,2]) %>%
  mutate(heme.cancer.in.FH. = fixScale(heme.cancer.in.FH., YES_NO)[,2]) %>%
  mutate(age.1st.ca.dx = fixrange(age.1st.ca.dx, range_hard, range_soft)) %>%
  mutate(age.myeloid.ca.dx = fixrange(age.myeloid.ca.dx, range_hard, range_soft))

reporttmp(Dat, NewDat1, "Report1.xlsx")


## test2 ##############
AutoChean <- function(Dat , rule , outfile = "Report.xlsx"){
  
  NewDat2 = Dat
  
  for(i in 1: length(rule[,2])){
    if (rule[i,2] == "char" & !is.null(rule[i,3][[1]]) ){
      NewDat2[rule[i,1][[1]]] <- fixScale(Dat[[rule[i,1][[1]]]] %>% toupper %>% str_trim, rule[i,3][[1]])[2]
    }
    
    if (rule[i,2] == "num" & !is.null(rule[i,4][[1]]) ){
      NewDat2[rule[i,1][[1]]] <- fixrange(Dat[[rule[i,1][[1]]]], rule[i,4][[1]])
    }
  }
  
  NewDat2 <- fixDate(NewDat2)
  reporttmp(Dat, NewDat2, outfile)
  NewDat2
}

NewDat <- AutoChean(Dat = Dat, rule = m,  outfile = "Report2.xlsx")











  



## next , matching colnames 
## insert dictionary 
## subgroup 
## find cancer type 






