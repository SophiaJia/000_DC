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
    else (I[j] = (as.character(d[[j - 1]]) %>% toupper %>% str_trim != as.character(d[[j]])))
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