## working on excel date, make sure the rjava is running. 

# input the date when it is a date column

# ways of import excel. 

path = "testdata/Other/eg_date.xlsx"  #gdata is the best, XLConnect is the second 

require(gdata)
d1 = read.xls (path, sheet = 1, header = TRUE)

library(readxl)
d2 <- read_excel(path)

require(XLConnect)
wb = loadWorkbook(path)
d3 = readWorksheet(wb, sheet = "Sheet1", header = TRUE)

require(xlsx)
d4 = read.xlsx(path, sheetName = "Sheet1")
d5 = read.xlsx2(path, sheetName = "Sheet1")

detach(xlsx)
library(openxlsx)
d6 = read.xlsx(path)

source("https://gist.github.com/schaunwheeler/5825002/raw/3526a15b032c06392740e20b6c9a179add2cee49/xlsxToR.r")
d7 = xlsxToR(path, header = TRUE)
 



