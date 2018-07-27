### working on Dat
sapply(Dat, class)%>% table


fixDate<- function(Data){
  Data %>% mutate_if(grepl("date", colnames(.)), mdy)
  #last.warning %>% names
}

fixRange<-function(var){
# if var not in the range , return NA  
  
}


Dat <- Dat %>% mutate_all(funs(toupper)) %>% mutate_all(funs(str_trim))
at$X1st.cancer.type %>% table %>% sort(decreasing = TRUE)


Dat %<>% mutate(age = age.1st.ca.dx %>% parse_number())
Age <- fixAge(Dat$age)
Dat <- fixDate(Dat)


#Dat <- apply(Dat,2,toupper) # this will return a matrix 


# Rules
CancerType <- c("BLADDER", "LUNG","PANCREATIC","BREAST","LYMPHOMA","PROSTATE","COLON","MELANOMA","MYELOMA","THYROID","KIDNEY","ORAL","UTERINE","SKIN","CERVICAL","RECTAL","NHL","HL","LEUKEMIA",
                "ANAL","PERITONEAL","TESTICULAR","N/A",
                "LARYNGEAL","CARCINOMA","ENDOMETRIAL","LIVER","ESOPHAGEAL","OVARIAN",
                "TONGUE","TONSILLAR","OSTEOSARCOMA","LEIOMYOSARCOMA","CROHN","PAROTID","PTLD","CLL")



Dat$X2nd..myeloid..cancer.type %>% table %>% names -> Myname
Dat$X1st.cancer.type %>% table %>% names -> CType


D <- adist(Dat$X1st.cancer.type, CancerType)
colnames(D) <- CancerType
rownames(D) <- Dat$X1st.cancer.type
i <- apply(D, 1, which.min)
tmp <- data.frame(rawtext = Dat$X1st.cancer.type, coded = CancerType[i])
tmp[tmp[,1]!=as.character(tmp[,2]),]

i <- apply(D, 1, function(x) x == 0)


D <- adist(Dat$X1st.cancer.type, CType)
colnames(D) <- CType
rownames(D) <- Dat$X1st.cancer.type
i <- apply(D, 1, which.min)

i <- apply(D, 1, function(x) x < 2)
data.frame(rawtext = Dat$X1st.cancer.type, coded = CType[i])




