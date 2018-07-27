############setup##############
library(tidyverse)
library(stringdist)
Org <- read.csv("scRubTestData.csv", stringsAsFactors = FALSE, na.string = TRUE) 
Org[!is.na(Org$MRN %>% parse_number()),] -> Org
Inlist <- c("MRN", 
            "X1st.cancer.date","X2nd..myeloid..cancer.date", "sample.date","XRT.date","date.last.f.u","date.death", 
            "date.to.calc..OS", "date.TMN.1st.dx","date.TMN.2nd.dx", 
            "X1st.cancer.type", "X2nd..myeloid..cancer.type", "heme.cancer.in.FH.", "age.1st.ca.dx", "Gender", "IPSS.R.value", "age.myeloid.ca.dx"
)

Org_trim <- Org %>% select(Inlist)
Dat <- as.tibble(Org_trim)

breast <- read.csv("breasttypo.txt", sep="", stringsAsFactors=FALSE)
headt <- read.csv("headtypo.txt", stringsAsFactors=FALSE)
mye <- read.csv("MYELOMA.txt", stringsAsFactors=FALSE)

test <- c(breast[[1]], headt[[1]])
#################
#test; mye
##adist
dic <- c("breast","liver","head")

tt<- adist(test , dic)
rownames(tt) <- test 
colnames(tt) <- dic
tt
i <- apply(tt, 1, which.min)
tmp <- data.frame(rawtext = test, coded = dic[i])
tmp

adist("lasy", "1 lazy 2")
adist(" breast", "breast", partial = TRUE)
adist("lasy", "1 lazy 2", useBytes = T)
#adist("lasy", "1 lazy 2", counts = T)
#adist("lasy", "1 lazy 2", costs = 2)

adist("lasy", "1 lazy 2")
adist("female", "male")
adist("female", "male", partial = TRUE)
adist("lasy", "1 lazy 2", partial = TRUE)
agrep("lasy", "1 lazy 2")
agrep("female", "male")

agrep("lasy", "1 lazy 2", value = TRUE)
agrep("head" , test, value = TRUE) # return only the matched value, can't return target value
agrep("lasy", c(" 1 lazy 2", "1 lasy 2"), max = list(sub = 0))

#sapply(dic, function(x) agrep(x,test))
#sapply(test, function(x) agrep(x,dic))

amatch(test,dic,maxDist=10)
amatch(test,dic,maxDist=5,nomatch=0)
amatch(test,dic)

mid <-amatch(test,dic,maxDist=Inf) 
test2 <-dic[mid]
(test_com <-cbind(test, test2))

mye #MYELOMA vs MELANOMA
dic2 <-c("MYELOMA", "MELANOMA")
adist(mye[[1]] , dic2)
mid2 <-amatch(mye[[1]],dic2,maxDist=Inf) 
test3 <-dic2[mid2]
(test_com <-cbind(mye[[1]], test3))


###########
Ctable <- Dat$X1st.cancer.type %>% table() %>% sort(decreasing = T)
Ctable[Ctable > quantile(Ctable, probs = 0.8)]

cancertype <-Dat[Dat$X1st.cancer.type != "N/A","X1st.cancer.type"]
Ctable <- cancertype %>% table() %>% sort(decreasing = T)
Ctable[Ctable > quantile(Ctable, probs = 0.6)]





