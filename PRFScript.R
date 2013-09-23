
library(foreign, pos=4)
prf <- read.spss("/home/shane/RDocs/Selection Clinic 1/PRF data N=158.sav", 
                 use.value.labels=TRUE, max.value.labels=Inf, to.data.frame=TRUE)
colnames(prf) <- tolower(colnames(prf))
