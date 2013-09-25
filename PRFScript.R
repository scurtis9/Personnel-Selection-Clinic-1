
library(foreign, pos=4)
prf <- read.spss("/home/shane/RDocs/Selection Clinic 1/PRF data N=158.sav", 
                 use.value.labels=TRUE, max.value.labels=Inf, to.data.frame=TRUE)
colnames(prf) <- tolower(colnames(prf))

library(psych)
library(car)
library(plyr)

#Reverse code all of the variables

vn1<-names(prf)
vn2<-gsub("p","y",vn1)
prf.re<-colwise(recode)(prf,recodes="1=0;0=1")
names(prf.re)<-vn2

#Combine the two dataframes into one

prf.all<-cbind.data.frame(prf,prf.re)

#Construct scales and find reliabilities

attach(prf.all)
pru1.df <- with(prf.all,data.frame(p015,p029,p110,y040,y081,p103,y332,p147,y169,p255,y233,y352))
alpha(pru1.df)

amb1.df <- with(prf.all,data.frame(p022,y075,y119,y163,y181,y207,p009,p013,p319,p317,p275,p231))
alpha(amb1.df)

pru<-data.frame(apply(pru1.df,1,mean),prf$socds)
names(pru)<- .(pru,socds)

amb<-data.frame(apply(amb1.df,1,mean),prf$socds)
names(amb)<- .(amb,socds)

# The the item other total correlations 
corr.test(pru1.df,amb)
corr.test(amb1.df,pru)
corr.test(amb$amb,pru$pru)