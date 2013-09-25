
library(foreign, pos=4)
prf <- read.spss("/home/shane/RDocs/Selection Clinic 1/PRF data N=158.sav", 
                 use.value.labels=TRUE, max.value.labels=Inf, to.data.frame=TRUE)
colnames(prf) <- tolower(colnames(prf))

library(psych)
library(car)
library(plyr)
library(xtable)

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
pru1sum <- alpha(pru1.df)
print(pru1sum)

amb1.df <- with(prf.all,data.frame(p022,y075,y119,y163,y181,y207,p009,p013,p319,p317,p275,p231))
amb1sum <- alpha(amb1.df)
print(amb1sum)

#Create a data frame of the scale means and the original scale totals

scale.totals<-data.frame(apply(pru1.df,1,mean), apply(amb1.df,1,mean), prf$socds)
names(scale.totals)<- .(pru, amb, socds)


# Calculate the the item other total correlations 
iot.pru <- corr.test(pru1.df,scale.totals[,2:3])
iot.amb <- corr.test(amb1.df,scale.totals[,1-3])
scale.cor <- lowerCor(scale.totals)
diag(scale.cor) <- c(pru1sum$total$raw_alpha, amb1sum$total$raw_alpha, 1)

#Create a data frame of the summary statistics

pru.table <- round(data.frame(pru1sum$item.stats,iot.pru$r),2)
amb.table <- round(data.frame(amb1sum$item.stats,iot.amb$r),2)

pru.table[,c(5,6,3,7,8)]
amb.table[,c(5,6,3,7,8)]

