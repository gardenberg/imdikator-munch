#Reshape av tilskuddsdata til fylkes- og næringsregionnivå
#revidert 14.mai 2015

#bibliotek
library(reshape)

#Datainnlesning
tilskudd_2011_kommune <- read.csv("~/R/datamerge_indikator/input/93-tilskudd-kommune-2011.csv", sep=";", dec=",")
tilskudd_2012_kommune <- read.csv("~/R/datamerge_indikator/input/93-tilskudd-kommune-2012.csv", sep=";", dec=",")
tilskudd_2013_kommune <- read.csv("~/R/datamerge_indikator/input/93-tilskudd-kommune-2013.csv", sep=";", dec=",")
tilskudd_2014_kommune <- read.csv("~/R/datamerge_indikator/input/93-tilskudd-kommune-2014.csv", sep=";", dec=",")

#SCRIPT FOR Å LEGGE TIL NÆRINGSREGIONNR
#legger også til all annen informasjon om kommunen som ligger i kommunesett-fila
kommunesett <- read.csv("H:/My Documents/R/datamerge_indikator/kommunesort.csv", sep=";")
#fjerne noen unødvenige rader i kommunesettet, og renavner disse med opprinnelige navn
k_navn = names(kommunesett)
kommunesett = data.frame(kommunesett[,1],kommunesett[,7])
names(kommunesett) = c(k_navn[1],"naregnr")
# script for å legge til næringsregionnr, legger kun til informasjon om kommuner vi har info om
tilskudd_2011_kommune = merge(tilskudd_2011_kommune,kommunesett,by.x="komnr",by.y="Nr",all.x=T,all.y=F)
tilskudd_2012_kommune = merge(tilskudd_2012_kommune,kommunesett,by.x="komnr",by.y="Nr",all.x=T,all.y=F)
tilskudd_2013_kommune = merge(tilskudd_2013_kommune,kommunesett,by.x="komnr",by.y="Nr",all.x=T,all.y=F)
tilskudd_2014_kommune = merge(tilskudd_2014_kommune,kommunesett,by.x="komnr",by.y="Nr",all.x=T,all.y=F)

#SCRIPT FOR Å FINNE FYLKESNR FRA KOMNR
#før kommunesammenslåing i 2013 var det flere kommuner, men kommunenr følger samme regel for fylkestilhørighet
breaks=c(0,199,299,399,499,599,699,799,899,999,1099,1199,1299,1399,1499,1599,1699,1799,1899,1999,2099)
b_labels=c("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20")
b_labels=as.factor(b_labels)
tilskudd_2011_kommune$fylkenr = cut(tilskudd_2011_kommune$komnr,breaks,labels=b_labels)
tilskudd_2012_kommune$fylkenr = cut(tilskudd_2012_kommune$komnr,breaks,labels=b_labels)
tilskudd_2013_kommune$fylkenr = cut(tilskudd_2013_kommune$komnr,breaks,labels=b_labels)
tilskudd_2014_kommune$fylkenr = cut(tilskudd_2014_kommune$komnr,breaks,labels=b_labels)

#tilskudd 2014
data_tilskudd_2014 = melt.data.frame(tilskudd_2014_kommune,id.vars=c("kom","komnr","navn","fylkenr","naregnr"))
tilskudd_2014_fylke = cast(data_tilskudd_2014,fylkenr~variable,fun.aggregate=sum,na.rm=T,add.missing=T)
tilskudd_2014_nareg = cast(data_tilskudd_2014,naregnr~variable,fun.aggregate=sum,na.rm=T,add.missing=T)
write.csv2(tilskudd_2014_fylke,file="tilskudd_2014_fylke.csv")
write.csv2(tilskudd_2014_nareg,file="tilskudd_2014_nareg.csv")

#tilskudd 2013
data_tilskudd_2013 = melt.data.frame(tilskudd_2013_kommune,id.vars=c("kom","komnr","navn","fylkenr","naregnr"))
tilskudd_2013_fylke = cast(data_tilskudd_2013,fylkenr~variable,fun.aggregate=sum,na.rm=T,add.missing=T)
tilskudd_2013_nareg = cast(data_tilskudd_2013,naregnr~variable,fun.aggregate=sum,na.rm=T,add.missing=T)
write.csv2(tilskudd_2013_fylke,file="tilskudd_2013_fylke.csv")
write.csv2(tilskudd_2013_nareg,file="tilskudd_2013_nareg.csv")

#tilskudd 2012
data_tilskudd_2012 = melt.data.frame(tilskudd_2012_kommune,id.vars=c("kom","komnr","navn","fylkenr"))
tilskudd_2012_fylke = cast(data_tilskudd_2012,fylkenr~variable,fun.aggregate=sum,na.rm=T,add.missing=T)
#tilskudd_2013_nareg = cast(data_tilskudd_2013,naregnr~variable,fun.aggregate=sum,na.rm=T,add.missing=T)
write.csv2(tilskudd_2012_fylke,file="tilskudd_2012_fylke.csv")
#write.csv2(tilskudd_2013_nareg,file="tilskudd_2013_nareg.csv")

#tilskudd 2011
data_tilskudd_2011 = melt.data.frame(tilskudd_2011_kommune,id.vars=c("kom","komnr","navn","fylkenr"))
tilskudd_2011_fylke = cast(data_tilskudd_2011,fylkenr~variable,fun.aggregate=sum,na.rm=T,add.missing=T)
#tilskudd_2013_nareg = cast(data_tilskudd_2013,naregnr~variable,fun.aggregate=sum,na.rm=T,add.missing=T)
write.csv2(tilskudd_2011_fylke,file="tilskudd_2011_fylke.csv")
#write.csv2(tilskudd_2013_nareg,file="tilskudd_2013_nareg.csv")


