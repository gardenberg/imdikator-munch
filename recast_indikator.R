#RESHAPER DATA FRA KOMMUNE TIL ANDRE NIV?ER

#bibliotek
library(reshape)

#flytter om på data slik at variabler er kolonner, og rader er kommuner
#bruker  melt og cast fra reshape-package
#Problem med manglende aggregering løst gjennom å bruke ...-plassen etter fun.aggregate til å sende argumenter til aggregeringsfunksjon
#PT_kommune = cast(data_kommune,Komnr~variable,fun.aggregate=sum,na.rm=T,add.missing=T)
#na.rm=T dropper NA, nr.rm=F inkluderer na, dvs. ved summering droppes na-celler når na.rm=F.

#SCRIPT FOR Å FINNE FYLKESNR FRA KOMNR
#før kommunesammenslåing i 2013 var det flere kommuner, men kommunenr følger samme regel for fylkestilhørighet
breaks=c(0,199,299,399,499,599,699,799,899,999,1099,1199,1299,1399,1499,1599,1699,1799,1899,1999,2099)
b_labels=c("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20")
b_labels=as.factor(b_labels)
tilskudd_2011_kommune$fylkenr = cut(tilskudd_2011_kommune$komnr,breaks,labels=b_labels)

#bosetting 2012
data_bosett_2012 = melt.data.frame(bosett_2012_2,id.vars=c("nr","navn","kommunesett.Nr","kommunesett.Fylke","kommunesett.Fylkenr","kommunesett.Naringregnr","kommunesett.Naringreg"))
bosett_2012_fylke = cast(data_bosett_2012,kommunesett.Fylkenr~variable,fun.aggregate=sum,na.rm=T,add.missing=T)
bosett_2012_narreg = cast(data_bosett_2012,kommunesett.Naringregnr~variable,fun.aggregate=sum,na.rm=T,add.missing=T)

#bosetting 2013
data_bosett_2013 = melt.data.frame(bosett_2013_2,id.vars=c("nr","navn","kommunesett.Nr","kommunesett.Fylke","kommunesett.Fylkenr","kommunesett.Naringregnr","kommunesett.Naringreg"))
bosett_2013_fylke = cast(data_bosett_2013,kommunesett.Fylkenr~variable,fun.aggregate=sum,na.rm=T,add.missing=T)
bosett_2013_narreg = cast(data_bosett_2013,kommunesett.Naringregnr~variable,fun.aggregate=sum,na.rm=T,add.missing=T)

#bosetting 2014
data_bosett_2014 = melt.data.frame(bosetting_kommune_2014,id.vars=c("nr","navn","narringsregionnr"))
bosett_2014_narreg_narm1 = cast(data_bosett_2014,narringsregionnr~variable,fun.aggregate=sum,na.rm=T,add.missing=T)

#dataoutput
write.csv2(bosett_2014_narreg_narm1,file="bosatte_2014_narreg_narm1.csv")

#eksperiment med na.rm=F
#bosett_2014_narreg_narm0 = cast(data_bosett_2014,narringsregionnr~variable,fun.aggregate=sum,na.rm=F,add.missing=T)
#write.csv2(bosett_2014_narreg_narm0,file="bosatte_2014_narreg_narm0.csv")

#folketall i kommuner 2011-2015
data_folketall = melt.data.frame(folketall,id.vars=c("nr","navn","naregnr"))
folketall_naregnr = cast(data_folketall,naregnr~variable,fun.aggregate=sum,na.rm=T,add.missing=T)
write.csv2(folketall_naregnr,file="folketall_2011_2015_nareg.csv")

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



