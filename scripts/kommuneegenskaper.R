#GEOGRAFISKE NIVÅER
#fil for å generere ei fil med sammenhengen mellom ulike geografiske nivåer
#hvilke kommuner er medlem i hvilke næringsregioner og hvilke fylker?

#kilder: kommunestandarder henta fra http://stabas.ssb.no/ClassificationFrames.asp?ID=964501&Language=nb, komplett kommune- og fylkesliste skrapa fra ssb sin statistikkbank, næringsregioner tilsendt på epost fra Knut Vareide

kommune_2014 <- read.csv("~/R/imdikator-munch/parameters/kommune_2014.csv", sep=";", stringsAsFactors=FALSE,colClasses="character")
apply(kommune_2014,2, function(x){nlevels(as.factor(x))})
kommune_2014 = subset(kommune_2014,Kode!="")
kommune_2014$aar = "2014"
kommune = subset(kommune_2014,select=c(2,4,6,12))

kommune_2013 <- read.csv("~/R/imdikator-munch/parameters/kommune_2013.csv", sep=";", stringsAsFactors=FALSE,colClasses="character")
apply(kommune_2013,2, function(x){nlevels(as.factor(x))})
kommune_2013$aar = "2013"
kommune = merge(kommune,subset(kommune_2013,select=c(2,4,6,12)),by=c("Kode","Tittel","aar","Generell.note"),all.x=T,all.y=T)

kommune_2012 <- read.csv("~/R/imdikator-munch/parameters/kommune_2012.csv", sep=";", stringsAsFactors=FALSE,colClasses="character")
apply(kommune_2012,2, function(x){nlevels(as.factor(x))})
kommune_2012$aar = "2012"
kommune = merge(kommune,subset(kommune_2012,select=c(2,4,6,12)),by=c("Kode","Tittel","aar","Generell.note"),all.x=T,all.y=T)

kommune_2008 <- read.csv("~/R/imdikator-munch/parameters/kommune_2008.csv", sep=";", stringsAsFactors=FALSE,colClasses="character")
apply(kommune_2008,2, function(x){nlevels(as.factor(x))})
kommune_2008$aar = "2008"
kommune = merge(kommune,subset(kommune_2008,select=c(2,4,6,12)),by=c("Kode","Tittel","aar","Generell.note"),all.x=T,all.y=T)

kommune_2006 <- read.csv("~/R/imdikator-munch/parameters/kommune_2006.csv", sep=";", stringsAsFactors=FALSE,colClasses="character")
apply(kommune_2006,2, function(x){nlevels(as.factor(x))})
kommune_2006$aar = "2006"
kommune = merge(kommune,subset(kommune_2006,select=c(2,4,6,12)),by=c("Kode","Tittel","aar","Generell.note"),all.x=T,all.y=T)

kommune_2006 <- read.csv("~/R/imdikator-munch/parameters/kommune_2006.csv", sep=";", stringsAsFactors=FALSE,colClasses="character")
apply(kommune_2006,2, function(x){nlevels(as.factor(x))})
kommune_2006$aar = "2006"
kommune = merge(kommune,subset(kommune_2006,select=c(2,4,6,12)),by=c("Kode","Tittel","aar","Generell.note"),all.x=T,all.y=T)

kommune_2005 <- read.csv("~/R/imdikator-munch/parameters/kommune_2005.csv", sep=";", stringsAsFactors=FALSE,colClasses="character")
apply(kommune_2005,2, function(x){nlevels(as.factor(x))})
kommune_2005$aar = "2005"
kommune = merge(kommune,subset(kommune_2005,select=c(2,4,6,12)),by=c("Kode","Tittel","aar","Generell.note"),all.x=T,all.y=T)

kommune_2003 <- read.csv("~/R/imdikator-munch/parameters/kommune_2003.csv", sep=";", stringsAsFactors=FALSE,colClasses="character")
apply(kommune_2003,2, function(x){nlevels(as.factor(x))})
kommune_2003$aar = "2003"
kommune = merge(kommune,subset(kommune_2003,select=c(2,4,6,12)),by=c("Kode","Tittel","aar","Generell.note"),all.x=T,all.y=T)

kommune_2002 <- read.csv("~/R/imdikator-munch/parameters/kommune_2002.csv", sep=";", stringsAsFactors=FALSE,colClasses="character")
apply(kommune_2002,2, function(x){nlevels(as.factor(x))})
kommune_2002$aar = "2002"
kommune = merge(kommune,subset(kommune_2002,select=c(2,4,6,12)),by=c("Kode","Tittel","aar","Generell.note"),all.x=T,all.y=T)

#legger også til den komplette kodelista, skrapa fra SSBs statistikkbank.
kommune_komplett <- read.csv("~/R/imdikator-munch/parameters/kommune_komplett.csv", sep=";", stringsAsFactors=FALSE,colClasses="character")
apply(kommune_komplett,2, function(x){nlevels(as.factor(x))})
kommune_komplett$aar = "komplett_2016"
kommune = merge(kommune,kommune_komplett,by=c("Kode","Tittel","aar","Generell.note"),all.x=T,all.y=T)

#fylker
library(dplyr)
#dette funker så lenge kommunenr følger samme regel for fylkestilhørighet
#komplette kodelista har også med 21xx->Svalbard,22xx->Jan Mayen og 23xx->Kontinentalsokkelen, amt 9999.
breaks=c(0,199,299,399,499,599,699,799,899,999,1099,1199,1299,1399,1499,1599,1699,1799,1899,1999,2099,2199,2299,2399,9999)
b_labels=c("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23","99")
b_labels=as.factor(b_labels)
kommune$fylke_nr = cut(as.numeric(kommune$Kode),breaks,labels=b_labels)
fylker_komplett = read.csv("~/R/imdikator-munch/parameters/fylker_komplett.csv", sep=";", stringsAsFactors=FALSE,colClasses="character")
kommune = inner_join(kommune,fylker_komplett,by="fylke_nr")

#næringsregion
naringsregion = read.csv("~/R/imdikator-munch/parameters/naringsregion-2000_2015.csv", sep=";", stringsAsFactors=FALSE,colClasses="character")
kommune = merge(kommune,naringsregion,by.x="Kode",by.y="kommune_nr",all.x=T,all.Y=T)
sum(is.na(kommune$Navn.gammel)==F) == sum(kommune$Tittel==kommune$Navn.gammel,na.rm=T)
#visuell inspeksjon av avvikene
t=subset(kommune,Tittel!=Navn.gammel&is.na(Navn.gammel)==F)

#opprydding av navn og kolonner i endelig fil
kommune = select(kommune,-(Nr:Navn.gammel))
names(kommune)= c("kommune_nr","kommune_navn","standard_utgave","kommune_note","fylke_nr","fylke_navn","fylke_note","naringsregion_nr","naringsregion_navn")
write.csv(kommune,file="parameters/kommuneegenskaper.csv",row.names=F)
