#BOSATT_ANMODEDE
#21. januar 2016
#1. kommunedata 2014
#2. andre nivåer 2014
#3. kommunedata 2015-2017
#4. fylkesdata 2015-2017
#5. andre nivåer 2015-2017

#kommune 2014
library(tidyr)
bosatt_anmodede <- read.csv("D:/R/imdikator-munch/data_crossed_input/bosatt_anmodede-kommune-2014.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=";", dec=".",colClasses="character")
bosatt_anmodede = subset(bosatt_anmodede,select=c(1,3,4,5))
names(bosatt_anmodede)=c("kommune_nr","anmodning_personer","vedtak_personer","bosatt_personer")
bosatt_anmodede$anmodning_prosent = (as.numeric(bosatt_anmodede$anmodning_personer)/as.numeric(bosatt_anmodede$anmodning_personer))*100
bosatt_anmodede$vedtak_prosent = (as.numeric(bosatt_anmodede$vedtak_personer)/as.numeric(bosatt_anmodede$anmodning_personer))*100
bosatt_anmodede$bosatt_prosent = (as.numeric(bosatt_anmodede$bosatt_personer)/as.numeric(bosatt_anmodede$anmodning_personer))*100
df = gather(bosatt_anmodede,"variabler","tabellvariabel",2:7)
df = separate(df,variabler,c("bosetting","enhet"),sep="_")
df$tabellvariabel[is.na(df$tabellvariabel)==T]="."
#logisk sjekk
nrow(df)==429*3*2
#resterende
df$tabell_navn="bosatt_anmodede"
df$aar="2014"
write.csv(df,"D:/R/imdikator-munch/data_flat_output/bosetting_anmodede-kommune-2014.csv",row.names=F)

#næringsregion 2014
kinfo <- read.csv("D:/R/imdikator-munch/parameters/kommunesort.csv", sep=";", stringsAsFactors=FALSE)
kinfo = subset(kinfo,select=c("Nr","Naringregnr"))
names(kinfo)[2] = "naringsregion_nr"
kinfo$Nr[nchar(kinfo$Nr)==3] = paste0("0",kinfo$Nr[nchar(kinfo$Nr)==3])
kinfo$naringsregion_nr[nchar(kinfo$naringsregion_nr)==1] = paste0("0",kinfo$naringsregion_nr[nchar(kinfo$naringsregion_nr)==1])
bosatt_anmodede <- read.csv("D:/R/imdikator-munch/data_crossed_input/bosatt_anmodede-kommune-2014-tilaggregering.csv", row.names=NULL, na.strings=c("NA"), stringsAsFactors=FALSE, sep=";", dec=".")
sum(is.na(bosatt_anmodede))
sum(bosatt_anmodede==":")
sum(bosatt_anmodede==".")
#skal ikke leses inn som character, men med NA.strings ="."&":"
#logikken er at det ikke er : på dette nivået, fordi tallene skal aggregeres - det er bare missing
#og hvis det er ":", så skal disse uansett bli . på aggregert n ivå
bosatt_anmodede <- read.csv("D:/R/imdikator-munch/data_crossed_input/bosatt_anmodede-kommune-2014-tilaggregering.csv", row.names=NULL, na.strings=c("NA","."), stringsAsFactors=FALSE, sep=";", dec=".")
checksum_start = sum(bosatt_anmodede$bosatt_2014,na.rm=T)
bosatt_anmodede$kommune_nr[nchar(bosatt_anmodede$kommune_nr)==3] = paste0("0",bosatt_anmodede$kommune_nr[nchar(bosatt_anmodede$kommune_nr)==3])
#vedtak kan være ".", hvis henvendelse ikke er besvart-.
#bosetting og anmodning kan ikke være ".", skal være 0
sum(is.na(bosatt_anmodede$bosatt_2014)==T)
bosatt_anmodede$bosatt_2014[is.na(bosatt_anmodede$bosatt_2014)==T]=0
sum(is.na(bosatt_anmodede$anmodning_2014)==T)
bosatt_anmodede$anmodning_2014[is.na(bosatt_anmodede$anmodning_2014)==T]=0
sum(is.na(bosatt_anmodede$vedtak_2014)==T)
bosatt_anmodede$vedtak_2014[is.na(bosatt_anmodede$vedtak_2014)==T]=0
df = merge(bosatt_anmodede,kinfo,by.x="kommune_nr",by.y ="Nr",all.x=T,all.y=T)
library(reshape)
bosatt_anmodede_m = melt.data.frame(df,id.vars=c("kommune_nr","naringsregion_nr"),na.rm=F)
bosatt_anmodede_nareg = cast(bosatt_anmodede_m,naringsregion_nr~variable,fun.aggregate=sum,na.rm=F,add.missing=T,fill=NA)
checksum_start == sum(bosatt_anmodede_nareg$bosatt_2014)
names(bosatt_anmodede_nareg)=c("naringsregion_nr","bosatt_personer","anmodning_personer","vedtak_personer")
bosatt_anmodede_nareg$anmodning_prosent = (as.numeric(bosatt_anmodede_nareg$anmodning_personer)/as.numeric(bosatt_anmodede_nareg$anmodning_personer))*100
bosatt_anmodede_nareg$vedtak_prosent = (as.numeric(bosatt_anmodede_nareg$vedtak_personer)/as.numeric(bosatt_anmodede_nareg$anmodning_personer))*100
bosatt_anmodede_nareg$bosatt_prosent = (as.numeric(bosatt_anmodede_nareg$bosatt_personer)/as.numeric(bosatt_anmodede_nareg$anmodning_personer))*100
df = gather(bosatt_anmodede_nareg,"variabler","tabellvariabel",2:7)
df = separate(df,variabler,c("bosetting","enhet"),sep="_")
df$tabellvariabel[is.na(df$tabellvariabel)==T]="."
#logisk sjekk
nrow(df)==84*3*2
#resterende
df$tabell_navn="bosatt_anmodede"
df$aar="2014"
write.csv(df,"D:/R/imdikator-munch/data_flat_output/bosetting_anmodede-naringsregion-2014.csv",row.names=F)

#fylke 2014
kinfo = read.csv("D:/R/imdikator-munch/parameters/kommunesort.csv", sep=";", stringsAsFactors=FALSE)
kinfo = subset(kinfo,select=c("Nr","Fylkenr"))
names(kinfo)[2] = "fylke_nr"
kinfo$fylke_nr[nchar(kinfo$fylke_nr)==1] = paste0("0",kinfo$fylke_nr[nchar(kinfo$fylke_nr)==1])
#bosettingsdata spesielt og data for aggregering generelt skal ikke leses inn som character, men med NA.strings ="."&":"
#logikken er at det ikke er : på dette nivået, fordi tallene skal aggregeres - det er bare missing
#og hvis det er ":", så skal disse uansett bli . på aggregert n ivå
bosatt_anmodede <- read.csv("D:/R/imdikator-munch/data_crossed_input/bosatt_anmodede-kommune-2014-tilaggregering.csv", row.names=NULL, na.strings=c("NA","."), stringsAsFactors=FALSE, sep=";", dec=".")
checksum_start = sum(bosatt_anmodede$bosatt_2014,na.rm=T)
#vedtak kan være ".", hvis henvendelse ikke er besvart-.
#bosetting og anmodning kan ikke være ".", skal være 0
sum(is.na(bosatt_anmodede$bosatt_2014)==T)
bosatt_anmodede$bosatt_2014[is.na(bosatt_anmodede$bosatt_2014)==T]=0
sum(is.na(bosatt_anmodede$anmodning_2014)==T)
bosatt_anmodede$anmodning_2014[is.na(bosatt_anmodede$anmodning_2014)==T]=0
sum(is.na(bosatt_anmodede$vedtak_2014)==T)
bosatt_anmodede$vedtak_2014[is.na(bosatt_anmodede$vedtak_2014)==T]=0
df = merge(bosatt_anmodede,kinfo,by.x="kommune_nr",by.y ="Nr",all.x=T,all.y=T)
library(reshape)
bosatt_anmodede_m = melt.data.frame(df,id.vars=c("kommune_nr","fylke_nr"),na.rm=F)
bosatt_anmodede_fylke = cast(bosatt_anmodede_m,fylke_nr~variable,fun.aggregate=sum,na.rm=F,add.missing=T,fill=NA, margins="grand_row")
bosatt_anmodede_fylke = subset(bosatt_anmodede_fylke,select=-5)
checksum_start == sum(bosatt_anmodede_fylke$bosatt_2014)/2
names(bosatt_anmodede_fylke)=c("fylke_nr","bosatt_personer","anmodning_personer","vedtak_personer")
bosatt_anmodede_fylke$anmodning_prosent = (as.numeric(bosatt_anmodede_fylke$anmodning_personer)/as.numeric(bosatt_anmodede_fylke$anmodning_personer))*100
bosatt_anmodede_fylke$vedtak_prosent = (as.numeric(bosatt_anmodede_fylke$vedtak_personer)/as.numeric(bosatt_anmodede_fylke$anmodning_personer))*100
bosatt_anmodede_fylke$bosatt_prosent = (as.numeric(bosatt_anmodede_fylke$bosatt_personer)/as.numeric(bosatt_anmodede_fylke$anmodning_personer))*100
df = gather(bosatt_anmodede_fylke,"variabler","tabellvariabel",2:7)
df = separate(df,variabler,c("bosetting","enhet"),sep="_")
sum(is.na(df$tabellvariabel)==T)
df$tabellvariabel[is.na(df$tabellvariabel)==T]="."
#logisk sjekk
nrow(df)==21*3*2
#resterende
df$tabell_navn="bosatt_anmodede"
df$aar="2014"
#.==regexp any single character
df$fylke_nr = gsub(".all.","00",df$fylke_nr)
write.csv(df,"D:/R/imdikator-munch/data_flat_output/bosetting_anmodede-fylke-2014.csv",row.names=F)

#3. kommunedata 2015-2017
bosatt_anmodede = read.csv("D:/R/imdikator-munch/data_crossed_input/bosatt_anmodede-kommune-2015_2017.csv", row.names=NULL, na.strings=c("NA",":","."), stringsAsFactors=FALSE, sep=";", dec=",")
checksum = sum(bosatt_anmodede$vedtak_2016_personer,na.rm=T)
df = gather(bosatt_anmodede,variables,tabellvariabel,2:19)
df = separate(df,variables,into=c("bosetting","aar","enhet"),sep="_")
nlevels(as.factor(df$kommune_nr))
#i anmodnings- og vedtakstall på kommunenivå 2015-2017 er bosetting manglende, anmodning ikke : eller . men 0, og vedtak enten 0 eller NA.
df$tabellvariabel[df$bosetting=="bosatt"&is.na(df$tabellvariabel)==T]="."
df$tabellvariabel[df$bosetting=="anmodning"&is.na(df$tabellvariabel)==T]=0
df$tabellvariabel[df$bosetting=="vedtak"&is.na(df$tabellvariabel)==T]="."
checksum == sum(as.numeric(df$tabellvariabel[df$bosetting=="vedtak"&df$aar=="2016"&df$enhet=="personer"]),na.rm=T)
#logisk sjekk
nrow(df)==428*3*3*2
#resterende
df$tabell_navn="bosatt_anmodede"
df$kommune_nr[nchar(df$kommune_nr)==3] = paste0("0",df$kommune_nr[nchar(df$kommune_nr)==3])
write.csv(df,"D:/R/imdikator-munch/data_flat_output/bosetting_anmodede-kommune-2015_2017.csv",row.names=F)

#4. fylkesdata 2015-2017
bosatt_anmodede = read.csv("D:/R/imdikator-munch/data_crossed_input/bosatt_anmodede-fylke-2015_2017.csv", row.names=NULL, na.strings=c("NA",":","."), stringsAsFactors=FALSE, sep=";", dec=",")
checksum = sum(bosatt_anmodede$vedtak_2016_personer,na.rm=T)/2
df = gather(bosatt_anmodede,variables,tabellvariabel,2:19)
df = separate(df,variables,into=c("bosetting","aar","enhet"),sep="_")
nlevels(as.factor(df$fylke_nr))
#i anmodnings- og vedtakstall på fylkenivå 2015-2017 er bosetting manglende, anmodning ikke : eller . men 0, og vedtak enten 0 eller NA.
df$tabellvariabel[df$bosetting=="bosatt"&is.na(df$tabellvariabel)==T]="."
df$tabellvariabel[df$bosetting=="anmodning"&is.na(df$tabellvariabel)==T]=0
df$tabellvariabel[df$bosetting=="vedtak"&is.na(df$tabellvariabel)==T]="."
checksum == sum(as.numeric(df$tabellvariabel[df$bosetting=="vedtak"&df$aar=="2016"&df$enhet=="personer"]),na.rm=T)
#logisk sjekk
nrow(df)==20*3*3*2
#resterende
df$tabell_navn="bosatt_anmodede"
df$fylke_nr[nchar(df$fylke_nr)==1] = paste0("0",df$fylke_nr[nchar(df$fylke_nr)==1])
write.csv(df,"D:/R/imdikator-munch/data_flat_output/bosetting_anmodede-fylke-2015_2017.csv",row.names=F)

#5. næringsregion 2015-2017
bosatt_anmodede = read.csv("D:/R/imdikator-munch/data_crossed_input/bosatt_anmodede-kommune-2015_2017.csv", row.names=NULL, na.strings=c("NA",":","."), stringsAsFactors=FALSE, sep=";", dec=",")
checksum = sum(bosatt_anmodede$anmodning_2016_personer,na.rm=T)
bosatt_anmodede = subset(bosatt_anmodede,select=1:10)
kinfo <- read.csv("D:/R/imdikator-munch/parameters/kommunesort.csv", sep=";", stringsAsFactors=FALSE)
kinfo = subset(kinfo,select=c("Nr","Naringregnr"))
names(kinfo)[2] = "naringsregion_nr"
kinfo$naringsregion_nr[nchar(kinfo$naringsregion_nr)==1] = paste0("0",kinfo$naringsregion_nr[nchar(kinfo$naringsregion_nr)==1])
df = merge(bosatt_anmodede,kinfo,by.x="kommune_nr",by.y ="Nr",all.x=T,all.y=T)
#vedtak kan være ".", hvis henvendelse ikke er besvart. ved aggregering av vedtak
# settes disse lik 0. det samme gjelder NA bosetting, og NA anmodning
#bosetting og anmodning kan ikke være ".", skal være 0
df[is.na(df)==T]=0
bosatt_anmodede_m = melt.data.frame(df,id.vars=c("kommune_nr","naringsregion_nr"),na.rm=F)
bosatt_anmodede_nareg = cast(bosatt_anmodede_m,naringsregion_nr~variable,fun.aggregate=sum,na.rm=F,add.missing=T,fill=NA)

bosatt_anmodede_nareg$anmodning_2015_prosent = (as.numeric(bosatt_anmodede_nareg$anmodning_2015_personer)/as.numeric(bosatt_anmodede_nareg$anmodning_2015_personer))*100
bosatt_anmodede_nareg$vedtak_2015_prosent = (as.numeric(bosatt_anmodede_nareg$vedtak_2015_personer)/as.numeric(bosatt_anmodede_nareg$anmodning_2015_personer))*100
bosatt_anmodede_nareg$bosatt_2015_prosent = (as.numeric(bosatt_anmodede_nareg$bosatt_2015_personer)/as.numeric(bosatt_anmodede_nareg$anmodning_2015_personer))*100
bosatt_anmodede_nareg$anmodning_2016_prosent = (as.numeric(bosatt_anmodede_nareg$anmodning_2016_personer)/as.numeric(bosatt_anmodede_nareg$anmodning_2016_personer))*100
bosatt_anmodede_nareg$vedtak_2016_prosent = (as.numeric(bosatt_anmodede_nareg$vedtak_2016_personer)/as.numeric(bosatt_anmodede_nareg$anmodning_2016_personer))*100
bosatt_anmodede_nareg$bosatt_2016_prosent = (as.numeric(bosatt_anmodede_nareg$bosatt_2016_personer)/as.numeric(bosatt_anmodede_nareg$anmodning_2016_personer))*100
bosatt_anmodede_nareg$anmodning_2017_prosent = (as.numeric(bosatt_anmodede_nareg$anmodning_2017_personer)/as.numeric(bosatt_anmodede_nareg$anmodning_2017_personer))*100
bosatt_anmodede_nareg$vedtak_2017_prosent = (as.numeric(bosatt_anmodede_nareg$vedtak_2017_personer)/as.numeric(bosatt_anmodede_nareg$anmodning_2017_personer))*100
bosatt_anmodede_nareg$bosatt_2017_prosent = (as.numeric(bosatt_anmodede_nareg$bosatt_2017_personer)/as.numeric(bosatt_anmodede_nareg$anmodning_2017_personer))*100

df = gather(bosatt_anmodede_nareg,variables,tabellvariabel,2:19)
df = separate(df,variables,into=c("bosetting","aar","enhet"),sep="_")
nlevels(as.factor(df$naringsregion_nr))

#i anmodnings- og vedtakstall på naringsregion 2015-2017 er
# bosetting manglende (.), anmodning 0, og vedtak enten 0 eller ".".
df$tabellvariabel[df$bosetting=="bosatt"&df$tabellvariabel==0]="."
df$tabellvariabel[df$bosetting=="vedtak"&df$tabellvariabel=="0"]="."
#logisk sjekk
nrow(df)==84*3*3*2
#resterende
df$tabell_navn="bosatt_anmodede"
write.csv(df,"D:/R/imdikator-munch/data_flat_output/bosetting_anmodede-naringsregion-2015_2017.csv",row.names=F)

#AGGREGERING OG FLATFILERING AV 2016-DATA
#forbehandla krysstabell i excel
#31. januar, 3. februar, 12. februar
data = read.csv("D:/R/imdikator-munch/data_crossed_input/bosatt_anmodede-kommune-2016-160212.csv", row.names=NULL, na.strings=c("NA",":","."), stringsAsFactors=FALSE, sep=";", dec=",")
library(tidyr)
#KOMMUNE 2016
bosatt_anmodede = data
names(bosatt_anmodede)=c("kommune_nr","aar", "anmodning_personer","vedtak_personer","bosatt_personer")
checksum = sum(bosatt_anmodede$vedtak_personer,na.rm=T)
bosatt_anmodede$anmodning_prosent = bosatt_anmodede$anmodning_personer/bosatt_anmodede$anmodning_personer*100
bosatt_anmodede$vedtak_prosent = bosatt_anmodede$vedtak_personer/bosatt_anmodede$anmodning_personer*100
bosatt_anmodede$bosatt_prosent = bosatt_anmodede$bosatt_personer/bosatt_anmodede$anmodning_personer*100
df = gather(bosatt_anmodede,variables,tabellvariabel,3:8)
df = separate(df,variables,into=c("bosetting","enhet"),sep="_")
nlevels(as.factor(df$kommune_nr))==428
#i anmodnings- og vedtakstall på kommunenivå 2015-2017 er bosetting manglende, anmodning ikke : eller . men 0, og vedtak enten 0 eller ".".
df$tabellvariabel[df$bosetting=="bosatt"&is.na(df$tabellvariabel)==T]="."
df$tabellvariabel[df$bosetting=="anmodning"&is.na(df$tabellvariabel)==T]=0
df$tabellvariabel[df$bosetting=="vedtak"&is.na(df$tabellvariabel)==T]="."
checksum == sum(as.numeric(df$tabellvariabel[df$bosetting=="vedtak"&df$enhet=="personer"]),na.rm=T)
#logisk sjekk
nrow(df)==428*3*2
#resterende
df$tabell_navn="bosatt_anmodede"
df$kommune_nr[nchar(df$kommune_nr)==3] = paste0("0",df$kommune_nr[nchar(df$kommune_nr)==3])
write.csv(df,"D:/R/imdikator-munch/data_flat_output/bosatt_anmodede-kommune-2016-160212.csv",row.names=F)

#FYLKE 2016
kinfo = read.csv("D:/R/imdikator-munch/parameters/kommunesort.csv", sep=";", stringsAsFactors=FALSE)
kinfo = subset(kinfo,select=c("Nr","Fylkenr"))
names(kinfo)[2] = "fylke_nr"
kinfo$fylke_nr[nchar(kinfo$fylke_nr)==1] = paste0("0",kinfo$fylke_nr[nchar(kinfo$fylke_nr)==1])
#bosettingsdata spesielt og data for aggregering generelt skal ikke leses inn som character, men med NA.strings ="."&":"
#logikken er at det ikke er : på dette nivået, fordi tallene skal aggregeres - det er bare missing
#og hvis det er ":", så skal disse uansett bli . på aggregert n ivå
bosatt_anmodede = data
checksum_start = sum(bosatt_anmodede$bosetting.vedtak[bosatt_anmodede$aar==2016],na.rm=T)
sum(is.na(bosatt_anmodede$bosetting.bosatt)==T)
bosatt_anmodede$bosetting.bosatt[is.na(bosatt_anmodede$bosetting.bosatt)==T]=0
sum(is.na(bosatt_anmodede$bosetting.anmodning)==T)
bosatt_anmodede$bosetting.anmodning[is.na(bosatt_anmodede$bosetting.anmodning)==T]=0
sum(is.na(bosatt_anmodede$bosetting.vedtak)==T)
bosatt_anmodede$bosetting.vedtak[is.na(bosatt_anmodede$bosetting.vedtak)==T]=0
df = merge(bosatt_anmodede,kinfo,by.x="kommune_nr",by.y ="Nr",all.x=T,all.y=T)
library(reshape)
bosatt_anmodede_m = melt.data.frame(df,id.vars=c("kommune_nr","fylke_nr","aar"),na.rm=F)
bosatt_anmodede_fylke = cast(bosatt_anmodede_m,fylke_nr~variable,fun.aggregate=sum,na.rm=T,add.missing=T,margins="grand_row")
bosatt_anmodede_fylke = subset(bosatt_anmodede_fylke,select=-5)
checksum_start == sum(bosatt_anmodede_fylke$bosetting.vedtak)/2
names(bosatt_anmodede_fylke)=c("fylke_nr","anmodning_personer_2016","vedtak_personer_2016","bosatt_personer_2016")
bosatt_anmodede_fylke$anmodning_prosent_2016 = (as.numeric(bosatt_anmodede_fylke$anmodning_personer_2016)/as.numeric(bosatt_anmodede_fylke$anmodning_personer_2016))*100
bosatt_anmodede_fylke$vedtak_prosent_2016 = (as.numeric(bosatt_anmodede_fylke$vedtak_personer_2016)/as.numeric(bosatt_anmodede_fylke$anmodning_personer_2016))*100
bosatt_anmodede_fylke$bosatt_prosent_2016 = (as.numeric(bosatt_anmodede_fylke$bosatt_personer_2016)/as.numeric(bosatt_anmodede_fylke$anmodning_personer_2016))*100
df = gather(bosatt_anmodede_fylke,"variabler","tabellvariabel",-1)
df = separate(df,variabler,c("bosetting","enhet","aar"),sep="_")
sum(is.na(df$tabellvariabel)==T)
df$tabellvariabel[is.na(df$tabellvariabel)==T]="."
#logisk sjekk
nrow(df)==21*3*2
#.==regexp any single character
df$fylke_nr = gsub(".all.","00",df$fylke_nr)
checksum_start==df$tabellvariabel[df$fylke_nr=="00"&df$bosetting=="vedtak"&df$aar=="2016"&df$enhet=="personer"]
#resterende
df$tabell_navn="bosatt_anmodede"
df$tabellvariabel[df$bosetting=="bosatt"]="."
write.csv(df,"D:/R/imdikator-munch/data_flat_output/bosatt_anmodede-fylke-2016-160212.csv",row.names=F)

#Næringsregion 2016
kinfo <- read.csv("D:/R/imdikator-munch/parameters/kommunesort.csv", sep=";", stringsAsFactors=FALSE)
kinfo = subset(kinfo,select=c("Nr","Naringregnr"))
names(kinfo)[2] = "naringsregion_nr"
kinfo$naringsregion_nr[nchar(kinfo$naringsregion_nr)==1] = paste0("0",kinfo$naringsregion_nr[nchar(kinfo$naringsregion_nr)==1])
#bosettingsdata spesielt og data for aggregering generelt skal ikke leses inn som character, men med NA.strings ="."&":"
#logikken er at det ikke er : på dette nivået, fordi tallene skal aggregeres - det er bare missing
#og hvis det er ":", så skal disse uansett bli . på aggregert n ivå
bosatt_anmodede = data
checksum_start = sum(bosatt_anmodede$bosetting.vedtak[bosatt_anmodede$aar==2016],na.rm=T)
sum(is.na(bosatt_anmodede$bosetting.bosatt)==T)
bosatt_anmodede$bosetting.bosatt[is.na(bosatt_anmodede$bosetting.bosatt)==T]=0
sum(is.na(bosatt_anmodede$bosetting.anmodning)==T)
bosatt_anmodede$bosetting.anmodning[is.na(bosatt_anmodede$bosetting.anmodning)==T]=0
sum(is.na(bosatt_anmodede$bosetting.vedtak)==T)
bosatt_anmodede$bosetting.vedtak[is.na(bosatt_anmodede$bosetting.vedtak)==T]=0
df = merge(bosatt_anmodede,kinfo,by.x="kommune_nr",by.y ="Nr",all.x=T,all.y=T)
library(reshape)
bosatt_anmodede_m = melt.data.frame(df,id.vars=c("kommune_nr","naringsregion_nr","aar"),na.rm=F)
bosatt_anmodede_naringsregion = cast(bosatt_anmodede_m,naringsregion_nr~variable,fun.aggregate=sum,na.rm=T)
checksum_start == sum(bosatt_anmodede_naringsregion$bosetting.vedtak)
names(bosatt_anmodede_naringsregion)=c("naringsregion_nr","anmodning_personer_2016","vedtak_personer_2016","bosatt_personer_2016")
bosatt_anmodede_naringsregion$anmodning_prosent_2016 = (as.numeric(bosatt_anmodede_naringsregion$anmodning_personer_2016)/as.numeric(bosatt_anmodede_naringsregion$anmodning_personer_2016))*100
bosatt_anmodede_naringsregion$vedtak_prosent_2016 = (as.numeric(bosatt_anmodede_naringsregion$vedtak_personer_2016)/as.numeric(bosatt_anmodede_naringsregion$anmodning_personer_2016))*100
bosatt_anmodede_naringsregion$bosatt_prosent_2016 = (as.numeric(bosatt_anmodede_naringsregion$bosatt_personer_2016)/as.numeric(bosatt_anmodede_naringsregion$anmodning_personer_2016))*100
df = gather(bosatt_anmodede_naringsregion,"variabler","tabellvariabel",-1)
df = separate(df,variabler,c("bosetting","enhet","aar"),sep="_")
sum(is.na(df$tabellvariabel)==T)
df$tabellvariabel[is.na(df$tabellvariabel)==T]="."
#logisk sjekk
nrow(df)==84*3*2
#resterende
df$tabell_navn="bosatt_anmodede"
df$tabellvariabel[df$bosetting=="bosatt"]="."
write.csv(df,"D:/R/imdikator-munch/data_flat_output/bosatt_anmodede-naringsregion-2016-160212.csv",row.names=F)
