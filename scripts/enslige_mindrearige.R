#AGGREGERING OG FLATFILERING AV DaTA OM ENSLIGE MINDREÅRIGE
#forbehandla krysstabell i excel, med fem kolonner i følgende rekkefølge: kommunenr, aar, anmodning, vedtak og bosatt

#biblioteker
library(tidyr)
library(reshape)
library(dplyr)

#parametre
dato <- "161004"
#data = read.csv(paste0("H:/My Documents/R/imdikator-munch/data_crossed_input/enslige_mindrearige-kommune-2016-",dato,".csv"), row.names=NULL, na.strings=c("NA",":","."), stringsAsFactors=FALSE, sep=";", dec=",")
data = read.csv(paste0("H:/My Documents/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Bosettingsdata 2016/enslige_mindrearige-kommune-2016-",dato,"-rå.csv"), row.names=NULL, na.strings=c("NA",":","."), stringsAsFactors=FALSE, sep=";", dec=",")

#KOMMUNE 2016
enslige_mindrearige = data
names(enslige_mindrearige)=c("kommune_nr","aar", "anmodning_personer","vedtak_personer","bosatt_personer")
checksum = sum(enslige_mindrearige$vedtak_personer,na.rm=T)
enslige_mindrearige$anmodning_prosent = round(enslige_mindrearige$anmodning_personer/enslige_mindrearige$anmodning_personer*100,1)
enslige_mindrearige$vedtak_prosent = round(enslige_mindrearige$vedtak_personer/enslige_mindrearige$anmodning_personer*100,1)
enslige_mindrearige$bosatt_prosent = round(enslige_mindrearige$bosatt_personer/enslige_mindrearige$anmodning_personer*100,1)
df = gather(enslige_mindrearige,variables,tabellvariabel,3:8)
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
df$tabell_navn="enslige_mindrearige"
df$kommune_nr[nchar(df$kommune_nr)==3] = paste0("0",df$kommune_nr[nchar(df$kommune_nr)==3])
#write.csv(df,paste0("H:/My Documents/R/imdikator-munch/data_flat_output/enslige_mindrearige-kommune-2016-",dato,".csv"),row.names=F)
write.csv(df,paste0("H:/My Documents/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Bosettingsdata 2016/enslige_mindrearige-kommune-2016-",dato,".csv"),row.names=F)

#FYLKE 2016
kinfo = read.csv("H:/My Documents/R/imdikator-munch/parameters/kommunesort.csv", sep=";", stringsAsFactors=FALSE)
kinfo = subset(kinfo,select=c("Nr","Fylkenr"))
names(kinfo)[2] = "fylke_nr"
kinfo$fylke_nr[nchar(kinfo$fylke_nr)==1] = paste0("0",kinfo$fylke_nr[nchar(kinfo$fylke_nr)==1])
#bosettingsdata spesielt og data for aggregering generelt skal ikke leses inn som character, men med NA.strings ="."&":"
#logikken er at det ikke er : på dette nivået, fordi tallene skal aggregeres - det er bare missing
#og hvis det er ":", så skal disse uansett bli . på aggregert n ivå
enslige_mindrearige = data
checksum_start = sum(enslige_mindrearige$vedtak[enslige_mindrearige$aar==2016],na.rm=T)
sum(is.na(enslige_mindrearige$bosatt)==T)
enslige_mindrearige$bosatt[is.na(enslige_mindrearige$bosatt)==T]=0
sum(is.na(enslige_mindrearige$anmodning)==T)
enslige_mindrearige$anmodning[is.na(enslige_mindrearige$anmodning)==T]=0
sum(is.na(enslige_mindrearige$vedtak)==T)
enslige_mindrearige$vedtak[is.na(enslige_mindrearige$vedtak)==T]=0
df = merge(enslige_mindrearige,kinfo,by.x="kommune_nr",by.y ="Nr",all.x=T,all.y=T)
enslige_mindrearige_m = melt.data.frame(df,id.vars=c("kommune_nr","fylke_nr","aar"),na.rm=F)
enslige_mindrearige_fylke = cast(enslige_mindrearige_m,fylke_nr~variable,fun.aggregate=sum,na.rm=T,add.missing=T,margins="grand_row")
enslige_mindrearige_fylke = subset(enslige_mindrearige_fylke,select=-5)
checksum_start == sum(enslige_mindrearige_fylke$vedtak)/2
names(enslige_mindrearige_fylke)=c("fylke_nr","anmodning_personer_2016","vedtak_personer_2016","bosatt_personer_2016")
enslige_mindrearige_fylke$anmodning_prosent_2016 = round((as.numeric(enslige_mindrearige_fylke$anmodning_personer_2016)/as.numeric(enslige_mindrearige_fylke$anmodning_personer_2016))*100,1)
enslige_mindrearige_fylke$vedtak_prosent_2016 = round((as.numeric(enslige_mindrearige_fylke$vedtak_personer_2016)/as.numeric(enslige_mindrearige_fylke$anmodning_personer_2016))*100,1)
enslige_mindrearige_fylke$bosatt_prosent_2016 = round((as.numeric(enslige_mindrearige_fylke$bosatt_personer_2016)/as.numeric(enslige_mindrearige_fylke$anmodning_personer_2016))*100,1)
df = gather(enslige_mindrearige_fylke,"variabler","tabellvariabel",-1)
df = separate(df,variabler,c("bosetting","enhet","aar"),sep="_")
sum(is.na(df$tabellvariabel)==T)
df$tabellvariabel[is.na(df$tabellvariabel)==T]="."
#logisk sjekk
nrow(df)==21*3*2
#.==regexp any single character
df$fylke_nr = gsub(".all.","00",df$fylke_nr)
checksum_start==df$tabellvariabel[df$fylke_nr=="00"&df$bosetting=="vedtak"&df$aar=="2016"&df$enhet=="personer"]
#resterende
df$tabell_navn="enslige_mindrearige"
df$tabellvariabel[df$bosetting=="bosatt"]="."
write.csv(df,paste0("H:/My Documents/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Bosettingsdata 2016/enslige_mindrearige-fylke-2016-",dato,".csv"),row.names=F)

#Næringsregion 2016
kinfo <- read.csv("H:/My Documents/R/imdikator-munch/parameters/kommunesort.csv", sep=";", stringsAsFactors=FALSE)
kinfo = subset(kinfo,select=c("Nr","Naringregnr"))
names(kinfo)[2] = "naringsregion_nr"
kinfo$naringsregion_nr[nchar(kinfo$naringsregion_nr)==1] = paste0("0",kinfo$naringsregion_nr[nchar(kinfo$naringsregion_nr)==1])
#bosettingsdata spesielt og data for aggregering generelt skal ikke leses inn som character, men med NA.strings ="."&":"
#logikken er at det ikke er : på dette nivået, fordi tallene skal aggregeres - det er bare missing
#og hvis det er ":", så skal disse uansett bli . på aggregert n ivå
enslige_mindrearige = data
checksum_start = sum(enslige_mindrearige$vedtak[enslige_mindrearige$aar==2016],na.rm=T)
sum(is.na(enslige_mindrearige$bosatt)==T)
enslige_mindrearige$bosatt[is.na(enslige_mindrearige$bosatt)==T]=0
sum(is.na(enslige_mindrearige$anmodning)==T)
enslige_mindrearige$anmodning[is.na(enslige_mindrearige$anmodning)==T]=0
sum(is.na(enslige_mindrearige$vedtak)==T)
enslige_mindrearige$vedtak[is.na(enslige_mindrearige$vedtak)==T]=0
df = merge(enslige_mindrearige,kinfo,by.x="kommune_nr",by.y ="Nr",all.x=T,all.y=T)
enslige_mindrearige_m = melt.data.frame(df,id.vars=c("kommune_nr","naringsregion_nr","aar"),na.rm=F)
enslige_mindrearige_naringsregion = cast(enslige_mindrearige_m,naringsregion_nr~variable,fun.aggregate=sum,na.rm=T)
checksum_start == sum(enslige_mindrearige_naringsregion$vedtak)
names(enslige_mindrearige_naringsregion)=c("naringsregion_nr","anmodning_personer_2016","vedtak_personer_2016","bosatt_personer_2016")
enslige_mindrearige_naringsregion$anmodning_prosent_2016 = round((as.numeric(enslige_mindrearige_naringsregion$anmodning_personer_2016)/as.numeric(enslige_mindrearige_naringsregion$anmodning_personer_2016))*100,1)
enslige_mindrearige_naringsregion$vedtak_prosent_2016 = round((as.numeric(enslige_mindrearige_naringsregion$vedtak_personer_2016)/as.numeric(enslige_mindrearige_naringsregion$anmodning_personer_2016))*100,1)
enslige_mindrearige_naringsregion$bosatt_prosent_2016 = round((as.numeric(enslige_mindrearige_naringsregion$bosatt_personer_2016)/as.numeric(enslige_mindrearige_naringsregion$anmodning_personer_2016))*100,1)
df = gather(enslige_mindrearige_naringsregion,"variabler","tabellvariabel",-1)
df = separate(df,variabler,c("bosetting","enhet","aar"),sep="_")
sum(is.na(df$tabellvariabel)==T)
df$tabellvariabel[is.na(df$tabellvariabel)==T]="."
#logisk sjekk
nrow(df)==84*3*2
#resterende
df$tabell_navn="enslige_mindrearige"
df$tabellvariabel[df$bosetting=="bosatt"]="."
write.csv(df,paste0("H:/My Documents/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Bosettingsdata 2016/enslige_mindrearige-naringsregion-2016-",dato,".csv"),row.names=F)

#2014 og 2015-data
data_2014 = read.csv("H:/My Documents/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 1-2016 fra Vest/leveranse_1/EM 2014 iFakta.csv", row.names=NULL, colClasses="character", sep=";", dec=",")
data_2014 = select(data_2014,1:5)
data_2014$aar ="2014"
data_2015 = read.csv("H:/My Documents/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 1-2016 fra Vest/leveranse_1/EM 2015 iFakta.csv", row.names=NULL, colClasses="character", sep=";", dec=",")
data_2015 = select(data_2015,1:5)
names(data_2015)[5]="Bosatt"
data_2015$aar = "2015"
data = rbind(data_2014,data_2015)
data = select(data,-Kommunenavn)

#KOMMUNE 2014-2015
enslige_mindrearige = data
names(enslige_mindrearige)=c("kommune_nr","anmodning_personer","vedtak_personer","bosatt_personer","aar")
checksum = sum(extract_numeric(enslige_mindrearige$vedtak_personer),na.rm=T)
enslige_mindrearige = gather(enslige_mindrearige,bosetting,tabellvariabel,2:4)
#i anmodnings- og vedtakstall på kommunenivå 2014-2015 er 
#anmodning som mangler 0
#vedtak som mangler 0
#bosetting som mangler 0
#manglende data 0
sum(is.na(enslige_mindrearige$tabellvariabel))
levels(as.factor(enslige_mindrearige$tabellvariabel))
enslige_mindrearige$tabellvariabel[enslige_mindrearige$tabellvariabel==""|enslige_mindrearige$tabellvariabel==" "|enslige_mindrearige$tabellvariabel=="  "]="0"
levels(as.factor(enslige_mindrearige$tabellvariabel))
data = enslige_mindrearige
#prikking
enslige_mindrearige$tabellvariabel[extract_numeric(enslige_mindrearige$tabellvariabel)<4&extract_numeric(enslige_mindrearige$tabellvariabel)>0&enslige_mindrearige$bosetting=="bosatt_personer"]=":"
enslige_mindrearige = spread(enslige_mindrearige,bosetting,tabellvariabel)
#prosenter
enslige_mindrearige$anmodning_prosent = round(extract_numeric(enslige_mindrearige$anmodning_personer)/extract_numeric(enslige_mindrearige$anmodning_personer)*100,1)
enslige_mindrearige$vedtak_prosent = round(extract_numeric(enslige_mindrearige$vedtak_personer)/extract_numeric(enslige_mindrearige$anmodning_personer)*100,1)
enslige_mindrearige$bosatt_prosent = round(extract_numeric(enslige_mindrearige$bosatt_personer)/extract_numeric(enslige_mindrearige$anmodning_personer)*100,1)
df = gather(enslige_mindrearige,variables,tabellvariabel,3:8)
df = separate(df,variables,into=c("bosetting","enhet"),sep="_")
#må kode om NaN og Inf
levels(as.factor(df$tabellvariabel))
sum(is.na(df$tabellvariabel)==T)
df$tabellvariabel[is.na(df$tabellvariabel)==T]="." #NA kodes her som ".".
df$tabellvariabel[df$tabellvariabel=="NaN"]="." #NaN kodes her som "."
df$tabellvariabel[df$tabellvariabel=="Inf"]="." #Inf kodes her som "."
nlevels(as.factor(df$kommune_nr))==429
checksum == sum(extract_numeric(df$tabellvariabel[df$bosetting=="vedtak"&df$enhet=="personer"]),na.rm=T)
#logisk sjekk
nrow(df)==429*2*3*2
levels(as.factor(df$tabellvariabel))
#resterende
df$tabell_navn="enslige_mindrearige"
df$kommune_nr[nchar(df$kommune_nr)==3] = paste0("0",df$kommune_nr[nchar(df$kommune_nr)==3])
#write.csv(df,paste0("H:/My Documents/R/imdikator-munch/data_flat_output/enslige_mindrearige-kommune-2016-",dato,".csv"),row.names=F)
write.csv(df,"H:/My Documents/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 1-2016 fra Vest/enslige_mindrearige-kommune-2014_2015.csv",row.names=F)

#FYLKE 2016
kinfo = read.csv("H:/My Documents/R/imdikator-munch/parameters/kommunesort.csv", sep=";", stringsAsFactors=FALSE)
kinfo = subset(kinfo,select=c("Nr","Fylkenr"))
names(kinfo)[2] = "fylke_nr"
kinfo$fylke_nr[nchar(kinfo$fylke_nr)==1] = paste0("0",kinfo$fylke_nr[nchar(kinfo$fylke_nr)==1])
#bosettingsdata spesielt og data for aggregering generelt skal ikke leses inn som character, men med NA.strings ="."&":"
#logikken er at det ikke er : på dette nivået, fordi tallene skal aggregeres - det er bare missing
#og hvis det er ":", så skal disse uansett bli . på aggregert n ivå
enslige_mindrearige = data
enslige_mindrearige$tabellvariabel = extract_numeric(enslige_mindrearige$tabellvariabel)
enslige_mindrearige = spread(enslige_mindrearige,bosetting,tabellvariabel)
checksum_start = sum(enslige_mindrearige$vedtak_personer,na.rm=T)
sum(is.na(enslige_mindrearige$bosatt_personer)==T)
sum(is.na(enslige_mindrearige$anmodning_personer)==T)
sum(is.na(enslige_mindrearige$vedtak_personer)==T)
df = merge(enslige_mindrearige,kinfo,by.x="kommune_nr",by.y ="Nr",all.x=T,all.y=T)
data_enslige_mindrearige = melt.data.frame(df,id.vars=c("kommune_nr","fylke_nr","aar"),na.rm=F)
enslige_mindrearige_fylke = cast(data_enslige_mindrearige,fylke_nr~variable+aar,fun.aggregate=sum,na.rm=T,add.missing=T,margins="grand_row")
enslige_mindrearige_fylke = select(enslige_mindrearige_fylke,-grep("_(all)",names(enslige_mindrearige_fylke),fixed=T),-grep("(all)_",names(enslige_mindrearige_fylke),fixed=T))
df = gather(enslige_mindrearige_fylke,"variabler","tabellvariabel",-1)
df = separate(df,variabler,into=c("variabler","enhet","aar"),"_")
#prikking av alle bosettingstall mellom 4 og 0
df$tabellvariabel[df$tabellvariabel<4&df$tabellvariabel>0&df$variabler=="bosatt"]=":"
df = spread(df,variabler,tabellvariabel)
enslige_mindrearige_fylke=select(df,-enhet)
#prosenter
names(enslige_mindrearige_fylke)=c("fylke_nr","aar","anmodning_personer","bosatt_personer","vedtak_personer")
enslige_mindrearige_fylke$anmodning_prosent = round(as.numeric(enslige_mindrearige_fylke$anmodning_personer)/as.numeric(enslige_mindrearige_fylke$anmodning_personer)*100,1)
enslige_mindrearige_fylke$vedtak_prosent = round(as.numeric(enslige_mindrearige_fylke$vedtak_personer)/as.numeric(enslige_mindrearige_fylke$anmodning_personer)*100,1)
enslige_mindrearige_fylke$bosatt_prosent = round(as.numeric(enslige_mindrearige_fylke$bosatt_personer)/as.numeric(enslige_mindrearige_fylke$anmodning_personer)*100,1)
#her lager jeg midlertidig NaN-verdier og en Inf-verdi
df = gather(enslige_mindrearige_fylke,"variabler","tabellvariabel",anmodning_personer:bosatt_prosent)
df = separate(df,variabler,c("bosetting","enhet"),sep="_")
levels(as.factor(df$fylke_nr))
levels(as.factor(df$tabellvariabel))
sum(is.na(df$tabellvariabel)==T)
df$tabellvariabel[df$tabellvariabel=="NaN"]="."
levels(as.factor(df$tabellvariabel))
#logisk sjekk
nrow(df)==21*3*2*2
#.==regexp any single character
df$fylke_nr = gsub(".all.","00",df$fylke_nr)
levels(as.factor(df$fylke_nr))
checksum_start==sum(extract_numeric(df$tabellvariabel[df$fylke_nr=="00"&df$bosetting=="vedtak"&df$enhet=="personer"]))
#resterende
df$tabell_navn="enslige_mindrearige"
write.csv(df,"H:/My Documents/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 1-2016 fra Vest/enslige_mindrearige-fylke-2014_2015.csv",row.names=F)

#NÆRINGSREGION
kinfo = read.csv("H:/My Documents/R/imdikator-munch/parameters/kommunesort.csv", sep=";", stringsAsFactors=FALSE)
kinfo = subset(kinfo,select=c("Nr","Naringregnr"))
names(kinfo)[2] = "naringsregion_nr"
kinfo$naringsregion_nr[nchar(kinfo$naringsregion_nr)==1] = paste0("0",kinfo$naringsregion_nr[nchar(kinfo$naringsregion_nr)==1])
#bosettingsdata spesielt og data for aggregering generelt skal ikke leses inn som character, men med NA.strings ="."&":"
#logikken er at det ikke er : på dette nivået, fordi tallene skal aggregeres - det er bare missing
#og hvis det er ":", så skal disse uansett bli . på aggregert n ivå
enslige_mindrearige = data
enslige_mindrearige$tabellvariabel = extract_numeric(enslige_mindrearige$tabellvariabel)
enslige_mindrearige = spread(enslige_mindrearige,bosetting,tabellvariabel)
checksum_start = sum(enslige_mindrearige$vedtak,na.rm=T)
sum(is.na(enslige_mindrearige$bosatt_personer)==T)
sum(is.na(enslige_mindrearige$anmodning_personer)==T)
sum(is.na(enslige_mindrearige$vedtak_personer)==T)
df = merge(enslige_mindrearige,kinfo,by.x="kommune_nr",by.y ="Nr",all.x=T,all.y=T)
data_enslige_mindrearige = melt.data.frame(df,id.vars=c("kommune_nr","naringsregion_nr","aar"),na.rm=F)
enslige_mindrearige_nareg = cast(data_enslige_mindrearige,naringsregion_nr~variable+aar,fun.aggregate=sum,na.rm=T,add.missing=T)
df = gather(enslige_mindrearige_nareg,"variabler","tabellvariabel",-1)
df = separate(df,variabler,into=c("variabler","enhet","aar"),"_")
#prikking av alle bosettingstall mellom 4 og 0
df$tabellvariabel[df$tabellvariabel<4&df$tabellvariabel>0&df$variabler=="bosatt"]=":"
df = spread(df,variabler,tabellvariabel)
enslige_mindrearige_nareg=select(df,-enhet)
#prosenter
names(enslige_mindrearige_nareg)=c("naringsregion_nr","aar","anmodning_personer","bosatt_personer","vedtak_personer")
enslige_mindrearige_nareg$anmodning_prosent = round(as.numeric(enslige_mindrearige_nareg$anmodning_personer)/as.numeric(enslige_mindrearige_nareg$anmodning_personer)*100,1)
enslige_mindrearige_nareg$vedtak_prosent = round(as.numeric(enslige_mindrearige_nareg$vedtak_personer)/as.numeric(enslige_mindrearige_nareg$anmodning_personer)*100,1)
enslige_mindrearige_nareg$bosatt_prosent = round(as.numeric(enslige_mindrearige_nareg$bosatt_personer)/as.numeric(enslige_mindrearige_nareg$anmodning_personer)*100,1)
df = gather(enslige_mindrearige_nareg,"variabler","tabellvariabel",anmodning_personer:bosatt_prosent)
df = separate(df,variabler,c("bosetting","enhet"),sep="_")
sum(is.na(df$tabellvariabel)==T)
df$tabellvariabel[is.na(df$tabellvariabel)==T]="."
df$tabellvariabel[df$tabellvariabel==Inf]="."
df$tabellvariabel[df$tabellvariabel=="NaN"]="."
#logisk sjekk
nrow(df)==84*3*2*2
levels(as.factor(df$naringsregion_nr))
levels(as.factor(df$tabellvariabel))
checksum_start==sum(extract_numeric(df$tabellvariabel[df$bosetting=="vedtak"&df$enhet=="personer"]))
#resterende
df$tabell_navn="enslige_mindrearige"
write.csv(df,"H:/My Documents/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 1-2016 fra Vest/enslige_mindrearige-nareg-2014_2015.csv",row.names=F)

