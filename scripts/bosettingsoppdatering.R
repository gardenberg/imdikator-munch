#AGGREGERING OG FLATFILERING AV 2016-DATA
#forbehandla krysstabell i excel, med fem kolonner i følgende rekkefølge: kommunenr, aar, anmodning, vedtak og bosatt
#31. januar, 3. februar, 12. februar, 18. februar, 1. mars, 8.mars, 15. mars,22.mars, 8. august, 5. oktober.
#biblioteker

library(tidyverse)
library(reshape)

#parametre
dato <- "161004"
dato_bosatt <- "160930"
dato_anmodning <- "161006"

#data
data = read.csv(paste0("H:/My Documents/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Bosettingsdata 2016/bosatt_anmodede-kommune-2016-",dato,"-rå.csv"), row.names=NULL, na.strings=c("NA",":","."), stringsAsFactors=FALSE, sep=";", dec=",")
data_bosatte = read.csv(paste0("H:/My Documents/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Bosettingsdata 2016/bosatt-kommune-2016-",dato_bosatt,"-rå.csv"), row.names=NULL, na.strings=c("NA",":","."), stringsAsFactors=FALSE, sep=";", dec=",")
data_bosatte$aar = 2016
names(data_bosatte) = c("kommune_nr","bosatte","aar")
data = select(full_join(data,data_bosatte,by=c("kommune_nr","aar")),-bosatte.x)
names(data) = c("kommune_nr","aar","anmodning","vedtak","bosatte")

#2017-data
data_anmodning_2017 = read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Bosettingsdata 2016/anmodning_2017_161006_2.csv", sep=";", stringsAsFactors=FALSE)
data_anmodning_2017 = data_anmodning_2017[grep("[012]",data_anmodning_2017$Kommune),1:2]
data_anmodning_2017 = separate(data_anmodning_2017,Kommune,into=c("kommune_nr","kommune_navn"),sep=5)
data_anmodning_2017 = select(data_anmodning_2017,-kommune_navn)
data_anmodning_2017$kommune_nr = as.integer(data_anmodning_2017$kommune_nr)
names(data_anmodning_2017) = c("kommune_nr","anmodning")
data_anmodning_2017$aar = 2017
data_anmodning_2017$vedtak = 0
data_anmodning_2017$bosatte = 0
data = rbind(data,data_anmodning_2017)

#KOMMUNE 2016
bosatt_anmodede = data
names(bosatt_anmodede)=c("kommune_nr","aar", "anmodning_personer","vedtak_personer","bosatt_personer")
checksum = sum(bosatt_anmodede$vedtak_personer,na.rm=T)
bosatt_anmodede$anmodning_prosent = round(bosatt_anmodede$anmodning_personer/bosatt_anmodede$anmodning_personer*100,1)
bosatt_anmodede$vedtak_prosent = round(bosatt_anmodede$vedtak_personer/bosatt_anmodede$anmodning_personer*100,1)
bosatt_anmodede$bosatt_prosent = round(bosatt_anmodede$bosatt_personer/bosatt_anmodede$anmodning_personer*100,1)
df = gather(bosatt_anmodede,variables,tabellvariabel,3:8)
df = separate(df,variables,into=c("bosetting","enhet"),sep="_")
nlevels(as.factor(df$kommune_nr))==429 #inkluderer kommune 9999
#i anmodnings- og vedtakstall på kommunenivå 2015-2017 er bosetting manglende, anmodning ikke : eller . men 0, og vedtak enten 0 eller ".".
df$tabellvariabel[df$bosetting=="bosatt"&is.na(df$tabellvariabel)==T]="."
df$tabellvariabel[df$bosetting=="anmodning"&is.na(df$tabellvariabel)==T]=0
df$tabellvariabel[df$bosetting=="vedtak"&is.na(df$tabellvariabel)==T]="."
checksum == sum(as.numeric(df$tabellvariabel[df$bosetting=="vedtak"&df$enhet=="personer"]),na.rm=T)
#logisk sjekk
nrow(df)==(429*3*2)+(428*3*2) #knr 9999 i 2016, ikke 2017
#resterende
df$tabell_navn="bosatt_anmodede"
df$kommune_nr[nchar(df$kommune_nr)==3] = paste0("0",df$kommune_nr[nchar(df$kommune_nr)==3])
write.csv(df,paste0("H:/My Documents/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Bosettingsdata 2016/bosatt_anmodede-kommune-2016_2017-",dato,".csv"),row.names=F)

#FYLKE 2016
kinfo = read.csv("H:/My Documents/R/imdikator-munch/parameters/kommunesort.csv", sep=";", stringsAsFactors=FALSE)
kinfo = subset(kinfo,select=c("Nr","Fylkenr"))
names(kinfo)[2] = "fylke_nr"
kinfo$fylke_nr[nchar(kinfo$fylke_nr)==1] = paste0("0",kinfo$fylke_nr[nchar(kinfo$fylke_nr)==1])
#bosettingsdata spesielt og data for aggregering generelt skal ikke leses inn som character, men med NA.strings ="."&":"
#logikken er at det ikke er : på dette nivået, fordi tallene skal aggregeres - det er bare missing
#og hvis det er ":", så skal disse uansett bli . på aggregert n ivå
bosatt_anmodede = data
names(bosatt_anmodede)=c("kommune_nr","aar", "anmodning","vedtak","bosatt")
checksum_start = sum(bosatt_anmodede$vedtak[bosatt_anmodede$aar==2016],na.rm=T)
sum(is.na(bosatt_anmodede$bosatt)==T)
bosatt_anmodede$bosatt[is.na(bosatt_anmodede$bosatt)==T]=0
sum(is.na(bosatt_anmodede$anmodning)==T)
bosatt_anmodede$anmodning[is.na(bosatt_anmodede$anmodning)==T]=0
sum(is.na(bosatt_anmodede$vedtak)==T)
bosatt_anmodede$vedtak[is.na(bosatt_anmodede$vedtak)==T]=0
df = merge(bosatt_anmodede,kinfo,by.x="kommune_nr",by.y ="Nr",all.x=T,all.y=T)
bosatt_anmodede_m = melt.data.frame(df,id.vars=c("kommune_nr","fylke_nr","aar"),na.rm=F)
bosatt_anmodede_fylke = cast(bosatt_anmodede_m,fylke_nr~variable+aar,fun.aggregate=sum,na.rm=T,add.missing=T,margins="grand_row")

#fjerne overflødige kombinasjoner
bosatt_anmodede_fylke = select(bosatt_anmodede_fylke,-grep("_(all)",names(bosatt_anmodede_fylke),fixed=T),-grep("(all)_",names(bosatt_anmodede_fylke),fixed=T),-grep("_NA",names(bosatt_anmodede_fylke),fixed=T))
bosatt_anmodede_fylke$fylke_nr = levels(bosatt_anmodede_fylke$fylke_nr)[bosatt_anmodede_fylke$fylke_nr]
bosatt_anmodede_fylke$fylke_nr = gsub(".all.","00",bosatt_anmodede_fylke$fylke_nr)

#lager aar
bosatt_anmodede_fylke = gather(bosatt_anmodede_fylke,"variabler","tabellvariabel",-1)
bosatt_anmodede_fylke = separate(bosatt_anmodede_fylke,variabler,c("bosetting","aar"),sep="_")

#lager prosent
bosatt_anmodede_fylke = spread(bosatt_anmodede_fylke,bosetting,tabellvariabel)
checksum_start == sum(bosatt_anmodede_fylke$vedtak)/2
names(bosatt_anmodede_fylke)=c("fylke_nr","aar","anmodning_personer","bosatt_personer","vedtak_personer")
bosatt_anmodede_fylke$anmodning_prosent = round((as.numeric(bosatt_anmodede_fylke$anmodning_personer)/as.numeric(bosatt_anmodede_fylke$anmodning_personer))*100,1)
bosatt_anmodede_fylke$vedtak_prosent = round((as.numeric(bosatt_anmodede_fylke$vedtak_personer)/as.numeric(bosatt_anmodede_fylke$anmodning_personer))*100,1)
bosatt_anmodede_fylke$bosatt_prosent = round((as.numeric(bosatt_anmodede_fylke$bosatt_personer)/as.numeric(bosatt_anmodede_fylke$anmodning_personer))*100,1)

df = gather(bosatt_anmodede_fylke,"variabler","tabellvariabel",-1:-2)
df = separate(df,variabler,c("bosetting","enhet"),sep="_")
sum(is.na(df$tabellvariabel)==T)
df$tabellvariabel[is.na(df$tabellvariabel)==T]="."
#logisk sjekk
nrow(df)==21*3*2*2
checksum_start==as.integer(df$tabellvariabel[df$fylke_nr=="00"&df$bosetting=="vedtak"&df$aar=="2016"&df$enhet=="personer"])+as.integer(df$tabellvariabel[df$fylke_nr=="00"&df$bosetting=="vedtak"&df$aar=="2017"&df$enhet=="personer"])
#resterende
df$tabell_navn="bosatt_anmodede"
df$tabellvariabel[df$tabellvariabel=="Inf"]="."
write.csv(df,paste0("H:/My Documents/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Bosettingsdata 2016/bosatt_anmodede-fylke-2016_2017-",dato,".csv"),row.names=F)

#Næringsregion 2016
kinfo <- read.csv("H:/My Documents/R/imdikator-munch/parameters/kommunesort.csv", sep=";", stringsAsFactors=FALSE)
kinfo = subset(kinfo,select=c("Nr","Naringregnr"))
names(kinfo)[2] = "naringsregion_nr"
kinfo$naringsregion_nr[nchar(kinfo$naringsregion_nr)==1] = paste0("0",kinfo$naringsregion_nr[nchar(kinfo$naringsregion_nr)==1])
#bosettingsdata spesielt og data for aggregering generelt skal ikke leses inn som character, men med NA.strings ="."&":"
#logikken er at det ikke er : på dette nivået, fordi tallene skal aggregeres - det er bare missing
#og hvis det er ":", så skal disse uansett bli . på aggregert n ivå
bosatt_anmodede = data
names(bosatt_anmodede)=c("kommune_nr","aar", "anmodning","vedtak","bosatt")
checksum_start = sum(bosatt_anmodede$vedtak[bosatt_anmodede$aar==2016],na.rm=T)
sum(is.na(bosatt_anmodede$bosatt)==T)
bosatt_anmodede$bosatt[is.na(bosatt_anmodede$bosatt)==T]=0
sum(is.na(bosatt_anmodede$anmodning)==T)
bosatt_anmodede$anmodning[is.na(bosatt_anmodede$anmodning)==T]=0
sum(is.na(bosatt_anmodede$vedtak)==T)
bosatt_anmodede$vedtak[is.na(bosatt_anmodede$vedtak)==T]=0
df = merge(bosatt_anmodede,kinfo,by.x="kommune_nr",by.y ="Nr",all.x=T,all.y=T)
bosatt_anmodede_m = melt.data.frame(df,id.vars=c("kommune_nr","naringsregion_nr","aar"),na.rm=F)
bosatt_anmodede_naringsregion = cast(bosatt_anmodede_m,naringsregion_nr~variable+aar,fun.aggregate=sum,na.rm=T)

#fjerne overflødige kombinasjoner
bosatt_anmodede_naringsregion = select(bosatt_anmodede_naringsregion,-grep("_NA",names(bosatt_anmodede_naringsregion),fixed=T))

#lager aar
df = gather(bosatt_anmodede_naringsregion,"variabler","tabellvariabel",-1)
df = separate(df,variabler,c("bosetting","aar"),sep="_")
bosatt_anmodede_naringsregion = spread(df,bosetting,tabellvariabel)

checksum_start == sum(bosatt_anmodede_naringsregion$vedtak)
names(bosatt_anmodede_naringsregion)=c("naringsregion_nr","aar","anmodning_personer","bosatt_personer","vedtak_personer")
bosatt_anmodede_naringsregion$anmodning_prosent = round((as.numeric(bosatt_anmodede_naringsregion$anmodning_personer)/as.numeric(bosatt_anmodede_naringsregion$anmodning_personer))*100,1)
bosatt_anmodede_naringsregion$vedtak_prosent = round((as.numeric(bosatt_anmodede_naringsregion$vedtak_personer)/as.numeric(bosatt_anmodede_naringsregion$anmodning_personer))*100,1)
bosatt_anmodede_naringsregion$bosatt_prosent = round((as.numeric(bosatt_anmodede_naringsregion$bosatt_personer)/as.numeric(bosatt_anmodede_naringsregion$anmodning_personer))*100,1)
df = gather(bosatt_anmodede_naringsregion,"variabler","tabellvariabel",-1:-2)
df = separate(df,variabler,c("bosetting","enhet"),sep="_")
sum(is.na(df$tabellvariabel)==T)
df$tabellvariabel[is.na(df$tabellvariabel)==T]="."
#logisk sjekk
nrow(df)==84*3*2*2
#resterende
df$tabell_navn="bosatt_anmodede"
df$tabellvariabel[df$tabellvariabel=="Inf"]="."
write.csv(df,paste0("H:/My Documents/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Bosettingsdata 2016/bosatt_anmodede-naringsregion-2016_2017-",dato,".csv"),row.names=F)

#HIsTORISKE DATA
data <- read.csv("~/Ifakta/Dataleveranse/Bosetting/anmodning_vedtak_1995-2013.csv", sep=";", stringsAsFactors=FALSE, na.strings = c(" ",""))
names(data) = c("kommune","anmodning","vedtak","aar")
data$anmodning = as.integer(data$anmodning)
data$vedtak = as.integer(data$vedtak)
data_kommune = data[grep("[012]",data$kommune),]
data_kommune = separate(data_kommune,kommune,into=c("kommune_nr","kommune_navn"),sep=4)

#eksporter evt. dette som svar på personhenvendelsen

#sjekker innholdet
apply(data_kommune,2, function(x){levels(as.factor(x))})

#legger inn bosettingsdata som vi har
bosatte <- read.csv("~/Ifakta/Dataleveranse/Bosetting/bosatt_anmodede-161021.csv", stringsAsFactors=FALSE, fileEncoding = "UTF-8-BOM")
bosatte = select(filter(bosatte,kommune_nr!="NULL",bosetting=="bosatt",enhet=="personer"),aar,bosetting,tabellvariabel,kommune_nr)
bosatte = spread(bosatte,bosetting,tabellvariabel)
data_kommune = full_join(data_kommune,bosatte,by=c("kommune_nr","aar"))

names(data_kommune)=c("kommune_nr","kommune_navn","anmodning_personer","vedtak_personer","aar","bosatt_personer")
data_kommune$bosatt_personer = as.integer(data_kommune$bosatt_personer)

data_kommune$anmodning_prosent = round(data_kommune$anmodning_personer/data_kommune$anmodning_personer*100,1)
data_kommune$vedtak_prosent = round(data_kommune$vedtak_personer/data_kommune$anmodning_personer*100,1)
data_kommune$bosatt_prosent = round(data_kommune$bosatt_personer/data_kommune$anmodning_personer*100,1)

df = gather(select(data_kommune,-kommune_navn),variabler,tabellvariabel,c(2,3,5,6,7,8))
df = separate(df,variabler,into=c("bosetting","enhet"),sep="_")
df$kommune_nr = as.integer(df$kommune_nr)

#i anmodnings- og vedtakstall på kommunenivå 1995-2013 er 
#bosetting manglende i hovedsak, 
#ikke oppgitt anmodning er anmodning på "." - vi har ikke anmoda denne kommunen. | eller 0 - 
#ikke oppgitt vedtak er "." - vi har manglende data.
#ikke oppgitt vedtak fordi kommunen ikke er med er "."

sum(is.na(df$tabellvariabel))
df$tabellvariabel[is.na(df$tabellvariabel)==T] = "."

#legger til manglende kommuner
kommuneegenskaper <- read.csv("~/R/imdikator-munch/parameters/kommuneegenskaper.csv", stringsAsFactors=FALSE)
kommuneegenskaper = select(distinct(kommuneegenskaper,kommune_nr),kommune_nr,kommune_navn,fylke_nr,naringsregion_nr)
t = full_join(df,kommuneegenskaper)

#duplikater
duplikater = rbind(t[duplicated(t,fromLast = F),],t[duplicated(t,fromLast = T),]) #finner bare nøyaktige kopier
t = distinct(t) #fjerner alle som er helt like, men ikke de som har ulik tabellvariabel
duplikater_2 = rbind(t[duplicated(select(t,-tabellvariabel),fromLast = F),],t[duplicated(select(t,-tabellvariabel),fromLast = T),])

#manuell fjerning
duplikater_2 = t[c(8566,8567,26736,26737,3801,3802,21971,21972,28029,28030),]
t = t[-c(8567,26737,3801,21972,28029),]

#duplikater_2 = anti_join(t,distinct(t,kommune_nr,aar,bosetting,enhet))
#duplikater_2 = t[c(8477,8478,26380,26381,3801,3802,21704,21705,27673,27674),]
#t_unik = t[-c(8478,26381,3801,21704,27674),]
#t_unik = distinct(t,kommune_nr,aar,bosetting,enhet) - gir tilfeldige enheter her.

#muterer inn manglende kombinasjoner
t = spread(t,aar,tabellvariabel,fill=".")
t = select(filter(t,is.na(bosetting)==F),-`<NA>`)
t = gather(t,aar,tabellvariabel,7:25)

#sjekk av antallet kombinasjoner
nlevels(as.factor(t$aar))
nrow(t) == nrow(distinct(t,kommune_nr))*3*2*19 #441 kommuner i dette settet

#resterende
t$tabell_navn="bosatt_anmodede"
t$kommune_nr[nchar(df$kommune_nr)==3] = paste0("0",t$kommune_nr[nchar(df$kommune_nr)==3])
write.csv(select(t,-fylke_nr,-naringsregion_nr,-kommune_navn),"bosatte_anmodning-kommune-1995-2013.csv",row.names=F)