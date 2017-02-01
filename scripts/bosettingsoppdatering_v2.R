#AGGREGERING OG FLATFILERING AV 2016-DATA
#forbehandla krysstabell i excel, med fem kolonner i følgende rekkefølge: kommunenr, aar, anmodning, vedtak og bosatt
#31. januar, 3. februar, 12. februar, 18. februar, 1. mars, 8.mars, 15. mars,22.mars, 8. august, 5. oktober.
#biblioteker

library(tidyverse)
library(reshape)

#parametre
dato <- "161103"
dato_bosatt <- "161031"
sti = "H:/My Documents/Ifakta/Dataleveranse/Bosetting/"
variabelnavn = paste0("V",1:121)
vnavn = c("kommune","bosatt_2014","anmodning_2015","vedtak_2015","bosatt_2015","anmodning.1_2016","anmodning.2_2016","anmodning.2.em_2016","vedtak_2016","vedtak.em_2016","anmodning.1_2017","anmodning.2_2017","anmodning.2.em_2017","vedtak.1_2017","vedtak.1.em_2017","anmodning.3_2017","anmodning.3.em_2017","anmodning.em.o15_2017","anmodning.em.u15_2017","vedtak.2_2017","vedtak.2.em_2017","vedtak.em.o15_2017","vedtak.em.u15_2017")
variabelnavn[1:23] = vnavn

#data
data = read.csv(paste0(sti,"bosatt_anmodede-kommune-2016-",dato,"-rå.csv"), header=F,row.names=NULL, na.strings=c("NA",":","."), stringsAsFactors=FALSE, sep=";", dec=",",col.names=variabelnavn)

#munger
t = select(data,kommune,anmodning.2_2016,anmodning.2.em_2016,vedtak_2016,vedtak.em_2016,anmodning.3_2017,anmodning.3.em_2017,vedtak.2_2017,vedtak.2.em_2017)
t = t[grep("[012]",t$kommune),] #ender opp med tre linjer for mye
t = t[-1:-3,]
t = separate(t,kommune,into=c("kommune_nr","kommune_navn"),sep=4) #skiller ut kommunenummer
#t = separate(t,anmodning.2.kombi_2016,into=c("anmodning_2016","anmodning.em_2016"),sep="[:punct:]",remove=T) #skiller ut em 2016 - nå defunct
names(t)[3:10]=c("anmodning_2016","anmodning.em_2016","vedtak_2016","vedtak.em_2016","anmodning_2017","anmodning.em_2017","vedtak_2017","vedtak.em_2017")
t = gather(t,variabler,tabellvariabel,3:10)
t = separate(t,variabler,into=c("variabler","aar"),sep="_")
t = spread(t,variabler,tabellvariabel)
t$kommune_nr = as.integer(t$kommune_nr)
t$aar = as.integer(t$aar)

#bosettingsdata leveres i egen fil...
data_bosatte = read.csv(paste0(sti,"bosatt-kommune-2016-",dato_bosatt,"-rå.csv"), row.names=NULL, na.strings=c("NA",":","."), stringsAsFactors=FALSE, sep=";", dec=",")
data_bosatte$aar = 2016
names(data_bosatte) = c("kommune_nr","bosatte","aar")

#joiner
data = select(full_join(t,data_bosatte,by=c("kommune_nr","aar")),-kommune_navn)
data$bosatte.em = "."
data[,2:8] = apply(data[,2:8],2, function(x){as.integer(x)})

#KOMMUNE
bosatt_anmodede = data
names(bosatt_anmodede)=c("kommune_nr","aar", "anmodning_personer","anmodning.em_personer","vedtak_personer","vedtak.em_personer","bosatt_personer","bosatt.em_personer")
checksum = sum(bosatt_anmodede$vedtak_personer,na.rm=T)
bosatt_anmodede$anmodning_prosent = round(bosatt_anmodede$anmodning_personer/bosatt_anmodede$anmodning_personer*100,1)
bosatt_anmodede$anmodning.em_prosent = round(bosatt_anmodede$anmodning.em_personer/bosatt_anmodede$anmodning.em_personer*100,1)
bosatt_anmodede$vedtak_prosent = round(bosatt_anmodede$vedtak_personer/bosatt_anmodede$anmodning_personer*100,1)
bosatt_anmodede$vedtak.em_prosent = round(bosatt_anmodede$vedtak.em_personer/bosatt_anmodede$anmodning.em_personer*100,1)
bosatt_anmodede$bosatt_prosent = round(bosatt_anmodede$bosatt_personer/bosatt_anmodede$anmodning_personer*100,1)
bosatt_anmodede$bosatt.em_prosent = round(bosatt_anmodede$bosatt.em_personer/bosatt_anmodede$anmodning.em_personer*100,1)
df = gather(bosatt_anmodede,variables,tabellvariabel,3:14)
df = separate(df,variables,into=c("bosetting","enhet"),sep="_")
nlevels(as.factor(df$kommune_nr))==429 #inkluderer kommune 9999
#i anmodnings- og vedtakstall på kommunenivå 2015-2017 er bosetting manglende, anmodning ikke : eller . men 0, og vedtak enten 0 eller ".".
df$tabellvariabel[df$bosetting=="bosatt"&is.na(df$tabellvariabel)==T]="."
df$tabellvariabel[df$bosetting=="bosatt.em"&is.na(df$tabellvariabel)==T]="."
df$tabellvariabel[df$bosetting=="anmodning"&is.na(df$tabellvariabel)==T]=0
df$tabellvariabel[df$bosetting=="anmodning.em"&is.na(df$tabellvariabel)==T]=0
df$tabellvariabel[df$bosetting=="vedtak"&is.na(df$tabellvariabel)==T]="."
df$tabellvariabel[df$bosetting=="vedtak.em"&is.na(df$tabellvariabel)==T]="."
checksum == sum(as.numeric(df$tabellvariabel[df$bosetting=="vedtak"&df$enhet=="personer"]),na.rm=T)
#logisk sjekk
nrow(df)==(429*2*6)+(428*2*6) #knr 9999 i 2016, ikke 2017
#resterende
df$tabell_navn="bosatt_anmodede"
df$kommune_nr[nchar(df$kommune_nr)==3] = paste0("0",df$kommune_nr[nchar(df$kommune_nr)==3])
#na-sjekk
sum(is.na(df$aar))
#bosetting
write.csv(filter(df,bosetting=="anmodning"|bosetting=="vedtak"|bosetting=="bosatt"),paste0(sti,"bosatt_anmodede-kommune-2016_2017-",dato,".csv"),row.names=F)
#enslige mindreårige
t = separate(filter(df,bosetting=="anmodning.em"|bosetting=="vedtak.em"|bosetting=="bosatt.em"),bosetting,into="bosetting",sep="\\.")
t$tabell_navn = "enslige_mindrearige"
write.csv(t,paste0(sti,"enslige_mindrearige-kommune-2016_2017-",dato,".csv"),row.names=F)

#FYLKE 2016
kinfo = read.csv("H:/My Documents/R/imdikator-munch/parameters/kommunesort.csv", sep=";", stringsAsFactors=FALSE)
kinfo = subset(kinfo,select=c("Nr","Fylkenr"))
names(kinfo)[2] = "fylke_nr"
kinfo$fylke_nr[nchar(kinfo$fylke_nr)==1] = paste0("0",kinfo$fylke_nr[nchar(kinfo$fylke_nr)==1])
#bosettingsdata spesielt og data for aggregering generelt skal ikke leses inn som character, men med NA.strings ="."&":"
#logikken er at det ikke er : på dette nivået, fordi tallene skal aggregeres - det er bare missing
#og hvis det er ":", så skal disse uansett bli . på aggregert n ivå
bosatt_anmodede = data
checksum_start = sum(bosatt_anmodede$vedtak[bosatt_anmodede$aar==2016],na.rm=T)
sum(is.na(bosatt_anmodede))
bosatt_anmodede[is.na(bosatt_anmodede)==T]=0
sum(is.na(bosatt_anmodede))
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
names(bosatt_anmodede_fylke)=c("fylke_nr","aar","anmodning_personer","anmodning.em_personer","bosatt_personer","bosatt.em_personer","vedtak_personer","vedtak.em_personer")
bosatt_anmodede_fylke$anmodning_prosent = round((as.numeric(bosatt_anmodede_fylke$anmodning_personer)/as.numeric(bosatt_anmodede_fylke$anmodning_personer))*100,1)
bosatt_anmodede_fylke$anmodning.em_prosent = round((as.numeric(bosatt_anmodede_fylke$anmodning.em_personer)/as.numeric(bosatt_anmodede_fylke$anmodning.em_personer))*100,1)
bosatt_anmodede_fylke$vedtak_prosent = round((as.numeric(bosatt_anmodede_fylke$vedtak_personer)/as.numeric(bosatt_anmodede_fylke$anmodning_personer))*100,1)
bosatt_anmodede_fylke$vedtak.em_prosent = round((as.numeric(bosatt_anmodede_fylke$vedtak.em_personer)/as.numeric(bosatt_anmodede_fylke$anmodning.em_personer))*100,1)
bosatt_anmodede_fylke$bosatt_prosent = round((as.numeric(bosatt_anmodede_fylke$bosatt_personer)/as.numeric(bosatt_anmodede_fylke$anmodning_personer))*100,1)
bosatt_anmodede_fylke$bosatt.em_prosent = round((as.numeric(bosatt_anmodede_fylke$bosatt.em_personer)/as.numeric(bosatt_anmodede_fylke$anmodning.em_personer))*100,1)

df = gather(bosatt_anmodede_fylke,"variabler","tabellvariabel",-1:-2)
df = separate(df,variabler,c("bosetting","enhet"),sep="_")
sum(is.na(df$tabellvariabel)==T)
df$tabellvariabel[is.na(df$tabellvariabel)==T]="."
#logisk sjekk
nrow(df)==21*2*6*2
checksum==as.integer(df$tabellvariabel[df$fylke_nr=="00"&df$bosetting=="vedtak"&df$aar=="2016"&df$enhet=="personer"])+as.integer(df$tabellvariabel[df$fylke_nr=="00"&df$bosetting=="vedtak"&df$aar=="2017"&df$enhet=="personer"])
#resterende
df$tabell_navn="bosatt_anmodede"
df$tabellvariabel[df$tabellvariabel=="Inf"]="."

#bosetting
write.csv(filter(df,bosetting=="anmodning"|bosetting=="vedtak"|bosetting=="bosatt"),paste0(sti,"bosatt_anmodede-fylke-2016_2017-",dato,".csv"),row.names=F)
#enslige mindreårige
t = separate(filter(df,bosetting=="anmodning.em"|bosetting=="vedtak.em"|bosetting=="bosatt.em"),bosetting,into="bosetting",sep="\\.")
t$tabell_navn = "enslige_mindrearige"
write.csv(t,paste0(sti,"enslige_mindrearige-fylke-2016_2017-",dato,".csv"),row.names=F)

#Næringsregion 2016
kinfo <- read.csv("H:/My Documents/R/imdikator-munch/parameters/kommunesort.csv", sep=";", stringsAsFactors=FALSE)
kinfo = subset(kinfo,select=c("Nr","Naringregnr"))
names(kinfo)[2] = "naringsregion_nr"
kinfo$naringsregion_nr[nchar(kinfo$naringsregion_nr)==1] = paste0("0",kinfo$naringsregion_nr[nchar(kinfo$naringsregion_nr)==1])
#bosettingsdata spesielt og data for aggregering generelt skal ikke leses inn som character, men med NA.strings ="."&":"
#logikken er at det ikke er : på dette nivået, fordi tallene skal aggregeres - det er bare missing
#og hvis det er ":", så skal disse uansett bli . på aggregert n ivå
bosatt_anmodede = data
checksum_start = sum(bosatt_anmodede$vedtak[bosatt_anmodede$aar==2016],na.rm=T)
sum(is.na(bosatt_anmodede))
bosatt_anmodede[is.na(bosatt_anmodede)==T]=0
sum(is.na(bosatt_anmodede))
df = merge(bosatt_anmodede,kinfo,by.x="kommune_nr",by.y ="Nr",all.x=T,all.y=T)
bosatt_anmodede_m = melt.data.frame(df,id.vars=c("kommune_nr","naringsregion_nr","aar"),na.rm=F)
bosatt_anmodede_naringsregion = cast(bosatt_anmodede_m,naringsregion_nr~variable+aar,fun.aggregate=sum,na.rm=T)

#lager aar
df = gather(bosatt_anmodede_naringsregion,"variabler","tabellvariabel",-1)
df = separate(df,variabler,c("bosetting","aar"),sep="_")
bosatt_anmodede_naringsregion = spread(df,bosetting,tabellvariabel)

checksum_start == sum(bosatt_anmodede_naringsregion$vedtak[bosatt_anmodede_naringsregion$aar=="2016"])
names(bosatt_anmodede_naringsregion)=c("naringsregion_nr","aar","anmodning_personer","anmodning.em_personer","bosatt_personer","bosatt.em_personer","vedtak_personer","vedtak.em_personer")
bosatt_anmodede_naringsregion$anmodning_prosent = round((as.numeric(bosatt_anmodede_naringsregion$anmodning_personer)/as.numeric(bosatt_anmodede_naringsregion$anmodning_personer))*100,1)
bosatt_anmodede_naringsregion$anmodning.em_prosent = round((as.numeric(bosatt_anmodede_naringsregion$anmodning.em_personer)/as.numeric(bosatt_anmodede_naringsregion$anmodning.em_personer))*100,1)
bosatt_anmodede_naringsregion$vedtak_prosent = round((as.numeric(bosatt_anmodede_naringsregion$vedtak_personer)/as.numeric(bosatt_anmodede_naringsregion$anmodning_personer))*100,1)
bosatt_anmodede_naringsregion$vedtak.em_prosent = round((as.numeric(bosatt_anmodede_naringsregion$vedtak.em_personer)/as.numeric(bosatt_anmodede_naringsregion$anmodning.em_personer))*100,1)
bosatt_anmodede_naringsregion$bosatt_prosent = round((as.numeric(bosatt_anmodede_naringsregion$bosatt_personer)/as.numeric(bosatt_anmodede_naringsregion$anmodning_personer))*100,1)
bosatt_anmodede_naringsregion$bosatt.em_prosent = round((as.numeric(bosatt_anmodede_naringsregion$bosatt.em_personer)/as.numeric(bosatt_anmodede_naringsregion$anmodning.em_personer))*100,1)
df = gather(bosatt_anmodede_naringsregion,"variabler","tabellvariabel",-1:-2)
df = separate(df,variabler,c("bosetting","enhet"),sep="_")
sum(is.na(df$tabellvariabel)==T)
df$tabellvariabel[is.na(df$tabellvariabel)==T]="."
#logisk sjekk
nrow(df)==84*2*6*2
#resterende
df$tabell_navn="bosatt_anmodede"
df$tabellvariabel[df$tabellvariabel=="Inf"]="."
#bosetting
write.csv(filter(df,bosetting=="anmodning"|bosetting=="vedtak"|bosetting=="bosatt"),paste0(sti,"bosatt_anmodede-naringsregion-2016_2017-",dato,".csv"),row.names=F)
#enslige mindreårige
t = separate(filter(df,bosetting=="anmodning.em"|bosetting=="vedtak.em"|bosetting=="bosatt.em"),bosetting,into="bosetting",sep="\\.")
t$tabell_navn = "enslige_mindrearige"
write.csv(t,paste0(sti,"enslige_mindrearige-naringsregion-2016_2017-",dato,".csv"),row.names=F)

#BYDELER
bosatte_bydel = read.csv(paste0(sti,"bosatt-bydel-2016-",dato_bosatt,"-rå.csv"), row.names=NULL, na.strings=c("NA",":","."), stringsAsFactors=FALSE, sep=";", dec=",")
bydelsnr <- read.csv("H:/My Documents/R/imdikator-munch/parameters/bydelsnummer.csv", sep=";", stringsAsFactors=FALSE,colClasses = "character")
df = merge(bosatte_bydel,bydelsnr,by.x="Bydel",by.y ="bydel_navn",all.x=T,all.y=T)
df = select(df,-Bydel)
names(df)=c("bosatt","bydel_nr")
df$anmodning ="."
df$vedtak ="."
df$aar = "2016"
t = gather(df,bosetting,personer,c(bosatt,vedtak,anmodning))
t$prosent ="."
t = gather(t,enhet,tabellvariabel,c(personer,prosent))
t$tabell_navn="bosatt_anmodede"
write.csv(t,paste0(sti,"bosatt_anmodede-bydel-2016",dato,".csv"),row.names=F)

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

#legger inn bosettingsdata som vi har for denne perioden
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
#duplikater_2 = t[c(8566,8567,26736,26737,3801,3802,21971,21972,28029,28030),]
t = t[-c(3801,21970,28027),]

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
t$kommune_nr[nchar(t$kommune_nr)==3] = paste0("0",t$kommune_nr[nchar(t$kommune_nr)==3])
write.csv(select(t,-fylke_nr,-naringsregion_nr,-kommune_navn),"bosatte_anmodning-kommune-1995-2013.csv",row.names=F)

data = t

#fylker
t = data
#bosettingsdata spesielt og data for aggregering generelt skal ikke leses inn som character, men med NA.strings ="."&":"
t$tabellvariabel[t$tabellvariabel=="Inf"]=0
t$tabellvariabel = as.numeric(t$tabellvariabel)

#prosentene må uansett regnes på nytt og kan tas ut
t = filter(t,enhet=="personer")

checksum_start = sum(t$tabellvariabel,na.rm=T)

sum(is.na(t$tabellvariabel))
t$tabellvariabel[is.na(t$tabellvariabel)==T]=0
sum(is.na(t$tabellvariabel))
sum(is.na(t$fylke_nr))

t_m = melt.data.frame(t,measure.vars="tabellvariabel",na.rm=F)
t_fylke = cast(t_m,fylke_nr~variable+aar+bosetting,fun.aggregate=sum,na.rm=T,add.missing=T,margins="grand_row")

#fjerne overflødige kombinasjoner
t_fylke = select(t_fylke,-grep("_(all)",names(t_fylke),fixed=T),-grep("(all)_",names(t_fylke),fixed=T),-grep("_NA",names(t_fylke),fixed=T))
t_fylke$fylke_nr = levels(t_fylke$fylke_nr)[t_fylke$fylke_nr]
t_fylke$fylke_nr = gsub(".all.","00",t_fylke$fylke_nr)

#lager aar og bosetting
t_fylke = gather(t_fylke,"variabler","tabellvariabel",-1)
t_fylke = separate(t_fylke,variabler,c("t","aar","bosetting"),sep="_")
t_fylke = select(t_fylke,-t)

#lager prosent
t_fylke = spread(t_fylke,bosetting,tabellvariabel)
names(t_fylke)=c("fylke_nr","aar","anmodning_personer","bosatt_personer","vedtak_personer")
t_fylke$anmodning_prosent = round((as.numeric(t_fylke$anmodning_personer)/as.numeric(t_fylke$anmodning_personer))*100,1)
t_fylke$vedtak_prosent = round((as.numeric(t_fylke$vedtak_personer)/as.numeric(t_fylke$anmodning_personer))*100,1)
t_fylke$bosatt_prosent = round((as.numeric(t_fylke$bosatt_personer)/as.numeric(t_fylke$anmodning_personer))*100,1)

df = gather(t_fylke,"variabler","tabellvariabel",-1:-2)
df = separate(df,variabler,c("bosetting","enhet"),sep="_")
sum(is.na(df$tabellvariabel)==T)
df$tabellvariabel[is.na(df$tabellvariabel)==T]="."
#logisk sjekk
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
apply(df,2, function(x){nlevels(as.factor(x))})

nrow(df)==20*19*3*2
#resterende
df$tabell_navn="bosatt_anmodede"
df$tabellvariabel[df$tabellvariabel=="Inf"]="."
df$tabellvariabel[df$bosetting=="bosatt"&as.numeric(df$aar)<2012]="."

write.csv(df,"bosatt_anmodede-fylke-1995-2013.csv",row.names=F)


#næringsregioner
t = data
#bosettingsdata spesielt og data for aggregering generelt skal ikke leses inn som character, men med NA.strings ="."&":"
t$tabellvariabel[t$tabellvariabel=="Inf"]=0
t$tabellvariabel = as.numeric(t$tabellvariabel)

#prosentene må uansett regnes på nytt og kan tas ut
t = filter(t,enhet=="personer")

checksum_start = sum(t$tabellvariabel,na.rm=T)

sum(is.na(t$tabellvariabel))
t$tabellvariabel[is.na(t$tabellvariabel)==T]=0
sum(is.na(t$tabellvariabel))
sum(is.na(t$naringsregion_nr))
t$naringsregion_nr[is.na(t$naringsregion_nr)==T]=99

t_m = melt.data.frame(t,measure.vars="tabellvariabel",na.rm=F)
t_nareg = cast(t_m,naringsregion_nr~variable+aar+bosetting,fun.aggregate=sum,na.rm=T,add.missing=T)

#lager aar og bosetting
t_nareg = gather(t_nareg,"variabler","tabellvariabel",-1)
t_nareg = separate(t_nareg,variabler,c("t","aar","bosetting"),sep="_")
t_nareg = select(t_nareg,-t)

#lager prosent
t_nareg = spread(t_nareg,bosetting,tabellvariabel)
names(t_nareg)=c("naringsregion_nr","aar","anmodning_personer","bosatt_personer","vedtak_personer")
t_nareg$anmodning_prosent = round((as.numeric(t_nareg$anmodning_personer)/as.numeric(t_nareg$anmodning_personer))*100,1)
t_nareg$vedtak_prosent = round((as.numeric(t_nareg$vedtak_personer)/as.numeric(t_nareg$anmodning_personer))*100,1)
t_nareg$bosatt_prosent = round((as.numeric(t_nareg$bosatt_personer)/as.numeric(t_nareg$anmodning_personer))*100,1)

df = gather(t_nareg,"variabler","tabellvariabel",-1:-2)
df = separate(df,variabler,c("bosetting","enhet"),sep="_")
sum(is.na(df$tabellvariabel)==T)
df$tabellvariabel[is.na(df$tabellvariabel)==T]="."
#logisk sjekk
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
apply(df,2, function(x){nlevels(as.factor(x))})

nrow(df)==85*19*3*2
#resterende
df$tabell_navn="bosatt_anmodede"
df$tabellvariabel[df$tabellvariabel=="Inf"]="."
df$tabellvariabel[df$bosetting=="bosatt"&as.numeric(df$aar)<2012]="."

write.csv(df,"bosatt_anmodede-naringsregion-1995-2013.csv",row.names=F)
