#AGGREGERING OG FLATFILERING AV 2016-DATA
#forbehandla krysstabell i excel
#31. januar, 3. februar, 12. februar, 18. februar, 1. mars, 8.mars, 15. mars,22.mars
dato = "160322"
data = read.csv(paste0("D:/R/imdikator-munch/data_crossed_input/bosatt_anmodede-kommune-2016-",dato,".csv"), row.names=NULL, na.strings=c("NA",":","."), stringsAsFactors=FALSE, sep=";", dec=",")
#biblioteker
library(tidyr)
library(reshape)

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
write.csv(df,paste0("D:/R/imdikator-munch/data_flat_output/bosatt_anmodede-kommune-2016-",dato,".csv"),row.names=F)

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
write.csv(df,paste0("D:/R/imdikator-munch/data_flat_output/bosatt_anmodede-fylke-2016-",dato,".csv"),row.names=F)

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
write.csv(df,paste0("D:/R/imdikator-munch/data_flat_output/bosatt_anmodede-naringsregion-2016-",dato,".csv"),row.names=F)