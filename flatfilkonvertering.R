#SCRIPT FOR KONVERTERING TIL FLATFIL
#Todo: 
#test av tidy-pakke på indikatordata
library(tidyr)

test <- read.csv("training_files/04-befolkning_alder-bydel-2014.csv", header=FALSE, row.names=NULL, sep=";", na.strings = c("NA",".",":"),stringsAsFactors = F)

#tilnærming: multiple variables stored in one column
#steg 1: samle verdier til en kolonnetittel
dataHead = data.frame(test[1:4,])
dataHead = data.frame(t(dataHead))
navn = colnames(dataHead)
dataHead$kode <- do.call(paste, c(dataHead[navn], sep="-"))
dataHead = data.frame(t(dataHead))

#STEG 2:setter inn en ny rad på et spesifisert sted og fjerner øverste rader
over_kutt=4
testDF <- rbind(test[1:over_kutt,],dataHead[5,],test[-(1:over_kutt),])
colnames(testDF)=testDF[5,]
testDF=testDF[6:24,]

#rydder slik at første kolonne inneholder riktige rownames og fjerner overflødige kolonner
#bydel må være en variabel på linje med de andre, må legges inn over-.
testDF=testDF[,2:66]
testDF=subset(testDF,select=-`Bydel\nnavn---`)

#STEG 3: Split
#samler alt opp først
temp_a=names(testDF[1])
samlaDF=gather(data=testDF,variabler,verdier,-`bydel_nr---`)

#splitter så opp. må gjøres posisjonsvis.
splitDF=separate(samlaDF,variabler,c("innvkat","alder_grupper","kjonn","enhet"),"-")

#splitter verdiene
splitDF=separate(splitDF,innvkat,c("variabel","innvkat_3"),"\\.")
splitDF=separate(splitDF,alder_grupper,c("variabel","alder_grupper"),"\\.")
splitDF=separate(splitDF,kjonn,c("variabel","kjonn"),"\\.")
splitDF=separate(splitDF,enhet,c("variabel","enhet"),"\\.")

splitDF[,1]
#ferdig DF
ferdigDF=data.frame(splitDF[,1],splitDF$innvkat_3,splitDF$alder_grupper,splitDF$kjonn,splitDF$enhet,splitDF$verdier)
colnames(ferdigDF)=c("bydel_nr","innvkat_3","alder_grupper_5","kjonn","enhet","tabellvariabel")

#test mot benglers konverterte flatfil
library(dplyr)
kontroll_1 <- read.csv("data/befolkning_alder.csv", header=T, row.names=NULL, sep=",", na.strings = "NA",stringsAsFactors = F)
kontroll_2=subset(kontroll_1,subset=bydel_nr!="NULL",select=c(-fylke_nr,-kommune_nr,-naringsregion_nr,-tabell_navn,-aar))
kontroll_3=anti_join(kontroll_2,ferdigDF)

#KONVERTERING TIL FLATFIL - BARNEHAGEDELTAKELSE
#BYDEL 2013
#23. oktober 2015
library(tidyr)

#feil: brukte na.strings, overskriver "." og ":".
barnehagedeltakelse <- read.csv("data_crossed_input/30-barnehagedeltakelse-bydel-2013.csv", header=T, row.names=NULL, sep=";", dec=",",,stringsAsFactors = F)
#tilnærming: multiple variables stored in one column
#steg 1: samle verdier til en kolonnetittel
#gjort i excel
#STEG 2:setter inn en ny rad på et spesifisert sted og fjerner øverste rader
#gjort i excel
#STEG 3: Split
#samler alt opp først. 
samlaDF=gather(data=barnehagedeltakelse,variabler,tabellvariabel,c(-1,-starts_with("barnikommune")))
samlaDF=gather(data=samlaDF,variabler_2,tabellvariabel_2,starts_with("barnikommune"))
#er det introdusert NA? erstatt i så fall med "."
#fjernet - NA-erstatting er gjort av SSB pre-processing
#lager to df, en med barnikommune og en med spraak
samlaDF_1 = subset(samlaDF,select = -c(variabler_2,tabellvariabel_2))
samlaDF_2 = subset(samlaDF,select = -c(variabler,tabellvariabel))
#splitter så opp. må gjøres posisjonsvis. SPRAAK FØRST
#bruk av _ som separator i variabelnavn gir trøbbel, 
splitDF=separate(samlaDF_1,variabler,c("spraak","bhgalder","enhet"),"_")
splitDF=separate(splitDF,spraak,c("variabel","spraak"),7)
splitDF=subset(splitDF,select=-variabel)
splitDF=separate(splitDF,bhgalder,c("variabel","bhgalder"),9)
splitDF=subset(splitDF,select=-variabel)
splitDF=separate(splitDF,enhet,c("variabel","enhet"),6)
splitDF=subset(splitDF,select=-variabel)
#erstatter nye tegn
splitDF$bydel_nr[splitDF$bydel_nr<99999]=paste0("0",splitDF$bydel_nr)
splitDF$spraak = gsub("\\.","_",splitDF$spraak)
splitDF$bhgalder = gsub("\\.","_",splitDF$bhgalder)
#legger til øvrige nødvendige verdier
splitDF$aar = 2013
splitDF$tabell_navn = "barnehagedeltakelse"
write.csv(splitDF,"D:/R/imdikator-munch/data_flat_output/30-barnehagedeltakelse_1-bydel-2013.csv",row.names=F)
#BHGALDER
#bruk av _ som separator i variabelnavn gir trøbbel, 
splitDF=separate(samlaDF_2,variabler_2,c("barnikommune","bhgalder","enhet"),"_")
splitDF=separate(splitDF,barnikommune,c("variabel","barnikommune"),(nchar("barnikommume")+1))
splitDF=subset(splitDF,select=-variabel)
splitDF=separate(splitDF,bhgalder,c("variabel","bhgalder"),9)
splitDF=subset(splitDF,select=-variabel)
splitDF=separate(splitDF,enhet,c("variabel","enhet"),6)
splitDF=subset(splitDF,select=-variabel)
#erstatter nye tegn
splitDF$bydel_nr[splitDF$bydel_nr<99999]=paste0("0",splitDF$bydel_nr)
splitDF$barnikommune = gsub("\\.","_",splitDF$barnikommune)
splitDF$bhgalder = gsub("\\.","_",splitDF$bhgalder)
#legger til øvrige nødvendige verdier
splitDF$aar = 2013
splitDF$tabell_navn = "barnehagedeltakelse"
#skriver ut fil
write.csv(splitDF,"D:/R/imdikator-munch/data_flat_output/30-barnehagedeltakelse_2-bydel-2013.csv",row.names=F)

#KONVERTERING TIL FLATFIL - BARNEHAGEDELTAKELSE - BYDEL2014
barnehagedeltakelse <- read.csv("data_crossed_input/30-barnehagedeltakelse-bydel-2014.csv", header=T, row.names=NULL, sep=";", dec=",", stringsAsFactors = F)
samlaDF=gather(data=barnehagedeltakelse,variabler,tabellvariabel,c(-1,-starts_with("barnikommune")))
samlaDF=gather(data=samlaDF,variabler_2,tabellvariabel_2,starts_with("barnikommune"))
#lager to df, en med barnikommune og en med spraak
samlaDF_1 = subset(samlaDF,select = -c(variabler_2,tabellvariabel_2))
samlaDF_2 = subset(samlaDF,select = -c(variabler,tabellvariabel))
#SPRAAK
splitDF=separate(samlaDF_1,variabler,c("spraak","bhgalder","enhet"),"_")
splitDF=separate(splitDF,spraak,c("variabel","spraak"),7)
splitDF=subset(splitDF,select=-variabel)
splitDF=separate(splitDF,bhgalder,c("variabel","bhgalder"),9)
splitDF=subset(splitDF,select=-variabel)
splitDF=separate(splitDF,enhet,c("variabel","enhet"),6)
splitDF=subset(splitDF,select=-variabel)
splitDF$bydel_nr[splitDF$bydel_nr<99999]=paste0("0",splitDF$bydel_nr)
splitDF$spraak = gsub("\\.","_",splitDF$spraak)
splitDF$bhgalder = gsub("\\.","_",splitDF$bhgalder)
splitDF$aar = 2014
splitDF$tabell_navn = "barnehagedeltakelse"
write.csv(splitDF,"D:/R/imdikator-munch/data_flat_output/30-barnehagedeltakelse_1-bydel-2014.csv",row.names=F)
#BHGALDER
splitDF=separate(samlaDF_2,variabler_2,c("barnikommune","bhgalder","enhet"),"_")
splitDF=separate(splitDF,barnikommune,c("variabel","barnikommune"),(nchar("barnikommume")+1))
splitDF=subset(splitDF,select=-variabel)
splitDF=separate(splitDF,bhgalder,c("variabel","bhgalder"),9)
splitDF=subset(splitDF,select=-variabel)
splitDF=separate(splitDF,enhet,c("variabel","enhet"),6)
splitDF=subset(splitDF,select=-variabel)
splitDF$bydel_nr[splitDF$bydel_nr<99999]=paste0("0",splitDF$bydel_nr)
splitDF$barnikommune = gsub("\\.","_",splitDF$barnikommune)
splitDF$bhgalder = gsub("\\.","_",splitDF$bhgalder)
splitDF$aar = 2014
splitDF$tabell_navn = "barnehagedeltakelse"
write.csv(splitDF,"D:/R/imdikator-munch/data_flat_output/30-barnehagedeltakelse_2-bydel-2014.csv",row.names=F)

#KONVERTERING TIL FLATFIL - BARNEHAGEDELTAKELSE - FYLKE2013
barnehagedeltakelse <- read.csv("data_crossed_input/30-barnehagedeltakelse-fylke-2013.csv", header=T, row.names=NULL, sep=";", dec=",",stringsAsFactors = F)
samlaDF=gather(data=barnehagedeltakelse,variabler,tabellvariabel,c(-1,-starts_with("barnikommune")))
samlaDF=gather(data=samlaDF,variabler_2,tabellvariabel_2,starts_with("barnikommune"))
sum(is.na(samlaDF$tabellvariabel))
sum(is.na(samlaDF$tabellvariabel_2))
samlaDF$tabellvariabel[is.na(samlaDF$tabellvariabel)]="."
samlaDF$tabellvariabel_2[is.na(samlaDF$tabellvariabel_2)]="."
sum(is.na(samlaDF$tabellvariabel))
sum(is.na(samlaDF$tabellvariabel_2))
#lager to df, en med barnikommune og en med spraak
samlaDF_1 = subset(samlaDF,select = -c(variabler_2,tabellvariabel_2))
samlaDF_2 = subset(samlaDF,select = -c(variabler,tabellvariabel))
#SPRAAK
splitDF=separate(samlaDF_1,variabler,c("spraak","bhgalder","enhet"),"_")
splitDF=separate(splitDF,spraak,c("variabel","spraak"),7)
splitDF=subset(splitDF,select=-variabel)
splitDF=separate(splitDF,bhgalder,c("variabel","bhgalder"),9)
splitDF=subset(splitDF,select=-variabel)
splitDF=separate(splitDF,enhet,c("variabel","enhet"),6)
splitDF=subset(splitDF,select=-variabel)
splitDF$fylke_nr[nchar(splitDF$fylke_nr)==1] = paste0("0",splitDF$fylke_nr[nchar(splitDF$fylke_nr)==1])
splitDF$spraak = gsub("\\.","_",splitDF$spraak)
splitDF$bhgalder = gsub("\\.","_",splitDF$bhgalder)
splitDF$aar = 2013
splitDF$tabell_navn = "barnehagedeltakelse"
write.csv(splitDF,"D:/R/imdikator-munch/data_flat_output/30-barnehagedeltakelse_1-fylke-2013.csv",row.names=F)
#BHGALDER
splitDF=separate(samlaDF_2,variabler_2,c("barnikommune","bhgalder","enhet"),"_")
splitDF=separate(splitDF,barnikommune,c("variabel","barnikommune"),(nchar("barnikommume")+1))
splitDF=subset(splitDF,select=-variabel)
splitDF=separate(splitDF,bhgalder,c("variabel","bhgalder"),9)
splitDF=subset(splitDF,select=-variabel)
splitDF=separate(splitDF,enhet,c("variabel","enhet"),6)
splitDF=subset(splitDF,select=-variabel)
splitDF$fylke_nr[nchar(splitDF$fylke_nr)==1] = paste0("0",splitDF$fylke_nr[nchar(splitDF$fylke_nr)==1])
splitDF$barnikommune = gsub("\\.","_",splitDF$barnikommune)
splitDF$bhgalder = gsub("\\.","_",splitDF$bhgalder)
splitDF$aar = 2013
splitDF$tabell_navn = "barnehagedeltakelse"
write.csv(splitDF,"D:/R/imdikator-munch/data_flat_output/30-barnehagedeltakelse_2-fylke-2013.csv",row.names=F)

#KONVERTERING TIL FLATFIL - BARNEHAGEDELTAKELSE - FYLKE2014
barnehagedeltakelse <- read.csv("data_crossed_input/30-barnehagedeltakelse-fylke-2014.csv", header=T, row.names=NULL, sep=";", dec=",",stringsAsFactors = F)
samlaDF=gather(data=barnehagedeltakelse,variabler,tabellvariabel,c(-1,-starts_with("barnikommune")))
samlaDF=gather(data=samlaDF,variabler_2,tabellvariabel_2,starts_with("barnikommune"))
sum(is.na(samlaDF$tabellvariabel))
sum(is.na(samlaDF$tabellvariabel_2))
samlaDF$tabellvariabel[is.na(samlaDF$tabellvariabel)]="."
samlaDF$tabellvariabel_2[is.na(samlaDF$tabellvariabel_2)]="."
sum(is.na(samlaDF$tabellvariabel))
sum(is.na(samlaDF$tabellvariabel_2))
#lager to df, en med barnikommune og en med spraak
samlaDF_1 = subset(samlaDF,select = -c(variabler_2,tabellvariabel_2))
samlaDF_2 = subset(samlaDF,select = -c(variabler,tabellvariabel))
#SPRAAK
splitDF=separate(samlaDF_1,variabler,c("spraak","bhgalder","enhet"),"_")
splitDF=separate(splitDF,spraak,c("variabel","spraak"),7)
splitDF=subset(splitDF,select=-variabel)
splitDF=separate(splitDF,bhgalder,c("variabel","bhgalder"),9)
splitDF=subset(splitDF,select=-variabel)
splitDF=separate(splitDF,enhet,c("variabel","enhet"),6)
splitDF=subset(splitDF,select=-variabel)
splitDF$fylke_nr[nchar(splitDF$fylke_nr)==1] = paste0("0",splitDF$fylke_nr[nchar(splitDF$fylke_nr)==1])
splitDF$spraak = gsub("\\.","_",splitDF$spraak)
splitDF$bhgalder = gsub("\\.","_",splitDF$bhgalder)
splitDF$aar = 2014
splitDF$tabell_navn = "barnehagedeltakelse"
write.csv(splitDF,"D:/R/imdikator-munch/data_flat_output/30-barnehagedeltakelse_1-fylke-2014.csv",row.names=F)
#BHGALDER
splitDF=separate(samlaDF_2,variabler_2,c("barnikommune","bhgalder","enhet"),"_")
splitDF=separate(splitDF,barnikommune,c("variabel","barnikommune"),(nchar("barnikommume")+1))
splitDF=subset(splitDF,select=-variabel)
splitDF=separate(splitDF,bhgalder,c("variabel","bhgalder"),9)
splitDF=subset(splitDF,select=-variabel)
splitDF=separate(splitDF,enhet,c("variabel","enhet"),6)
splitDF=subset(splitDF,select=-variabel)
splitDF$fylke_nr[nchar(splitDF$fylke_nr)==1] = paste0("0",splitDF$fylke_nr[nchar(splitDF$fylke_nr)==1])
splitDF$barnikommune = gsub("\\.","_",splitDF$barnikommune)
splitDF$bhgalder = gsub("\\.","_",splitDF$bhgalder)
splitDF$aar = 2014
splitDF$tabell_navn = "barnehagedeltakelse"
write.csv(splitDF,"D:/R/imdikator-munch/data_flat_output/30-barnehagedeltakelse_2-fylke-2014.csv",row.names=F)

#KONVERTERING TIL FLATFIL - BARNEHAGEDELTAKELSE - KOMMUNE2013
barnehagedeltakelse <- read.csv("data_crossed_input/30-barnehagedeltakelse-kommune-2013.csv", header=T, row.names=NULL, sep=";", dec=",", stringsAsFactors = F)
samlaDF=gather(data=barnehagedeltakelse,variabler,tabellvariabel,c(-1,-starts_with("barnikommune")))
samlaDF=gather(data=samlaDF,variabler_2,tabellvariabel_2,starts_with("barnikommune"))
sum(is.na(samlaDF$tabellvariabel))
sum(is.na(samlaDF$tabellvariabel_2))
samlaDF$tabellvariabel[is.na(samlaDF$tabellvariabel)]="."
samlaDF$tabellvariabel_2[is.na(samlaDF$tabellvariabel_2)]="."
sum(is.na(samlaDF$tabellvariabel))
sum(is.na(samlaDF$tabellvariabel_2))
#lager to df, en med barnikommune og en med spraak
samlaDF_1 = subset(samlaDF,select = -c(variabler_2,tabellvariabel_2))
samlaDF_2 = subset(samlaDF,select = -c(variabler,tabellvariabel))
#SPRAAK
splitDF=separate(samlaDF_1,variabler,c("spraak","bhgalder","enhet"),"_")
splitDF=separate(splitDF,spraak,c("variabel","spraak"),7)
splitDF=subset(splitDF,select=-variabel)
splitDF=separate(splitDF,bhgalder,c("variabel","bhgalder"),9)
splitDF=subset(splitDF,select=-variabel)
splitDF=separate(splitDF,enhet,c("variabel","enhet"),6)
splitDF=subset(splitDF,select=-variabel)
splitDF$kommune_nr[nchar(splitDF$kommune_nr)==3] = paste0("0",splitDF$kommune_nr[nchar(splitDF$kommune_nr)==3])
splitDF$spraak = gsub("\\.","_",splitDF$spraak)
splitDF$bhgalder = gsub("\\.","_",splitDF$bhgalder)
splitDF$aar = 2013
splitDF$tabell_navn = "barnehagedeltakelse"
write.csv(splitDF,"D:/R/imdikator-munch/data_flat_output/30-barnehagedeltakelse_1-kommune-2013.csv",row.names=F)
#BHGALDER
splitDF=separate(samlaDF_2,variabler_2,c("barnikommune","bhgalder","enhet"),"_")
splitDF=separate(splitDF,barnikommune,c("variabel","barnikommune"),(nchar("barnikommume")+1))
splitDF=subset(splitDF,select=-variabel)
splitDF=separate(splitDF,bhgalder,c("variabel","bhgalder"),9)
splitDF=subset(splitDF,select=-variabel)
splitDF=separate(splitDF,enhet,c("variabel","enhet"),6)
splitDF=subset(splitDF,select=-variabel)
splitDF$kommune_nr[nchar(splitDF$kommune_nr)==3] = paste0("0",splitDF$kommune_nr[nchar(splitDF$kommune_nr)==3])
splitDF$barnikommune = gsub("\\.","_",splitDF$barnikommune)
splitDF$bhgalder = gsub("\\.","_",splitDF$bhgalder)
splitDF$aar = 2013
splitDF$tabell_navn = "barnehagedeltakelse"
write.csv(splitDF,"D:/R/imdikator-munch/data_flat_output/30-barnehagedeltakelse_2-kommune-2013.csv",row.names=F)

#KONVERTERING TIL FLATFIL - BARNEHAGEDELTAKELSE - KOMMUNE2014
barnehagedeltakelse <- read.csv("data_crossed_input/30-barnehagedeltakelse-kommune-2014.csv", header=T, row.names=NULL, sep=";", dec=",", stringsAsFactors = F)
samlaDF=gather(data=barnehagedeltakelse,variabler,tabellvariabel,c(-1,-starts_with("barnikommune")))
samlaDF=gather(data=samlaDF,variabler_2,tabellvariabel_2,starts_with("barnikommune"))
sum(is.na(samlaDF$tabellvariabel))
sum(is.na(samlaDF$tabellvariabel_2))
samlaDF$tabellvariabel[is.na(samlaDF$tabellvariabel)]="."
samlaDF$tabellvariabel_2[is.na(samlaDF$tabellvariabel_2)]="."
sum(is.na(samlaDF$tabellvariabel))
sum(is.na(samlaDF$tabellvariabel_2))
#lager to df, en med barnikommune og en med spraak
samlaDF_1 = subset(samlaDF,select = -c(variabler_2,tabellvariabel_2))
samlaDF_2 = subset(samlaDF,select = -c(variabler,tabellvariabel))
#SPRAAK
splitDF=separate(samlaDF_1,variabler,c("spraak","bhgalder","enhet"),"_")
splitDF=separate(splitDF,spraak,c("variabel","spraak"),7)
splitDF=subset(splitDF,select=-variabel)
splitDF=separate(splitDF,bhgalder,c("variabel","bhgalder"),9)
splitDF=subset(splitDF,select=-variabel)
splitDF=separate(splitDF,enhet,c("variabel","enhet"),6)
splitDF=subset(splitDF,select=-variabel)
splitDF$kommune_nr[nchar(splitDF$kommune_nr)==3] = paste0("0",splitDF$kommune_nr[nchar(splitDF$kommune_nr)==3])
splitDF$spraak = gsub("\\.","_",splitDF$spraak)
splitDF$bhgalder = gsub("\\.","_",splitDF$bhgalder)
splitDF$aar = 2014
splitDF$tabell_navn = "barnehagedeltakelse"
write.csv(splitDF,"D:/R/imdikator-munch/data_flat_output/30-barnehagedeltakelse_1-kommune-2014.csv",row.names=F)
#BHGALDER
splitDF=separate(samlaDF_2,variabler_2,c("barnikommune","bhgalder","enhet"),"_")
splitDF=separate(splitDF,barnikommune,c("variabel","barnikommune"),(nchar("barnikommume")+1))
splitDF=subset(splitDF,select=-variabel)
splitDF=separate(splitDF,bhgalder,c("variabel","bhgalder"),9)
splitDF=subset(splitDF,select=-variabel)
splitDF=separate(splitDF,enhet,c("variabel","enhet"),6)
splitDF=subset(splitDF,select=-variabel)
splitDF$kommune_nr[nchar(splitDF$kommune_nr)==3] = paste0("0",splitDF$kommune_nr[nchar(splitDF$kommune_nr)==3])
splitDF$barnikommune = gsub("\\.","_",splitDF$barnikommune)
splitDF$bhgalder = gsub("\\.","_",splitDF$bhgalder)
splitDF$aar = 2014
splitDF$tabell_navn = "barnehagedeltakelse"
write.csv(splitDF,"D:/R/imdikator-munch/data_flat_output/30-barnehagedeltakelse_2-kommune-2014.csv",row.names=F)

#KONVERTERING TIL FLATFIL - BARNEHAGEDELTAKELSE - NÆRINGSREGION2013
barnehagedeltakelse <- read.csv("data_crossed_input/30-barnehagedeltakelse-naringsregion-2013.csv", header=T, row.names=NULL, sep=";", dec=",", stringsAsFactors = F)
samlaDF=gather(data=barnehagedeltakelse,variabler,tabellvariabel,c(-1,-starts_with("barnikommune")))
samlaDF=gather(data=samlaDF,variabler_2,tabellvariabel_2,starts_with("barnikommune"))
sum(is.na(samlaDF$tabellvariabel))
sum(is.na(samlaDF$tabellvariabel_2))
samlaDF$tabellvariabel[is.na(samlaDF$tabellvariabel)]="."
samlaDF$tabellvariabel_2[is.na(samlaDF$tabellvariabel_2)]="."
sum(is.na(samlaDF$tabellvariabel))
sum(is.na(samlaDF$tabellvariabel_2))
#lager to df, en med barnikommune og en med spraak
samlaDF_1 = subset(samlaDF,select = -c(variabler_2,tabellvariabel_2))
samlaDF_2 = subset(samlaDF,select = -c(variabler,tabellvariabel))
#SPRAAK
splitDF=separate(samlaDF_1,variabler,c("spraak","bhgalder","enhet"),"_")
splitDF=separate(splitDF,spraak,c("variabel","spraak"),7)
splitDF=subset(splitDF,select=-variabel)
splitDF=separate(splitDF,bhgalder,c("variabel","bhgalder"),9)
splitDF=subset(splitDF,select=-variabel)
splitDF=separate(splitDF,enhet,c("variabel","enhet"),6)
splitDF=subset(splitDF,select=-variabel)
splitDF$naringsregion_nr[nchar(splitDF$naringsregion_nr)==1] = paste0("0",splitDF$naringsregion_nr[nchar(splitDF$naringsregion_nr)==1])
splitDF$spraak = gsub("\\.","_",splitDF$spraak)
splitDF$bhgalder = gsub("\\.","_",splitDF$bhgalder)
splitDF$aar = 2013
splitDF$tabell_navn = "barnehagedeltakelse"
write.csv(splitDF,"D:/R/imdikator-munch/data_flat_output/30-barnehagedeltakelse_1-naringsregion-2013.csv",row.names=F)
#BHGALDER
splitDF=separate(samlaDF_2,variabler_2,c("barnikommune","bhgalder","enhet"),"_")
splitDF=separate(splitDF,barnikommune,c("variabel","barnikommune"),(nchar("barnikommume")+1))
splitDF=subset(splitDF,select=-variabel)
splitDF=separate(splitDF,bhgalder,c("variabel","bhgalder"),9)
splitDF=subset(splitDF,select=-variabel)
splitDF=separate(splitDF,enhet,c("variabel","enhet"),6)
splitDF=subset(splitDF,select=-variabel)
splitDF$naringsregion_nr[nchar(splitDF$naringsregion_nr)==1] = paste0("0",splitDF$naringsregion_nr[nchar(splitDF$naringsregion_nr)==1])
splitDF$barnikommune = gsub("\\.","_",splitDF$barnikommune)
splitDF$bhgalder = gsub("\\.","_",splitDF$bhgalder)
splitDF$aar = 2013
splitDF$tabell_navn = "barnehagedeltakelse"
write.csv(splitDF,"D:/R/imdikator-munch/data_flat_output/30-barnehagedeltakelse_2-naringsregion-2013.csv",row.names=F)

#KONVERTERING TIL FLATFIL - BARNEHAGEDELTAKELSE - NÆRINGSREGION2014
barnehagedeltakelse <- read.csv("data_crossed_input/30-barnehagedeltakelse-naringsregion-2014.csv", header=T, row.names=NULL, sep=";", dec=",", stringsAsFactors = F)
samlaDF=gather(data=barnehagedeltakelse,variabler,tabellvariabel,c(-1,-starts_with("barnikommune")))
samlaDF=gather(data=samlaDF,variabler_2,tabellvariabel_2,starts_with("barnikommune"))
sum(is.na(samlaDF$tabellvariabel))
sum(is.na(samlaDF$tabellvariabel_2))
samlaDF$tabellvariabel[is.na(samlaDF$tabellvariabel)]="."
samlaDF$tabellvariabel_2[is.na(samlaDF$tabellvariabel_2)]="."
sum(is.na(samlaDF$tabellvariabel))
sum(is.na(samlaDF$tabellvariabel_2))
#lager to df, en med barnikommune og en med spraak
samlaDF_1 = subset(samlaDF,select = -c(variabler_2,tabellvariabel_2))
samlaDF_2 = subset(samlaDF,select = -c(variabler,tabellvariabel))
#SPRAAK
splitDF=separate(samlaDF_1,variabler,c("spraak","bhgalder","enhet"),"_")
splitDF=separate(splitDF,spraak,c("variabel","spraak"),7)
splitDF=subset(splitDF,select=-variabel)
splitDF=separate(splitDF,bhgalder,c("variabel","bhgalder"),9)
splitDF=subset(splitDF,select=-variabel)
splitDF=separate(splitDF,enhet,c("variabel","enhet"),6)
splitDF=subset(splitDF,select=-variabel)
splitDF$naringsregion_nr[nchar(splitDF$naringsregion_nr)==1] = paste0("0",splitDF$naringsregion_nr[nchar(splitDF$naringsregion_nr)==1])
splitDF$spraak = gsub("\\.","_",splitDF$spraak)
splitDF$bhgalder = gsub("\\.","_",splitDF$bhgalder)
splitDF$aar = 2014
splitDF$tabell_navn = "barnehagedeltakelse"
write.csv(splitDF,"D:/R/imdikator-munch/data_flat_output/30-barnehagedeltakelse_1-naringsregion-2014.csv",row.names=F)
#BHGALDER
splitDF=separate(samlaDF_2,variabler_2,c("barnikommune","bhgalder","enhet"),"_")
splitDF=separate(splitDF,barnikommune,c("variabel","barnikommune"),(nchar("barnikommume")+1))
splitDF=subset(splitDF,select=-variabel)
splitDF=separate(splitDF,bhgalder,c("variabel","bhgalder"),9)
splitDF=subset(splitDF,select=-variabel)
splitDF=separate(splitDF,enhet,c("variabel","enhet"),6)
splitDF=subset(splitDF,select=-variabel)
splitDF$naringsregion_nr[nchar(splitDF$naringsregion_nr)==1] = paste0("0",splitDF$naringsregion_nr[nchar(splitDF$naringsregion_nr)==1])
splitDF$barnikommune = gsub("\\.","_",splitDF$barnikommune)
splitDF$bhgalder = gsub("\\.","_",splitDF$bhgalder)
splitDF$aar = 2014
splitDF$tabell_navn = "barnehagedeltakelse"
write.csv(splitDF,"D:/R/imdikator-munch/data_flat_output/30-barnehagedeltakelse_2-naringsregion-2014.csv",row.names=F)