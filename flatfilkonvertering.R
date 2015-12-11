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

#30. oktober: MASSIVE FEIL OPPDAGA - LAGER 36 DUPLIKATER AV DATASETTET
#TRENGER FEILSIKRING MOT DETTE!

#feil: brukte na.strings, overskriver "." og ":".
barnehagedeltakelse <- read.csv("data_crossed_input/30-barnehagedeltakelse-bydel-2013.csv", header=T, row.names=NULL, sep=";", dec=",",stringsAsFactors = F)
#tilnærming: multiple variables stored in one column
#steg 1: samle verdier til en kolonnetittel
#gjort i excel
#STEG 2:setter inn en ny rad på et spesifisert sted og fjerner øverste rader
#gjort i excel
#STEG 3: Split
#samler alt opp først. 
samlaDF_1=gather(data=barnehagedeltakelse,variabler,tabellvariabel,c(-1,-starts_with("barnikommune")))
samlaDF_1 = subset(samlaDF_1,select = c(1,variabler,tabellvariabel))
samlaDF_2=gather(data=barnehagedeltakelse,variabler,tabellvariabel,starts_with("barnikommune"))
samlaDF_2 = subset(samlaDF_2,select = c(1,variabler,tabellvariabel))

#er det introdusert NA? erstatt i så fall med "."
sum(is.na(samlaDF_1$tabellvariabel))
sum(is.na(samlaDF_2$tabellvariabel))
#fjernet - NA-erstatting er gjort av SSB pre-processing
#lager to df, en med barnikommune og en med spraak

#sjekk av duplikate linjer
#bør ha en logisk test av at riktig antall kombinasjoner er med
#18 bydeler*72 ulike variabler delt på to sett med spraak og barnikkomune
if(nrow(samlaDF_1)>((nrow(barnehagedeltakelse)*(ncol(barnehagedeltakelse)-1)/2))){
        testDF_1=unique(samlaDF_1)
}
if(nrow(samlaDF_2)>((nrow(barnehagedeltakelse)*(ncol(barnehagedeltakelse)-1)/2))){
        testDF_2=unique(samlaDF_2)
}

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
splitDF$aar = "2013"
splitDF$tabell_navn = "barnehagedeltakelse"
write.csv(splitDF,"D:/R/imdikator-munch/data_flat_output/barnehagedeltakelse_spraak-bydel-2013.csv",row.names=F)
#BHGALDER
#bruk av _ som separator i variabelnavn gir trøbbel, 
splitDF=separate(samlaDF_2,variabler,c("barnikommune","bhgalder","enhet"),"_")
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
splitDF$aar = "2013"
splitDF$tabell_navn = "barnehagedeltakelse"
#skriver ut fil
write.csv(splitDF,"D:/R/imdikator-munch/data_flat_output/barnehagedeltakelse_barnikommune-bydel-2013.csv",row.names=F)

#KONVERTERING TIL FLATFIL - BARNEHAGEDELTAKELSE - BYDEL2014
barnehagedeltakelse <- read.csv("data_crossed_input/30-barnehagedeltakelse-bydel-2014.csv", header=T, row.names=NULL, sep=";", dec=",", stringsAsFactors = F)

samlaDF_1=gather(data=barnehagedeltakelse,variabler,tabellvariabel,c(-1,-starts_with("barnikommune")))
samlaDF_1 = subset(samlaDF_1,select = c(1,variabler,tabellvariabel))
samlaDF_2=gather(data=barnehagedeltakelse,variabler,tabellvariabel,starts_with("barnikommune"))
samlaDF_2 = subset(samlaDF_2,select = c(1,variabler,tabellvariabel))
#sjekk av duplikate linjer
if(nrow(samlaDF_1)>((nrow(barnehagedeltakelse)*(ncol(barnehagedeltakelse)-1)/2))){
        testDF_1=unique(samlaDF_1)
}
if(nrow(samlaDF_2)>((nrow(barnehagedeltakelse)*(ncol(barnehagedeltakelse)-1)/2))){
        testDF_2=unique(samlaDF_2)
}

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
splitDF$aar = "2014"
splitDF$tabell_navn = "barnehagedeltakelse"
write.csv(splitDF,"D:/R/imdikator-munch/data_flat_output/barnehagedeltakelse_spraak-bydel-2014.csv",row.names=F)
#BHGALDER
splitDF=separate(samlaDF_2,variabler,c("barnikommune","bhgalder","enhet"),"_")
splitDF=separate(splitDF,barnikommune,c("variabel","barnikommune"),(nchar("barnikommume")+1))
splitDF=subset(splitDF,select=-variabel)
splitDF=separate(splitDF,bhgalder,c("variabel","bhgalder"),9)
splitDF=subset(splitDF,select=-variabel)
splitDF=separate(splitDF,enhet,c("variabel","enhet"),6)
splitDF=subset(splitDF,select=-variabel)
splitDF$bydel_nr[splitDF$bydel_nr<99999]=paste0("0",splitDF$bydel_nr)
splitDF$barnikommune = gsub("\\.","_",splitDF$barnikommune)
splitDF$bhgalder = gsub("\\.","_",splitDF$bhgalder)
splitDF$aar = "2014"
splitDF$tabell_navn = "barnehagedeltakelse"
write.csv(splitDF,"D:/R/imdikator-munch/data_flat_output/barnehagedeltakelse_barnikommune-bydel-2014.csv",row.names=F)

#KONVERTERING TIL FLATFIL - BARNEHAGEDELTAKELSE - FYLKE2013
barnehagedeltakelse <- read.csv("data_crossed_input/30-barnehagedeltakelse-fylke-2013.csv", header=T, row.names=NULL, sep=";", dec=",",stringsAsFactors = F)
samlaDF_1=gather(data=barnehagedeltakelse,variabler,tabellvariabel,c(-1,-starts_with("barnikommune")))
samlaDF_1 = subset(samlaDF_1,select = c(1,variabler,tabellvariabel))
samlaDF_2=gather(data=barnehagedeltakelse,variabler,tabellvariabel,starts_with("barnikommune"))
samlaDF_2 = subset(samlaDF_2,select = c(1,variabler,tabellvariabel))
#sjekk av duplikate linjer
if(nrow(samlaDF_1)>((nrow(barnehagedeltakelse)*(ncol(barnehagedeltakelse)-1)/2))){
        testDF_1=unique(samlaDF_1)
}
if(nrow(samlaDF_2)>((nrow(barnehagedeltakelse)*(ncol(barnehagedeltakelse)-1)/2))){
        testDF_2=unique(samlaDF_2)
}#SPRAAK
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
splitDF$aar = "2013"
splitDF$tabell_navn = "barnehagedeltakelse"
write.csv(splitDF,"D:/R/imdikator-munch/data_flat_output/barnehagedeltakelse_spraak-fylke-2013.csv",row.names=F)
#BHGALDER
splitDF=separate(samlaDF_2,variabler,c("barnikommune","bhgalder","enhet"),"_")
splitDF=separate(splitDF,barnikommune,c("variabel","barnikommune"),(nchar("barnikommume")+1))
splitDF=subset(splitDF,select=-variabel)
splitDF=separate(splitDF,bhgalder,c("variabel","bhgalder"),9)
splitDF=subset(splitDF,select=-variabel)
splitDF=separate(splitDF,enhet,c("variabel","enhet"),6)
splitDF=subset(splitDF,select=-variabel)
splitDF$fylke_nr[nchar(splitDF$fylke_nr)==1] = paste0("0",splitDF$fylke_nr[nchar(splitDF$fylke_nr)==1])
splitDF$barnikommune = gsub("\\.","_",splitDF$barnikommune)
splitDF$bhgalder = gsub("\\.","_",splitDF$bhgalder)
splitDF$aar = "2013"
splitDF$tabell_navn = "barnehagedeltakelse"
write.csv(splitDF,"D:/R/imdikator-munch/data_flat_output/barnehagedeltakelse_barnikommune-fylke-2013.csv",row.names=F)

#KONVERTERING TIL FLATFIL - BARNEHAGEDELTAKELSE - FYLKE2014
barnehagedeltakelse <- read.csv("data_crossed_input/30-barnehagedeltakelse-fylke-2014.csv", header=T, row.names=NULL, sep=";", dec=",",stringsAsFactors = F)
samlaDF_1=gather(data=barnehagedeltakelse,variabler,tabellvariabel,c(-1,-starts_with("barnikommune")))
samlaDF_1 = subset(samlaDF_1,select = c(1,variabler,tabellvariabel))
samlaDF_2=gather(data=barnehagedeltakelse,variabler,tabellvariabel,starts_with("barnikommune"))
samlaDF_2 = subset(samlaDF_2,select = c(1,variabler,tabellvariabel))
#sjekk av duplikate linjer
if(nrow(samlaDF_1)>((nrow(barnehagedeltakelse)*(ncol(barnehagedeltakelse)-1)/2))){
        testDF_1=unique(samlaDF_1)
}
if(nrow(samlaDF_2)>((nrow(barnehagedeltakelse)*(ncol(barnehagedeltakelse)-1)/2))){
        testDF_2=unique(samlaDF_2)
}
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
splitDF$aar = "2014"
splitDF$tabell_navn = "barnehagedeltakelse"
write.csv(splitDF,"D:/R/imdikator-munch/data_flat_output/barnehagedeltakelse_spraak-fylke-2014.csv",row.names=F)
#BHGALDER
splitDF=separate(samlaDF_2,variabler,c("barnikommune","bhgalder","enhet"),"_")
splitDF=separate(splitDF,barnikommune,c("variabel","barnikommune"),(nchar("barnikommume")+1))
splitDF=subset(splitDF,select=-variabel)
splitDF=separate(splitDF,bhgalder,c("variabel","bhgalder"),9)
splitDF=subset(splitDF,select=-variabel)
splitDF=separate(splitDF,enhet,c("variabel","enhet"),6)
splitDF=subset(splitDF,select=-variabel)
splitDF$fylke_nr[nchar(splitDF$fylke_nr)==1] = paste0("0",splitDF$fylke_nr[nchar(splitDF$fylke_nr)==1])
splitDF$barnikommune = gsub("\\.","_",splitDF$barnikommune)
splitDF$bhgalder = gsub("\\.","_",splitDF$bhgalder)
splitDF$aar = "2014"
splitDF$tabell_navn = "barnehagedeltakelse"
write.csv(splitDF,"D:/R/imdikator-munch/data_flat_output/barnehagedeltakelse_barnikommune-fylke-2014.csv",row.names=F)

#KONVERTERING TIL FLATFIL - BARNEHAGEDELTAKELSE - KOMMUNE2013
barnehagedeltakelse <- read.csv("data_crossed_input/30-barnehagedeltakelse-kommune-2013.csv", header=T, row.names=NULL, sep=";", dec=",", stringsAsFactors = F)
samlaDF_1=gather(data=barnehagedeltakelse,variabler,tabellvariabel,c(-1,-starts_with("barnikommune")))
samlaDF_1 = subset(samlaDF_1,select = c(1,variabler,tabellvariabel))
samlaDF_2=gather(data=barnehagedeltakelse,variabler,tabellvariabel,starts_with("barnikommune"))
samlaDF_2 = subset(samlaDF_2,select = c(1,variabler,tabellvariabel))
#sjekk av duplikate linjer
if(nrow(samlaDF_1)>((nrow(barnehagedeltakelse)*(ncol(barnehagedeltakelse)-1)/2))){
        testDF_1=unique(samlaDF_1)
}
if(nrow(samlaDF_2)>((nrow(barnehagedeltakelse)*(ncol(barnehagedeltakelse)-1)/2))){
        testDF_2=unique(samlaDF_2)
}
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
splitDF$aar = "2013"
splitDF$tabell_navn = "barnehagedeltakelse"
write.csv(splitDF,"D:/R/imdikator-munch/data_flat_output/barnehagedeltakelse_spraak-kommune-2013.csv",row.names=F)
#BHGALDER
splitDF=separate(samlaDF_2,variabler,c("barnikommune","bhgalder","enhet"),"_")
splitDF=separate(splitDF,barnikommune,c("variabel","barnikommune"),(nchar("barnikommume")+1))
splitDF=subset(splitDF,select=-variabel)
splitDF=separate(splitDF,bhgalder,c("variabel","bhgalder"),9)
splitDF=subset(splitDF,select=-variabel)
splitDF=separate(splitDF,enhet,c("variabel","enhet"),6)
splitDF=subset(splitDF,select=-variabel)
splitDF$kommune_nr[nchar(splitDF$kommune_nr)==3] = paste0("0",splitDF$kommune_nr[nchar(splitDF$kommune_nr)==3])
splitDF$barnikommune = gsub("\\.","_",splitDF$barnikommune)
splitDF$bhgalder = gsub("\\.","_",splitDF$bhgalder)
splitDF$aar = "2013"
splitDF$tabell_navn = "barnehagedeltakelse"
write.csv(splitDF,"D:/R/imdikator-munch/data_flat_output/barnehagedeltakelse_barnikommune-kommune-2013.csv",row.names=F)

#KONVERTERING TIL FLATFIL - BARNEHAGEDELTAKELSE - KOMMUNE2014
barnehagedeltakelse <- read.csv("data_crossed_input/30-barnehagedeltakelse-kommune-2014.csv", header=T, row.names=NULL, sep=";", dec=",", stringsAsFactors = F)
samlaDF_1=gather(data=barnehagedeltakelse,variabler,tabellvariabel,c(-1,-starts_with("barnikommune")))
samlaDF_1 = subset(samlaDF_1,select = c(1,variabler,tabellvariabel))
samlaDF_2=gather(data=barnehagedeltakelse,variabler,tabellvariabel,starts_with("barnikommune"))
samlaDF_2 = subset(samlaDF_2,select = c(1,variabler,tabellvariabel))
#sjekk av duplikate linjer
if(nrow(samlaDF_1)>((nrow(barnehagedeltakelse)*(ncol(barnehagedeltakelse)-1)/2))){
        testDF_1=unique(samlaDF_1)
}
if(nrow(samlaDF_2)>((nrow(barnehagedeltakelse)*(ncol(barnehagedeltakelse)-1)/2))){
        testDF_2=unique(samlaDF_2)
}
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
splitDF$aar = "2014"
splitDF$tabell_navn = "barnehagedeltakelse"
write.csv(splitDF,"D:/R/imdikator-munch/data_flat_output/barnehagedeltakelse_spraak-kommune-2014.csv",row.names=F)
#BHGALDER
splitDF=separate(samlaDF_2,variabler,c("barnikommune","bhgalder","enhet"),"_")
splitDF=separate(splitDF,barnikommune,c("variabel","barnikommune"),(nchar("barnikommume")+1))
splitDF=subset(splitDF,select=-variabel)
splitDF=separate(splitDF,bhgalder,c("variabel","bhgalder"),9)
splitDF=subset(splitDF,select=-variabel)
splitDF=separate(splitDF,enhet,c("variabel","enhet"),6)
splitDF=subset(splitDF,select=-variabel)
splitDF$kommune_nr[nchar(splitDF$kommune_nr)==3] = paste0("0",splitDF$kommune_nr[nchar(splitDF$kommune_nr)==3])
splitDF$barnikommune = gsub("\\.","_",splitDF$barnikommune)
splitDF$bhgalder = gsub("\\.","_",splitDF$bhgalder)
splitDF$aar = "2014"
splitDF$tabell_navn = "barnehagedeltakelse"
write.csv(splitDF,"D:/R/imdikator-munch/data_flat_output/barnehagedeltakelse_barnikommune-kommune-2014.csv",row.names=F)

#KONVERTERING TIL FLATFIL - BARNEHAGEDELTAKELSE - NÆRINGSREGION2013
barnehagedeltakelse <- read.csv("data_crossed_input/30-barnehagedeltakelse-naringsregion-2013.csv", header=T, row.names=NULL, sep=";", dec=",", stringsAsFactors = F)
samlaDF_1=gather(data=barnehagedeltakelse,variabler,tabellvariabel,c(-1,-starts_with("barnikommune")))
samlaDF_1 = subset(samlaDF_1,select = c(1,variabler,tabellvariabel))
samlaDF_2=gather(data=barnehagedeltakelse,variabler,tabellvariabel,starts_with("barnikommune"))
samlaDF_2 = subset(samlaDF_2,select = c(1,variabler,tabellvariabel))
#sjekk av duplikate linjer
if(nrow(samlaDF_1)>((nrow(barnehagedeltakelse)*(ncol(barnehagedeltakelse)-1)/2))){
        testDF_1=unique(samlaDF_1)
}
if(nrow(samlaDF_2)>((nrow(barnehagedeltakelse)*(ncol(barnehagedeltakelse)-1)/2))){
        testDF_2=unique(samlaDF_2)
}
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
splitDF$aar = "2013"
splitDF$tabell_navn = "barnehagedeltakelse"
write.csv(splitDF,"D:/R/imdikator-munch/data_flat_output/barnehagedeltakelse_spraak-naringsregion-2013.csv",row.names=F)
#BHGALDER
splitDF=separate(samlaDF_2,variabler,c("barnikommune","bhgalder","enhet"),"_")
splitDF=separate(splitDF,barnikommune,c("variabel","barnikommune"),(nchar("barnikommume")+1))
splitDF=subset(splitDF,select=-variabel)
splitDF=separate(splitDF,bhgalder,c("variabel","bhgalder"),9)
splitDF=subset(splitDF,select=-variabel)
splitDF=separate(splitDF,enhet,c("variabel","enhet"),6)
splitDF=subset(splitDF,select=-variabel)
splitDF$naringsregion_nr[nchar(splitDF$naringsregion_nr)==1] = paste0("0",splitDF$naringsregion_nr[nchar(splitDF$naringsregion_nr)==1])
splitDF$barnikommune = gsub("\\.","_",splitDF$barnikommune)
splitDF$bhgalder = gsub("\\.","_",splitDF$bhgalder)
splitDF$aar = "2013"
splitDF$tabell_navn = "barnehagedeltakelse"
write.csv(splitDF,"D:/R/imdikator-munch/data_flat_output/barnehagedeltakelse_barnikommune-naringsregion-2013.csv",row.names=F)

#KONVERTERING TIL FLATFIL - BARNEHAGEDELTAKELSE - NÆRINGSREGION2014
barnehagedeltakelse <- read.csv("data_crossed_input/30-barnehagedeltakelse-naringsregion-2014.csv", header=T, row.names=NULL, sep=";", dec=",", stringsAsFactors = F)
samlaDF_1=gather(data=barnehagedeltakelse,variabler,tabellvariabel,c(-1,-starts_with("barnikommune")))
samlaDF_1 = subset(samlaDF_1,select = c(1,variabler,tabellvariabel))
samlaDF_2=gather(data=barnehagedeltakelse,variabler,tabellvariabel,starts_with("barnikommune"))
samlaDF_2 = subset(samlaDF_2,select = c(1,variabler,tabellvariabel))
#sjekk av duplikate linjer
if(nrow(samlaDF_1)>((nrow(barnehagedeltakelse)*(ncol(barnehagedeltakelse)-1)/2))){
        testDF_1=unique(samlaDF_1)
}
if(nrow(samlaDF_2)>((nrow(barnehagedeltakelse)*(ncol(barnehagedeltakelse)-1)/2))){
        testDF_2=unique(samlaDF_2)
}
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
splitDF$aar = "2014"
splitDF$tabell_navn = "barnehagedeltakelse"
write.csv(splitDF,"D:/R/imdikator-munch/data_flat_output/barnehagedeltakelse_spraak-naringsregion-2014.csv",row.names=F)
#BHGALDER
splitDF=separate(samlaDF_2,variabler,c("barnikommune","bhgalder","enhet"),"_")
splitDF=separate(splitDF,barnikommune,c("variabel","barnikommune"),(nchar("barnikommume")+1))
splitDF=subset(splitDF,select=-variabel)
splitDF=separate(splitDF,bhgalder,c("variabel","bhgalder"),9)
splitDF=subset(splitDF,select=-variabel)
splitDF=separate(splitDF,enhet,c("variabel","enhet"),6)
splitDF=subset(splitDF,select=-variabel)
splitDF$naringsregion_nr[nchar(splitDF$naringsregion_nr)==1] = paste0("0",splitDF$naringsregion_nr[nchar(splitDF$naringsregion_nr)==1])
splitDF$barnikommune = gsub("\\.","_",splitDF$barnikommune)
splitDF$bhgalder = gsub("\\.","_",splitDF$bhgalder)
splitDF$aar = "2014"
splitDF$tabell_navn = "barnehagedeltakelse"
write.csv(splitDF,"D:/R/imdikator-munch/data_flat_output/barnehagedeltakelse_barnikommune-naringsregion-2014.csv",row.names=F)

#NORSKPRØVER - RESULTATER - FYLKE 1. halvår 2014
norsk_prover <- read.csv("D:/R/imdikator-munch/data_crossed_input/norsk_prover-fylke-halvar2014.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE)
samlaDF=gather(data=norsk_prover,variabler,tabellvariabel,-1)
sum(is.na(samlaDF$tabellvariabel))
splitDF=separate(samlaDF,variabler,c("spraaknivaa","provetype","enhet"),"_")
splitDF=separate(splitDF,spraaknivaa,c("variabel","spraaknivaa"),(nchar("spraaknivaa")+1))
splitDF=subset(splitDF,select=-variabel)
splitDF=separate(splitDF,provetype,c("variabel","provetype"),(nchar("provetype")+1))
splitDF=subset(splitDF,select=-variabel)
splitDF=separate(splitDF,enhet,c("variabel","enhet"),6)
splitDF=subset(splitDF,select=-variabel)
splitDF$fylke_nr[nchar(splitDF$fylke_nr)==1] = paste0("0",splitDF$fylke_nr[nchar(splitDF$fylke_nr)==1])
splitDF$spraaknivaa = gsub("\\.","_",splitDF$spraaknivaa)
splitDF$provetype = gsub("\\.","_",splitDF$provetype)
splitDF$aar = "2014"
splitDF$tabell_navn = "norsk_prover"
write.csv(splitDF,"D:/R/imdikator-munch/data_flat_output/norsk_prover-fylke-2014.csv",row.names=F)

levels(as.factor(splitDF$aar))

#NORSKPRØVER - RESULTATER - KOMMUNE 1. halvår 2014
norsk_prover <- read.csv("D:/R/imdikator-munch/data_crossed_input/norsk_prover-kommune-halvar2014.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE)
samlaDF=gather(data=norsk_prover,variabler,tabellvariabel,-1)
sum(is.na(samlaDF$tabellvariabel))
#testDF = subset(samlaDF,is.na(tabellvariabel)==T)
splitDF=separate(samlaDF,variabler,c("spraaknivaa","provetype","enhet"),"_")
splitDF=separate(splitDF,spraaknivaa,c("variabel","spraaknivaa"),(nchar("spraaknivaa")+1))
splitDF=subset(splitDF,select=-variabel)
splitDF=separate(splitDF,provetype,c("variabel","provetype"),(nchar("provetype")+1))
splitDF=subset(splitDF,select=-variabel)
splitDF=separate(splitDF,enhet,c("variabel","enhet"),6)
splitDF=subset(splitDF,select=-variabel)
splitDF$kommune_nr[nchar(splitDF$kommune_nr)==3] = paste0("0",splitDF$kommune_nr[nchar(splitDF$kommune_nr)==3])
splitDF$spraaknivaa = gsub("\\.","_",splitDF$spraaknivaa)
splitDF$provetype = gsub("\\.","_",splitDF$provetype)
splitDF$aar = 2014
splitDF$tabell_navn = "norsk_prover"
write.csv(splitDF,"D:/R/imdikator-munch/data_flat_output/norsk_prover-kommune-2014.csv",row.names=F)

#INTRO_STATUS_ARBUTD - FYLKE
#27. okt, kjørt på nytt 17. nov
library(tidyr)
intro_status_arbutd <- read.csv("D:/R/imdikator-munch/data_crossed_input/intro_status_arbutd-fylke-2011_2013.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE,colClasses = "character")
samlaDF=gather(data=intro_status_arbutd,variabler,tabellvariabel,-1)
sum(is.na(samlaDF$tabellvariabel))
splitDF=separate(samlaDF,variabler,c("avslutta","kjonn","avslstat4","aar","enhet"),"_")
splitDF=separate(splitDF,avslutta,c("variabel","avslutta"),(nchar("avslutta")+1))
splitDF=subset(splitDF,select=-variabel)
splitDF=separate(splitDF,kjonn,c("variabel","kjonn"),(nchar("kjonn")+1))
splitDF=subset(splitDF,select=-variabel)
splitDF=separate(splitDF,avslstat4,c("variabel","avslstat4"),(nchar("avslutning.status.4")+1))
splitDF=subset(splitDF,select=-variabel)
splitDF=separate(splitDF,aar,c("variabel","aar"),(nchar("aar")+1))
splitDF=subset(splitDF,select=-variabel)
splitDF=separate(splitDF,enhet,c("variabel","enhet"),(nchar("enhet")+1))
splitDF=subset(splitDF,select=-variabel)
splitDF$fylke_nr[nchar(splitDF$fylke_nr)==1] = paste0("0",splitDF$fylke_nr[nchar(splitDF$fylke_nr)==1])
splitDF$avslutta = gsub("\\.","_",splitDF$avslutta)
splitDF$kjonn = gsub("\\.","_",splitDF$kjonn)
splitDF$avslstat4 = gsub("\\.","_",splitDF$avslstat4)
splitDF$aar = gsub("\\.","_",splitDF$aar)
splitDF$enhet = gsub("\\.","_",splitDF$enhet)
splitDF$tabell_navn = "intro_status_arbutd"

levels(as.factor(splitDF$avslstat4))
write.csv(splitDF,"D:/R/imdikator-munch/data_flat_output/intro_status_arbutd-fylke-2011_2013.csv",row.names=F)

#INTRO_STATUS_ARBUTD - KOMMUNE
#29. okt
library(tidyr)
intro_status_arbutd <- read.csv("D:/R/imdikator-munch/data_crossed_input/intro_status_arbutd-kommune-2011_2013.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE,colClasses = "character")
samlaDF=gather(data=intro_status_arbutd,variabler,tabellvariabel,-1)
sum(is.na(samlaDF$tabellvariabel))
splitDF=separate(samlaDF,variabler,c("avslutta","kjonn","avslstat4","aar","enhet"),"_")
splitDF=separate(splitDF,avslutta,c("variabel","avslutta"),(nchar("avslutta")+1))
splitDF=subset(splitDF,select=-variabel)
splitDF=separate(splitDF,kjonn,c("variabel","kjonn"),(nchar("kjonn")+1))
splitDF=subset(splitDF,select=-variabel)
splitDF=separate(splitDF,avslstat4,c("variabel","avslstat4"),(nchar("avslutning.status.4")+1))
splitDF=subset(splitDF,select=-variabel)
splitDF=separate(splitDF,aar,c("variabel","aar"),(nchar("aar")+1))
splitDF=subset(splitDF,select=-variabel)
splitDF=separate(splitDF,enhet,c("variabel","enhet"),(nchar("enhet")+1))
splitDF=subset(splitDF,select=-variabel)
splitDF$kommune_nr[nchar(splitDF$kommune_nr)==3] = paste0("0",splitDF$kommune_nr[nchar(splitDF$kommune_nr)==3])
splitDF$avslutta = gsub("\\.","_",splitDF$avslutta)
splitDF$kjonn = gsub("\\.","_",splitDF$kjonn)
splitDF$avslstat4 = gsub("\\.","_",splitDF$avslstat4)
splitDF$aar = gsub("\\.","_",splitDF$aar)
splitDF$enhet = gsub("\\.","_",splitDF$enhet)
splitDF$tabell_navn = "intro_status_arbutd"
levels(as.factor(splitDF$avslstat4))
write.csv(splitDF,"D:/R/imdikator-munch/data_flat_output/intro_status_arbutd-kommune-2011_2013.csv",row.names=F)

#INTRO_STATUS_ARBUTD - BYDEL
#29. okt
library(tidyr)
intro_status_arbutd <- read.csv("D:/R/imdikator-munch/data_crossed_input/intro_status_arbutd-bydel-2011_2013.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE,colClasses = "character")
samlaDF=gather(data=intro_status_arbutd,variabler,tabellvariabel,-1)
sum(is.na(samlaDF$tabellvariabel))
splitDF=separate(samlaDF,variabler,c("avslutta","kjonn","avslstat4","aar","enhet"),"_")
splitDF=separate(splitDF,avslutta,c("variabel","avslutta"),(nchar("avslutta")+1))
splitDF=subset(splitDF,select=-variabel)
splitDF=separate(splitDF,kjonn,c("variabel","kjonn"),(nchar("kjonn")+1))
splitDF=subset(splitDF,select=-variabel)
splitDF=separate(splitDF,avslstat4,c("variabel","avslstat4"),(nchar("avslutning.status.4")+1))
splitDF=subset(splitDF,select=-variabel)
splitDF=separate(splitDF,aar,c("variabel","aar"),(nchar("aar")+1))
splitDF=subset(splitDF,select=-variabel)
splitDF=separate(splitDF,enhet,c("variabel","enhet"),(nchar("enhet")+1))
splitDF=subset(splitDF,select=-variabel)
splitDF$bydel_nr[nchar(splitDF$bydel_nr)==5] = paste0("0",splitDF$bydel_nr[nchar(splitDF$bydel_nr)==5])
splitDF$bydel_nr[nchar(splitDF$bydel_nr)==3] = paste0("0",splitDF$bydel_nr[nchar(splitDF$bydel_nr)==3])
splitDF$avslutta = gsub("\\.","_",splitDF$avslutta)
splitDF$kjonn = gsub("\\.","_",splitDF$kjonn)
splitDF$avslstat4 = gsub("\\.","_",splitDF$avslstat4)
splitDF$aar = gsub("\\.","_",splitDF$aar)
splitDF$enhet = gsub("\\.","_",splitDF$enhet)
splitDF$tabell_navn = "intro_status_arbutd"
levels(as.factor(splitDF$avslstat4))
write.csv(splitDF,"D:/R/imdikator-munch/data_flat_output/intro_status_arbutd-bydel-2011_2013.csv",row.names=F)

#INTRO_STATUS_ARBUTD - NÆRINGSREGION
#17. nov
library(tidyr)
intro_status_arbutd <- read.csv("D:/R/imdikator-munch/data_crossed_input/intro_status_arbutd-naringsregion-2011_2013.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE,colClasses = "character")
samlaDF=gather(data=intro_status_arbutd,variabler,tabellvariabel,-1)
sum(is.na(samlaDF$tabellvariabel))
splitDF=separate(samlaDF,variabler,c("avslutta","kjonn","avslstat4","aar","enhet"),"_")
splitDF=separate(splitDF,avslutta,c("variabel","avslutta"),(nchar("avslutta")+1))
splitDF=subset(splitDF,select=-variabel)
splitDF=separate(splitDF,kjonn,c("variabel","kjonn"),(nchar("kjonn")+1))
splitDF=subset(splitDF,select=-variabel)
splitDF=separate(splitDF,avslstat4,c("variabel","avslstat4"),(nchar("avslutning.status.4")+1))
splitDF=subset(splitDF,select=-variabel)
splitDF=separate(splitDF,aar,c("variabel","aar"),(nchar("aar")+1))
splitDF=subset(splitDF,select=-variabel)
splitDF=separate(splitDF,enhet,c("variabel","enhet"),(nchar("enhet")+1))
splitDF=subset(splitDF,select=-variabel)
splitDF$naringsregion_nr[nchar(splitDF$naringsregion_nr)==1] = paste0("0",splitDF$naringsregion_nr[nchar(splitDF$naringsregion_nr)==1])
splitDF$avslutta = gsub("\\.","_",splitDF$avslutta)
splitDF$kjonn = gsub("\\.","_",splitDF$kjonn)
splitDF$avslstat4 = gsub("\\.","_",splitDF$avslstat4)
splitDF$aar = gsub("\\.","_",splitDF$aar)
splitDF$enhet = gsub("\\.","_",splitDF$enhet)
splitDF$tabell_navn = "intro_status_arbutd"
levels(as.factor(splitDF$avslstat4))
write.csv(splitDF,"D:/R/imdikator-munch/data_flat_output/intro_status_arbutd-naringsregion-2011_2013.csv",row.names=F)

#BOSETTING_MAANED
#27. okt
library(tidyr)
bosetting_maaned <- read.csv("D:/R/imdikator-munch/data_crossed_input/bosetting_maaned-kommune-2012.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE,colClasses = "character")
samlaDF=gather(data=bosetting_maaned,variabler,tabellvariabel,-1)
sum(is.na(samlaDF$tabellvariabel))
splitDF=separate(samlaDF,variabler,c("bosetting","enhet","maaned"),"_")
splitDF=separate(splitDF,bosetting,c("variabel","bosetting"),(nchar("bosetting")+1))
splitDF=subset(splitDF,select=-variabel)
splitDF=separate(splitDF,enhet,c("variabel","enhet"),(nchar("enhet")+1))
splitDF=subset(splitDF,select=-variabel)
splitDF=separate(splitDF,maaned,c("variabel","maaned"),(nchar("maned")+1))
splitDF=subset(splitDF,select=-variabel)
splitDF$kommune_nr[nchar(splitDF$kommune_nr)==3] = paste0("0",splitDF$kommune_nr[nchar(splitDF$kommune_nr)==3])
splitDF$tabell_navn = "bosetting_maaned"
splitDF$aar=2012
levels(as.factor(splitDF$maaned))
write.csv(splitDF,"D:/R/imdikator-munch/data_flat_output/bosetting_maaned-kommune-2012-dummy.csv",row.names=F)
