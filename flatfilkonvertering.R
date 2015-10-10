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
