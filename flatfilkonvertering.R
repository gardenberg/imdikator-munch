#test av tidy-pakke på indikatordata
install.packages("tidyr")
library(tidyr)

test <- read.csv("D:/R/tidy_r/04-befolkning_alder-bydel-2014.csv", header=FALSE, row.names=NULL, sep=";", na.strings = c("NA",".",":"),stringsAsFactors = F)

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
testDF=testDF[,2:65]

#STEG 3: Hvordan splitte dette?

#samler alt opp først
samlaDF=gather(data=testDF,variabler,verdier)

#splitter så opp. må gjøres posisjonsvis.
splitDF=separate(samlaDF,variabler,c("innvkat","alder_grupper","kjonn","enhet"),"-")

#splitter verdiene
splitDF=separate(splitDF,innvkat,c("variabel","innvkat"),"\\.")
splitDF=separate(splitDF,alder_grupper,c("variabel","alder_grupper"),"\\.")
splitDF=separate(splitDF,kjonn,c("variabel","kjonn"),"\\.")
splitDF=separate(splitDF,enhet,c("variabel","enhet"),"\\.")

#ferdig DF
ferdigDF=data.frame(splitDF[,2],splitDF$alder_grupper,splitDF$kjonn,splitDF$enhet,splitDF$verdier)
colnames(ferdigDF)=c("innvkat","alder_grupper","kjonn","enhet","verdier")

