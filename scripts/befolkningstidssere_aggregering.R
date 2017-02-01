#SCRIPT FOR AGGREGERING AV KOMMUNEDATA I BEFOLKNINGSTIDSERIE TIL NÆRINGSREGION

#biblioteker
library(reshape)

#leser inn data
befolkning_tidsserie <- read.csv("~/R/datamerge_indikator/befolkning_tidsserie_R.csv", sep=";", dec=",", na.strings=c("NA",".",":"))

#sjekker om det er noen faktorvaribler her introdusert med manglende definisjoner av na.strings
is.factor(befolkning_tidsserie)==T

kommunesett <- read.csv("H:/My Documents/R/datamerge_indikator/kommunesort.csv", sep=";")
#fjerne noen unødvenige rader i kommunesettet, og renavner disse med opprinnelige navn
k_navn = names(kommunesett)
kommunesett = data.frame(kommunesett[,1],kommunesett[,4],kommunesett[,6],kommunesett[,7])
names(kommunesett) = c(k_navn[1],k_navn[4],k_navn[6],k_navn[7])

#legger inn næringsregionnr
befolkning_ts = data.frame(befolkning_tidsserie,kommunesett[match(bosett_2012[,"nr"],kommunesett_mini[,"kommunesett.Nr"]),])
befolkning_ts = merge(kommunesett,befolkning_tidsserie,by.x="Nr",by.y="knr",all.x=T,all.y=T)

#melt-recast
data_befolkning_ts = melt.data.frame(befolkning_ts,id.vars=c("Nr","Fylkenr","IMDiregnr","Naringregnr","k"))
befolkning_ts_nareg = cast(data_befolkning_ts,Naringregnr~variable,fun.aggregate=sum,na.rm=T,add.missing=T)
write.csv2(befolkning_ts_nareg,file="befolkning_ts_nareg.csv")
