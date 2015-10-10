#SCRIPT FOR MERGING AV KOMMUNEDATA - BOSETTINGSDATA

#generelt script for å legge til kommuner som ikke er med i settet
#legger også til all annen informasjon om kommunen som ligger i kommunesett-fila
kommunesett <- read.csv("H:/My Documents/R/datamerge_indikator/kommunesort.csv", sep=";")
intro_2014_kommune_alle = merge(kommunesett,intro_2014_kommune,by.y="TILHKOMMUNECODE",by.x="Nr",all.x=T,all.y=F)
  
#Datalasting
bosetting <- read.csv("H:/My Documents/R/datamerge_indikator/bosetting.csv", sep=";", stringsAsFactors=TRUE)
anmved_2012 <- read.csv("H:/My Documents/R/datamerge_indikator/anmved_2012.csv", sep=";")
folketall_2012 <- read.csv("H:/My Documents/R/datamerge_indikator/folketall_2012.csv", sep=";")
anmved_2013 <- read.csv("H:/My Documents/R/datamerge_indikator/anmved_2013.csv", sep=";")
folketall <- read.csv("H:/My Documents/R/datamerge_indikator/folketall.csv", sep=";")

kommunesett <- read.csv("H:/My Documents/R/datamerge_indikator/kommunesort.csv", sep=";")
kommunesett_mini = data.frame(kommunesett$Nr,kommunesett$Fylke,kommunesett$Fylkenr,kommunesett$Naringregnr,kommunesett$Naringreg)

#Dataomkoding - missing
#0 i bosetting betyr at ingen er bosatt det ?ret
#NA i vedtak betyr at kommunen ikke har meldt tilbake til IMDi
#0 i vedtak betyr at kommunen har meddelt IMDi at de ikke vil bosette
#NA i anmodning betyr at kommunen ikke er anmodet. Dette kan kodes som 0.

bosetting$X2009[is.na(bosetting$X2009)] = 0
bosetting$X2010[is.na(bosetting$X2010)] = 0
bosetting$X2011[is.na(bosetting$X2011)] = 0
bosetting$X2012[is.na(bosetting$X2012)] = 0
bosetting$X2013[is.na(bosetting$X2013)] = 0

anmved_2012$Anmodning.2012[is.na(anmved_2012$Anmodning.2012)]=0
anmved_2013$Anmodning.2013[is.na(anmved_2013$Anmodning.2013)]

bosett_2012$anmodning[is.na(bosett_2012$anmodning)]=0
bosett_2013$anmodning[is.na(bosett_2013$anmodning)]=0

#disse utg?r, da det er betydningsforskjell mellom NA og 0.
#bosett_2012$vedtak[is.na(bosett_2012$vedtak)]=0
#anmved_2012$Vedtak.2012[is.na(anmved_2012$Vedtak.2012)]=0

# merge-tiln?rming, bygd etter sp@data = data.frame(sp@data, df[match(sp@data[,by], df[,by]),])
#data = data.frame(data,IDR[match(data[,"Country.Name"],IDR[,"Country.Name"]),])
# inkluderer alle observasjoner med matchende ID, utelater resten

bosett_2012 = data.frame(bosetting,anmved_2012[match(bosetting[,"Alle.kommuner"],anmved_2012[,"Kommunenr"]),])
bosett_2012 = data.frame(bosett_2012$Alle.kommuner,bosett_2012$X,bosett_2012$X2012,bosett_2012$Anmodning.2012,bosett_2012$Vedtak.2012)
names(bosett_2012)=c("nr","navn","bosatte","anmodning","vedtak")
bosett_2012 = data.frame(bosett_2012,folketall_2012[match(bosett_2012[,"nr"],folketall_2012[,"Nr"]),])
bosett_2012 = data.frame(bosett_2012[1:5],bosett_2012[8])

bosett_2013 = data.frame(bosetting,anmved_2013[match(bosetting[,"Alle.kommuner"],anmved_2013[,"Kommunenr"]),])
bosett_2013 = data.frame(bosett_2013$Alle.kommuner,bosett_2013$X,bosett_2013$X2013,bosett_2013$Anmodning.2013,bosett_2013$Vedtak.2013)
names(bosett_2013)=c("nr","navn","bosatte","anmodning","vedtak")
bosett_2013 = data.frame(bosett_2013,folketall[match(bosett_2013[,"nr"],folketall[,"Nr"]),])
bosett_2013 = data.frame(bosett_2013[1:5],bosett_2013[11])

#Legger inn fylker og n?ringsregion i bosett_2012 og bosett_2013
#se recast_indikator for recasting til andre aggregeringer
bosett_2012_2 = data.frame(bosett_2012,kommunesett_mini[match(bosett_2012[,"nr"],kommunesett_mini[,"kommunesett.Nr"]),])
bosett_2013_2 = data.frame(bosett_2013,kommunesett_mini[match(bosett_2013[,"nr"],kommunesett_mini[,"kommunesett.Nr"]),])

#Misc beregninger
bosett_2012$ved_anm = bosett_2012$vedtak/bosett_2012$anmodning
bosett_2012$bos_anm = bosett_2012$bosatte/bosett_2012$anmodning
bosett_2012$bos_pers = bosett_2012$bosatte/(bosett_2012$Personer/1000)

bosett_2013$ved_anm = bosett_2013$vedtak/bosett_2013$anmodning
bosett_2013$bos_anm = bosett_2013$bosatte/bosett_2013$anmodning
bosett_2013$bos_pers = bosett_2013$bosatte/(bosett_2013$X2013/1000)

bosett_2012_fylke$ved_anm = bosett_2012_fylke$vedtak/bosett_2012_fylke$anmodning
bosett_2012_fylke$bos_anm = bosett_2012_fylke$bosatte/bosett_2012_fylke$anmodning
bosett_2012_fylke$bos_pers = bosett_2012_fylke$bosatte/(bosett_2012_fylke$Personer/1000)

bosett_2013_fylke$ved_anm = bosett_2013_fylke$vedtak/bosett_2013_fylke$anmodning
bosett_2013_fylke$bos_anm = bosett_2013_fylke$bosatte/bosett_2013_fylke$anmodning
bosett_2013_fylke$bos_pers = bosett_2013_fylke$bosatte/(bosett_2013_fylke$X2013/1000)

bosett_2012_narreg$ved_anm = bosett_2012_narreg$vedtak/bosett_2012_narreg$anmodning
bosett_2012_narreg$bos_anm = bosett_2012_narreg$bosatte/bosett_2012_narreg$anmodning
bosett_2012_narreg$bos_pers = bosett_2012_narreg$bosatte/(bosett_2012_narreg$Personer/1000)

bosett_2013_narreg$ved_anm = bosett_2013_narreg$vedtak/bosett_2013_narreg$anmodning
bosett_2013_narreg$bos_anm = bosett_2013_narreg$bosatte/bosett_2013_narreg$anmodning
bosett_2013_narreg$bos_pers = bosett_2013_narreg$bosatte/(bosett_2013_narreg$X2013/1000)

#skriver til csv
write.csv2(bosett_2012,file="bosatte_2012.csv")
write.csv2(bosett_2012_fylke,file="bosatte_2012_fylke.csv")
write.csv2(bosett_2012_narreg,file="bosatte_2012_narreg.csv")
write.csv2(bosett_2013,file="bosatte_2013.csv")
write.csv2(bosett_2013_fylke,file="bosatte_2013_fylke.csv")
write.csv2(bosett_2013_narreg,file="bosatte_2013_narreg.csv")

#Bruk av merge
#PT_2013_t4 = merge(PT_jan2013_utb,PT_aug2012_utb,by.x="Orgnr",by.y="ORGNR",all=T,suffixes=c("_jan","_aug"))
#PT_2013_dyr=merge(PT_aug2012,PT_jan2013,by.x="Orgnr",by.y="Orgnr",all=T,suffixes=c("_aug","_jan"))

