#SCRIPT for å lage datasett med anslag på behov for tolk

library(tidyverse)
library(reshape)

spraak <- read.csv("~/Ifakta/Datasett/prosjekt tolkebehov/fra NTREG 1-desember-2016 - frmLandSpraak.csv", sep=";", stringsAsFactors=FALSE)
ssb_internasjonale_landkoder <- read.csv("~/R/imdikator-munch/parameters/ssb_internasjonale_landkoder.csv", sep=";", stringsAsFactors=FALSE,colClasses = "character")
befolkning <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 4-2016 fra SSB/9_befolkning_opprinnelsesland_botid/tilrettelagt for tolk/befolkning_opprinnelsesland_botid.csv", stringsAsFactors=FALSE,colClasses = "character")

apply(ssb_internasjonale_landkoder,2, function(x){levels(as.factor(x))})
apply(df,2, function(x){nlevels(as.factor(x))})

ssb_internasjonale_landkoder = filter(ssb_internasjonale_landkoder,Nivånr.=="3")
ssb_internasjonale_landkoder = select(ssb_internasjonale_landkoder,Kode,Tittel,Nivånr.)

df = full_join(befolkning,ssb_internasjonale_landkoder,by=c("landbakgrunn"="Kode"))
t = full_join(df,spraak,by=c("Tittel"="Land"))

apply(t,2, function(x){levels(as.factor(x))})
apply(t,2, function(x){nlevels(as.factor(x))})

#smelter og recaster
data = melt.data.frame(t,measure.vars="tabellvariabel",na.rm=F)
data_sprak = cast(data,kommune_nr~variable+Sprak,fun.aggregate=sum,na.rm=T,add.missing=T)

#sender ut en datafil til tolkeseksjonen
t = filter(t,enhet=="personer",kommune_nr!="NULL",botid_3=="0_4")
t = select(t,kommune_nr,botid_3,landbakgrunn,Tittel,Sprak,tabellvariabel)
names(t)[3:4] = c("landbakgrunn_kode","landbakgrunn_navn")

write.csv2(t,"test/befolkning_sprak_landbakgrunn.csv", row.names=F)
