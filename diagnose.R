#SCRIPT FOR DIAGNOSE

#RYDDEAMBISJON
#dette scriptet må dokumentere hva som er kommet inn, og hva som er gjort med disse filene.
#først: hvilke endringer og rettelser som er gjort på Benglers flatfilleveranse
#så: hva som er gjort med SSBs leveranse i 2015
#ambisjonen er at dette scriptet kaller funksjoner, som utfører de vanligste oppgavene, og så blir resultatet resultatet.

#dette scriptet bør derfor først samle alle operasjoner som nå gjøres i ulike script, 
#og så fordele disse på en mer fornuftig måte

#fordelingen kan være på en datasett-basis, etter nummerering

#1 - BEFOLKNING_HOVEDGRUPPER

#SCRIPT FOR AGGREGERING AV KOMMUNEDATA I BEFOLKNINGSTIDSERIE TIL NÆRINGSREGION
#10. oktober 2015
#egen fil med tidsserie er avviklet, kode beholdt for aggregeringsformål.
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

#

#TABELL 4 - BEFOLKNING-ALDER
#3. februar 2016
#bydel
befolkning_alder <- read.csv2("D:/R/imdikator-munch/data_flat_input/befolkning_alder_B_2006_2013.csv", stringsAsFactors=FALSE,colClasses = "character")
#etter visuell inspeksjon
#1: en kolonne for mye
#2: alder_grupper-variabler har - som skilletegn, ikke _
#3: X_id, ikke X_nr
#4: bydel_nr mangler 0301 først.
#5: enhet-variabel er person, ikke personer
#6: alder_grupper-dimensjon mangler alle-variabel
#7: må ha sep=, dec=.
#8: fylke=00 og alle andre fylker kan ikke legges i ulike filer. disse må splices.
#9: logisk test
#bydel
#1
befolkning_alder = subset(befolkning_alder,select=-9)
#2
befolkning_alder$alder_grupper = gsub("\\-","_",befolkning_alder$alder_grupper)
levels(as.factor(befolkning_alder$alder_grupper))
#3
names(befolkning_alder)[3] = gsub("id","nr",names(befolkning_alder[3]))
#4
befolkning_alder$bydel_nr = paste0("0301",befolkning_alder$bydel_nr)
#5
levels(as.factor(befolkning_alder$enhet))
#6 -logisk
nrow(befolkning_alder)==18*8*3*3*7
#mangler 1 observasjon.
#7 - allevariabel
df=spread(befolkning_alder,alder_grupper,tabellvariabel)
#df$alle=extract_numeric(df[,10])+extract_numeric(df[,11])+extract_numeric(df[,12])+extract_numeric(df[,13])+extract_numeric(df[,14])+extract_numeric(df[,15])+extract_numeric(df[,16])
df$alle=apply(df[,7:13],1,
              function(x){
                      sum(extract_numeric(x))
              }
)
test = subset(df,is.na(df$alle)==T)
df=gather(df,alder_grupper,tabellvariabel,`0_5`:alle)
sum(is.na(df$tabellvariabel))
df$tabellvariabel[is.na(df$tabellvariabel)==T]="."
#8 - ny logisk
nrow(df)==18*8*3*3*8
#utskrift
write.csv(df,"D:/R/imdikator-munch/data_flat_output/befolkning_alder-bydel-2006_2013.csv",row.names=F)

#KOMMUNE
befolkning_alder <- read.csv2("D:/R/imdikator-munch/data_flat_input/befolkning_alder_K_2006_2013.csv", stringsAsFactors=FALSE,colClasses = "character")
befolkning_alder = subset(befolkning_alder,select=-9)
befolkning_alder$alder_grupper = gsub("\\-","_",befolkning_alder$alder_grupper)
levels(as.factor(befolkning_alder$alder_grupper))
names(befolkning_alder)[3] = gsub("id","nr",names(befolkning_alder[3]))
levels(as.factor(befolkning_alder$enhet))
nrow(befolkning_alder)==428*8*3*3*7
df=spread(befolkning_alder,alder_grupper,tabellvariabel)
df$alle=apply(df[,7:13],1,
              function(x){
                      sum(extract_numeric(x))
              }
)
test = subset(df,is.na(df$alle)==T)
df=gather(df,alder_grupper,tabellvariabel,`0_5`:alle)
sum(is.na(df$tabellvariabel))
nrow(df)==428*8*3*3*8
write.csv(df,"D:/R/imdikator-munch/data_flat_output/befolkning_alder-kommune-2006_2013.csv",row.names=F)

#NÆRINGSREGION
befolkning_alder <- read.csv2("D:/R/imdikator-munch/data_flat_input/befolkning_alder_N_2006_2013.csv", stringsAsFactors=FALSE,colClasses = "character")
befolkning_alder = subset(befolkning_alder,select=-9)
befolkning_alder$alder_grupper = gsub("\\-","_",befolkning_alder$alder_grupper)
levels(as.factor(befolkning_alder$alder_grupper))
names(befolkning_alder)[3] = gsub("id","nr",names(befolkning_alder[3]))
levels(as.factor(befolkning_alder$enhet))
befolkning_alder$enhet="personer"
nrow(befolkning_alder)==83*8*3*3*7
df=spread(befolkning_alder,alder_grupper,tabellvariabel)
df$alle=apply(df[,7:13],1,
              function(x){
                      sum(extract_numeric(x))
              }
)
test = subset(df,is.na(df$alle)==T)
df=gather(df,alder_grupper,tabellvariabel,`0_5`:alle)
sum(is.na(df$tabellvariabel))
df$tabellvariabel[is.na(df$tabellvariabel)==T]="."
nrow(df)==83*8*3*3*8
write.csv(df,"D:/R/imdikator-munch/data_flat_output/befolkning_alder-naringsregion-2006_2013.csv",row.names=F)

#FYLKE INKL LANDET
befolkning_alder <- read.csv2("D:/R/imdikator-munch/data_flat_input/befolkning_alder_F_2006_2013.csv", stringsAsFactors=FALSE,colClasses = "character")
befolkning_alder_landet <- read.csv2("D:/R/imdikator-munch/data_flat_input/befolkning_alder_L_2006_2013.csv", stringsAsFactors=FALSE,colClasses = "character")
befolkning_alder = rbind(befolkning_alder,befolkning_alder_landet)
befolkning_alder = subset(befolkning_alder,select=-9)
befolkning_alder$alder_grupper = gsub("\\-","_",befolkning_alder$alder_grupper)
levels(as.factor(befolkning_alder$alder_grupper))
names(befolkning_alder)[3] = gsub("id","nr",names(befolkning_alder[3]))
levels(as.factor(befolkning_alder$fylke_nr))
levels(as.factor(befolkning_alder$enhet))
nrow(befolkning_alder)==20*8*3*3*7
df=spread(befolkning_alder,alder_grupper,tabellvariabel)
df$alle=apply(df[,7:13],1,
              function(x){
                      sum(extract_numeric(x))
              }
)
test = subset(df,is.na(df$alle)==T)
df=gather(df,alder_grupper,tabellvariabel,`0_5`:alle)
sum(is.na(df$tabellvariabel))
df$tabellvariabel[is.na(df$tabellvariabel)==T]="."
nrow(df)==20*8*3*3*8

#substansiell sjekk
df$tabellvariabel[df$alder_grupper=="alle"&df$aar=="2013"&df$innvkat_3=="alle"&df$fylke_nr=="00"&df$kjonn=="alle"]

#utskrift
write.csv(df,"D:/R/imdikator-munch/data_flat_output/befolkning_alder-fylke-2006_2013.csv",row.names=F)

#BARNEHAGEDELTAKELSE
test <- read.csv("D:/R/imdikator-munch/data_flat_output/barnehagedeltakelse_spraak-bydel-2013.csv", stringsAsFactors=FALSE,colClasses = "character")
write.csv(test,"data_flat_output/test.csv",row.names = F)

#19.01.2016
#nytt problem oppdaga - trodde først ettåringer er for høye
#men det ser ut til å være feilkoding - ettåringer/barn ellers -> alle/barn ellers
#ettåringer må fortsatt beregnes, men etter omkoding
#alle prosenter for barn ellers alle grupper bhgalder må regnes på nytt
barn <- read.csv("D:/R/imdikator-munch/data_flat_input/barnehagedeltakelse_barnikommune-kommune-2014.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
#omkoding
barn$tabellvariabel[barn$barnikommune=="barn_ellers"&barn$bhgalder=="1_5"&barn$enhet=="personer"]=barn$tabellvariabel[barn$barnikommune=="barn_ellers"&barn$bhgalder=="1"&barn$enhet=="personer"]
#løses ved å beregne residual for barn ellers ett år personer. gir 16 NA, som prikkes som manglende data.
barn$tabellvariabel[barn$barnikommune=="barn_ellers"&barn$bhgalder=="1"&barn$enhet=="personer"] = as.numeric(barn$tabellvariabel[barn$barnikommune=="barn_ellers"&barn$bhgalder=="1_5"&barn$enhet=="personer"])-as.numeric(barn$tabellvariabel[barn$barnikommune=="barn_ellers"&barn$bhgalder=="2"&barn$enhet=="personer"])-as.numeric(barn$tabellvariabel[barn$barnikommune=="barn_ellers"&barn$bhgalder=="3"&barn$enhet=="personer"])-as.numeric(barn$tabellvariabel[barn$barnikommune=="barn_ellers"&barn$bhgalder=="4"&barn$enhet=="personer"])-as.numeric(barn$tabellvariabel[barn$barnikommune=="barn_ellers"&barn$bhgalder=="5"&barn$enhet=="personer"])
sum(is.na(barn$tabellvariabel))
barn$tabellvariabel[is.na(barn$tabellvariabel)==T] = "."
sum(is.na(barn$tabellvariabel))
#sjekk visuelt
df = subset(barn,barnikommune=="barn_ellers"&bhgalder=="1_5"&enhet=="personer")
df = subset(barn,barnikommune=="barn_ellers"&bhgalder=="1"&enhet=="personer")
#må da også beregne prosent på nytt
#viser seg at det er store avvik i tallene, har spurt SSB om oppklaring.
df = spread(barn,barnikommune,tabellvariabel)
df = spread(df,enhet,tabellvariabel)

barn$tabellvariabel[barn$barnikommune=="barn_ellers"&barn$bhgalder=="1_5"&barn$enhet=="prosent"] = as.numeric(barn$tabellvariabel[barn$barnikommune=="barn_ellers"&barn$bhgalder=="1_5"&barn$enhet=="personer"])/as.numeric(barn$tabellvariabel[barn$barnikommune=="alle"&barn$bhgalder=="1_5"&barn$enhet=="personer"])
barn$tabellvariabel[barn$barnikommune=="barn_ellers"&barn$bhgalder=="1"&barn$enhet=="prosent"] = as.numeric(barn$tabellvariabel[barn$barnikommune=="barn_ellers"&barn$bhgalder=="1"&barn$enhet=="personer"])/as.numeric(barn$tabellvariabel[barn$barnikommune=="alle"&barn$bhgalder=="1"&barn$enhet=="personer"])
barn$tabellvariabel[barn$barnikommune=="barn_ellers"&barn$bhgalder=="2"&barn$enhet=="prosent"] = as.numeric(barn$tabellvariabel[barn$barnikommune=="barn_ellers"&barn$bhgalder=="2"&barn$enhet=="personer"])/as.numeric(barn$tabellvariabel[barn$barnikommune=="alle"&barn$bhgalder=="2"&barn$enhet=="personer"])
barn$tabellvariabel[barn$barnikommune=="barn_ellers"&barn$bhgalder=="3"&barn$enhet=="prosent"] = as.numeric(barn$tabellvariabel[barn$barnikommune=="barn_ellers"&barn$bhgalder=="3"&barn$enhet=="personer"])/as.numeric(barn$tabellvariabel[barn$barnikommune=="alle"&barn$bhgalder=="3"&barn$enhet=="personer"])
barn$tabellvariabel[barn$barnikommune=="barn_ellers"&barn$bhgalder=="4"&barn$enhet=="prosent"] = as.numeric(barn$tabellvariabel[barn$barnikommune=="barn_ellers"&barn$bhgalder=="4"&barn$enhet=="personer"])/as.numeric(barn$tabellvariabel[barn$barnikommune=="alle"&barn$bhgalder=="4"&barn$enhet=="personer"])
barn$tabellvariabel[barn$barnikommune=="barn_ellers"&barn$bhgalder=="5"&barn$enhet=="prosent"] = as.numeric(barn$tabellvariabel[barn$barnikommune=="barn_ellers"&barn$bhgalder=="5"&barn$enhet=="personer"])/as.numeric(barn$tabellvariabel[barn$barnikommune=="alle"&barn$bhgalder=="5"&barn$enhet=="personer"])
sum(is.na(barn$tabellvariabel[barn$barnikommune=="barn_ellers"&barn$enhet=="prosent"]))
barn$tabellvariabel[is.na(barn$tabellvariabel)]="."

df=subset(barn,enhet=="prosent")
df = spread(df,barnikommune,tabellvariabel)

#GRUNNSKOLEPOENG
#26.10.2015
grunnskolepoeng <- read.csv("D:/R/imdikator-munch/data_flat_input/grunnskolepoeng.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE)
df = subset(grunnskolepoeng,kommune_nr!="NULL",select=-c(bydel_nr,fylke_nr,naringsregion_nr))

df_test = subset(grunnskolepoeng,bydel_nr!="NULL")

#diagnose:
#kjønnsfordeling for kommune er ikke uniform (har 0 og 1 for alle, og alle for andre kategorier)
#vreg3 og invalder er kun tilgjengelig for andre enheter enn kommune, og tegnes ikke ut i grensesnittet
#skaper det trøbbel at de inngår?
#løsning 1: økse alt anna enn kjønn, fylle inn manglende kombinasjoner av kjønn med manglende data
#løsning 2: fylle inn manglende komboer av kjønn, ikke røre noe annet.

#NORSK - PROVER
norsk_prover <- read.csv("D:/R/imdikator-munch/data_flat_input/norsk_prover.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE)

#provetype mangler alle
#spraakniva.alle mangler prosenter
#løses i excel, lettere å legge sammen der

norsk_prover <- read.csv("D:/R/imdikator-munch/data_crossed_input/norsk_prover-fylke-halvar2014.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE)
#krysstabell, ikke flat.

#25. januar
norsk_prover <- read.csv("D:/R/imdikator-munch/data_crossed_input/norsk_prover-fylke-2015.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=";", dec=",",colClasses="character")

#intro avslutning direkte
#26. oktober
#utfordringer: dele i to filer, en 8-delt og en 3-delt.
#kode om prosenter fra ":" til ".".

#INTRO_STATUS_ARBUTD
#27.okt
intro_status_arbutd <- read.csv("D:/R/imdikator-munch/data_crossed_input/intro_status_arbutd-fylke-2011_2013.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE,colClasses = "character")
#17. nov
intro_status_arbutd <- read.csv("D:/R/imdikator-munch/data_crossed_input/intro_status_arbutd-naringsregion-2011_2013.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE,colClasses = "character")

#Befolkning_hovedgruppe
#6. desember 2015
befolkning_hovedgruppe <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_hovedgruppe_B.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE)
#1. Bydel-ID har feil lengde
#2. Bydel_nr
#3. feil sep
befolkning_hovedgruppe$bydel_id[nchar(befolkning_hovedgruppe$bydel_id)==1] = paste0("0",befolkning_hovedgruppe$bydel_id[nchar(befolkning_hovedgruppe$bydel_id)==1])
befolkning_hovedgruppe$bydel_id[nchar(befolkning_hovedgruppe$bydel_id)==2] = paste0("0301",befolkning_hovedgruppe$bydel_id[nchar(befolkning_hovedgruppe$bydel_id)==2])
names(befolkning_hovedgruppe)[3] = "bydel_nr"
write.csv(befolkning_hovedgruppe,"D:/R/imdikator-munch/data_flat_output/befolkning_hovedgruppe-bydel-2015.csv",row.names=F)
befolkning_hovedgruppe <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_hovedgruppe_K.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE)
#1. kommune-ID har feil lengde
#2. Bydel_nr
#3. feil sep
befolkning_hovedgruppe = subset(befolkning_hovedgruppe,select=-8)
befolkning_hovedgruppe$kommune_id[nchar(befolkning_hovedgruppe$kommune_id)==3] = paste0("0",befolkning_hovedgruppe$kommune_id[nchar(befolkning_hovedgruppe$kommune_id)==3])
names(befolkning_hovedgruppe)[3] = "kommune_nr"
write.csv(befolkning_hovedgruppe,"D:/R/imdikator-munch/data_flat_output/befolkning_hovedgruppe-kommune-2015.csv",row.names=F)
befolkning_hovedgruppe <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_hovedgruppe_N.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE)
#0. en kolonne for mye
#1. kommune-ID har feil lengde
#2. Bydel_nr
#3. feil sep
befolkning_hovedgruppe = subset(befolkning_hovedgruppe,select=-8)
befolkning_hovedgruppe$naringsregion_id[nchar(befolkning_hovedgruppe$naringsregion_id)==1] = paste0("0",befolkning_hovedgruppe$naringsregion_id[nchar(befolkning_hovedgruppe$naringsregion_id)==1])
names(befolkning_hovedgruppe)[3] = "naringsregion_nr"
write.csv(befolkning_hovedgruppe,"D:/R/imdikator-munch/data_flat_output/befolkning_hovedgruppe-naringsregion-2015.csv",row.names=F)
befolkning_hovedgruppe <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_hovedgruppe_F.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE)
#0. en kolonne for mye
#1. kommune-ID har feil lengde
#2. Bydel_nr
#3. feil sep
#4. nasjonstall lagret i egen fil
befolkning_hovedgruppe = subset(befolkning_hovedgruppe,select=-8)
befolkning_hovedgruppe$fylke_id[nchar(befolkning_hovedgruppe$fylke_id)==1] = paste0("0",befolkning_hovedgruppe$fylke_id[nchar(befolkning_hovedgruppe$fylke_id)==1])
names(befolkning_hovedgruppe)[3] = "fylke_nr"
befolkning_hovedgruppe_norge <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_hovedgruppe_L.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE)
befolkning_hovedgruppe_norge = subset(befolkning_hovedgruppe_norge,select=-8)
befolkning_hovedgruppe_norge$fylke_id[nchar(befolkning_hovedgruppe_norge$fylke_id)==1] = paste0("0",befolkning_hovedgruppe_norge$fylke_id[nchar(befolkning_hovedgruppe_norge$fylke_id)==1])
names(befolkning_hovedgruppe_norge)[3] = "fylke_nr"
befolkning_hovedgruppe = rbind(befolkning_hovedgruppe,befolkning_hovedgruppe_norge)
write.csv(befolkning_hovedgruppe,"D:/R/imdikator-munch/data_flat_output/befolkning_hovedgruppe-fylke-2015.csv",row.names=F)

#BEfolkning_innvandringsgrunn
#6. desember 2015
befolkning_innvandringsgrunn <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_innvandringsgrunn_B_2014.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE)
#0. en kolonne for mye
#1. Bydel-ID har feil lengde
#2. Bydel_nr
#3. feil sep
#4. innvgrunn_6 har tomme verdier
befolkning_innvandringsgrunn = subset(befolkning_innvandringsgrunn,select=-8)
befolkning_innvandringsgrunn$bydel_id[nchar(befolkning_innvandringsgrunn$bydel_id)==1] = paste0("0",befolkning_innvandringsgrunn$bydel_id[nchar(befolkning_innvandringsgrunn$bydel_id)==1])
befolkning_innvandringsgrunn$bydel_id[nchar(befolkning_innvandringsgrunn$bydel_id)==2] = paste0("0301",befolkning_innvandringsgrunn$bydel_id[nchar(befolkning_innvandringsgrunn$bydel_id)==2])
names(befolkning_innvandringsgrunn)[3] = "bydel_nr"
levels(as.factor(befolkning_innvandringsgrunn$innvgrunn_6))
befolkning_innvandringsgrunn = subset(befolkning_innvandringsgrunn,innvgrunn_6!=" ")
write.csv(befolkning_innvandringsgrunn,"D:/R/imdikator-munch/data_flat_output/befolkning_innvandringsgrunn-bydel-2014.csv",row.names=F)
#2015
befolkning_innvandringsgrunn <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_innvandringsgrunn_B_2015.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE)
#0. en kolonne for mye
#1. Bydel-ID har feil lengde
#2. Bydel_nr
#3. feil sep
#4. innvgrunn_6 har tomme verdier
befolkning_innvandringsgrunn = subset(befolkning_innvandringsgrunn,select=-8)
befolkning_innvandringsgrunn$bydel_id[nchar(befolkning_innvandringsgrunn$bydel_id)==1] = paste0("0",befolkning_innvandringsgrunn$bydel_id[nchar(befolkning_innvandringsgrunn$bydel_id)==1])
befolkning_innvandringsgrunn$bydel_id[nchar(befolkning_innvandringsgrunn$bydel_id)==2] = paste0("0301",befolkning_innvandringsgrunn$bydel_id[nchar(befolkning_innvandringsgrunn$bydel_id)==2])
names(befolkning_innvandringsgrunn)[3] = "bydel_nr"
levels(as.factor(befolkning_innvandringsgrunn$innvgrunn_6))
befolkning_innvandringsgrunn = subset(befolkning_innvandringsgrunn,innvgrunn_6!=" ")
write.csv(befolkning_innvandringsgrunn,"D:/R/imdikator-munch/data_flat_output/befolkning_innvandringsgrunn-bydel-2015.csv",row.names=F)

#Bydel 2006-2013
befolkning_innvandringsgrunn_06 <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_innvandringsgrunn_B_2006.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE,colClasses="character")
befolkning_innvandringsgrunn_07 <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_innvandringsgrunn_B_2007.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE,colClasses="character")
befolkning_innvandringsgrunn_08 <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_innvandringsgrunn_B_2008.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE,colClasses="character")
befolkning_innvandringsgrunn_09 <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_innvandringsgrunn_B_2009.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE,colClasses="character")
befolkning_innvandringsgrunn_10 <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_innvandringsgrunn_B_2010.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE,colClasses="character")
befolkning_innvandringsgrunn_11 <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_innvandringsgrunn_B_2011.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE,colClasses="character")
befolkning_innvandringsgrunn_12 <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_innvandringsgrunn_B_2012.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE,colClasses="character")
befolkning_innvandringsgrunn_13 <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_innvandringsgrunn_B_2013.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE,colClasses="character")
befolkning_innvandringsgrunn = rbind(befolkning_innvandringsgrunn_06,befolkning_innvandringsgrunn_07,befolkning_innvandringsgrunn_08,befolkning_innvandringsgrunn_09,befolkning_innvandringsgrunn_10,befolkning_innvandringsgrunn_11,befolkning_innvandringsgrunn_12,befolkning_innvandringsgrunn_13)

#ny leveranse, 2006-2015 i ei fil
befolkning_innvandringsgrunn = read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_innvandringsgrunn_B_2006_2015.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE,colClasses="character")
#0. en kolonne for mye
#1. Bydel-ID har feil lengde
#2. Bydel_nr
#3. feil sep
#4. innvgrunn_6 har tomme verdier
#5 enhet skal være personer
befolkning_innvandringsgrunn = subset(befolkning_innvandringsgrunn,select=-8)
befolkning_innvandringsgrunn$bydel_id[nchar(befolkning_innvandringsgrunn$bydel_id)==1] = paste0("0",befolkning_innvandringsgrunn$bydel_id[nchar(befolkning_innvandringsgrunn$bydel_id)==1])
befolkning_innvandringsgrunn$bydel_id[nchar(befolkning_innvandringsgrunn$bydel_id)==2] = paste0("0301",befolkning_innvandringsgrunn$bydel_id[nchar(befolkning_innvandringsgrunn$bydel_id)==2])
names(befolkning_innvandringsgrunn)[3] = "bydel_nr"
levels(as.factor(befolkning_innvandringsgrunn$innvgrunn_6))
befolkning_innvandringsgrunn = subset(befolkning_innvandringsgrunn,innvgrunn_6!=" ")
levels(as.factor(befolkning_innvandringsgrunn$kjonn))
levels(as.factor(befolkning_innvandringsgrunn$enhet))
befolkning_innvandringsgrunn$tabellvariabel = gsub("\\,","\\.",befolkning_innvandringsgrunn$tabellvariabel)
#befolkning_innvandringsgrunn$enhet[befolkning_innvandringsgrunn$enhet=="person"]="personer"
#logisk sjekk
nrow(befolkning_innvandringsgrunn)==10*18*6*3*2
write.csv(befolkning_innvandringsgrunn,"D:/R/imdikator-munch/data_flat_output/befolkning_innvandringsgrunn-bydel-2006_2015.csv",row.names=F)

#2014-kommune
befolkning_innvandringsgrunn <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_innvandringsgrunn_K_2014.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE)
#0. en kolonne for mye
#1. Bydel-ID har feil lengde
#2. Bydel_nr
#3. feil sep
#4. innvgrunn_6 har tomme verdier
befolkning_innvandringsgrunn = subset(befolkning_innvandringsgrunn,select=-8)
befolkning_innvandringsgrunn$kommune_id[nchar(befolkning_innvandringsgrunn$kommune_id)==3] = paste0("0",befolkning_innvandringsgrunn$kommune_id[nchar(befolkning_innvandringsgrunn$kommune_id)==3])
names(befolkning_innvandringsgrunn)[3] = "kommune_nr"
levels(as.factor(befolkning_innvandringsgrunn$innvgrunn_6))
befolkning_innvandringsgrunn = subset(befolkning_innvandringsgrunn,innvgrunn_6!=" ")
write.csv(befolkning_innvandringsgrunn,"D:/R/imdikator-munch/data_flat_output/befolkning_innvandringsgrunn-kommune-2014.csv",row.names=F)

#2015-kommune
befolkning_innvandringsgrunn <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_innvandringsgrunn_K_2015.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE)
#0. en kolonne for mye
#1. Bydel-ID har feil lengde
#2. Bydel_nr
#3. feil sep
#4. innvgrunn_6 har tomme verdier
befolkning_innvandringsgrunn = subset(befolkning_innvandringsgrunn,select=-8)
befolkning_innvandringsgrunn$kommune_id[nchar(befolkning_innvandringsgrunn$kommune_id)==3] = paste0("0",befolkning_innvandringsgrunn$kommune_id[nchar(befolkning_innvandringsgrunn$kommune_id)==3])
names(befolkning_innvandringsgrunn)[3] = "kommune_nr"
levels(as.factor(befolkning_innvandringsgrunn$innvgrunn_6))
befolkning_innvandringsgrunn = subset(befolkning_innvandringsgrunn,innvgrunn_6!=" ")
write.csv(befolkning_innvandringsgrunn,"D:/R/imdikator-munch/data_flat_output/befolkning_innvandringsgrunn-kommune-2015.csv",row.names=F)

#Kommune 2006-2013
befolkning_innvandringsgrunn_06 <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_innvandringsgrunn_K_2006.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE,colClasses="character")
befolkning_innvandringsgrunn_07 <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_innvandringsgrunn_K_2007.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE,colClasses="character")
befolkning_innvandringsgrunn_08 <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_innvandringsgrunn_K_2008.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE,colClasses="character")
befolkning_innvandringsgrunn_09 <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_innvandringsgrunn_K_2009.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE,colClasses="character")
befolkning_innvandringsgrunn_10 <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_innvandringsgrunn_K_2010.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE,colClasses="character")
befolkning_innvandringsgrunn_11 <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_innvandringsgrunn_K_2011.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE,colClasses="character")
befolkning_innvandringsgrunn_12 <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_innvandringsgrunn_K_2012.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE,colClasses="character")
befolkning_innvandringsgrunn_13 <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_innvandringsgrunn_K_2013.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE,colClasses="character")
befolkning_innvandringsgrunn = rbind(befolkning_innvandringsgrunn_06,befolkning_innvandringsgrunn_07,befolkning_innvandringsgrunn_08,befolkning_innvandringsgrunn_09,befolkning_innvandringsgrunn_10,befolkning_innvandringsgrunn_11,befolkning_innvandringsgrunn_12,befolkning_innvandringsgrunn_13)

#kommune 2006-2015
befolkning_innvandringsgrunn <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_innvandringsgrunn_K_2006_2015.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE,colClasses="character")

#0. en kolonne for mye
#1. Bydel-ID har feil lengde
#2. Bydel_nr
#3. feil sep
#4. innvgrunn_6 har tomme verdier
#5 enhet skal være personer
befolkning_innvandringsgrunn = subset(befolkning_innvandringsgrunn,select=-8)
befolkning_innvandringsgrunn$kommune_id[nchar(befolkning_innvandringsgrunn$kommune_id)==3] = paste0("0",befolkning_innvandringsgrunn$kommune_id[nchar(befolkning_innvandringsgrunn$kommune_id)==3])
names(befolkning_innvandringsgrunn)[3] = "kommune_nr"
levels(as.factor(befolkning_innvandringsgrunn$innvgrunn_6))
befolkning_innvandringsgrunn = subset(befolkning_innvandringsgrunn,innvgrunn_6!=" ")
levels(as.factor(befolkning_innvandringsgrunn$kjonn))
levels(as.factor(befolkning_innvandringsgrunn$enhet))
#befolkning_innvandringsgrunn$enhet[befolkning_innvandringsgrunn$enhet=="person"]="personer"
nlevels(as.factor(befolkning_innvandringsgrunn$kommune_nr))
befolkning_innvandringsgrunn$tabellvariabel = gsub("\\,","\\.",befolkning_innvandringsgrunn$tabellvariabel)
#logisk sjekk
nrow(befolkning_innvandringsgrunn)==10*428*6*3*2
write.csv(befolkning_innvandringsgrunn,"D:/R/imdikator-munch/data_flat_output/befolkning_innvandringsgrunn-kommune-2006_2015.csv",row.names=F)

#2014-næringsregion
befolkning_innvandringsgrunn <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_innvandringsgrunn_N_2014.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE)
#0. en kolonne for mye
#1. Bydel-ID har feil lengde
#2. Bydel_nr
#3. feil sep
#4. innvgrunn_6 har tomme verdier
befolkning_innvandringsgrunn = subset(befolkning_innvandringsgrunn,select=-8)
befolkning_innvandringsgrunn$naringsregion_id[nchar(befolkning_innvandringsgrunn$naringsregion_id)==1] = paste0("0",befolkning_innvandringsgrunn$naringsregion_id[nchar(befolkning_innvandringsgrunn$naringsregion_id)==1])
names(befolkning_innvandringsgrunn)[3] = "naringsregion_nr"
levels(as.factor(befolkning_innvandringsgrunn$innvgrunn_6))
befolkning_innvandringsgrunn = subset(befolkning_innvandringsgrunn,innvgrunn_6!=" ")
write.csv(befolkning_innvandringsgrunn,"D:/R/imdikator-munch/data_flat_output/befolkning_innvandringsgrunn-naringsregion-2014.csv",row.names=F)

#2015-næringsregion
befolkning_innvandringsgrunn <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_innvandringsgrunn_N_2015.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE)
#0. en kolonne for mye
#1. Bydel-ID har feil lengde
#2. Bydel_nr
#3. feil sep
#4. innvgrunn_6 har tomme verdier
befolkning_innvandringsgrunn = subset(befolkning_innvandringsgrunn,select=-8)
befolkning_innvandringsgrunn$naringsregion_id[nchar(befolkning_innvandringsgrunn$naringsregion_id)==1] = paste0("0",befolkning_innvandringsgrunn$naringsregion_id[nchar(befolkning_innvandringsgrunn$naringsregion_id)==1])
names(befolkning_innvandringsgrunn)[3] = "naringsregion_nr"
levels(as.factor(befolkning_innvandringsgrunn$innvgrunn_6))
befolkning_innvandringsgrunn = subset(befolkning_innvandringsgrunn,innvgrunn_6!=" ")
write.csv(befolkning_innvandringsgrunn,"D:/R/imdikator-munch/data_flat_output/befolkning_innvandringsgrunn-naringsregion-2015.csv",row.names=F)

#Næringsregion 2006-2013
befolkning_innvandringsgrunn_06 <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_innvandringsgrunn_N_2006.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE,colClasses="character")
befolkning_innvandringsgrunn_07 <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_innvandringsgrunn_N_2007.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE,colClasses="character")
befolkning_innvandringsgrunn_08 <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_innvandringsgrunn_N_2008.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE,colClasses="character")
befolkning_innvandringsgrunn_09 <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_innvandringsgrunn_N_2009.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE,colClasses="character")
befolkning_innvandringsgrunn_10 <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_innvandringsgrunn_N_2010.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE,colClasses="character")
befolkning_innvandringsgrunn_11 <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_innvandringsgrunn_N_2011.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE,colClasses="character")
befolkning_innvandringsgrunn_12 <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_innvandringsgrunn_N_2012.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE,colClasses="character")
befolkning_innvandringsgrunn_13 <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_innvandringsgrunn_N_2013.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE,colClasses="character")
befolkning_innvandringsgrunn = rbind(befolkning_innvandringsgrunn_06,befolkning_innvandringsgrunn_07,befolkning_innvandringsgrunn_08,befolkning_innvandringsgrunn_09,befolkning_innvandringsgrunn_10,befolkning_innvandringsgrunn_11,befolkning_innvandringsgrunn_12,befolkning_innvandringsgrunn_13)

#2006-2015
befolkning_innvandringsgrunn <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_innvandringsgrunn_N_2006_2015.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE,colClasses="character")

befolkning_innvandringsgrunn = subset(befolkning_innvandringsgrunn,select=-8)
befolkning_innvandringsgrunn$naringsregion_id[nchar(befolkning_innvandringsgrunn$naringsregion_id)==1] = paste0("0",befolkning_innvandringsgrunn$naringsregion_id[nchar(befolkning_innvandringsgrunn$naringsregion_id)==1])
names(befolkning_innvandringsgrunn)[3] = "naringsregion_nr"
levels(as.factor(befolkning_innvandringsgrunn$innvgrunn_6))
befolkning_innvandringsgrunn = subset(befolkning_innvandringsgrunn,innvgrunn_6!=" ")
levels(as.factor(befolkning_innvandringsgrunn$kjonn))
levels(as.factor(befolkning_innvandringsgrunn$enhet))
#befolkning_innvandringsgrunn$enhet[befolkning_innvandringsgrunn$enhet=="person"]="personer"
nlevels(as.factor(befolkning_innvandringsgrunn$naringsregion_nr))
levels(as.factor(befolkning_innvandringsgrunn$naringsregion_nr))
befolkning_innvandringsgrunn$tabellvariabel = gsub("\\,","\\.",befolkning_innvandringsgrunn$tabellvariabel)
#logisk sjekk
nrow(befolkning_innvandringsgrunn)==10*83*6*3*2
#test = subset(befolkning_innvandringsgrunn,naringsregion_nr=="99")
write.csv(befolkning_innvandringsgrunn,"D:/R/imdikator-munch/data_flat_output/befolkning_innvandringsgrunn-naringsregion-2006_2015.csv",row.names=F)

#2014-fylke
befolkning_innvandringsgrunn <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_innvandringsgrunn_F_2014.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE)
#0. en kolonne for mye
#1. Bydel-ID har feil lengde
#2. Bydel_nr
#3. feil sep
#4. innvgrunn_6 har tomme verdier
#5. mangler nasjonale tall
befolkning_innvandringsgrunn = subset(befolkning_innvandringsgrunn,select=-8)
befolkning_innvandringsgrunn$fylke_id[nchar(befolkning_innvandringsgrunn$fylke_id)==1] = paste0("0",befolkning_innvandringsgrunn$fylke_id[nchar(befolkning_innvandringsgrunn$fylke_id)==1])
names(befolkning_innvandringsgrunn)[3] = "fylke_nr"
levels(as.factor(befolkning_innvandringsgrunn$innvgrunn_6))
befolkning_innvandringsgrunn = subset(befolkning_innvandringsgrunn,innvgrunn_6!=" ")
befolkning_innvandringsgrunn_norge <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_innvandringsgrunn_L_2014.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE)
befolkning_innvandringsgrunn_norge = subset(befolkning_innvandringsgrunn_norge,select=-8)
befolkning_innvandringsgrunn_norge$fylke_id[nchar(befolkning_innvandringsgrunn_norge$fylke_id)==1] = paste0("0",befolkning_innvandringsgrunn_norge$fylke_id[nchar(befolkning_innvandringsgrunn_norge$fylke_id)==1])
names(befolkning_innvandringsgrunn_norge)[3] = "fylke_nr"
levels(as.factor(befolkning_innvandringsgrunn_norge$innvgrunn_6))
befolkning_innvandringsgrunn_norge = subset(befolkning_innvandringsgrunn_norge,innvgrunn_6!=" ")
befolkning_innvandringsgrunn = rbind(befolkning_innvandringsgrunn,befolkning_innvandringsgrunn_norge)
write.csv(befolkning_innvandringsgrunn,"D:/R/imdikator-munch/data_flat_output/befolkning_innvandringsgrunn-fylke-2014.csv",row.names=F)

#2015-fylke
befolkning_innvandringsgrunn <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_innvandringsgrunn_F_2015.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE)
#0. en kolonne for mye
#1. Bydel-ID har feil lengde
#2. Bydel_nr
#3. feil sep
#4. innvgrunn_6 har tomme verdier
#5. mangler nasjonale tall
befolkning_innvandringsgrunn = subset(befolkning_innvandringsgrunn,select=-8)
befolkning_innvandringsgrunn$fylke_id[nchar(befolkning_innvandringsgrunn$fylke_id)==1] = paste0("0",befolkning_innvandringsgrunn$fylke_id[nchar(befolkning_innvandringsgrunn$fylke_id)==1])
names(befolkning_innvandringsgrunn)[3] = "fylke_nr"
levels(as.factor(befolkning_innvandringsgrunn$innvgrunn_6))
befolkning_innvandringsgrunn = subset(befolkning_innvandringsgrunn,innvgrunn_6!=" ")
befolkning_innvandringsgrunn_norge <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_innvandringsgrunn_L_2015.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE)
befolkning_innvandringsgrunn_norge = subset(befolkning_innvandringsgrunn_norge,select=-8)
befolkning_innvandringsgrunn_norge$fylke_id[nchar(befolkning_innvandringsgrunn_norge$fylke_id)==1] = paste0("0",befolkning_innvandringsgrunn_norge$fylke_id[nchar(befolkning_innvandringsgrunn_norge$fylke_id)==1])
names(befolkning_innvandringsgrunn_norge)[3] = "fylke_nr"
levels(as.factor(befolkning_innvandringsgrunn_norge$innvgrunn_6))
befolkning_innvandringsgrunn_norge = subset(befolkning_innvandringsgrunn_norge,innvgrunn_6!=" ")
befolkning_innvandringsgrunn = rbind(befolkning_innvandringsgrunn,befolkning_innvandringsgrunn_norge)
write.csv(befolkning_innvandringsgrunn,"D:/R/imdikator-munch/data_flat_output/befolkning_innvandringsgrunn-fylke-2015.csv",row.names=F)

#Fylke og land 2006-2013
befolkning_innvandringsgrunn_06 <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_innvandringsgrunn_F_2006.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE,colClasses="character")
befolkning_innvandringsgrunn_07 <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_innvandringsgrunn_F_2007.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE,colClasses="character")
befolkning_innvandringsgrunn_08 <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_innvandringsgrunn_F_2008.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE,colClasses="character")
befolkning_innvandringsgrunn_09 <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_innvandringsgrunn_F_2009.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE,colClasses="character")
befolkning_innvandringsgrunn_10 <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_innvandringsgrunn_F_2010.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE,colClasses="character")
befolkning_innvandringsgrunn_11 <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_innvandringsgrunn_F_2011.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE,colClasses="character")
befolkning_innvandringsgrunn_12 <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_innvandringsgrunn_F_2012.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE,colClasses="character")
befolkning_innvandringsgrunn_13 <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_innvandringsgrunn_F_2013.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE,colClasses="character")
befolkning_innvandringsgrunn = rbind(befolkning_innvandringsgrunn_06,befolkning_innvandringsgrunn_07,befolkning_innvandringsgrunn_08,befolkning_innvandringsgrunn_09,befolkning_innvandringsgrunn_10,befolkning_innvandringsgrunn_11,befolkning_innvandringsgrunn_12,befolkning_innvandringsgrunn_13)

befolkning_innvandringsgrunn_06 <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_innvandringsgrunn_L_2006.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE,colClasses="character")
befolkning_innvandringsgrunn_07 <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_innvandringsgrunn_L_2007.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE,colClasses="character")
befolkning_innvandringsgrunn_08 <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_innvandringsgrunn_L_2008.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE,colClasses="character")
befolkning_innvandringsgrunn_09 <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_innvandringsgrunn_L_2009.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE,colClasses="character")
befolkning_innvandringsgrunn_10 <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_innvandringsgrunn_L_2010.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE,colClasses="character")
befolkning_innvandringsgrunn_11 <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_innvandringsgrunn_L_2011.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE,colClasses="character")
befolkning_innvandringsgrunn_12 <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_innvandringsgrunn_L_2012.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE,colClasses="character")
befolkning_innvandringsgrunn_13 <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_innvandringsgrunn_L_2013.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE,colClasses="character")
befolkning_innvandringsgrunn_land = rbind(befolkning_innvandringsgrunn_06,befolkning_innvandringsgrunn_07,befolkning_innvandringsgrunn_08,befolkning_innvandringsgrunn_09,befolkning_innvandringsgrunn_10,befolkning_innvandringsgrunn_11,befolkning_innvandringsgrunn_12,befolkning_innvandringsgrunn_13)
befolkning_innvandringsgrunn = rbind(befolkning_innvandringsgrunn,befolkning_innvandringsgrunn_land)

#2006-2015
befolkning_innvandringsgrunn_land <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_innvandringsgrunn_L_2006_2015.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE,colClasses="character")
befolkning_innvandringsgrunn <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_innvandringsgrunn_F_2006_2015.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE,colClasses="character")
befolkning_innvandringsgrunn = rbind(befolkning_innvandringsgrunn,befolkning_innvandringsgrunn_land)

befolkning_innvandringsgrunn = subset(befolkning_innvandringsgrunn,select=-8)
befolkning_innvandringsgrunn$fylke_id[nchar(befolkning_innvandringsgrunn$fylke_id)==1] = paste0("0",befolkning_innvandringsgrunn$fylke_id[nchar(befolkning_innvandringsgrunn$fylke_id)==1])
names(befolkning_innvandringsgrunn)[3] = "fylke_nr"
levels(as.factor(befolkning_innvandringsgrunn$innvgrunn_6))
befolkning_innvandringsgrunn = subset(befolkning_innvandringsgrunn,innvgrunn_6!=" ")
befolkning_innvandringsgrunn$enhet[befolkning_innvandringsgrunn$enhet=="person"]="personer"

nlevels(as.factor(befolkning_innvandringsgrunn$fylke_nr))
levels(as.factor(befolkning_innvandringsgrunn$fylke_nr))
befolkning_innvandringsgrunn$tabellvariabel = gsub("\\,","\\.",befolkning_innvandringsgrunn$tabellvariabel)
#logisk sjekk
nrow(befolkning_innvandringsgrunn)==10*20*6*3*2
write.csv(befolkning_innvandringsgrunn,"D:/R/imdikator-munch/data_flat_output/befolkning_innvandringsgrunn-fylke-2006_2015.csv",row.names=F)

#substanssjekk
df = subset(befolkning_innvandringsgrunn,enhet=="prosent"&befolkning_innvandringsgrunn$fylke_nr=="00"&befolkning_innvandringsgrunn$aar=="2015")

#BEFOLKNING_OPPRINNELSESLAND
#kjørt på nytt 31. januar.
befolkning_opprinnelsesland <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_opprinnelsesland_B_2006_2013.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE,colClasses = "character")
#2 kolonner for mye
#1. Bydel-ID har feil lengde
#2. Bydel_nr
#3. feil sep
levels(as.factor(befolkning_opprinnelsesland$kjonn))
befolkning_opprinnelsesland = subset(befolkning_opprinnelsesland,select=c(-8,-kjonn))
befolkning_opprinnelsesland$bydel_id[nchar(befolkning_opprinnelsesland$bydel_id)==1] = paste0("0",befolkning_opprinnelsesland$bydel_id[nchar(befolkning_opprinnelsesland$bydel_id)==1])
befolkning_opprinnelsesland$bydel_id[nchar(befolkning_opprinnelsesland$bydel_id)==2] = paste0("0301",befolkning_opprinnelsesland$bydel_id[nchar(befolkning_opprinnelsesland$bydel_id)==2])
names(befolkning_opprinnelsesland)[3] = "bydel_nr"
write.csv(befolkning_opprinnelsesland,"D:/R/imdikator-munch/data_flat_output/befolkning_opprinnelsesland-bydel-2006_2013.csv",row.names=F)

#kommune
befolkning_opprinnelsesland <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_opprinnelsesland_K_2006_2013.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE,colClasses = "character")
#1. kommune-ID har feil lengde
#2. kommune_nr
#3. feil sep
levels(as.factor(befolkning_opprinnelsesland$kjonn))
befolkning_opprinnelsesland = subset(befolkning_opprinnelsesland,select=c(-8,-kjonn))
befolkning_opprinnelsesland$kommune_id[nchar(befolkning_opprinnelsesland$kommune_id)==3] = paste0("0",befolkning_opprinnelsesland$kommune_id[nchar(befolkning_opprinnelsesland$kommune_id)==3])
names(befolkning_opprinnelsesland)[3] = "kommune_nr"
write.csv(befolkning_opprinnelsesland,"D:/R/imdikator-munch/data_flat_output/befolkning_opprinnelsesland-kommune-2006_2013.csv",row.names=F)

#naringsregion
befolkning_opprinnelsesland <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_opprinnelsesland_N_2006_2013.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE,colClasses = "character")
#1. naringsregion-ID har feil lengde
#2. naringsregion_nr
#3. feil sep
levels(as.factor(befolkning_opprinnelsesland$kjonn))
befolkning_opprinnelsesland = subset(befolkning_opprinnelsesland,select=c(-8,-kjonn))
befolkning_opprinnelsesland$naringsregion_id[nchar(befolkning_opprinnelsesland$naringsregion_id)==1] = paste0("0",befolkning_opprinnelsesland$naringsregion_id[nchar(befolkning_opprinnelsesland$naringsregion_id)==1])
names(befolkning_opprinnelsesland)[3] = "naringsregion_nr"
write.csv(befolkning_opprinnelsesland,"D:/R/imdikator-munch/data_flat_output/befolkning_opprinnelsesland-naringsregion-2006_2013.csv",row.names=F)

#fylke + land
befolkning_opprinnelsesland <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_opprinnelsesland_F_2006_2013.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE)
befolkning_opprinnelsesland_N <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_opprinnelsesland_L_2006_2013.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE)
befolkning_opprinnelsesland = rbind(befolkning_opprinnelsesland,befolkning_opprinnelsesland_N)
#1. fylke-ID har feil lengde
#2. fylke_nr
#3. feil sep
levels(as.factor(befolkning_opprinnelsesland$kjonn))
befolkning_opprinnelsesland = subset(befolkning_opprinnelsesland,select=c(-8,-kjonn))
befolkning_opprinnelsesland$fylke_id[nchar(befolkning_opprinnelsesland$fylke_id)==1] = paste0("0",befolkning_opprinnelsesland$fylke_id[nchar(befolkning_opprinnelsesland$fylke_id)==1])
names(befolkning_opprinnelsesland)[3] = "fylke_nr"
write.csv(befolkning_opprinnelsesland,"D:/R/imdikator-munch/data_flat_output/befolkning_opprinnelsesland-fylke-2006_2013.csv",row.names=F)

#BEFOLKNING_Verdensregion
#6. desember 2015
#1. feil tabellnavn
#2. mangler alle-variabel på vreg_variabel, feil vreg-variabel
#3. id, ikke nr, feil sep
#4. land er egen fil, ikke i fylket.
# en kolonne for mye

#BEFOLKNING_VERDENSREGION_3 - Bydel
befolkning_verdensregion_2 <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_verdensregion_2B.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=";", dec=",")
befolkning_verdensregion_2 = subset(befolkning_verdensregion_2,select=-8)
befolkning_verdensregion_2$bydel_id[nchar(befolkning_verdensregion_2$bydel_id)==1] = paste0("0",befolkning_verdensregion_2$bydel_id[nchar(befolkning_verdensregion_2$bydel_id)==1])
befolkning_verdensregion_2$bydel_id[nchar(befolkning_verdensregion_2$bydel_id)==2] = paste0("0301",befolkning_verdensregion_2$bydel_id[nchar(befolkning_verdensregion_2$bydel_id)==2])
names(befolkning_verdensregion_2)[3] = "bydel_nr"
library(tidyr)
df=spread(befolkning_verdensregion_2,vreg_2,tabellvariabel)
df$alle=extract_numeric(df[,6])+extract_numeric(df[,7])
#er det innført NA?
sum(is.na(df$alle))
#koder denne om til "." manglende data
#samler vreg-variabler til vreg_3
df_2=gather(df,vreg_3,tabellvariabel,6:8,na.rm=F)
#er ny df utvidet med riktig nrow?
nrow(df_2)==nrow(befolkning_verdensregion_2)+nrow(df)-sum(is.na(df$alle))
nrow(df_2)==nrow(befolkning_verdensregion_2)+sum(befolkning_verdensregion_2$vreg_2=="1")
sum(df_2$vreg_3=="alle")==sum(befolkning_verdensregion_2$vreg_2==1)
#ingen avvik.
#anna informasjon som må oppdateres
df_2$tabell_navn="befolkning_verdensregion_3"
write.csv(df_2,"D:/R/imdikator-munch/data_flat_output/befolkning_verdensregion_3-bydel-2015.csv",row.names=F)

#BEFOLKNING_VERDENSREGION_3 - kommune
befolkning_verdensregion_2 <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_verdensregion_2K.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=";", dec=",")
befolkning_verdensregion_2 = subset(befolkning_verdensregion_2,select=-8)
befolkning_verdensregion_2$kommune_id[nchar(befolkning_verdensregion_2$kommune_id)==3] = paste0("0",befolkning_verdensregion_2$kommune_id[nchar(befolkning_verdensregion_2$kommune_id)==3])
names(befolkning_verdensregion_2)[3] = "kommune_nr"
library(tidyr)
df=spread(befolkning_verdensregion_2,vreg_2,tabellvariabel)
df$alle=extract_numeric(df[,6])+extract_numeric(df[,7])
#er det innført NA?
sum(is.na(df$alle))
df_2=gather(df,vreg_3,tabellvariabel,6:8,na.rm=F)
#er ny df utvidet med riktig nrow?
nrow(df_2)==nrow(befolkning_verdensregion_2)+nrow(df)-sum(is.na(df$alle))
nrow(df_2)==nrow(befolkning_verdensregion_2)+sum(befolkning_verdensregion_2$vreg_2=="1")
sum(df_2$vreg_3=="alle")==sum(befolkning_verdensregion_2$vreg_2==1)
#anna informasjon som må oppdateres
df_2$tabell_navn="befolkning_verdensregion_3"
write.csv(df_2,"D:/R/imdikator-munch/data_flat_output/befolkning_verdensregion_3-kommune-2015.csv",row.names=F)

#BEFOLKNING_VERDENSREGION_3 - naringsregion
befolkning_verdensregion_2 <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_verdensregion_2N.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=";", dec=",")
befolkning_verdensregion_2 = subset(befolkning_verdensregion_2,select=-8)
befolkning_verdensregion_2$naringsregion_id[nchar(befolkning_verdensregion_2$naringsregion_id)==1] = paste0("0",befolkning_verdensregion_2$naringsregion_id[nchar(befolkning_verdensregion_2$naringsregion_id)==1])
names(befolkning_verdensregion_2)[3] = "naringsregion_nr"
library(tidyr)
df=spread(befolkning_verdensregion_2,vreg_2,tabellvariabel)
df$alle=extract_numeric(df[,6])+extract_numeric(df[,7])
#er det innført NA?
sum(is.na(df$alle))
df_2=gather(df,vreg_3,tabellvariabel,6:8,na.rm=F)
#er ny df utvidet med riktig nrow?
nrow(df_2)==nrow(befolkning_verdensregion_2)+nrow(df)-sum(is.na(df$alle))
nrow(df_2)==nrow(befolkning_verdensregion_2)+sum(befolkning_verdensregion_2$vreg_2=="1")
sum(df_2$vreg_3=="alle")==sum(befolkning_verdensregion_2$vreg_2==1)
#anna informasjon som må oppdateres
df_2$tabell_navn="befolkning_verdensregion_3"
write.csv(df_2,"D:/R/imdikator-munch/data_flat_output/befolkning_verdensregion_3-naringsregion-2015.csv",row.names=F)

#BEFOLKNING_VERDENSREGION_3 - fylke
befolkning_verdensregion_2 <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_verdensregion_2F.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=";", dec=",")
befolkning_verdensregion_2_norge <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_verdensregion_2L.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=";", dec=",")
befolkning_verdensregion_2 = rbind (befolkning_verdensregion_2,befolkning_verdensregion_2_norge)
befolkning_verdensregion_2 = subset(befolkning_verdensregion_2,select=-8)
befolkning_verdensregion_2$fylke_id[nchar(befolkning_verdensregion_2$fylke_id)==1] = paste0("0",befolkning_verdensregion_2$fylke_id[nchar(befolkning_verdensregion_2$fylke_id)==1])
names(befolkning_verdensregion_2)[3] = "fylke_nr"
library(tidyr)
df=spread(befolkning_verdensregion_2,vreg_2,tabellvariabel)
df$alle=extract_numeric(df[,6])+extract_numeric(df[,7])
#er det innført NA?
sum(is.na(df$alle))
df_2=gather(df,vreg_3,tabellvariabel,6:8,na.rm=F)
#er ny df utvidet med riktig nrow?
nrow(df_2)==nrow(befolkning_verdensregion_2)+nrow(df)-sum(is.na(df$alle))
nrow(df_2)==nrow(befolkning_verdensregion_2)+sum(befolkning_verdensregion_2$vreg_2=="1")
sum(df_2$vreg_3=="alle")==sum(befolkning_verdensregion_2$vreg_2==1)
#anna informasjon som må oppdateres
df_2$tabell_navn="befolkning_verdensregion_3"
write.csv(df_2,"D:/R/imdikator-munch/data_flat_output/befolkning_verdensregion_3-fylke-2015.csv",row.names=F)

#BEFOLKNING_VERDENSREGION_6 - Bydel
befolkning_verdensregion_5 <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_verdensregion_5B.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=";", dec=",",colClasses="character")
befolkning_verdensregion_5 = subset(befolkning_verdensregion_5,select=c(-8,-kjonn))
befolkning_verdensregion_5$bydel_id[nchar(befolkning_verdensregion_5$bydel_id)==2] = paste0("0301",befolkning_verdensregion_5$bydel_id[nchar(befolkning_verdensregion_5$bydel_id)==2])
names(befolkning_verdensregion_5)[3] = "bydel_nr"
library(tidyr)
df=spread(befolkning_verdensregion_5,vreg_5,tabellvariabel)
df$alle=extract_numeric(df[,5])+extract_numeric(df[,6])+extract_numeric(df[,7])+extract_numeric(df[,8])+extract_numeric(df[,9])
#er det innført NA?
sum(is.na(df$alle))
df_2=gather(df,vreg_6,tabellvariabel,5:10,na.rm=F)
#er ny df utvidet med riktig nrow?
nrow(df_2)==nrow(befolkning_verdensregion_5)+nrow(df)-sum(is.na(df$alle))
nrow(df_2)==nrow(befolkning_verdensregion_5)+sum(befolkning_verdensregion_5$vreg_5=="afrika")
sum(df_2$vreg_6=="alle")==sum(befolkning_verdensregion_5$vreg_5=="afrika")
#ingen avvik.
#anna informasjon som må oppdateres
df_2$tabell_navn="befolkning_verdensregion_6"
write.csv(df_2,"D:/R/imdikator-munch/data_flat_output/befolkning_verdensregion_6-bydel-2015.csv",row.names=F)

#BEFOLKNING_VERDENSREGION_6 - kommune
befolkning_verdensregion_5 <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_verdensregion_5K.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=";", dec=",",colClasses="character")
befolkning_verdensregion_5 = subset(befolkning_verdensregion_5,select=c(-8,-kjonn))
names(befolkning_verdensregion_5)[3] = "kommune_nr"
library(tidyr)
df=spread(befolkning_verdensregion_5,vreg_5,tabellvariabel)
df$alle=extract_numeric(df[,5])+extract_numeric(df[,6])+extract_numeric(df[,7])+extract_numeric(df[,8])+extract_numeric(df[,9])
#er det innført NA?
sum(is.na(df$alle))
df_2=gather(df,vreg_6,tabellvariabel,5:10,na.rm=F)
#er ny df utvidet med riktig nrow?
nrow(df_2)==nrow(befolkning_verdensregion_5)+nrow(df)-sum(is.na(df$alle))
nrow(df_2)==nrow(befolkning_verdensregion_5)+sum(befolkning_verdensregion_5$vreg_5=="afrika")
sum(df_2$vreg_6=="alle")==sum(befolkning_verdensregion_5$vreg_5=="afrika")
#ingen avvik.
#anna informasjon som må oppdateres
df_2$tabell_navn="befolkning_verdensregion_6"
write.csv(df_2,"D:/R/imdikator-munch/data_flat_output/befolkning_verdensregion_6-kommune-2015.csv",row.names=F)

#BEFOLKNING_VERDENSREGION_6 - naringsregion
befolkning_verdensregion_5 <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_verdensregion_5N.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=";", dec=",",colClasses="character")
befolkning_verdensregion_5 = subset(befolkning_verdensregion_5,select=c(-8,-kjonn))
names(befolkning_verdensregion_5)[3] = "naringsregion_nr"
library(tidyr)
df=spread(befolkning_verdensregion_5,vreg_5,tabellvariabel)
df$alle=extract_numeric(df[,5])+extract_numeric(df[,6])+extract_numeric(df[,7])+extract_numeric(df[,8])+extract_numeric(df[,9])
#er det innført NA?
sum(is.na(df$alle))
df_2=gather(df,vreg_6,tabellvariabel,5:10,na.rm=F)
#er ny df utvidet med riktig nrow?
nrow(df_2)==nrow(befolkning_verdensregion_5)+nrow(df)-sum(is.na(df$alle))
nrow(df_2)==nrow(befolkning_verdensregion_5)+sum(befolkning_verdensregion_5$vreg_5=="afrika")
sum(df_2$vreg_6=="alle")==sum(befolkning_verdensregion_5$vreg_5=="afrika")
#ingen avvik.
#anna informasjon som må oppdateres
df_2$tabell_navn="befolkning_verdensregion_6"
write.csv(df_2,"D:/R/imdikator-munch/data_flat_output/befolkning_verdensregion_6-naringsregion-2015.csv",row.names=F)

#BEFOLKNING_VERDENSREGION_6 - fylke + landet
befolkning_verdensregion_5 <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_verdensregion_5F.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=";", dec=",",colClasses="character")
befolkning_verdensregion_5_norge <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_verdensregion_5L.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=";", dec=",",colClasses="character")
befolkning_verdensregion_5=rbind(befolkning_verdensregion_5,befolkning_verdensregion_5_norge)
befolkning_verdensregion_5 = subset(befolkning_verdensregion_5,select=c(-8,-kjonn))
names(befolkning_verdensregion_5)[3] = "fylke_nr"
library(tidyr)
df=spread(befolkning_verdensregion_5,vreg_5,tabellvariabel)
df$alle=extract_numeric(df[,5])+extract_numeric(df[,6])+extract_numeric(df[,7])+extract_numeric(df[,8])+extract_numeric(df[,9])
#er det innført NA?
sum(is.na(df$alle))
df_2=gather(df,vreg_6,tabellvariabel,5:10,na.rm=F)
#er ny df utvidet med riktig nrow?
nrow(df_2)==nrow(befolkning_verdensregion_5)+nrow(df)-sum(is.na(df$alle))
nrow(df_2)==nrow(befolkning_verdensregion_5)+sum(befolkning_verdensregion_5$vreg_5=="afrika")
sum(df_2$vreg_6=="alle")==sum(befolkning_verdensregion_5$vreg_5=="afrika")
#ingen avvik.
#anna informasjon som må oppdateres
df_2$tabell_navn="befolkning_verdensregion_6"
write.csv(df_2,"D:/R/imdikator-munch/data_flat_output/befolkning_verdensregion_6-fylke-2015.csv",row.names=F)

#BEFOLKNING-VERDENSREGION-9 - bydel
#6. desember
befolkning_verdensregion_9 <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_verdensregion_9B.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=";", dec=",",colClasses="character")
befolkning_verdensregion_9 = subset(befolkning_verdensregion_9,select=c(-8,-kjonn))
befolkning_verdensregion_9$bydel_id[nchar(befolkning_verdensregion_9$bydel_id)==2] = paste0("0301",befolkning_verdensregion_9$bydel_id[nchar(befolkning_verdensregion_9$bydel_id)==2])
names(befolkning_verdensregion_9)[3] = "bydel_nr"
befolkning_verdensregion_9$tabell_navn="befolkning_verdensregion_9"
write.csv(befolkning_verdensregion_9,"D:/R/imdikator-munch/data_flat_output/befolkning_verdensregion_9-bydel-2015.csv",row.names=F)

#BEFOLKNING-VERDENSREGION-9 - kommune
#6. desember
befolkning_verdensregion_9 <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_verdensregion_9K.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=";", dec=",",colClasses="character")
befolkning_verdensregion_9 = subset(befolkning_verdensregion_9,select=c(-8,-kjonn))
names(befolkning_verdensregion_9)[3] = "kommune_nr"
befolkning_verdensregion_9$tabell_navn="befolkning_verdensregion_9"
write.csv(befolkning_verdensregion_9,"D:/R/imdikator-munch/data_flat_output/befolkning_verdensregion_9-kommune-2015.csv",row.names=F)

#BEFOLKNING-VERDENSREGION-9 - naringsregion
#6. desember
befolkning_verdensregion_9 <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_verdensregion_9N.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=";", dec=",",colClasses="character")
befolkning_verdensregion_9 = subset(befolkning_verdensregion_9,select=c(-8,-kjonn))
names(befolkning_verdensregion_9)[3] = "naringsregion_nr"
befolkning_verdensregion_9$tabell_navn="befolkning_verdensregion_9"
write.csv(befolkning_verdensregion_9,"D:/R/imdikator-munch/data_flat_output/befolkning_verdensregion_9-naringsregion-2015.csv",row.names=F)

#BEFOLKNING-VERDENSREGION-9 - fylke
#6. desember
befolkning_verdensregion_9 <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_verdensregion_9F.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=";", dec=",",colClasses="character")
befolkning_verdensregion_9_norge <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_verdensregion_9L.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=";", dec=",",colClasses="character")
befolkning_verdensregion_9 = rbind(befolkning_verdensregion_9,befolkning_verdensregion_9_norge)
befolkning_verdensregion_9 = subset(befolkning_verdensregion_9,select=c(-8,-kjonn))
names(befolkning_verdensregion_9)[3] = "fylke_nr"
befolkning_verdensregion_9$tabell_navn="befolkning_verdensregion_9"
write.csv(befolkning_verdensregion_9,"D:/R/imdikator-munch/data_flat_output/befolkning_verdensregion_9-fylke-2015.csv",row.names=F)

#Befolkning_botid - bydel
#7. desember 2015
befolkning_botid <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_botid_B_2015.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=";", dec=",",colClasses="character")
# bydel_id må bli bydel_nr
# bydel_id = " " må ut
# botid_5 == " " må bli "alle"
# person må bli personer
# bydel_nr må få 0301 foran.
# tabellnavn skrevet med stor forbokstav
names(befolkning_botid)[3] = "bydel_nr"
befolkning_botid = subset(befolkning_botid,bydel_nr!=" ")
befolkning_botid$bydel_nr[nchar(befolkning_botid$bydel_nr)==2] = paste0("0301",befolkning_botid$bydel_nr[nchar(befolkning_botid$bydel_nr)==2])
befolkning_botid$botid_5[befolkning_botid$botid_5==" "]="alle"
befolkning_botid$tabell_navn="befolkning_botid"
befolkning_botid$enhet = "personer"
#logisk sjekk av antall: 18 bydeler x 3 vreg x 4 botid x 1 enhet
nrow(befolkning_botid)==18*3*4
#viser seg at det mangler data for botid_5==alle for vreg_3==1 og ==2
df=spread(befolkning_botid,botid_5,tabellvariabel,drop=F)
df$alle_kopi=df$alle
df$alle=extract_numeric(df[,6])+extract_numeric(df[,7])+extract_numeric(df[,8])
df = subset(df,select=-alle_kopi)
sum(is.na(df$alle))
befolkning_botid=gather(df,botid_5,tabellvariabel,6:9,na.rm=F)
write.csv(befolkning_botid,"D:/R/imdikator-munch/data_flat_output/befolkning_botid-bydel-2015.csv",row.names=F)

#Befolkning_botid - kommune
#7. desember 2015
befolkning_botid <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_botid_K_2015_v2.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=";", dec=",",colClasses="character")
# bydel_id må bli bydel_nr
# bydel_id = " " må ut
# botid_5 == " " må bli "alle"
# person må bli personer
# bydel_nr må få 0301 foran.
# tabellnavn skrevet med stor forbokstav
# enhet må bli personer
# logisk sjekk av innholdet
names(befolkning_botid)[3] = "kommune_nr"
befolkning_botid = subset(befolkning_botid,kommune_nr!="NULL")
befolkning_botid$tabell_navn="befolkning_botid"
befolkning_botid$enhet = "personer"
#mangler alle-kategori for botid
df=spread(befolkning_botid,botid_5,tabellvariabel,drop=F,fill=".")
df$alle=extract_numeric(df[,6])+extract_numeric(df[,7])+extract_numeric(df[,8])
df = subset(df,select=-alle_kopi)
sum(is.na(df$alle))
df$alle[is.na(df$alle)==T]="."
befolkning_botid=gather(df,botid_5,tabellvariabel,6:9,na.rm=F)
#fire botidsnivåer x 3 vreg x 428 kommuner
4*3*428
nlevels(as.factor(befolkning_botid$kommune_nr))
levels(as.factor(befolkning_botid$vreg_3))
write.csv(befolkning_botid,"D:/R/imdikator-munch/data_flat_output/befolkning_botid-kommune-2015.csv",row.names=F)

#Befolkning_botid - naringsregion
#7. desember 2015
befolkning_botid <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_botid_N_2015.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=";", dec=",",colClasses="character")
# naringsregion_id = "NULL" må ut
# tabellnavn skrevet med stor forbokstav
# person må bli personer
# logisk sjekk av innholdet
# botid_5 == " " må bli "alle"
# vreg_3 == " " må bli alle
# enhet må bli personer

befolkning_botid = subset(befolkning_botid,naringsregion_nr!="NULL")
befolkning_botid$tabell_navn="befolkning_botid"
befolkning_botid$enhet = "personer"
#4 botidsnivåer x 3 vregnivåer x 83 nareg
4*3*83
levels(as.factor(befolkning_botid$vreg_3))
befolkning_botid$vreg_3[befolkning_botid$vreg_3==" "]="alle"
levels(as.factor(befolkning_botid$botid_5))
#mangler alle-kategori for botid for vreg==1 og ==2.
df=spread(befolkning_botid,botid_5,tabellvariabel,drop=F)
df$alle_kopi=df$alle
df$alle=extract_numeric(df[,6])+extract_numeric(df[,7])+extract_numeric(df[,8])
df = subset(df,select=-alle_kopi)
sum(is.na(df$alle))
befolkning_botid=gather(df,botid_5,tabellvariabel,6:9,na.rm=F)
#fire botidsnivåer x 3 vreg x 83 naringsregionr
4*3*83
nlevels(as.factor(befolkning_botid$naringsregion_nr))
write.csv(befolkning_botid,"D:/R/imdikator-munch/data_flat_output/befolkning_botid-naringsregion-2015.csv",row.names=F)

#Befolkning_botid - fylke
#7. desember 2015
befolkning_botid <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_botid_F_2015.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=";", dec=",",colClasses="character")
# tabellnavn skrevet med stor forbokstav
# person må bli personer
# logisk sjekk av innholdet
# botid_5 == " " må bli "alle"
# vreg_3 == " " må bli alle
# enhet må bli personer
befolkning_botid$tabell_navn="befolkning_botid"
befolkning_botid$enhet = "personer"
#4 botidsnivåer x 3 vregnivåer x nlevels(as.factor(befolkning_botid$fylke_nr))
4*3*20
levels(as.factor(befolkning_botid$vreg_3))
levels(as.factor(befolkning_botid$botid_5))
#mangler vreg_3==alle data for botidskategorier
df=spread(befolkning_botid,vreg_3,tabellvariabel,drop=F)
df$alle_kopi=df$alle
df$alle=extract_numeric(df[,6])+extract_numeric(df[,7])
df = subset(df,select=-alle_kopi)
sum(is.na(df$alle))
df2=gather(df,vreg_3,tabellvariabel,6:8,na.rm=F)
#mangler alle-kategori for botid for vreg==1 og ==2.
df=spread(df2,botid_5,tabellvariabel,drop=F)
df$alle=extract_numeric(df[,6])+extract_numeric(df[,7])+extract_numeric(df[,8])
sum(is.na(df$alle))
befolkning_botid=gather(df,botid_5,tabellvariabel,6:9,na.rm=F)
#fire botidsnivåer x 3 vreg x 83 fylker
4*3*20
write.csv(befolkning_botid,"D:/R/imdikator-munch/data_flat_output/befolkning_botid-fylke-2015.csv",row.names=F)

#BEFOLKNING_FLYTTING
#befolkning_flytting - bydel
#9. desember 2015
library(tidyr)
befolkning_flytting <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_flytting_B_2014.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
# bydel_id må bli bydel_nr
# bydel_nr må få 0301 foran.
# tabellnavn må bli tabell_navn
# tabell må splittes i to: en med vreg og en uten vreg
# invkat2 må bli til invkat_3, dvs. befolkningen_forøvrig = alle-innvandrere
# invkat_3 må bli innvkat_2
# logisk sjekk av antall observasjoner etter det
names(befolkning_flytting)[1] = "tabell_navn"
names(befolkning_flytting)[3] = "bydel_nr"
befolkning_flytting$bydel_nr[nchar(befolkning_flytting$bydel_nr)==2] = paste0("0301",befolkning_flytting$bydel_nr[nchar(befolkning_flytting$bydel_nr)==2])

#befolkning_flytting_vreg
befolkning_flytting_vreg = subset(befolkning_flytting,invkat_2 =='innvandrere',select=-invkat_2)
befolkning_flytting_vreg$tabell_navn ="befolkning_flytting_vreg"
#logisksjekk: 486 = 3*nlevels(as.factor(befolkning_flytting_vreg$bydel_nr))*nlevels(as.factor(befolkning_flytting_vreg$flytting))
nrow(befolkning_flytting_vreg)==3*nlevels(as.factor(befolkning_flytting_vreg$bydel_nr))*nlevels(as.factor(befolkning_flytting_vreg$flytting))
write.csv(befolkning_flytting_vreg,"D:/R/imdikator-munch/data_flat_output/befolkning_flytting_vreg-bydel-2014.csv",row.names=F)

#befolkning_flytting
befolkning_flytting=subset(befolkning_flytting,vreg_3 =='alle',select=-vreg_3)
df=spread(befolkning_flytting,invkat_2,tabellvariabel,drop=F)
sum(is.na(df$alle))
sum(is.na(df$innvandrere))
df$befolkningen_ellers=extract_numeric(df[,6])-extract_numeric(df[,7])
df = subset(df,select=-alle_kopi)
sum(is.na(df$befolkningen_ellers))
befolkning_flytting=gather(df,innvkat_3,tabellvariabel,6:8,na.rm=F)
nrow(befolkning_flytting)==9*3*18
write.csv(befolkning_flytting,"D:/R/imdikator-munch/data_flat_output/befolkning_flytting-bydel-2014.csv",row.names=F)

#kommune
befolkning_flytting <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_flytting_K_2014.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
# tilflyttingskommune må bli kommune_nr
# tabellnavn må bli tabell_navn
# tabell må splittes i to: en med vreg og en uten vreg
# invkat2 må bli til invkat_3, dvs. befolkningen_forøvrig = alle-innvandrere
# invkat_3 må bli innvkat_2
# logisk sjekk av antall observasjoner etter det
names(befolkning_flytting)[1] = "tabell_navn"
names(befolkning_flytting)[3] = "kommune_nr"
levels(as.factor(befolkning_flytting$flytting))

#befolkning_flytting_vreg
befolkning_flytting_vreg = subset(befolkning_flytting,invkat_2 =='innvandrere',select=-invkat_2)
befolkning_flytting_vreg$tabell_navn ="befolkning_flytting_vreg"
#logisksjekk: 
nrow(befolkning_flytting_vreg)==3*nlevels(as.factor(befolkning_flytting_vreg$kommune_nr))*nlevels(as.factor(befolkning_flytting_vreg$flytting))
write.csv(befolkning_flytting_vreg,"D:/R/imdikator-munch/data_flat_output/befolkning_flytting_vreg-kommune-2014.csv",row.names=F)

#befolkning_flytting
befolkning_flytting=subset(befolkning_flytting,vreg_3 =='alle',select=-vreg_3)
df=spread(befolkning_flytting,invkat_2,tabellvariabel,drop=F)
sum(is.na(df$alle))
sum(is.na(df$innvandrere))
df$befolkningen_ellers=extract_numeric(df[,6])-extract_numeric(df[,7])
sum(is.na(df$befolkningen_ellers))
befolkning_flytting=gather(df,innvkat_3,tabellvariabel,6:8,na.rm=F)
nrow(befolkning_flytting)==9*3*428
write.csv(befolkning_flytting,"D:/R/imdikator-munch/data_flat_output/befolkning_flytting-kommune-2014.csv",row.names=F)

#naringsregion
befolkning_flytting <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_flytting_N_2014.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
# tilflyttingsnaringsregion må bli naringsregion_nr
# tabellnavn må bli tabell_navn
# tabell må splittes i to: en med vreg og en uten vreg
# invkat2 må bli til invkat_3, dvs. befolkningen_forøvrig = alle-innvandrere
# invkat_3 må bli innvkat_2
# logisk sjekk av antall observasjoner etter det
names(befolkning_flytting)[1] = "tabell_navn"
names(befolkning_flytting)[3] = "naringsregion_nr"
levels(as.factor(befolkning_flytting$flytting))

#befolkning_flytting_vreg
befolkning_flytting_vreg = subset(befolkning_flytting,invkat_2 =='innvandrere',select=-invkat_2)
befolkning_flytting_vreg$tabell_navn ="befolkning_flytting_vreg"
#logisksjekk: 
nrow(befolkning_flytting_vreg)==3*83*9
write.csv(befolkning_flytting_vreg,"D:/R/imdikator-munch/data_flat_output/befolkning_flytting_vreg-naringsregion-2014.csv",row.names=F)

#befolkning_flytting
befolkning_flytting=subset(befolkning_flytting,vreg_3 =='alle',select=-vreg_3)
df=spread(befolkning_flytting,invkat_2,tabellvariabel,drop=F)
sum(is.na(df$alle))
sum(is.na(df$innvandrere))
df$befolkningen_ellers=extract_numeric(df[,6])-extract_numeric(df[,7])
sum(is.na(df$befolkningen_ellers))
befolkning_flytting=gather(df,innvkat_3,tabellvariabel,6:8,na.rm=F)
nrow(befolkning_flytting)==9*3*83
write.csv(befolkning_flytting,"D:/R/imdikator-munch/data_flat_output/befolkning_flytting-naringsregion-2014.csv",row.names=F)

#fylke
befolkning_flytting <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_flytting_FL_2014.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
# fylke_id må bli fylke_nr
# tabellnavn må bli tabell_navn
# fylke = 0 må bli 00
# tabell må splittes i to: en med vreg og en uten vreg
# invkat2 må bli til invkat_3, dvs. befolkningen_forøvrig = alle-innvandrere
# invkat_3 må bli innvkat_2
# logisk sjekk av antall observasjoner etter det
names(befolkning_flytting)[1] = "tabell_navn"
names(befolkning_flytting)[3] = "fylke_nr"
befolkning_flytting$fylke_nr[nchar(befolkning_flytting$fylke_nr)==1] = paste0("0",befolkning_flytting$fylke_nr[nchar(befolkning_flytting$fylke_nr)==1])
levels(as.factor(befolkning_flytting$flytting))

#befolkning_flytting_vreg
befolkning_flytting_vreg = subset(befolkning_flytting,invkat_2 =='innvandrere',select=-invkat_2)
befolkning_flytting_vreg$tabell_navn ="befolkning_flytting_vreg"
#logisksjekk: 
nrow(befolkning_flytting_vreg)==3*20*9
write.csv(befolkning_flytting_vreg,"D:/R/imdikator-munch/data_flat_output/befolkning_flytting_vreg-fylke-2014.csv",row.names=F)

#befolkning_flytting
befolkning_flytting=subset(befolkning_flytting,vreg_3 =='alle',select=-vreg_3)
df=spread(befolkning_flytting,invkat_2,tabellvariabel,drop=F)
sum(is.na(df$alle))
sum(is.na(df$innvandrere))
df$befolkningen_ellers=extract_numeric(df[,6])-extract_numeric(df[,7])
sum(is.na(df$befolkningen_ellers))
befolkning_flytting=gather(df,innvkat_3,tabellvariabel,6:8,na.rm=F)
nrow(befolkning_flytting)==9*3*20
write.csv(befolkning_flytting,"D:/R/imdikator-munch/data_flat_output/befolkning_flytting-fylke-2014.csv",row.names=F)

#FLYKTNING_BOTID_FLYTTING
flyktning_botid_flytting <- read.csv("D:/R/imdikator-munch/data_flat_input/flyktning_botid_flytting_B_2015.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=";", dec=",",colClasses="character")
# bydel_id må bli bydel_nr
# fjerne ekstra kolonne
# kode om 0-4 til 0_4
# bydeler må få 0301 foran
# logisk sjekk av antall observasjoner etter det
names(flyktning_botid_flytting)[3] = "bydel_nr"
flyktning_botid_flytting$bydel_nr[nchar(flyktning_botid_flytting$bydel_nr)==2] = paste0("0301",flyktning_botid_flytting$bydel_nr[nchar(flyktning_botid_flytting$bydel_nr)==2])
flyktning_botid_flytting$enhet[flyktning_botid_flytting$enhet=="person"]="personer"
flyktning_botid_flytting = subset(flyktning_botid_flytting,select=-X)
levels(as.factor(flyktning_botid_flytting$botid_3))
levels(as.factor(flyktning_botid_flytting$forste_bosettingskommune))
flyktning_botid_flytting$botid_3 = gsub("\\-","_",flyktning_botid_flytting$botid_3)
flyktning_botid_flytting$tabellvariabel = gsub("\\,",".",flyktning_botid_flytting$tabellvariabel)
3*4*18*2==nrow(flyktning_botid_flytting)
df=spread(flyktning_botid_flytting,botid_3,tabellvariabel,drop=F)
df$alle_kopi=df$alle
#problem med komma som desimaltegn, må gsubbes til punktum lenger opp
df$alle=extract_numeric(df[,6])+extract_numeric(df[,7])
df$alle=as.numeric(df[,6])+as.numeric(df[,7])
df = subset(df,select=-alle_kopi)
sum(is.na(df$alle))
df_2=gather(df,botid_3,tabellvariabel,6:8,na.rm=F)
write.csv(df_2,"D:/R/imdikator-munch/data_flat_output/flyktning_botid_flytting-bydel-2015.csv",row.names=F)

#FLYKTNING_BOTID_FLYTTING
#KOMMUNE
flyktning_botid_flytting <- read.csv("D:/R/imdikator-munch/data_flat_input/flyktning_botid_flytting_K_2015.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=";", dec=",",colClasses="character")
# kommune_id må bli kommune_nr
# fjerne ekstra kolonne
# kode om 0-4 til 0_4
#problem med komma som desimaltegn, må gsubbes til punktum lenger opp
# logisk sjekk av antall observasjoner etter det
names(flyktning_botid_flytting)[3] = "kommune_nr"
flyktning_botid_flytting$enhet[flyktning_botid_flytting$enhet=="person"]="personer"
flyktning_botid_flytting = subset(flyktning_botid_flytting,select=-X)
levels(as.factor(flyktning_botid_flytting$botid_3))
levels(as.factor(flyktning_botid_flytting$forste_bosettingskommune))
flyktning_botid_flytting$botid_3 = gsub("\\-","_",flyktning_botid_flytting$botid_3)
flyktning_botid_flytting$tabellvariabel = gsub("\\,",".",flyktning_botid_flytting$tabellvariabel)
nlevels(as.factor(flyktning_botid_flytting$kommune_nr))
3*4*354*2==nrow(flyktning_botid_flytting)
df=spread(flyktning_botid_flytting,botid_3,tabellvariabel,drop=F)
df$alle_kopi=df$alle
df$alle=extract_numeric(df[,6])+extract_numeric(df[,7])
sum(is.na(df$alle))
df = subset(df,select=-alle_kopi)
sum(is.na(df$alle))
df_2=gather(df,botid_3,tabellvariabel,6:8,na.rm=F)
3*4*354*2==nrow(df_2)
write.csv(df_2,"D:/R/imdikator-munch/data_flat_output/flyktning_botid_flytting-kommune-2015.csv",row.names=F)

#FLYKTNING_BOTID_FLYTTING
#NÆRINGSREGION
flyktning_botid_flytting <- read.csv("D:/R/imdikator-munch/data_flat_input/flyktning_botid_flytting_N_2015.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=";", dec=",",colClasses="character")
# naringsregion_id må bli naringsregion_nr
# fjerne ekstra kolonne
# kode om 0-4 til 0_4
#problem med komma som desimaltegn, må gsubbes til punktum lenger opp
# logisk sjekk av antall observasjoner etter det
names(flyktning_botid_flytting)[3] = "naringsregion_nr"
flyktning_botid_flytting$enhet[flyktning_botid_flytting$enhet=="person"]="personer"
flyktning_botid_flytting = subset(flyktning_botid_flytting,select=-X)
levels(as.factor(flyktning_botid_flytting$botid_3))
levels(as.factor(flyktning_botid_flytting$forste_bosettingskommune))
flyktning_botid_flytting$botid_3 = gsub("\\-","_",flyktning_botid_flytting$botid_3)
flyktning_botid_flytting$tabellvariabel = gsub("\\,",".",flyktning_botid_flytting$tabellvariabel)
nlevels(as.factor(flyktning_botid_flytting$naringsregion_nr))
3*4*83*2==nrow(flyktning_botid_flytting)
df=spread(flyktning_botid_flytting,botid_3,tabellvariabel,drop=F)
df$alle_kopi=df$alle
df$alle=extract_numeric(df[,6])+extract_numeric(df[,7])
sum(is.na(df$alle))
df = subset(df,select=-alle_kopi)
sum(is.na(df$alle))
df_2=gather(df,botid_3,tabellvariabel,6:8,na.rm=F)
3*4*83*2==nrow(df_2)
write.csv(df_2,"D:/R/imdikator-munch/data_flat_output/flyktning_botid_flytting-naringsregion-2015.csv",row.names=F)

#FLYKTNING_BOTID_FLYTTING
#FYLKE
flyktning_botid_flytting <- read.csv("D:/R/imdikator-munch/data_flat_input/flyktning_botid_flytting_F_2015.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=";", dec=",",colClasses="character")
flyktning_botid_flytting_norge <- read.csv("D:/R/imdikator-munch/data_flat_input/flyktning_botid_flytting_L_2015.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=";", dec=",",colClasses="character")
flyktning_botid_flytting = rbind (flyktning_botid_flytting,flyktning_botid_flytting_norge)
# fylke_id må bli fylke_nr
# fjerne ekstra kolonne
# kode om 0-4 til 0_4
#problem med komma som desimaltegn, må gsubbes til punktum lenger opp
# logisk sjekk av antall observasjoner etter det
names(flyktning_botid_flytting)[3] = "fylke_nr"
flyktning_botid_flytting$enhet[flyktning_botid_flytting$enhet=="person"]="personer"
flyktning_botid_flytting = subset(flyktning_botid_flytting,select=-X)
levels(as.factor(flyktning_botid_flytting$botid_3))
levels(as.factor(flyktning_botid_flytting$forste_bosettingskommune))
flyktning_botid_flytting$botid_3 = gsub("\\-","_",flyktning_botid_flytting$botid_3)
flyktning_botid_flytting$tabellvariabel = gsub("\\,",".",flyktning_botid_flytting$tabellvariabel)
nlevels(as.factor(flyktning_botid_flytting$fylke_nr))
3*4*20*2==nrow(flyktning_botid_flytting)
df=spread(flyktning_botid_flytting,botid_3,tabellvariabel,drop=F)
df$alle_kopi=df$alle
df$alle=extract_numeric(df[,6])+extract_numeric(df[,7])
sum(is.na(df$alle))
df = subset(df,select=-alle_kopi)
sum(is.na(df$alle))
df_2=gather(df,botid_3,tabellvariabel,6:8,na.rm=F)
3*4*20*2==nrow(df_2)
write.csv(df_2,"D:/R/imdikator-munch/data_flat_output/flyktning_botid_flytting-fylke-2015.csv",row.names=F)

#SOSIALHJELP
#10. desember
sosialhjelp <- read.csv("D:/R/imdikator-munch/data_flat_input/sosialhjelp_2013-2014.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
nlevels(as.factor(sosialhjelp$bydel_nr))-1
nlevels(as.factor(sosialhjelp$kommune_nr))-1
nlevels(as.factor(sosialhjelp$naringsregion_nr))-1
nlevels(as.factor(sosialhjelp$fylke_nr))-1
#fylke 0 == 00
sosialhjelp$fylke_nr[sosialhjelp$fylke_nr=="0"]="00"
sosialhjelp$enhet[sosialhjelp$enhet=="person"]="personer"
#logisk sjekk: 
nrow(sosialhjelp)==(18*3*3*2*2)+(428*3*3*2*2)+(83*3*3*2*2)+(20*3*3*2*2)
write.csv(sosialhjelp,"D:/R/imdikator-munch/data_flat_output/sosialhjelp-alle-2013_2014.csv",row.names=F)

#GRUNNSKOLEPOENG
#10. desember
#bydel - 2014
# aar er feil - må kodes som siste året i intervallet, avgangsåret.
# person må bli personer
# logiske sjekker
grunnskolepoeng <- read.csv("D:/R/imdikator-munch/data_flat_input/grunnskolepoeng_bydel_2014_ny.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=";", dec=",",colClasses="character")
grunnskolepoeng$aar[grunnskolepoeng$aar=="2013-2014"]="2014"
grunnskolepoeng$enhet[grunnskolepoeng$enhet=="person"]="personer"
#logisk: 18 bydeler, 3 kjønn, 5 innvkat, 4 vreg, 4 invalder
18*3*5*4*4
library(tidyr)
df=spread(grunnskolepoeng,innvkat_5,tabellvariabel,drop=F)
#noe må gjøres med denne, kanskje tilsvarende som for flyktninger: skjære ut innvandrere som vi har mer informasjon om til egen tabell
#kun hvis deler er lopsided, blir ulike headergroups? hvis ikke...

nlevels(as.factor(grunnskolepoeng$bydel_nr))
df_grunn = subset(grunnskolepoeng,subset=c((vreg_3=="alle"|vreg_3=="NULL")&(invalder_3=="alle"|invalder_3=="NULL")))
grunnskolepoeng_innvkat5 = subset(df_grunn,select=-c(vreg_3,invalder_3))
#1: grunnsettet - 17 bydeler * 3 kjønn * 5innvkat * 2 enheter
nrow(grunnskolepoeng_innvkat5)==17*3*5*2
write.csv(grunnskolepoeng_innvkat5,"D:/R/imdikator-munch/data_flat_output/grunnskolepoeng-bydel-2014.csv",row.names=F)

#setter opp en ny tabell med tabell_navn grunnskolepoeng_innvandrere
df_innv = subset(grunnskolepoeng,subset=c(vreg_3!="NULL"|invalder_3!="NULL"))
grunnskolepoeng_innvandrere = subset(df_innv,select=-innvkat_5)
nrow(grunnskolepoeng_innvandrere)==17*3*1*3*3*2
grunnskolepoeng_innvandrere$tabell_navn="grunnskolepoeng_innvandrere"
write.csv(grunnskolepoeng_innvandrere,"D:/R/imdikator-munch/data_flat_output/grunnskolepoeng_innvandrere-bydel-2014.csv",row.names=F)

#alle observasjoner er her, men hva skjer ved innlesning? Får vi headergroups som kan brukes?
#svaret er nei: innleseren leser headergroups ut fra hva som er gyldig og hva som har null
#men det gir en headergroup for innvandrere og en for de andre.

#bydel - 2015
# aar er feil - må kodes som siste året i intervallet, avgangsåret.
# person må bli personer
# logiske sjekker
grunnskolepoeng <- read.csv("D:/R/imdikator-munch/data_flat_input/grunnskolepoeng_bydel_2015_ny.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=";", dec=",",colClasses="character")
grunnskolepoeng$aar[grunnskolepoeng$aar=="2014-2015"]="2015"
grunnskolepoeng$enhet[grunnskolepoeng$enhet=="person"]="personer"
nlevels(as.factor(grunnskolepoeng$bydel_nr))
df_grunn = subset(grunnskolepoeng,subset=c((vreg_3=="alle"|vreg_3=="NULL")&(invalder_3=="alle"|invalder_3=="NULL")))
grunnskolepoeng_innvkat5 = subset(df_grunn,select=-c(vreg_3,invalder_3))
nrow(grunnskolepoeng_innvkat5)==17*3*5*2
write.csv(grunnskolepoeng_innvkat5,"D:/R/imdikator-munch/data_flat_output/grunnskolepoeng-bydel-2015.csv",row.names=F)

df_innv = subset(grunnskolepoeng,subset=c(vreg_3!="NULL"|invalder_3!="NULL"))
grunnskolepoeng_innvandrere = subset(df_innv,select=-innvkat_5)
nrow(grunnskolepoeng_innvandrere)==17*3*1*3*3*2
grunnskolepoeng_innvandrere$tabell_navn="grunnskolepoeng_innvandrere"
write.csv(grunnskolepoeng_innvandrere,"D:/R/imdikator-munch/data_flat_output/grunnskolepoeng_innvandrere-bydel-2015.csv",row.names=F)

#kommune - 2014
# aar er feil - må kodes som siste året i intervallet, avgangsåret.
# person må bli personer
# logiske sjekker
grunnskolepoeng <- read.csv("D:/R/imdikator-munch/data_flat_input/grunnskolepoeng_kommune_2014_ny.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=";", dec=",",colClasses="character")
grunnskolepoeng$aar[grunnskolepoeng$aar=="2013-2014"]="2014"
grunnskolepoeng$enhet[grunnskolepoeng$enhet=="person"]="personer"
nlevels(as.factor(grunnskolepoeng$kommune_nr))
df_grunn = subset(grunnskolepoeng,subset=c((vreg_3=="alle"|vreg_3=="NULL")&(invalder_3=="alle"|invalder_3=="NULL")))
grunnskolepoeng_innvkat5 = subset(df_grunn,select=-c(vreg_3,invalder_3))
nrow(grunnskolepoeng_innvkat5)==427*3*5*2
write.csv(grunnskolepoeng_innvkat5,"D:/R/imdikator-munch/data_flat_output/grunnskolepoeng-kommune-2014.csv",row.names=F)

df_innv = subset(grunnskolepoeng,subset=c(vreg_3!="NULL"|invalder_3!="NULL"))
grunnskolepoeng_innvandrere = subset(df_innv,select=-innvkat_5)
nrow(grunnskolepoeng_innvandrere)==427*3*1*3*3*2
grunnskolepoeng_innvandrere$tabell_navn="grunnskolepoeng_innvandrere"
write.csv(grunnskolepoeng_innvandrere,"D:/R/imdikator-munch/data_flat_output/grunnskolepoeng_innvandrere-kommune-2014.csv",row.names=F)

#kommune - 2015
# aar er feil - må kodes som siste året i intervallet, avgangsåret.
# person må bli personer
# logiske sjekker
grunnskolepoeng <- read.csv("D:/R/imdikator-munch/data_flat_input/grunnskolepoeng_kommune_2015_ny.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=";", dec=",",colClasses="character")
grunnskolepoeng$aar[grunnskolepoeng$aar=="2014-2015"]="2015"
grunnskolepoeng$enhet[grunnskolepoeng$enhet=="person"]="personer"
nlevels(as.factor(grunnskolepoeng$kommune_nr))
df_grunn = subset(grunnskolepoeng,subset=c((vreg_3=="alle"|vreg_3=="NULL")&(invalder_3=="alle"|invalder_3=="NULL")))
grunnskolepoeng_innvkat5 = subset(df_grunn,select=-c(vreg_3,invalder_3))
nrow(grunnskolepoeng_innvkat5)==427*3*5*2
write.csv(grunnskolepoeng_innvkat5,"D:/R/imdikator-munch/data_flat_output/grunnskolepoeng-kommune-2015.csv",row.names=F)

df_innv = subset(grunnskolepoeng,subset=c(vreg_3!="NULL"|invalder_3!="NULL"))
grunnskolepoeng_innvandrere = subset(df_innv,select=-innvkat_5)
nrow(grunnskolepoeng_innvandrere)==427*3*1*3*3*2
grunnskolepoeng_innvandrere$tabell_navn="grunnskolepoeng_innvandrere"
write.csv(grunnskolepoeng_innvandrere,"D:/R/imdikator-munch/data_flat_output/grunnskolepoeng_innvandrere-kommune-2015.csv",row.names=F)

#næringsregion - 2014
# aar er feil - må kodes som siste året i intervallet, avgangsåret.
# person må bli personer
# logiske sjekker
grunnskolepoeng <- read.csv("D:/R/imdikator-munch/data_flat_input/grunnskolepoeng_næringsregion_2014_ny.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=";", dec=",",colClasses="character")
grunnskolepoeng$aar[grunnskolepoeng$aar=="2013-2014"]="2014"
grunnskolepoeng$enhet[grunnskolepoeng$enhet=="person"]="personer"
nlevels(as.factor(grunnskolepoeng$naringsregion_nr))
df_grunn = subset(grunnskolepoeng,subset=c((vreg_3=="alle"|vreg_3=="NULL")&(invalder_3=="alle"|invalder_3=="NULL")))
grunnskolepoeng_innvkat5 = subset(df_grunn,select=-c(vreg_3,invalder_3))
nrow(grunnskolepoeng_innvkat5)==84*3*5*2
write.csv(grunnskolepoeng_innvkat5,"D:/R/imdikator-munch/data_flat_output/grunnskolepoeng-naringsregion-2014.csv",row.names=F)

df_innv = subset(grunnskolepoeng,subset=c(vreg_3!="NULL"|invalder_3!="NULL"))
grunnskolepoeng_innvandrere = subset(df_innv,select=-innvkat_5)
nrow(grunnskolepoeng_innvandrere)==84*3*1*3*3*2
grunnskolepoeng_innvandrere$tabell_navn="grunnskolepoeng_innvandrere"
write.csv(grunnskolepoeng_innvandrere,"D:/R/imdikator-munch/data_flat_output/grunnskolepoeng_innvandrere-naringsregion-2014.csv",row.names=F)

#næringsregion - 2015
# aar er feil - må kodes som siste året i intervallet, avgangsåret.
# person må bli personer
# logiske sjekker
grunnskolepoeng <- read.csv("D:/R/imdikator-munch/data_flat_input/grunnskolepoeng_næringsregion_2015_ny.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=";", dec=",",colClasses="character")
grunnskolepoeng$aar[grunnskolepoeng$aar=="2014-2015"]="2015"
grunnskolepoeng$enhet[grunnskolepoeng$enhet=="person"]="personer"
nlevels(as.factor(grunnskolepoeng$naringsregion_nr))
df_grunn = subset(grunnskolepoeng,subset=c((vreg_3=="alle"|vreg_3=="NULL")&(invalder_3=="alle"|invalder_3=="NULL")))
grunnskolepoeng_innvkat5 = subset(df_grunn,select=-c(vreg_3,invalder_3))
nrow(grunnskolepoeng_innvkat5)==84*3*5*2
write.csv(grunnskolepoeng_innvkat5,"D:/R/imdikator-munch/data_flat_output/grunnskolepoeng-naringsregion-2015.csv",row.names=F)

df_innv = subset(grunnskolepoeng,subset=c(vreg_3!="NULL"|invalder_3!="NULL"))
grunnskolepoeng_innvandrere = subset(df_innv,select=-innvkat_5)
nrow(grunnskolepoeng_innvandrere)==84*3*1*3*3*2
grunnskolepoeng_innvandrere$tabell_navn="grunnskolepoeng_innvandrere"
write.csv(grunnskolepoeng_innvandrere,"D:/R/imdikator-munch/data_flat_output/grunnskolepoeng_innvandrere-naringsregion-2015.csv",row.names=F)

# fylke - 2014
# aar er feil - må kodes som siste året i intervallet, avgangsåret.
# person må bli personer
# logiske sjekker
grunnskolepoeng <- read.csv("D:/R/imdikator-munch/data_flat_input/grunnskolepoeng_fylke_2014_ny.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=";", dec=",",colClasses="character")
grunnskolepoeng$aar[grunnskolepoeng$aar=="2013-2014"]="2014"
grunnskolepoeng$enhet[grunnskolepoeng$enhet=="person"]="personer"
nlevels(as.factor(grunnskolepoeng$fylke_nr))
df_grunn = subset(grunnskolepoeng,subset=c((vreg_3=="alle"|vreg_3=="NULL")&(invalder_3=="alle"|invalder_3=="NULL")))
grunnskolepoeng_innvkat5 = subset(df_grunn,select=-c(vreg_3,invalder_3))
nrow(grunnskolepoeng_innvkat5)==21*3*5*2
write.csv(grunnskolepoeng_innvkat5,"D:/R/imdikator-munch/data_flat_output/grunnskolepoeng-fylke-2014.csv",row.names=F)

df_innv = subset(grunnskolepoeng,subset=c(vreg_3!="NULL"|invalder_3!="NULL"))
grunnskolepoeng_innvandrere = subset(df_innv,select=-innvkat_5)
nrow(grunnskolepoeng_innvandrere)==21*3*1*3*3*2
grunnskolepoeng_innvandrere$tabell_navn="grunnskolepoeng_innvandrere"
write.csv(grunnskolepoeng_innvandrere,"D:/R/imdikator-munch/data_flat_output/grunnskolepoeng_innvandrere-fylke-2014.csv",row.names=F)

# fylke - 2015
# aar er feil - må kodes som siste året i intervallet, avgangsåret.
# person må bli personer
# logiske sjekker
grunnskolepoeng <- read.csv("D:/R/imdikator-munch/data_flat_input/grunnskolepoeng_fylke_2015_ny.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=";", dec=",",colClasses="character")
grunnskolepoeng$aar[grunnskolepoeng$aar=="2014-2015"]="2015"
grunnskolepoeng$enhet[grunnskolepoeng$enhet=="person"]="personer"
nlevels(as.factor(grunnskolepoeng$fylke_nr))
df_grunn = subset(grunnskolepoeng,subset=c((vreg_3=="alle"|vreg_3=="NULL")&(invalder_3=="alle"|invalder_3=="NULL")))
grunnskolepoeng_innvkat5 = subset(df_grunn,select=-c(vreg_3,invalder_3))
nrow(grunnskolepoeng_innvkat5)==21*3*5*2
write.csv(grunnskolepoeng_innvkat5,"D:/R/imdikator-munch/data_flat_output/grunnskolepoeng-fylke-2015.csv",row.names=F)

df_innv = subset(grunnskolepoeng,subset=c(vreg_3!="NULL"|invalder_3!="NULL"))
grunnskolepoeng_innvandrere = subset(df_innv,select=-innvkat_5)
nrow(grunnskolepoeng_innvandrere)==21*3*1*3*3*2
grunnskolepoeng_innvandrere$tabell_navn="grunnskolepoeng_innvandrere"
write.csv(grunnskolepoeng_innvandrere,"D:/R/imdikator-munch/data_flat_output/grunnskolepoeng_innvandrere-fylke-2015.csv",row.names=F)

#VIDEREGÅENDE_FULLFØRT
library(tidyr)
#kjørt på nytt 25. januar
#sjekk av alle komboer - 2009
videregaende_fullfort <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/tab34_2009.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
levels(as.factor(videregaende_fullfort$invkat_5))
#geoenheter * 5 innvkat * 3 kjønn * 2 enheter
df = subset(videregaende_fullfort,fylke_nr!="NULL")
nlevels(as.factor(df$fylke_nr))
nrow(subset(videregaende_fullfort,fylke_nr!="NULL"))==20*5*3*2
df_2=spread(df,invkat_5,tabellvariabel,drop=F)
sum(is.na(df_2[,9:13])==T,na.rm=F)
df = subset(videregaende_fullfort,naringsregion_nr!="NULL")
nrow(df)==nlevels(as.factor(df$naringsregion_nr))*5*3*2
df_2=spread(df,invkat_5,tabellvariabel,drop=F)
sum(is.na(df_2[,9:13])==T,na.rm=F)
df = subset(videregaende_fullfort,kommune_nr!="NULL")
nrow(df)==nlevels(as.factor(df$kommune_nr))*5*3*2
df_2=spread(df,invkat_5,tabellvariabel,drop=F)
sum(is.na(df_2[,9:13])==T,na.rm=F)
df = subset(videregaende_fullfort,bydel_nr!="NULL")
nrow(df)==nlevels(as.factor(df$bydel_nr))*5*3*2
df_2=spread(df,invkat_5,tabellvariabel,drop=F)

#VIDEREGÅENDE_FULLFØRT
library(tidyr)
#sjekk av alle komboer - 2010
videregaende_fullfort <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/tab34_2010.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
levels(as.factor(videregaende_fullfort$invkat_5))
#geoenheter * 5 innvkat * 3 kjønn * 2 enheter
df = subset(videregaende_fullfort,fylke_nr!="NULL")
nrow(df)-nlevels(as.factor(df$fylke_nr))*5*3*2
df_2=spread(df,invkat_5,tabellvariabel,drop=F)
df = subset(videregaende_fullfort,naringsregion_nr!="NULL")
nrow(df)-nlevels(as.factor(df$naringsregion_nr))*5*3*2

df = subset(videregaende_fullfort,kommune_nr!="NULL")
nrow(df)-nlevels(as.factor(df$kommune_nr))*5*3*2

#VIDEREGÅENDE_FULLFØRT
library(tidyr)
#sjekk av alle komboer - 2011
videregaende_fullfort <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/tab34_2011.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
levels(as.factor(videregaende_fullfort$invkat_5))
#geoenheter * 5 innvkat * 3 kjønn * 2 enheter
df = subset(videregaende_fullfort,fylke_nr!="NULL")
nrow(df)-nlevels(as.factor(df$fylke_nr))*5*3*2

df = subset(videregaende_fullfort,naringsregion_nr!="NULL")
nrow(df)-nlevels(as.factor(df$naringsregion_nr))*5*3*2

df = subset(videregaende_fullfort,kommune_nr!="NULL")
nrow(df)-nlevels(as.factor(df$kommune_nr))*5*3*2

#VIDEREGÅENDE_FULLFØRT
library(tidyr)
#sjekk av alle komboer - 2012
videregaende_fullfort <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/tab34_2012.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
levels(as.factor(videregaende_fullfort$invkat_5))
#geoenheter * 5 innvkat * 3 kjønn * 2 enheter
df = subset(videregaende_fullfort,fylke_nr!="NULL")
nrow(df)-nlevels(as.factor(df$fylke_nr))*5*3*2
df = subset(videregaende_fullfort,naringsregion_nr!="NULL")
nrow(df)-nlevels(as.factor(df$naringsregion_nr))*5*3*2
df = subset(videregaende_fullfort,kommune_nr!="NULL")
nrow(df)-nlevels(as.factor(df$kommune_nr))*5*3*2

#sjekk av alle komboer - 2013
videregaende_fullfort <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/tab34_2013.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
levels(as.factor(videregaende_fullfort$invkat_5))
#geoenheter * 5 innvkat * 3 kjønn * 2 enheter
df = subset(videregaende_fullfort,fylke_nr!="NULL")
nrow(df)-nlevels(as.factor(df$fylke_nr))*5*3*2
df = subset(videregaende_fullfort,naringsregion_nr!="NULL")
nrow(df)-nlevels(as.factor(df$naringsregion_nr))*5*3*2
df = subset(videregaende_fullfort,kommune_nr!="NULL")
nrow(df)-nlevels(as.factor(df$kommune_nr))*5*3*2

#sjekk av alle komboer - 2014
videregaende_fullfort <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/tab34_2014.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
levels(as.factor(videregaende_fullfort$invkat_5))
#geoenheter * 5 innvkat * 3 kjønn * 2 enheter
df = subset(videregaende_fullfort,fylke_nr!="NULL")
nrow(df)-nlevels(as.factor(df$fylke_nr))*5*3*2
df = subset(videregaende_fullfort,naringsregion_nr!="NULL")
nrow(df)-nlevels(as.factor(df$naringsregion_nr))*5*3*2
df = subset(videregaende_fullfort,kommune_nr!="NULL")
nrow(df)-nlevels(as.factor(df$kommune_nr))*5*3*2

#Videregående-fullført
#25. januar
#1. aar kodet xaar
#2. innvkat5 kodet invkat5
videregaende_fullfort <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/tab34_2009.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
names(videregaende_fullfort)[2]="aar"
names(videregaende_fullfort)[7]="innvkat_5"
write.csv(videregaende_fullfort,"D:/R/imdikator-munch/data_flat_output/videregaende_fullfort-alle-2009.csv",row.names=F)
videregaende_fullfort <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/tab34_2010.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
names(videregaende_fullfort)[2]="aar"
names(videregaende_fullfort)[7]="innvkat_5"
write.csv(videregaende_fullfort,"D:/R/imdikator-munch/data_flat_output/videregaende_fullfort-alle-2010.csv",row.names=F)
videregaende_fullfort <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/tab34_2011.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
names(videregaende_fullfort)[2]="aar"
names(videregaende_fullfort)[7]="innvkat_5"
write.csv(videregaende_fullfort,"D:/R/imdikator-munch/data_flat_output/videregaende_fullfort-alle-2011.csv",row.names=F)
videregaende_fullfort <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/tab34_2012.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
names(videregaende_fullfort)[2]="aar"
names(videregaende_fullfort)[7]="innvkat_5"
write.csv(videregaende_fullfort,"D:/R/imdikator-munch/data_flat_output/videregaende_fullfort-alle-2012.csv",row.names=F)
videregaende_fullfort <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/tab34_2013.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
names(videregaende_fullfort)[2]="aar"
names(videregaende_fullfort)[7]="innvkat_5"
write.csv(videregaende_fullfort,"D:/R/imdikator-munch/data_flat_output/videregaende_fullfort-alle-2013.csv",row.names=F)
videregaende_fullfort <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/tab34_2014.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
names(videregaende_fullfort)[2]="aar"
names(videregaende_fullfort)[7]="innvkat_5"
write.csv(videregaende_fullfort,"D:/R/imdikator-munch/data_flat_output/videregaende_fullfort-alle-2014.csv",row.names=F)

#SYSSELSATTE_INNVANDRINGSGRUNN
library(tidyr)
#kjørt på nytt 27. januar, 31. januar og 3. februar
#sjekk av alle komboer
sysselsatte_innvandringsgrunn <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/Tab_1_60a.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
nlevels(as.factor(sysselsatte_innvandringsgrunn$kommune_nr))
levels(as.factor(sysselsatte_innvandringsgrunn$kommune_nr))
levels(as.factor(sysselsatte_innvandringsgrunn$innvgrunn_6))
levels(as.factor(sysselsatte_innvandringsgrunn$aar))
levels(as.factor(sysselsatte_innvandringsgrunn$enhet))
#logisk sjekk
df=subset(sysselsatte_innvandringsgrunn,aar=="2013")
nrow(df)==nlevels(as.factor(df[,2]))*7*3*2
#substanssjekk - populasjonen
df=subset(sysselsatte_innvandringsgrunn,aar=="2013"&kommune_nr=="0000"&kjonn=="alle"&enhet=="personer")
df=subset(sysselsatte_innvandringsgrunn,aar=="2013"&kommune_nr=="0000"&kjonn=="alle"&enhet=="prosent")

#for testing i testmiljø
sysselsatte_innvandringsgrunn$tabellvariabel[sysselsatte_innvandringsgrunn$enhet=="prosent"]=extract_numeric(sysselsatte_innvandringsgrunn$tabellvariabel[sysselsatte_innvandringsgrunn$enhet=="prosent"])*100
write.csv(sysselsatte_innvandringsgrunn,"D:/R/imdikator-munch/data_flat_output/sysselsatte_innvandringsgrunn-kommune-2006_2014.csv",row.names=F)

#bydel
sysselsatte_innvandringsgrunn <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/Tab_2_60b.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
nlevels(as.factor(sysselsatte_innvandringsgrunn$bydel_nr))
levels(as.factor(sysselsatte_innvandringsgrunn$bydel_nr))
levels(as.factor(sysselsatte_innvandringsgrunn$innvgrunn_6))
levels(as.factor(sysselsatte_innvandringsgrunn$aar))
levels(as.factor(sysselsatte_innvandringsgrunn$enhet))
levels(as.factor(sysselsatte_innvandringsgrunn$kjonn))
#logisk sjekk
df=subset(sysselsatte_innvandringsgrunn,aar=="2013")
nrow(df)==nlevels(as.factor(df[,2]))*7*3*2
#for testing i testmiljø
sysselsatte_innvandringsgrunn$tabellvariabel[sysselsatte_innvandringsgrunn$enhet=="prosent"]=extract_numeric(sysselsatte_innvandringsgrunn$tabellvariabel[sysselsatte_innvandringsgrunn$enhet=="prosent"])*100
write.csv(sysselsatte_innvandringsgrunn,"D:/R/imdikator-munch/data_flat_output/sysselsatte_innvandringsgrunn-bydel-2006_2014.csv",row.names=F)

#fylke
sysselsatte_innvandringsgrunn <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/Tab_3_60c.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
nlevels(as.factor(sysselsatte_innvandringsgrunn$fylke_nr))
levels(as.factor(sysselsatte_innvandringsgrunn$fylke_nr))
levels(as.factor(sysselsatte_innvandringsgrunn$innvgrunn_6))
levels(as.factor(sysselsatte_innvandringsgrunn$aar))
levels(as.factor(sysselsatte_innvandringsgrunn$enhet))
levels(as.factor(sysselsatte_innvandringsgrunn$kjonn))
#for testing i testmiljø
sysselsatte_innvandringsgrunn$tabellvariabel[sysselsatte_innvandringsgrunn$enhet=="prosent"]=extract_numeric(sysselsatte_innvandringsgrunn$tabellvariabel[sysselsatte_innvandringsgrunn$enhet=="prosent"])*100
write.csv(sysselsatte_innvandringsgrunn,"D:/R/imdikator-munch/data_flat_output/sysselsatte_innvandringsgrunn-fylke-2006_2014.csv",row.names=F)

#næringsregion
sysselsatte_innvandringsgrunn <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/Tab_4_60d.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
nlevels(as.factor(sysselsatte_innvandringsgrunn$naringsregion_nr))
levels(as.factor(sysselsatte_innvandringsgrunn$naringsregion_nr))
levels(as.factor(sysselsatte_innvandringsgrunn$innvgrunn_6))
levels(as.factor(sysselsatte_innvandringsgrunn$aar))
levels(as.factor(sysselsatte_innvandringsgrunn$enhet))
levels(as.factor(sysselsatte_innvandringsgrunn$kjonn))
#for testing i testmiljø
sysselsatte_innvandringsgrunn$tabellvariabel[sysselsatte_innvandringsgrunn$enhet=="prosent"]=extract_numeric(sysselsatte_innvandringsgrunn$tabellvariabel[sysselsatte_innvandringsgrunn$enhet=="prosent"])*100
write.csv(sysselsatte_innvandringsgrunn,"D:/R/imdikator-munch/data_flat_output/sysselsatte_innvandringsgrunn-naringsregion-2006_2014.csv",row.names=F)

#sysselsatte innvandringskategori
#kommune
sysselsatte_innvkat <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/Tab_5_611a.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
nlevels(as.factor(sysselsatte_innvkat$kommune_nr))
levels(as.factor(sysselsatte_innvkat$kommune_nr))
levels(as.factor(sysselsatte_innvkat$innvkat_2))
levels(as.factor(sysselsatte_innvkat$aar))
levels(as.factor(sysselsatte_innvkat$enhet))
#logisk sjekk
df=subset(sysselsatte_innvkat,aar=="2013")
nrow(df)==nlevels(as.factor(df[,2]))*3*2
#substanssjekk - populasjonen
df=subset(sysselsatte_innvkat,aar=="2014"&kommune_nr=="0000"&innvkat_2=="alle")
df=subset(sysselsatte_innvkat,aar=="2014"&kommune_nr=="0000")

#for testing i miljø
names(sysselsatte_innvkat)[3]="innvkat_3"
sysselsatte_innvkat$tabellvariabel[sysselsatte_innvkat$enhet=="prosent"]=extract_numeric(sysselsatte_innvkat$tabellvariabel[sysselsatte_innvkat$enhet=="prosent"])*100
df=subset(sysselsatte_innvkat,aar=="2014"&kommune_nr=="0000"&innvkat_3=="alle")
write.csv(sysselsatte_innvkat,"D:/R/imdikator-munch/data_flat_output/sysselsatte_innvkat-kommune-2006_2014.csv",row.names=F)

#bydel
sysselsatte_innvkat <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/Tab_6_611b.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
nlevels(as.factor(sysselsatte_innvkat$bydel_nr))
levels(as.factor(sysselsatte_innvkat$bydel_nr))
levels(as.factor(sysselsatte_innvkat$innvkat_2))
levels(as.factor(sysselsatte_innvkat$aar))
levels(as.factor(sysselsatte_innvkat$enhet))

#for testing i miljø
names(sysselsatte_innvkat)[3]="innvkat_3"
sysselsatte_innvkat$tabellvariabel[sysselsatte_innvkat$enhet=="prosent"]=extract_numeric(sysselsatte_innvkat$tabellvariabel[sysselsatte_innvkat$enhet=="prosent"])*100
write.csv(sysselsatte_innvkat,"D:/R/imdikator-munch/data_flat_output/sysselsatte_innvkat-bydel-2006_2014.csv",row.names=F)

#fylke
sysselsatte_innvkat <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/Tab_7_611c.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
nlevels(as.factor(sysselsatte_innvkat$fylke_nr))
levels(as.factor(sysselsatte_innvkat$fylke_nr))
levels(as.factor(sysselsatte_innvkat$innvkat_2))
levels(as.factor(sysselsatte_innvkat$aar))
levels(as.factor(sysselsatte_innvkat$enhet))

#for testing i miljø
names(sysselsatte_innvkat)[3]="innvkat_3"
sysselsatte_innvkat$tabellvariabel[sysselsatte_innvkat$enhet=="prosent"]=extract_numeric(sysselsatte_innvkat$tabellvariabel[sysselsatte_innvkat$enhet=="prosent"])*100
write.csv(sysselsatte_innvkat,"D:/R/imdikator-munch/data_flat_output/sysselsatte_innvkat-fylke-2006_2014.csv",row.names=F)

#næringsregion
sysselsatte_innvkat <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/Tab_8_611d.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
nlevels(as.factor(sysselsatte_innvkat$naringsregion_nr))
levels(as.factor(sysselsatte_innvkat$naringsregion_nr))
levels(as.factor(sysselsatte_innvkat$innvkat_2))
levels(as.factor(sysselsatte_innvkat$aar))
levels(as.factor(sysselsatte_innvkat$enhet))

#for testing i miljø
names(sysselsatte_innvkat)[3]="innvkat_3"
sysselsatte_innvkat$tabellvariabel[sysselsatte_innvkat$enhet=="prosent"]=extract_numeric(sysselsatte_innvkat$tabellvariabel[sysselsatte_innvkat$enhet=="prosent"])*100
write.csv(sysselsatte_innvkat,"D:/R/imdikator-munch/data_flat_output/sysselsatte_innvkat-naringsregion-2006_2014.csv",row.names=F)

#SYSSELSATTE_INNVKAT_ALDER
library(tidyr)
#kommune
sysselsatte_innvkat_alder <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/Tab_9_612a.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
nlevels(as.factor(sysselsatte_innvkat_alder$kommune_nr))
levels(as.factor(sysselsatte_innvkat_alder$arbeidsalder_18_69))
levels(as.factor(sysselsatte_innvkat_alder$innvkat_2))
levels(as.factor(sysselsatte_innvkat_alder$aar))
levels(as.factor(sysselsatte_innvkat_alder$enhet))
#logisk sjekk
df=subset(sysselsatte_innvkat_alder,aar=="2014")
nrow(df)==nlevels(as.factor(df[,2]))*3*4*2
#substanssjekk - populasjonen
df=subset(sysselsatte_innvkat_alder,aar=="2014"&kommune_nr=="0000"&innvkat_2=="alle"&arbeidsalder_18_69=="alle")

#for testing i miljø
names(sysselsatte_innvkat_alder)[3]="innvkat_3"
sysselsatte_innvkat_alder$tabellvariabel[sysselsatte_innvkat_alder$enhet=="prosent"]=extract_numeric(sysselsatte_innvkat_alder$tabellvariabel[sysselsatte_innvkat_alder$enhet=="prosent"])*100
write.csv(sysselsatte_innvkat_alder,"D:/R/imdikator-munch/data_flat_output/sysselsatte_innvkat_alder-kommune-2006_2014.csv",row.names=F)

#bydel
sysselsatte_innvkat_alder <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/Tab_10_612b.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
nlevels(as.factor(sysselsatte_innvkat_alder$bydel_nr))
levels(as.factor(sysselsatte_innvkat_alder$bydel_nr))
levels(as.factor(sysselsatte_innvkat_alder$arbeidsalder_18_69))
levels(as.factor(sysselsatte_innvkat_alder$innvkat_2))
levels(as.factor(sysselsatte_innvkat_alder$aar))
levels(as.factor(sysselsatte_innvkat_alder$enhet))
#logisk sjekk
df=subset(sysselsatte_innvkat_alder,aar=="2014")
nrow(df)==nlevels(as.factor(df[,2]))*3*4*2

#for testing i miljø
names(sysselsatte_innvkat_alder)[3]="innvkat_3"
sysselsatte_innvkat_alder$tabellvariabel[sysselsatte_innvkat_alder$enhet=="prosent"]=extract_numeric(sysselsatte_innvkat_alder$tabellvariabel[sysselsatte_innvkat_alder$enhet=="prosent"])*100
write.csv(sysselsatte_innvkat_alder,"D:/R/imdikator-munch/data_flat_output/sysselsatte_innvkat_alder-bydel-2006_2014.csv",row.names=F)

#fylke
sysselsatte_innvkat_alder <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/Tab_11_612c.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
nlevels(as.factor(sysselsatte_innvkat_alder$fylke_nr))
levels(as.factor(sysselsatte_innvkat_alder$fylke_nr))
levels(as.factor(sysselsatte_innvkat_alder$arbeidsalder_18_69))
levels(as.factor(sysselsatte_innvkat_alder$innvkat_2))
levels(as.factor(sysselsatte_innvkat_alder$aar))
levels(as.factor(sysselsatte_innvkat_alder$enhet))
#logisk sjekk
df=subset(sysselsatte_innvkat_alder,aar=="2014")
nrow(df)==nlevels(as.factor(df[,2]))*3*4*2

#for testing i miljø
names(sysselsatte_innvkat_alder)[3]="innvkat_3"
sysselsatte_innvkat_alder$tabellvariabel[sysselsatte_innvkat_alder$enhet=="prosent"]=extract_numeric(sysselsatte_innvkat_alder$tabellvariabel[sysselsatte_innvkat_alder$enhet=="prosent"])*100
write.csv(sysselsatte_innvkat_alder,"D:/R/imdikator-munch/data_flat_output/sysselsatte_innvkat_alder-fylke-2006_2014.csv",row.names=F)

#næringsregion
sysselsatte_innvkat_alder <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/Tab_12_612d.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
nlevels(as.factor(sysselsatte_innvkat_alder$naringsregion_nr))
levels(as.factor(sysselsatte_innvkat_alder$naringsregion_nr))
levels(as.factor(sysselsatte_innvkat_alder$arbeidsalder_18_69))
levels(as.factor(sysselsatte_innvkat_alder$innvkat_2))
levels(as.factor(sysselsatte_innvkat_alder$aar))
levels(as.factor(sysselsatte_innvkat_alder$enhet))
#logisk sjekk
df=subset(sysselsatte_innvkat_alder,aar=="2014")
nrow(df)==nlevels(as.factor(df[,2]))*3*4*2

#for testing i miljø
names(sysselsatte_innvkat_alder)[3]="innvkat_3"
sysselsatte_innvkat_alder$tabellvariabel[sysselsatte_innvkat_alder$enhet=="prosent"]=extract_numeric(sysselsatte_innvkat_alder$tabellvariabel[sysselsatte_innvkat_alder$enhet=="prosent"])*100
write.csv(sysselsatte_innvkat_alder,"D:/R/imdikator-munch/data_flat_output/sysselsatte_innvkat_alder-naringsregion-2006_2014.csv",row.names=F)

#SYSSELSATTE_BOTID
library(tidyr)
#kommune
sysselsatte_botid <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/Tab_13_62a.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
nlevels(as.factor(sysselsatte_botid$kommune_nr))
levels(as.factor(sysselsatte_botid$botid_5))
levels(as.factor(sysselsatte_botid$aar))
levels(as.factor(sysselsatte_botid$enhet))
#logisk sjekk
df=subset(sysselsatte_botid,aar=="2014")
nrow(df)==nlevels(as.factor(df[,2]))*4*2
#substanssjekk - populasjonen
df=subset(sysselsatte_botid,aar=="2014"&kommune_nr=="0000"&botid_5=="alle")
#substans - summering
df=subset(sysselsatte_botid,aar=="2014"&kommune_nr=="0000"&enhet=="personer")

#for testing i miljø
sysselsatte_botid$tabellvariabel[sysselsatte_botid$enhet=="prosent"]=extract_numeric(sysselsatte_botid$tabellvariabel[sysselsatte_botid$enhet=="prosent"])*100
write.csv(sysselsatte_botid,"D:/R/imdikator-munch/data_flat_output/sysselsatte_botid-kommune-2006_2014.csv",row.names=F)

#bydel
sysselsatte_botid <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/Tab_14_62b.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
nlevels(as.factor(sysselsatte_botid$bydel_nr))
levels(as.factor(sysselsatte_botid$bydel_nr))
levels(as.factor(sysselsatte_botid$botid_5))
levels(as.factor(sysselsatte_botid$aar))
levels(as.factor(sysselsatte_botid$enhet))
#logisk sjekk
df=subset(sysselsatte_botid,aar=="2014")
nrow(df)==nlevels(as.factor(df[,2]))*4*2

#for testing i miljø
sysselsatte_botid$tabellvariabel[sysselsatte_botid$enhet=="prosent"]=extract_numeric(sysselsatte_botid$tabellvariabel[sysselsatte_botid$enhet=="prosent"])*100
write.csv(sysselsatte_botid,"D:/R/imdikator-munch/data_flat_output/sysselsatte_botid-bydel-2006_2014.csv",row.names=F)

#fylke
sysselsatte_botid <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/Tab_15_62c.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
nlevels(as.factor(sysselsatte_botid$fylke_nr))
levels(as.factor(sysselsatte_botid$fylke_nr))
levels(as.factor(sysselsatte_botid$botid_5))
levels(as.factor(sysselsatte_botid$aar))
levels(as.factor(sysselsatte_botid$enhet))
#logisk sjekk
df=subset(sysselsatte_botid,aar=="2014")
nrow(df)==nlevels(as.factor(df[,2]))*4*2
#substanssjekk - populasjonen
df=subset(sysselsatte_botid,aar=="2014"&fylke_nr=="00"&botid_5=="alle")
#substans - summering
df=subset(sysselsatte_botid,aar=="2014"&fylke_nr=="00"&enhet=="personer")
#for testing i miljø
sysselsatte_botid$tabellvariabel[sysselsatte_botid$enhet=="prosent"]=extract_numeric(sysselsatte_botid$tabellvariabel[sysselsatte_botid$enhet=="prosent"])*100
write.csv(sysselsatte_botid,"D:/R/imdikator-munch/data_flat_output/sysselsatte_botid-fylke-2006_2014.csv",row.names=F)

#næringsregion
sysselsatte_botid <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/Tab_16_62d.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
nlevels(as.factor(sysselsatte_botid$naringsregion_nr))
levels(as.factor(sysselsatte_botid$naringsregion_nr))
levels(as.factor(sysselsatte_botid$botid_5))
levels(as.factor(sysselsatte_botid$aar))
levels(as.factor(sysselsatte_botid$enhet))
#logisk sjekk
df=subset(sysselsatte_botid,aar=="2014")
nrow(df)==nlevels(as.factor(df[,2]))*4*2
#for testing i miljø
sysselsatte_botid$tabellvariabel[sysselsatte_botid$enhet=="prosent"]=extract_numeric(sysselsatte_botid$tabellvariabel[sysselsatte_botid$enhet=="prosent"])*100
write.csv(sysselsatte_botid,"D:/R/imdikator-munch/data_flat_output/sysselsatte_botid-naringsregion-2006_2014.csv",row.names=F)

#SYSSELSATTE_BOTID_LAND
library(tidyr)
#kommune
sysselsatte_botid_land <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/Tab_17_631a.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
nlevels(as.factor(sysselsatte_botid_land$kommune_nr))
levels(as.factor(sysselsatte_botid_land$botid_5))
levels(as.factor(sysselsatte_botid_land$vreg_3))
levels(as.factor(sysselsatte_botid_land$aar))
levels(as.factor(sysselsatte_botid_land$enhet))
#logisk sjekk
df=subset(sysselsatte_botid_land,aar=="2014")
nrow(df)==nlevels(as.factor(df[,2]))*4*3*2
#substanssjekk - populasjonen
df=subset(sysselsatte_botid_land,aar=="2014"&kommune_nr=="0000"&botid_5=="alle"&vreg_3=="alle")
#substanssjekk - summering
df=subset(sysselsatte_botid_land,aar=="2014"&kommune_nr=="0000"&vreg_3=="alle"&enhet=="personer")
#for testing i miljø
sysselsatte_botid_land$tabellvariabel[sysselsatte_botid_land$enhet=="prosent"]=extract_numeric(sysselsatte_botid_land$tabellvariabel[sysselsatte_botid_land$enhet=="prosent"])*100
write.csv(sysselsatte_botid_land,"D:/R/imdikator-munch/data_flat_output/sysselsatte_botid_land-kommune-2006_2014.csv",row.names=F)

#bydel
sysselsatte_botid_land <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/Tab_18_631b.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
nlevels(as.factor(sysselsatte_botid_land$bydel_nr))
levels(as.factor(sysselsatte_botid_land$bydel_nr))
levels(as.factor(sysselsatte_botid_land$botid_5))
levels(as.factor(sysselsatte_botid_land$vreg_3))
levels(as.factor(sysselsatte_botid_land$aar))
levels(as.factor(sysselsatte_botid_land$enhet))
#logisk sjekk
df=subset(sysselsatte_botid_land,aar=="2014")
nrow(df)==nlevels(as.factor(df[,2]))*4*3*2
#for testing i miljø
sysselsatte_botid_land$tabellvariabel[sysselsatte_botid_land$enhet=="prosent"]=extract_numeric(sysselsatte_botid_land$tabellvariabel[sysselsatte_botid_land$enhet=="prosent"])*100
write.csv(sysselsatte_botid_land,"D:/R/imdikator-munch/data_flat_output/sysselsatte_botid_land-bydel-2006_2014.csv",row.names=F)

#fylke
sysselsatte_botid_land <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/Tab_19_631c.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
nlevels(as.factor(sysselsatte_botid_land$fylke_nr))
levels(as.factor(sysselsatte_botid_land$fylke_nr))
levels(as.factor(sysselsatte_botid_land$botid_5))
levels(as.factor(sysselsatte_botid_land$vreg_3))
levels(as.factor(sysselsatte_botid_land$aar))
levels(as.factor(sysselsatte_botid_land$enhet))
#logisk sjekk
df=subset(sysselsatte_botid_land,aar=="2014")
nrow(df)==nlevels(as.factor(df[,2]))*4*3*2
#for testing i miljø
sysselsatte_botid_land$tabellvariabel[sysselsatte_botid_land$enhet=="prosent"]=extract_numeric(sysselsatte_botid_land$tabellvariabel[sysselsatte_botid_land$enhet=="prosent"])*100
write.csv(sysselsatte_botid_land,"D:/R/imdikator-munch/data_flat_output/sysselsatte_botid_land-fylke-2006_2014.csv",row.names=F)

#naringsregion
sysselsatte_botid_land <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/Tab_20_631d.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
nlevels(as.factor(sysselsatte_botid_land$naringsregion_nr))
levels(as.factor(sysselsatte_botid_land$naringsregion_nr))
levels(as.factor(sysselsatte_botid_land$botid_5))
levels(as.factor(sysselsatte_botid_land$vreg_3))
levels(as.factor(sysselsatte_botid_land$aar))
levels(as.factor(sysselsatte_botid_land$enhet))
#logisk sjekk
df=subset(sysselsatte_botid_land,aar=="2014")
nrow(df)==nlevels(as.factor(df[,2]))*4*3*2
#for testing i miljø
sysselsatte_botid_land$tabellvariabel[sysselsatte_botid_land$enhet=="prosent"]=extract_numeric(sysselsatte_botid_land$tabellvariabel[sysselsatte_botid_land$enhet=="prosent"])*100
write.csv(sysselsatte_botid_land,"D:/R/imdikator-munch/data_flat_output/sysselsatte_botid_land-naringsregion-2006_2014.csv",row.names=F)

#SYSSELSATTE_KJONN_LAND
library(tidyr)
#kommune
sysselsatte_kjonn_land <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/Tab_21_632a.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
nlevels(as.factor(sysselsatte_kjonn_land$kommune_nr))
levels(as.factor(sysselsatte_kjonn_land$kommune_nr))
levels(as.factor(sysselsatte_kjonn_land$vreg_3))
levels(as.factor(sysselsatte_kjonn_land$kjonn))
levels(as.factor(sysselsatte_kjonn_land$aar))
levels(as.factor(sysselsatte_kjonn_land$enhet))
#logisk sjekk
df=subset(sysselsatte_kjonn_land,aar=="2014")
nrow(df)==nlevels(as.factor(df[,2]))*3*3*2
#substanssjekk - populasjonen
df=subset(sysselsatte_kjonn_land,aar=="2014"&kommune_nr=="0000"&vreg_3=="alle"&kjonn=="alle")
#substanssjekk - summering
df=subset(sysselsatte_kjonn_land,aar=="2014"&kommune_nr=="0000"&kjonn=="alle"&enhet=="personer")
df=subset(sysselsatte_kjonn_land,aar=="2014"&kommune_nr=="0000"&vreg_3=="alle"&enhet=="personer")

#for testing i miljø
sysselsatte_kjonn_land$tabellvariabel[sysselsatte_kjonn_land$enhet=="prosent"]=extract_numeric(sysselsatte_kjonn_land$tabellvariabel[sysselsatte_kjonn_land$enhet=="prosent"])*100
write.csv(sysselsatte_kjonn_land,"D:/R/imdikator-munch/data_flat_output/sysselsatte_kjonn_land-kommune-2006_2014.csv",row.names=F)

#bydel
sysselsatte_kjonn_land <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/Tab_22_632b.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
nlevels(as.factor(sysselsatte_kjonn_land$bydel_nr))
levels(as.factor(sysselsatte_kjonn_land$bydel_nr))
levels(as.factor(sysselsatte_kjonn_land$vreg_3))
levels(as.factor(sysselsatte_kjonn_land$kjonn))
levels(as.factor(sysselsatte_kjonn_land$aar))
levels(as.factor(sysselsatte_kjonn_land$enhet))
#logisk sjekk
df=subset(sysselsatte_kjonn_land,aar=="2014")
nrow(df)==nlevels(as.factor(df[,2]))*3*3*2

#for testing i miljø
sysselsatte_kjonn_land$tabellvariabel[sysselsatte_kjonn_land$enhet=="prosent"]=extract_numeric(sysselsatte_kjonn_land$tabellvariabel[sysselsatte_kjonn_land$enhet=="prosent"])*100
write.csv(sysselsatte_kjonn_land,"D:/R/imdikator-munch/data_flat_output/sysselsatte_kjonn_land-bydel-2006_2014.csv",row.names=F)

#fylke
sysselsatte_kjonn_land <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/Tab_23_632c.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
nlevels(as.factor(sysselsatte_kjonn_land$fylke_nr))
levels(as.factor(sysselsatte_kjonn_land$fylke_nr))
levels(as.factor(sysselsatte_kjonn_land$vreg_3))
levels(as.factor(sysselsatte_kjonn_land$kjonn))
levels(as.factor(sysselsatte_kjonn_land$aar))
levels(as.factor(sysselsatte_kjonn_land$enhet))
#logisk sjekk
df=subset(sysselsatte_kjonn_land,aar=="2014")
nrow(df)==nlevels(as.factor(df[,2]))*3*3*2

#for testing i miljø
sysselsatte_kjonn_land$tabellvariabel[sysselsatte_kjonn_land$enhet=="prosent"]=extract_numeric(sysselsatte_kjonn_land$tabellvariabel[sysselsatte_kjonn_land$enhet=="prosent"])*100
write.csv(sysselsatte_kjonn_land,"D:/R/imdikator-munch/data_flat_output/sysselsatte_kjonn_land-fylke-2006_2014.csv",row.names=F)

#naringsregion
sysselsatte_kjonn_land <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/Tab_24_632d.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
nlevels(as.factor(sysselsatte_kjonn_land$naringsregion_nr))
levels(as.factor(sysselsatte_kjonn_land$naringsregion_nr))
levels(as.factor(sysselsatte_kjonn_land$vreg_3))
levels(as.factor(sysselsatte_kjonn_land$kjonn))
levels(as.factor(sysselsatte_kjonn_land$aar))
levels(as.factor(sysselsatte_kjonn_land$enhet))
#logisk sjekk
df=subset(sysselsatte_kjonn_land,aar=="2014")
nrow(df)==nlevels(as.factor(df[,2]))*3*3*2

#for testing i miljø
sysselsatte_kjonn_land$tabellvariabel[sysselsatte_kjonn_land$enhet=="prosent"]=extract_numeric(sysselsatte_kjonn_land$tabellvariabel[sysselsatte_kjonn_land$enhet=="prosent"])*100
write.csv(sysselsatte_kjonn_land,"D:/R/imdikator-munch/data_flat_output/sysselsatte_kjonn_land-naringsregion-2006_2014.csv",row.names=F)

#SYSSELSATTE_LAND
library(tidyr)
#kommune
sysselsatte_land <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/Tab_25_633a.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
nlevels(as.factor(sysselsatte_land$kommune_nr))
levels(as.factor(sysselsatte_land$kommune_nr))
levels(as.factor(sysselsatte_land$vreg_9))
levels(as.factor(sysselsatte_land$aar))
levels(as.factor(sysselsatte_land$enhet))
#logisk sjekk
df=subset(sysselsatte_land,aar=="2014")
nrow(df)==nlevels(as.factor(df[,2]))*9*2
#substanssjekk - populasjonen
df=subset(sysselsatte_land,aar=="2014"&kommune_nr=="0000"&vreg_9=="alle")

#for testing i miljø
sysselsatte_land$tabellvariabel[sysselsatte_land$enhet=="prosent"]=extract_numeric(sysselsatte_land$tabellvariabel[sysselsatte_land$enhet=="prosent"])*100
write.csv(sysselsatte_land,"D:/R/imdikator-munch/data_flat_output/sysselsatte_land-kommune-2006_2014.csv",row.names=F)

#bydel
sysselsatte_land <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/Tab_26_633b.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
nlevels(as.factor(sysselsatte_land$bydel_nr))
levels(as.factor(sysselsatte_land$bydel_nr))
levels(as.factor(sysselsatte_land$vreg_9))
levels(as.factor(sysselsatte_land$aar))
levels(as.factor(sysselsatte_land$enhet))
#logisk sjekk
df=subset(sysselsatte_land,aar=="2014")
nrow(df)==nlevels(as.factor(df[,2]))*9*2

#for testing i miljø
sysselsatte_land$tabellvariabel[sysselsatte_land$enhet=="prosent"]=extract_numeric(sysselsatte_land$tabellvariabel[sysselsatte_land$enhet=="prosent"])*100
write.csv(sysselsatte_land,"D:/R/imdikator-munch/data_flat_output/sysselsatte_land-bydel-2006_2014.csv",row.names=F)

#fylke
sysselsatte_land <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/Tab_27_633c.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
nlevels(as.factor(sysselsatte_land$fylke_nr))
levels(as.factor(sysselsatte_land$fylke_nr))
levels(as.factor(sysselsatte_land$vreg_9))
levels(as.factor(sysselsatte_land$aar))
levels(as.factor(sysselsatte_land$enhet))
#logisk sjekk
df=subset(sysselsatte_land,aar=="2014")
nrow(df)==nlevels(as.factor(df[,2]))*9*2

#for testing i miljø
sysselsatte_land$tabellvariabel[sysselsatte_land$enhet=="prosent"]=extract_numeric(sysselsatte_land$tabellvariabel[sysselsatte_land$enhet=="prosent"])*100
write.csv(sysselsatte_land,"D:/R/imdikator-munch/data_flat_output/sysselsatte_land-fylke-2006_2014.csv",row.names=F)

#næringsregion
sysselsatte_land <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/Tab_28_633d.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
nlevels(as.factor(sysselsatte_land$naringsregion_nr))
levels(as.factor(sysselsatte_land$naringsregion_nr))
levels(as.factor(sysselsatte_land$vreg_9))
levels(as.factor(sysselsatte_land$aar))
levels(as.factor(sysselsatte_land$enhet))
#logisk sjekk
df=subset(sysselsatte_land,aar=="2014")
nrow(df)==nlevels(as.factor(df[,2]))*9*2

#for testing i miljø
sysselsatte_land$tabellvariabel[sysselsatte_land$enhet=="prosent"]=extract_numeric(sysselsatte_land$tabellvariabel[sysselsatte_land$enhet=="prosent"])*100
write.csv(sysselsatte_land,"D:/R/imdikator-munch/data_flat_output/sysselsatte_land-naringsregion-2006_2014.csv",row.names=F)

#ARBLEDIGE_INNVKAT_LAND
library(tidyr)
#kommune
arbledige_innvkat_land <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/Tab_29_64a.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
nlevels(as.factor(arbledige_innvkat_land$kommune_nr))
levels(as.factor(arbledige_innvkat_land$kommune_nr))
levels(as.factor(arbledige_innvkat_land$innvkat_3))
levels(as.factor(arbledige_innvkat_land$vreg_3))
levels(as.factor(arbledige_innvkat_land$enhet))
#logisk sjekk
df=subset(arbledige_innvkat_land,aar=="2014")
nrow(df)==nlevels(as.factor(df[,2]))*3*3*2
#substanssjekk - populasjonen
df=subset(arbledige_innvkat_land,aar=="2014"&kommune_nr=="0000"&innvkat_3=="alle")
df=subset(arbledige_innvkat_land,aar=="2014"&kommune_nr=="0000"&vreg_3=="alle")
#for testing i miljø
arbledige_innvkat_land$tabellvariabel[arbledige_innvkat_land$enhet=="prosent"]=extract_numeric(arbledige_innvkat_land$tabellvariabel[arbledige_innvkat_land$enhet=="prosent"])*100
arbledige_innvkat_land = subset(arbledige_innvkat_land,subset=(innvkat_3=="alle"&vreg_3=="alle")|(innvkat_3=="innvandrere")|(innvkat_3=="befolkningen_ellers"&vreg_3=="alle"))
write.csv(arbledige_innvkat_land,"D:/R/imdikator-munch/data_flat_output/arbledige_innvkat_land-kommune-2006_2014.csv",row.names=F)

#bydel
arbledige_innvkat_land <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/Tab_30_64b.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
nlevels(as.factor(arbledige_innvkat_land$bydel_nr))
levels(as.factor(arbledige_innvkat_land$bydel_nr))
levels(as.factor(arbledige_innvkat_land$innvkat_3))
levels(as.factor(arbledige_innvkat_land$vreg_3))
levels(as.factor(arbledige_innvkat_land$aar))
levels(as.factor(arbledige_innvkat_land$enhet))
#logisk sjekk
df=subset(arbledige_innvkat_land,aar=="2014")
nrow(df)==nlevels(as.factor(df[,2]))*3*3*2

#for testing i miljø
arbledige_innvkat_land$tabellvariabel[arbledige_innvkat_land$enhet=="prosent"]=extract_numeric(arbledige_innvkat_land$tabellvariabel[arbledige_innvkat_land$enhet=="prosent"])*100
arbledige_innvkat_land = subset(arbledige_innvkat_land,subset=(innvkat_3=="alle"&vreg_3=="alle")|(innvkat_3=="innvandrere")|(innvkat_3=="befolkningen_ellers"&vreg_3=="alle"))
write.csv(arbledige_innvkat_land,"D:/R/imdikator-munch/data_flat_output/arbledige_innvkat_land-bydel-2006_2014.csv",row.names=F)

#fylke
arbledige_innvkat_land <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/Tab_31_64c.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
nlevels(as.factor(arbledige_innvkat_land$fylke_nr))
levels(as.factor(arbledige_innvkat_land$fylke_nr))
levels(as.factor(arbledige_innvkat_land$innvkat_3))
levels(as.factor(arbledige_innvkat_land$vreg_3))
levels(as.factor(arbledige_innvkat_land$aar))
levels(as.factor(arbledige_innvkat_land$enhet))
#logisk sjekk
df=subset(arbledige_innvkat_land,aar=="2014")
nrow(df)==nlevels(as.factor(df[,2]))*3*3*2

#for testing i miljø
arbledige_innvkat_land$tabellvariabel[arbledige_innvkat_land$enhet=="prosent"]=extract_numeric(arbledige_innvkat_land$tabellvariabel[arbledige_innvkat_land$enhet=="prosent"])*100
arbledige_innvkat_land = subset(arbledige_innvkat_land,subset=(innvkat_3=="alle"&vreg_3=="alle")|(innvkat_3=="innvandrere")|(innvkat_3=="befolkningen_ellers"&vreg_3=="alle"))
write.csv(arbledige_innvkat_land,"D:/R/imdikator-munch/data_flat_output/arbledige_innvkat_land-fylke-2006_2014.csv",row.names=F)

#næringsregion
arbledige_innvkat_land <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/Tab_32_64d.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
nlevels(as.factor(arbledige_innvkat_land$naringsregion_nr))
levels(as.factor(arbledige_innvkat_land$naringsregion_nr))
levels(as.factor(arbledige_innvkat_land$innvkat_3))
levels(as.factor(arbledige_innvkat_land$vreg_3))
levels(as.factor(arbledige_innvkat_land$aar))
levels(as.factor(arbledige_innvkat_land$enhet))
#logisk sjekk
df=subset(arbledige_innvkat_land,aar=="2014")
nrow(df)==nlevels(as.factor(df[,2]))*3*3*2

#for testing i miljø
arbledige_innvkat_land$tabellvariabel[arbledige_innvkat_land$enhet=="prosent"]=extract_numeric(arbledige_innvkat_land$tabellvariabel[arbledige_innvkat_land$enhet=="prosent"])*100
arbledige_innvkat_land = subset(arbledige_innvkat_land,subset=(innvkat_3=="alle"&vreg_3=="alle")|(innvkat_3=="innvandrere")|(innvkat_3=="befolkningen_ellers"&vreg_3=="alle"))
write.csv(arbledige_innvkat_land,"D:/R/imdikator-munch/data_flat_output/arbledige_innvkat_land-naringsregion-2006_2014.csv",row.names=F)

#IKKE_ARBUTD_INNVKAT_LAND
library(tidyr)
#kommune
ikke_arbutd_innvkat_land <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/Tab_33_65a.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
nlevels(as.factor(ikke_arbutd_innvkat_land$kommune_nr))
levels(as.factor(ikke_arbutd_innvkat_land$kommune_nr))
levels(as.factor(ikke_arbutd_innvkat_land$innvkat_3))
levels(as.factor(ikke_arbutd_innvkat_land$vreg_3))
levels(as.factor(ikke_arbutd_innvkat_land$kjonn))
levels(as.factor(ikke_arbutd_innvkat_land$enhet))
#logisk sjekk
df=subset(ikke_arbutd_innvkat_land,aar=="2014")
nrow(df)==nlevels(as.factor(df[,2]))*3*3*3*2
#substanssjekk - populasjonen
df=subset(ikke_arbutd_innvkat_land,aar=="2013"&kommune_nr=="0000"&innvkat_3=="alle")
df=subset(ikke_arbutd_innvkat_land,aar=="2013"&kommune_nr=="0000"&vreg_3=="alle")

#for testing i miljø
ikke_arbutd_innvkat_land$tabellvariabel[ikke_arbutd_innvkat_land$enhet=="prosent"]=extract_numeric(ikke_arbutd_innvkat_land$tabellvariabel[ikke_arbutd_innvkat_land$enhet=="prosent"])*100
write.csv(ikke_arbutd_innvkat_land,"D:/R/imdikator-munch/data_flat_output/ikke_arbutd_innvkat_land-kommune-2006_2014.csv",row.names=F)

#bydel
ikke_arbutd_innvkat_land <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/Tab_34_65b.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
nlevels(as.factor(ikke_arbutd_innvkat_land$bydel_nr))
levels(as.factor(ikke_arbutd_innvkat_land$bydel_nr))
levels(as.factor(ikke_arbutd_innvkat_land$innvkat_3))
levels(as.factor(ikke_arbutd_innvkat_land$kjonn))
levels(as.factor(ikke_arbutd_innvkat_land$vreg_3))
levels(as.factor(ikke_arbutd_innvkat_land$aar))
levels(as.factor(ikke_arbutd_innvkat_land$enhet))

#for testing i miljø
ikke_arbutd_innvkat_land$tabellvariabel[ikke_arbutd_innvkat_land$enhet=="prosent"]=extract_numeric(ikke_arbutd_innvkat_land$tabellvariabel[ikke_arbutd_innvkat_land$enhet=="prosent"])*100
write.csv(ikke_arbutd_innvkat_land,"D:/R/imdikator-munch/data_flat_output/ikke_arbutd_innvkat_land-bydel-2006_2014.csv",row.names=F)

#fylke
ikke_arbutd_innvkat_land <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/Tab_35_65c.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
nlevels(as.factor(ikke_arbutd_innvkat_land$fylke_nr))
levels(as.factor(ikke_arbutd_innvkat_land$fylke_nr))
levels(as.factor(ikke_arbutd_innvkat_land$innvkat_3))
levels(as.factor(ikke_arbutd_innvkat_land$vreg_3))
levels(as.factor(ikke_arbutd_innvkat_land$kjonn))
levels(as.factor(ikke_arbutd_innvkat_land$aar))
levels(as.factor(ikke_arbutd_innvkat_land$enhet))
#for testing i miljø
ikke_arbutd_innvkat_land$tabellvariabel[ikke_arbutd_innvkat_land$enhet=="prosent"]=extract_numeric(ikke_arbutd_innvkat_land$tabellvariabel[ikke_arbutd_innvkat_land$enhet=="prosent"])*100
write.csv(ikke_arbutd_innvkat_land,"D:/R/imdikator-munch/data_flat_output/ikke_arbutd_innvkat_land-fylke-2006_2014.csv",row.names=F)

#næringsregion
ikke_arbutd_innvkat_land <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/Tab_36_65d.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
nlevels(as.factor(ikke_arbutd_innvkat_land$naringsregion_nr))
levels(as.factor(ikke_arbutd_innvkat_land$naringsregion_nr))
levels(as.factor(ikke_arbutd_innvkat_land$innvkat_3))
levels(as.factor(ikke_arbutd_innvkat_land$kjonn))
levels(as.factor(ikke_arbutd_innvkat_land$vreg_3))
levels(as.factor(ikke_arbutd_innvkat_land$aar))
levels(as.factor(ikke_arbutd_innvkat_land$enhet))
#for testing i miljø
ikke_arbutd_innvkat_land$tabellvariabel[ikke_arbutd_innvkat_land$enhet=="prosent"]=extract_numeric(ikke_arbutd_innvkat_land$tabellvariabel[ikke_arbutd_innvkat_land$enhet=="prosent"])*100
write.csv(ikke_arbutd_innvkat_land,"D:/R/imdikator-munch/data_flat_output/ikke_arbutd_innvkat_land-naringsregion-2006_2014.csv",row.names=F)

#ikke_arbutd_innvkat_alder
library(tidyr)
#kommune
ikke_arbutd_innvkat_alder <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/Tab_37_66a.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
nlevels(as.factor(ikke_arbutd_innvkat_alder$kommune_nr))
levels(as.factor(ikke_arbutd_innvkat_alder$kommune_nr))
levels(as.factor(ikke_arbutd_innvkat_alder$innvkat_3))
levels(as.factor(ikke_arbutd_innvkat_alder$aldersinndeling_etter_grskole))
levels(as.factor(ikke_arbutd_innvkat_alder$kjonn))
levels(as.factor(ikke_arbutd_innvkat_alder$enhet))
#logisk sjekk
df=subset(ikke_arbutd_innvkat_alder,aar=="2014")
nrow(df)==nlevels(as.factor(df[,2]))*3*3*3*2
#substanssjekk - populasjonen
df=subset(ikke_arbutd_innvkat_alder,aar=="2013"&kommune_nr=="0000"&innvkat_3=="innvandrere")
df=subset(ikke_arbutd_innvkat_alder,aar=="2013"&kommune_nr=="0000"&aldersinndeling_etter_grskole=="alle")
#for testing i miljø
ikke_arbutd_innvkat_alder$tabellvariabel[ikke_arbutd_innvkat_alder$enhet=="prosent"]=extract_numeric(ikke_arbutd_innvkat_alder$tabellvariabel[ikke_arbutd_innvkat_alder$enhet=="prosent"])*100
write.csv(ikke_arbutd_innvkat_alder,"D:/R/imdikator-munch/data_flat_output/ikke_arbutd_innvkat_alder-kommune-2006_2014.csv",row.names=F)

#bydel
ikke_arbutd_innvkat_alder <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/Tab_38_66b.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
nlevels(as.factor(ikke_arbutd_innvkat_alder$bydel_nr))
levels(as.factor(ikke_arbutd_innvkat_alder$bydel_nr))
levels(as.factor(ikke_arbutd_innvkat_alder$innvkat_2))
levels(as.factor(ikke_arbutd_innvkat_alder$kjonn))
levels(as.factor(ikke_arbutd_innvkat_alder[,5]))
levels(as.factor(ikke_arbutd_innvkat_alder$aar))
levels(as.factor(ikke_arbutd_innvkat_alder$enhet))
#for testing i miljø
ikke_arbutd_innvkat_alder$tabellvariabel[ikke_arbutd_innvkat_alder$enhet=="prosent"]=extract_numeric(ikke_arbutd_innvkat_alder$tabellvariabel[ikke_arbutd_innvkat_alder$enhet=="prosent"])*100
write.csv(ikke_arbutd_innvkat_alder,"D:/R/imdikator-munch/data_flat_output/ikke_arbutd_innvkat_alder-bydel-2006_2014.csv",row.names=F)

#fylke
ikke_arbutd_innvkat_alder <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/Tab_39_66c.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
nlevels(as.factor(ikke_arbutd_innvkat_alder$fylke_nr))
levels(as.factor(ikke_arbutd_innvkat_alder$fylke_nr))
levels(as.factor(ikke_arbutd_innvkat_alder$innvkat_2))
levels(as.factor(ikke_arbutd_innvkat_alder[,5]))
levels(as.factor(ikke_arbutd_innvkat_alder$kjonn))
levels(as.factor(ikke_arbutd_innvkat_alder$aar))
levels(as.factor(ikke_arbutd_innvkat_alder$enhet))
#for testing i miljø
ikke_arbutd_innvkat_alder$tabellvariabel[ikke_arbutd_innvkat_alder$enhet=="prosent"]=extract_numeric(ikke_arbutd_innvkat_alder$tabellvariabel[ikke_arbutd_innvkat_alder$enhet=="prosent"])*100
write.csv(ikke_arbutd_innvkat_alder,"D:/R/imdikator-munch/data_flat_output/ikke_arbutd_innvkat_alder-fylke-2006_2014.csv",row.names=F)

#næringsregion
ikke_arbutd_innvkat_alder <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/Tab_40_66d.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
nlevels(as.factor(ikke_arbutd_innvkat_alder$naringsregion_nr))
levels(as.factor(ikke_arbutd_innvkat_alder$naringsregion_nr))
levels(as.factor(ikke_arbutd_innvkat_alder$innvkat_2))
levels(as.factor(ikke_arbutd_innvkat_alder$kjonn))
levels(as.factor(ikke_arbutd_innvkat_alder$vreg_3))
levels(as.factor(ikke_arbutd_innvkat_alder$aar))
levels(as.factor(ikke_arbutd_innvkat_alder$enhet))
#for testing i miljø
ikke_arbutd_innvkat_alder$tabellvariabel[ikke_arbutd_innvkat_alder$enhet=="prosent"]=extract_numeric(ikke_arbutd_innvkat_alder$tabellvariabel[ikke_arbutd_innvkat_alder$enhet=="prosent"])*100
write.csv(ikke_arbutd_innvkat_alder,"D:/R/imdikator-munch/data_flat_output/ikke_arbutd_innvkat_alder-naringsregion-2006_2014.csv",row.names=F)

#VOKSNE_VIDEREGAENDE
library(tidyr)
#2013
voksne_videregaende <- read.csv("D:/R/imdikator-munch/data_flat_input/tab36_2012.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
#alle geografiske nivå i ei fil
#aar må kodes -> 2013
#invkat->innvkat
names(voksne_videregaende)[7]="innvkat_3"
nlevels(as.factor(voksne_videregaende$kommune_nr))
levels(as.factor(voksne_videregaende$naringsregion_nr))
levels(as.factor(voksne_videregaende$fylke_nr))
levels(as.factor(voksne_videregaende$bydel_nr))
levels(as.factor(voksne_videregaende$innvkat_3))
levels(as.factor(voksne_videregaende$kjonn))
levels(as.factor(voksne_videregaende$enhet))
levels(as.factor(voksne_videregaende$aar))
voksne_videregaende$aar = "2013"
nrow(subset(voksne_videregaende,kommune_nr!="NULL"))==(3*3*2*425)
nrow(subset(voksne_videregaende,fylke_nr!="NULL"))==(3*3*2*20)
nrow(subset(voksne_videregaende,naringsregion_nr!="NULL"))==(3*3*2*83)
nrow(subset(voksne_videregaende,bydel_nr!="NULL"))==(3*3*2*18)
#lopsided på en ukjent måte.
write.csv(voksne_videregaende,"D:/R/imdikator-munch/data_flat_output/voksne_videregaende-alle-2013.csv",row.names=F)

#2014
voksne_videregaende <- read.csv("D:/R/imdikator-munch/data_flat_input/tab36_2013.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
#alle geografiske nivå i ei fil
#aar må kodes -> 2013
#invkat->innvkat
names(voksne_videregaende)[7]="innvkat_3"
nlevels(as.factor(voksne_videregaende$kommune_nr))
nlevels(as.factor(voksne_videregaende$naringsregion_nr))
levels(as.factor(voksne_videregaende$naringsregion_nr))
nlevels(as.factor(voksne_videregaende$fylke_nr))
levels(as.factor(voksne_videregaende$fylke_nr))
nlevels(as.factor(voksne_videregaende$bydel_nr))
levels(as.factor(voksne_videregaende$bydel_nr))
levels(as.factor(voksne_videregaende$innvkat_3))
levels(as.factor(voksne_videregaende$kjonn))
levels(as.factor(voksne_videregaende$enhet))
levels(as.factor(voksne_videregaende$aar))
voksne_videregaende$aar = "2014"
nrow(subset(voksne_videregaende,kommune_nr!="NULL"))==(3*3*2*425)
nrow(subset(voksne_videregaende,fylke_nr!="NULL"))==(3*3*2*20)
nrow(subset(voksne_videregaende,naringsregion_nr!="NULL"))==(3*3*2*83)
nrow(subset(voksne_videregaende,bydel_nr!="NULL"))==(3*3*2*18)
#lopsided på en ukjent måte.
write.csv(voksne_videregaende,"D:/R/imdikator-munch/data_flat_output/voksne_videregaende-alle-2014.csv",row.names=F)


#utdanningsniva
#kjørt 16. desember, og på nytt 25. januar 2016
#2009
utdanningsniva <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/tab37_2009.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
names(utdanningsniva)
levels(as.factor(utdanningsniva$bydel_nr))
df=subset(utdanningsniva,utdanningsniva$bydel_nr=="1001")

#test av fylket
df = subset(utdanningsniva,fylke_nr!="NULL")
levels(as.factor(df$invkat_3))
levels(as.factor(df$kjonn))
levels(as.factor(df$utd_5))
nrow(df)-nlevels(as.factor(df$fylke_nr))*3*3*5*2
df_2 = spread(df,utd_5,tabellvariabel)

df = subset(utdanningsniva,naringsregion_nr!="NULL")
nrow(df)-nlevels(as.factor(df$naringsregion_nr))*3*3*5*2

df = subset(utdanningsniva,kommune_nr!="NULL")
nrow(df)-nlevels(as.factor(df$kommune_nr))*3*3*5*2

#2010
utdanningsniva <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/tab37_2010.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
names(utdanningsniva)
levels(as.factor(utdanningsniva$bydel_nr))
df = subset(utdanningsniva,fylke_nr!="NULL")
nrow(df)-nlevels(as.factor(df$fylke_nr))*3*3*5*2

#2011
utdanningsniva <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/tab37_2011.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
names(utdanningsniva)
levels(as.factor(utdanningsniva$bydel_nr))
df = subset(utdanningsniva,fylke_nr!="NULL")
nrow(df)-nlevels(as.factor(df$fylke_nr))*3*3*5*2

#2012
utdanningsniva <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/tab37_2012.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
names(utdanningsniva)
levels(as.factor(utdanningsniva$bydel_nr))
df = subset(utdanningsniva,fylke_nr!="NULL")
nrow(df)-nlevels(as.factor(df$fylke_nr))*3*3*5*2

#2013
utdanningsniva <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/tab37_2013.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
names(utdanningsniva)
levels(as.factor(utdanningsniva$bydel_nr))
df = subset(utdanningsniva,fylke_nr!="NULL")
nrow(df)-nlevels(as.factor(df$fylke_nr))*3*3*5*2

#2014
utdanningsniva <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/tab37_2014.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
names(utdanningsniva)
levels(as.factor(utdanningsniva$bydel_nr))
df = subset(utdanningsniva,fylke_nr!="NULL")
nrow(df)-nlevels(as.factor(df$fylke_nr))*3*3*5*2

#geoenheter * 5 innvkat * 3 kjønn * 2 enheter
df = subset(videregaende_fullfort,fylke_nr!="NULL")
nrow(df)-nlevels(as.factor(df$fylke_nr))*5*3*2

df = subset(videregaende_fullfort,naringsregion_nr!="NULL")
nrow(df)-nlevels(as.factor(df$naringsregion_nr))*5*3*2

df = subset(videregaende_fullfort,kommune_nr!="NULL")
nrow(df)-nlevels(as.factor(df$kommune_nr))*5*3*2


#VIDEREGÅENDE_DELTAKELSE
#2011-2013
#årstallet er feil, må kodes som siste år i perioden
videregaende_deltakelse <- read.csv("D:/R/imdikator-munch/data_flat_input/videregaende_deltakelse-alle-2011_2013.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
videregaende_deltakelse$aar = "2013"
write.csv(videregaende_deltakelse,"D:/R/imdikator-munch/data_flat_output/videregaende_deltakelse-alle-2011_2013.csv",row.names=F)
#2012-2014
videregaende_deltakelse <- read.csv("D:/R/imdikator-munch/data_flat_input/videregaende_deltakelse-alle-2012_2014.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
videregaende_deltakelse$aar = "2014"
write.csv(videregaende_deltakelse,"D:/R/imdikator-munch/data_flat_output/videregaende_deltakelse-alle-2012_2014.csv",row.names=F)

#BOSATT_ANMODEDE
#21. januar 2016
#1. kommunedata 2014
#2. andre nivåer 2014
#3. kommunedata 2015-2017
#4. fylkesdata 2015-2017
#5. andre nivåer 2015-2017

#kommune 2014
library(tidyr)
bosatt_anmodede <- read.csv("D:/R/imdikator-munch/data_crossed_input/bosatt_anmodede-kommune-2014.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=";", dec=".",colClasses="character")
bosatt_anmodede = subset(bosatt_anmodede,select=c(1,3,4,5))
names(bosatt_anmodede)=c("kommune_nr","anmodning_personer","vedtak_personer","bosatt_personer")
bosatt_anmodede$anmodning_prosent = (as.numeric(bosatt_anmodede$anmodning_personer)/as.numeric(bosatt_anmodede$anmodning_personer))*100
bosatt_anmodede$vedtak_prosent = (as.numeric(bosatt_anmodede$vedtak_personer)/as.numeric(bosatt_anmodede$anmodning_personer))*100
bosatt_anmodede$bosatt_prosent = (as.numeric(bosatt_anmodede$bosatt_personer)/as.numeric(bosatt_anmodede$anmodning_personer))*100
df = gather(bosatt_anmodede,"variabler","tabellvariabel",2:7)
df = separate(df,variabler,c("bosetting","enhet"),sep="_")
df$tabellvariabel[is.na(df$tabellvariabel)==T]="."
#logisk sjekk
nrow(df)==429*3*2
#resterende
df$tabell_navn="bosatt_anmodede"
df$aar="2014"
write.csv(df,"D:/R/imdikator-munch/data_flat_output/bosetting_anmodede-kommune-2014.csv",row.names=F)

#næringsregion 2014
kinfo <- read.csv("D:/R/imdikator-munch/parameters/kommunesort.csv", sep=";", stringsAsFactors=FALSE)
kinfo = subset(kinfo,select=c("Nr","Naringregnr"))
names(kinfo)[2] = "naringsregion_nr"
kinfo$Nr[nchar(kinfo$Nr)==3] = paste0("0",kinfo$Nr[nchar(kinfo$Nr)==3])
kinfo$naringsregion_nr[nchar(kinfo$naringsregion_nr)==1] = paste0("0",kinfo$naringsregion_nr[nchar(kinfo$naringsregion_nr)==1])
bosatt_anmodede <- read.csv("D:/R/imdikator-munch/data_crossed_input/bosatt_anmodede-kommune-2014-tilaggregering.csv", row.names=NULL, na.strings=c("NA"), stringsAsFactors=FALSE, sep=";", dec=".")
sum(is.na(bosatt_anmodede))
sum(bosatt_anmodede==":")
sum(bosatt_anmodede==".")
#skal ikke leses inn som character, men med NA.strings ="."&":"
#logikken er at det ikke er : på dette nivået, fordi tallene skal aggregeres - det er bare missing
#og hvis det er ":", så skal disse uansett bli . på aggregert n ivå
bosatt_anmodede <- read.csv("D:/R/imdikator-munch/data_crossed_input/bosatt_anmodede-kommune-2014-tilaggregering.csv", row.names=NULL, na.strings=c("NA","."), stringsAsFactors=FALSE, sep=";", dec=".")
checksum_start = sum(bosatt_anmodede$bosatt_2014,na.rm=T)
bosatt_anmodede$kommune_nr[nchar(bosatt_anmodede$kommune_nr)==3] = paste0("0",bosatt_anmodede$kommune_nr[nchar(bosatt_anmodede$kommune_nr)==3])
#vedtak kan være ".", hvis henvendelse ikke er besvart-.
#bosetting og anmodning kan ikke være ".", skal være 0
sum(is.na(bosatt_anmodede$bosatt_2014)==T)
bosatt_anmodede$bosatt_2014[is.na(bosatt_anmodede$bosatt_2014)==T]=0
sum(is.na(bosatt_anmodede$anmodning_2014)==T)
bosatt_anmodede$anmodning_2014[is.na(bosatt_anmodede$anmodning_2014)==T]=0
sum(is.na(bosatt_anmodede$vedtak_2014)==T)
bosatt_anmodede$vedtak_2014[is.na(bosatt_anmodede$vedtak_2014)==T]=0
df = merge(bosatt_anmodede,kinfo,by.x="kommune_nr",by.y ="Nr",all.x=T,all.y=T)
library(reshape)
bosatt_anmodede_m = melt.data.frame(df,id.vars=c("kommune_nr","naringsregion_nr"),na.rm=F)
bosatt_anmodede_nareg = cast(bosatt_anmodede_m,naringsregion_nr~variable,fun.aggregate=sum,na.rm=F,add.missing=T,fill=NA)
checksum_start == sum(bosatt_anmodede_nareg$bosatt_2014)
names(bosatt_anmodede_nareg)=c("naringsregion_nr","bosatt_personer","anmodning_personer","vedtak_personer")
bosatt_anmodede_nareg$anmodning_prosent = (as.numeric(bosatt_anmodede_nareg$anmodning_personer)/as.numeric(bosatt_anmodede_nareg$anmodning_personer))*100
bosatt_anmodede_nareg$vedtak_prosent = (as.numeric(bosatt_anmodede_nareg$vedtak_personer)/as.numeric(bosatt_anmodede_nareg$anmodning_personer))*100
bosatt_anmodede_nareg$bosatt_prosent = (as.numeric(bosatt_anmodede_nareg$bosatt_personer)/as.numeric(bosatt_anmodede_nareg$anmodning_personer))*100
df = gather(bosatt_anmodede_nareg,"variabler","tabellvariabel",2:7)
df = separate(df,variabler,c("bosetting","enhet"),sep="_")
df$tabellvariabel[is.na(df$tabellvariabel)==T]="."
#logisk sjekk
nrow(df)==84*3*2
#resterende
df$tabell_navn="bosatt_anmodede"
df$aar="2014"
write.csv(df,"D:/R/imdikator-munch/data_flat_output/bosetting_anmodede-naringsregion-2014.csv",row.names=F)

#fylke 2014
kinfo = read.csv("D:/R/imdikator-munch/parameters/kommunesort.csv", sep=";", stringsAsFactors=FALSE)
kinfo = subset(kinfo,select=c("Nr","Fylkenr"))
names(kinfo)[2] = "fylke_nr"
kinfo$fylke_nr[nchar(kinfo$fylke_nr)==1] = paste0("0",kinfo$fylke_nr[nchar(kinfo$fylke_nr)==1])
#bosettingsdata spesielt og data for aggregering generelt skal ikke leses inn som character, men med NA.strings ="."&":"
#logikken er at det ikke er : på dette nivået, fordi tallene skal aggregeres - det er bare missing
#og hvis det er ":", så skal disse uansett bli . på aggregert n ivå
bosatt_anmodede <- read.csv("D:/R/imdikator-munch/data_crossed_input/bosatt_anmodede-kommune-2014-tilaggregering.csv", row.names=NULL, na.strings=c("NA","."), stringsAsFactors=FALSE, sep=";", dec=".")
checksum_start = sum(bosatt_anmodede$bosatt_2014,na.rm=T)
#vedtak kan være ".", hvis henvendelse ikke er besvart-.
#bosetting og anmodning kan ikke være ".", skal være 0
sum(is.na(bosatt_anmodede$bosatt_2014)==T)
bosatt_anmodede$bosatt_2014[is.na(bosatt_anmodede$bosatt_2014)==T]=0
sum(is.na(bosatt_anmodede$anmodning_2014)==T)
bosatt_anmodede$anmodning_2014[is.na(bosatt_anmodede$anmodning_2014)==T]=0
sum(is.na(bosatt_anmodede$vedtak_2014)==T)
bosatt_anmodede$vedtak_2014[is.na(bosatt_anmodede$vedtak_2014)==T]=0
df = merge(bosatt_anmodede,kinfo,by.x="kommune_nr",by.y ="Nr",all.x=T,all.y=T)
library(reshape)
bosatt_anmodede_m = melt.data.frame(df,id.vars=c("kommune_nr","fylke_nr"),na.rm=F)
bosatt_anmodede_fylke = cast(bosatt_anmodede_m,fylke_nr~variable,fun.aggregate=sum,na.rm=F,add.missing=T,fill=NA, margins="grand_row")
bosatt_anmodede_fylke = subset(bosatt_anmodede_fylke,select=-5)
checksum_start == sum(bosatt_anmodede_fylke$bosatt_2014)/2
names(bosatt_anmodede_fylke)=c("fylke_nr","bosatt_personer","anmodning_personer","vedtak_personer")
bosatt_anmodede_fylke$anmodning_prosent = (as.numeric(bosatt_anmodede_fylke$anmodning_personer)/as.numeric(bosatt_anmodede_fylke$anmodning_personer))*100
bosatt_anmodede_fylke$vedtak_prosent = (as.numeric(bosatt_anmodede_fylke$vedtak_personer)/as.numeric(bosatt_anmodede_fylke$anmodning_personer))*100
bosatt_anmodede_fylke$bosatt_prosent = (as.numeric(bosatt_anmodede_fylke$bosatt_personer)/as.numeric(bosatt_anmodede_fylke$anmodning_personer))*100
df = gather(bosatt_anmodede_fylke,"variabler","tabellvariabel",2:7)
df = separate(df,variabler,c("bosetting","enhet"),sep="_")
sum(is.na(df$tabellvariabel)==T)
df$tabellvariabel[is.na(df$tabellvariabel)==T]="."
#logisk sjekk
nrow(df)==21*3*2
#resterende
df$tabell_navn="bosatt_anmodede"
df$aar="2014"
#.==regexp any single character
df$fylke_nr = gsub(".all.","00",df$fylke_nr)
write.csv(df,"D:/R/imdikator-munch/data_flat_output/bosetting_anmodede-fylke-2014.csv",row.names=F)

#3. kommunedata 2015-2017
bosatt_anmodede = read.csv("D:/R/imdikator-munch/data_crossed_input/bosatt_anmodede-kommune-2015_2017.csv", row.names=NULL, na.strings=c("NA",":","."), stringsAsFactors=FALSE, sep=";", dec=",")
checksum = sum(bosatt_anmodede$vedtak_2016_personer,na.rm=T)
df = gather(bosatt_anmodede,variables,tabellvariabel,2:19)
df = separate(df,variables,into=c("bosetting","aar","enhet"),sep="_")
nlevels(as.factor(df$kommune_nr))
#i anmodnings- og vedtakstall på kommunenivå 2015-2017 er bosetting manglende, anmodning ikke : eller . men 0, og vedtak enten 0 eller NA.
df$tabellvariabel[df$bosetting=="bosatt"&is.na(df$tabellvariabel)==T]="."
df$tabellvariabel[df$bosetting=="anmodning"&is.na(df$tabellvariabel)==T]=0
df$tabellvariabel[df$bosetting=="vedtak"&is.na(df$tabellvariabel)==T]="."
checksum == sum(as.numeric(df$tabellvariabel[df$bosetting=="vedtak"&df$aar=="2016"&df$enhet=="personer"]),na.rm=T)
#logisk sjekk
nrow(df)==428*3*3*2
#resterende
df$tabell_navn="bosatt_anmodede"
df$kommune_nr[nchar(df$kommune_nr)==3] = paste0("0",df$kommune_nr[nchar(df$kommune_nr)==3])
write.csv(df,"D:/R/imdikator-munch/data_flat_output/bosetting_anmodede-kommune-2015_2017.csv",row.names=F)

#4. fylkesdata 2015-2017
bosatt_anmodede = read.csv("D:/R/imdikator-munch/data_crossed_input/bosatt_anmodede-fylke-2015_2017.csv", row.names=NULL, na.strings=c("NA",":","."), stringsAsFactors=FALSE, sep=";", dec=",")
checksum = sum(bosatt_anmodede$vedtak_2016_personer,na.rm=T)/2
df = gather(bosatt_anmodede,variables,tabellvariabel,2:19)
df = separate(df,variables,into=c("bosetting","aar","enhet"),sep="_")
nlevels(as.factor(df$fylke_nr))
#i anmodnings- og vedtakstall på fylkenivå 2015-2017 er bosetting manglende, anmodning ikke : eller . men 0, og vedtak enten 0 eller NA.
df$tabellvariabel[df$bosetting=="bosatt"&is.na(df$tabellvariabel)==T]="."
df$tabellvariabel[df$bosetting=="anmodning"&is.na(df$tabellvariabel)==T]=0
df$tabellvariabel[df$bosetting=="vedtak"&is.na(df$tabellvariabel)==T]="."
checksum == sum(as.numeric(df$tabellvariabel[df$bosetting=="vedtak"&df$aar=="2016"&df$enhet=="personer"]),na.rm=T)
#logisk sjekk
nrow(df)==20*3*3*2
#resterende
df$tabell_navn="bosatt_anmodede"
df$fylke_nr[nchar(df$fylke_nr)==1] = paste0("0",df$fylke_nr[nchar(df$fylke_nr)==1])
write.csv(df,"D:/R/imdikator-munch/data_flat_output/bosetting_anmodede-fylke-2015_2017.csv",row.names=F)

#5. næringsregion 2015-2017
bosatt_anmodede = read.csv("D:/R/imdikator-munch/data_crossed_input/bosatt_anmodede-kommune-2015_2017.csv", row.names=NULL, na.strings=c("NA",":","."), stringsAsFactors=FALSE, sep=";", dec=",")
checksum = sum(bosatt_anmodede$anmodning_2016_personer,na.rm=T)
bosatt_anmodede = subset(bosatt_anmodede,select=1:10)
kinfo <- read.csv("D:/R/imdikator-munch/parameters/kommunesort.csv", sep=";", stringsAsFactors=FALSE)
kinfo = subset(kinfo,select=c("Nr","Naringregnr"))
names(kinfo)[2] = "naringsregion_nr"
kinfo$naringsregion_nr[nchar(kinfo$naringsregion_nr)==1] = paste0("0",kinfo$naringsregion_nr[nchar(kinfo$naringsregion_nr)==1])
df = merge(bosatt_anmodede,kinfo,by.x="kommune_nr",by.y ="Nr",all.x=T,all.y=T)
#vedtak kan være ".", hvis henvendelse ikke er besvart. ved aggregering av vedtak
# settes disse lik 0. det samme gjelder NA bosetting, og NA anmodning
#bosetting og anmodning kan ikke være ".", skal være 0
df[is.na(df)==T]=0
bosatt_anmodede_m = melt.data.frame(df,id.vars=c("kommune_nr","naringsregion_nr"),na.rm=F)
bosatt_anmodede_nareg = cast(bosatt_anmodede_m,naringsregion_nr~variable,fun.aggregate=sum,na.rm=F,add.missing=T,fill=NA)

bosatt_anmodede_nareg$anmodning_2015_prosent = (as.numeric(bosatt_anmodede_nareg$anmodning_2015_personer)/as.numeric(bosatt_anmodede_nareg$anmodning_2015_personer))*100
bosatt_anmodede_nareg$vedtak_2015_prosent = (as.numeric(bosatt_anmodede_nareg$vedtak_2015_personer)/as.numeric(bosatt_anmodede_nareg$anmodning_2015_personer))*100
bosatt_anmodede_nareg$bosatt_2015_prosent = (as.numeric(bosatt_anmodede_nareg$bosatt_2015_personer)/as.numeric(bosatt_anmodede_nareg$anmodning_2015_personer))*100
bosatt_anmodede_nareg$anmodning_2016_prosent = (as.numeric(bosatt_anmodede_nareg$anmodning_2016_personer)/as.numeric(bosatt_anmodede_nareg$anmodning_2016_personer))*100
bosatt_anmodede_nareg$vedtak_2016_prosent = (as.numeric(bosatt_anmodede_nareg$vedtak_2016_personer)/as.numeric(bosatt_anmodede_nareg$anmodning_2016_personer))*100
bosatt_anmodede_nareg$bosatt_2016_prosent = (as.numeric(bosatt_anmodede_nareg$bosatt_2016_personer)/as.numeric(bosatt_anmodede_nareg$anmodning_2016_personer))*100
bosatt_anmodede_nareg$anmodning_2017_prosent = (as.numeric(bosatt_anmodede_nareg$anmodning_2017_personer)/as.numeric(bosatt_anmodede_nareg$anmodning_2017_personer))*100
bosatt_anmodede_nareg$vedtak_2017_prosent = (as.numeric(bosatt_anmodede_nareg$vedtak_2017_personer)/as.numeric(bosatt_anmodede_nareg$anmodning_2017_personer))*100
bosatt_anmodede_nareg$bosatt_2017_prosent = (as.numeric(bosatt_anmodede_nareg$bosatt_2017_personer)/as.numeric(bosatt_anmodede_nareg$anmodning_2017_personer))*100

df = gather(bosatt_anmodede_nareg,variables,tabellvariabel,2:19)
df = separate(df,variables,into=c("bosetting","aar","enhet"),sep="_")
nlevels(as.factor(df$naringsregion_nr))

#i anmodnings- og vedtakstall på naringsregion 2015-2017 er
# bosetting manglende (.), anmodning 0, og vedtak enten 0 eller ".".
df$tabellvariabel[df$bosetting=="bosatt"&df$tabellvariabel==0]="."
df$tabellvariabel[df$bosetting=="vedtak"&df$tabellvariabel=="0"]="."
#logisk sjekk
nrow(df)==84*3*3*2
#resterende
df$tabell_navn="bosatt_anmodede"
write.csv(df,"D:/R/imdikator-munch/data_flat_output/bosetting_anmodede-naringsregion-2015_2017.csv",row.names=F)

#TILSKUDD - KOMMUNE 2015
#26. januar 2016
#1. tabellvariabel som skal være 0, er kodet NA
#2. spread kommer tilbake med duplicate identifiers - feil i grunnlagsdata med kommunenummer og totaler.
tilskudd <- read.csv("D:/R/imdikator-munch/data_flat_input/tilskudd-kommune-2015.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=";", dec=",")
#1. tilskuddsdataene har ikke skjulte eller manglende data
sum(is.na(tilskudd$tabellvariabel))
tilskudd$tabellvariabel[is.na(tilskudd$tabellvariabel)]=0
sum(is.na(tilskudd$tabellvariabel))
#logisk sjekk
nlevels(as.factor(tilskudd$kommune_nr))
nlevels(as.factor(tilskudd$tilskudd_til_kommuner))
# det er 11 kategorier tilskudd, en ny fra 2014
nrow(tilskudd)==424*11
#duplikatsjekk
df = spread(tilskudd,tilskudd_til_kommuner,tabellvariabel)
df = gather(df,tilskudd_til_kommuner,tabellvariabel,barnehage_tilsk:totalt)
#5 kommuner mottok ikke tilskudd fra IMDi i 2015. Disse bør legges til og kodes 0
kinfo <- read.csv("D:/R/imdikator-munch/parameters/kommunesort.csv", sep=";", stringsAsFactors=FALSE)
kinfo = subset(kinfo,select="Nr")
df = spread(tilskudd,tilskudd_til_kommuner,tabellvariabel)
df2 = merge(df,kinfo,by.x="kommune_nr",by.y="Nr",all.x=T,all.y=T)
df2 = gather(df2,tilskudd_til_kommuner,tabellvariabel,barnehage_tilsk:totalt)
#introduserer NA for kommunene som ikke har mottatt noen tilskudd
sum(is.na(df2$tabellvariabel))
df2$tabellvariabel[is.na(df2$tabellvariabel)]=0
sum(is.na(df2$tabellvariabel))
sum(is.na(df2$aar))
df2$aar[is.na(df2$aar)]=2015
sum(is.na(df2$aar))
sum(is.na(df2$enhet))
df2$enhet[is.na(df2$enhet)]="kroner"
sum(is.na(df2$enhet))
sum(is.na(df2$tabell_navn))
df2$tabell_navn[is.na(df2$tabell_navn)]="tilskudd"
sum(is.na(df2$tabell_navn))
sum(is.na(df2$tilskudd_til_kommuner))
sum(is.na(df2))
#logisk sjekk
nlevels(as.factor(df2$kommune_nr))
nlevels(as.factor(df2$tilskudd_til_kommuner)) # det er 11 kategorier tilskudd, en ny fra 201
nrow(df2)==429*11
#skriver ut fil
df2$kommune_nr[nchar(df2$kommune_nr)==3] = paste0("0",df2$kommune_nr[nchar(df2$kommune_nr)==3])
options(scipen = 999)
write.csv(df2,"D:/R/imdikator-munch/data_flat_output/tilskudd-kommune-2015.csv",row.names=F)
#problemer med scientific notation i output
#setter retta tilskuddsfil lik tilskudd
tilskudd = df2

#AGGREGERING TILSKUDD 2015
#26. januar, 3. februar
#næringsregion
library(reshape)
library(tidyr)
tilskudd <- read.csv("D:/R/imdikator-munch/data_flat_output/tilskudd-kommune-2015.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".")
kinfo <- read.csv("D:/R/imdikator-munch/parameters/kommunesort.csv", sep=";", stringsAsFactors=FALSE)
kinfo = subset(kinfo,select=c("Nr","Naringregnr"))
names(kinfo)[2] = "naringsregion_nr"
kinfo$naringsregion_nr[nchar(kinfo$naringsregion_nr)==1] = paste0("0",kinfo$naringsregion_nr[nchar(kinfo$naringsregion_nr)==1])
checksum_start = sum(tilskudd$tabellvariabel)
#input for aggregering bør være krysstabellen, ikke flatfila.
tilskudd = spread(tilskudd,tilskudd_til_kommuner,tabellvariabel)
df = merge(tilskudd,kinfo,by.x="kommune_nr",by.y ="Nr",all.x=T,all.y=T)
tilskudd_data = melt.data.frame(df,id.vars=c("kommune_nr","naringsregion_nr","aar","tabell_navn","enhet"),na.rm=F)
tilskudd_nareg = cast(tilskudd_data,naringsregion_nr~variable,fun.aggregate=sum,na.rm=F,add.missing=T,fill=NA)
df = gather(tilskudd_nareg,"tilskudd_til_kommuner","tabellvariabel",barnehage_tilsk:totalt)
checksum_start == sum(df$tabellvariabel)
#logisk sjekk
nrow(df)==84*11
#resterende
df$tabell_navn="tilskudd"
df$aar="2015"
df$enhet="kroner"
write.csv(df,"D:/R/imdikator-munch/data_flat_output/tilskudd-naringsregion-2015.csv",row.names=F)

#fylke 2014
tilskudd <- read.csv("D:/R/imdikator-munch/data_flat_output/tilskudd-kommune-2015.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".")
kinfo <- read.csv("D:/R/imdikator-munch/parameters/kommunesort.csv", sep=";", stringsAsFactors=FALSE)
kinfo = subset(kinfo,select=c("Nr","Fylkenr"))
names(kinfo)[2] = "fylke_nr"
kinfo$fylke_nr[nchar(kinfo$fylke_nr)==1] = paste0("0",kinfo$fylke_nr[nchar(kinfo$fylke_nr)==1])
checksum_start = sum(tilskudd$tabellvariabel)
#input for aggregering bør være krysstabellen, ikke flatfila.
tilskudd = spread(tilskudd,tilskudd_til_kommuner,tabellvariabel)
df = merge(tilskudd,kinfo,by.x="kommune_nr",by.y ="Nr",all.x=T,all.y=T)
tilskudd_data = melt.data.frame(df,id.vars=c("kommune_nr","fylke_nr","aar","tabell_navn","enhet"),na.rm=F)
tilskudd_fylke = cast(tilskudd_data,fylke_nr~variable,fun.aggregate=sum,na.rm=F,add.missing=T,fill=NA,margins="grand_row")
tilskudd_fylke = subset(tilskudd_fylke,select=-13)
tilskudd_fylke$fylke_nr = gsub(".all.","00",tilskudd_fylke$fylke_nr)
df = gather(tilskudd_fylke,"tilskudd_til_kommuner","tabellvariabel",barnehage_tilsk:totalt)
checksum_start == sum(df$tabellvariabel)/2
#logisk sjekk
nrow(df)==21*11
#resterende
df$tabell_navn="tilskudd"
df$aar="2015"
df$enhet="kroner"
write.csv(df,"D:/R/imdikator-munch/data_flat_output/tilskudd-fylke-2015.csv",row.names=F)

#TILSKUDD
#fiks av manglende summer
options(scipen = 999)
#kommune
library(tidyr)
tilskudd <- read.csv("D:/R/imdikator-munch/data_flat_input/tilskudd-kommune-2015.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",fileEncoding = "UTF-8-BOM")
df=spread(tilskudd,tilskudd_til_kommuner,tabellvariabel)
#først ny inttilsk_1_5
df$inttilsk_1_5_ny = df$inttilsk_1+df$inttilsk_2_5
df$sjekk = df$inttilsk_1_5_ny-df$inttilsk_1_5
df$inttilsk_1_5 = df$inttilsk_1_5_ny
df = subset(df,select=c(-inttilsk_1_5_ny,-sjekk))
df$totalt_2=extract_numeric(df[,5])+extract_numeric(df[,6])+extract_numeric(df[,7])+extract_numeric(df[,8])+extract_numeric(df[,9])+extract_numeric(df[,11])+extract_numeric(df[,13])+extract_numeric(df[,14])
df$sjekk = df$totalt-df$totalt_2
#noen avvik
write.csv(df,"D:/R/imdikator-munch/data_crossed_output/tilskudd-kommune-2015.csv",row.names=F)
checksum_k = sum(df$totalt_2)
sum(is.na(df$totalt_2))
df$totalt=df$totalt_2
df = subset(df,select=-c(totalt_2,sjekk))
df=gather(df,tilskudd_til_kommuner,tabellvariabel,barnehage_tilsk:totalt)
df$kommune_nr[nchar(df$kommune_nr)==3] = paste0("0",df$kommune_nr[nchar(df$kommune_nr)==3])
write.csv(df,"D:/R/imdikator-munch/data_flat_output/tilskudd-kommune-2015.csv",row.names=F)

#fylke
library(tidyr)
tilskudd <- read.csv("D:/R/imdikator-munch/data_flat_input/tilskudd-fylke-2015.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",fileEncoding = "UTF-8-BOM")
df=spread(tilskudd,tilskudd_til_kommuner,tabellvariabel)
df$totalt_2=extract_numeric(df[,5])+extract_numeric(df[,6])+extract_numeric(df[,7])+extract_numeric(df[,8])+extract_numeric(df[,9])+extract_numeric(df[,11])+extract_numeric(df[,13])+extract_numeric(df[,14])
df$sjekk = df$totalt-df$totalt_2
#noen avvik
write.csv(df,"D:/R/imdikator-munch/data_crossed_output/tilskudd-fylke-2015.csv",row.names=F)
checksum_f = sum(df$totalt_2[df$fylke_nr!=00])
sum(is.na(df$totalt_2))
df$totalt=df$totalt_2
df = subset(df,select=-c(totalt_2,sjekk))
df=gather(df,tilskudd_til_kommuner,tabellvariabel,barnehage_tilsk:totalt)
df$fylke_nr[nchar(df$fylke_nr)==1] = paste0("0",df$fylke_nr[nchar(df$fylke_nr)==1])
write.csv(df,"D:/R/imdikator-munch/data_flat_output/tilskudd-fylke-2015.csv",row.names=F)

#næringsregion
library(tidyr)
tilskudd <- read.csv("D:/R/imdikator-munch/data_flat_input/tilskudd-naringsregion-2015.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",fileEncoding = "UTF-8-BOM")
df=spread(tilskudd,tilskudd_til_kommuner,tabellvariabel)
df$totalt_2=extract_numeric(df[,5])+extract_numeric(df[,6])+extract_numeric(df[,7])+extract_numeric(df[,8])+extract_numeric(df[,9])+extract_numeric(df[,11])+extract_numeric(df[,13])+extract_numeric(df[,14])
df$sjekk = df$totalt-df$totalt_2
#noen avvik
write.csv(df,"D:/R/imdikator-munch/data_crossed_output/tilskudd-naringsregion-2015.csv",row.names=F)
checksum_n = sum(df$totalt_2)
sum(is.na(df$totalt_2))
df$totalt=df$totalt_2
df = subset(df,select=-c(totalt_2,sjekk))
df=gather(df,tilskudd_til_kommuner,tabellvariabel,barnehage_tilsk:totalt)
df$naringsregion_nr[nchar(df$naringsregion_nr)==1] = paste0("0",df$naringsregion_nr[nchar(df$naringsregion_nr)==1])
write.csv(df,"D:/R/imdikator-munch/data_flat_output/tilskudd-naringsregion-2015.csv",row.names=F)

#UTDANNINGSNIVA - omregning av prosent
#26. januar
#prosentene er regnet av totalen, ikke gruppetotalen.
#utfordringen er å ta vare på informasjon om ":" og ".", men få gjort beregninger
#2009
utdanningsniva <- read.csv("D:/R/imdikator-munch/data_flat_input/utdanningsniva-alle-2009.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
checksum_1 = nrow(utdanningsniva)
checksum_2 = sum(extract_numeric(utdanningsniva$tabellvariabel[utdanningsniva$enhet=="personer"]),na.rm=T)
#df = recast(subset(utdanningsniva,fylke_nr!="NULL"),fylke_nr~variable+enhet+innvkat_3+kjonn+utd_5,measure.var = "tabellvariabel")
df = spread(utdanningsniva,utd_5,tabellvariabel)
df = subset(df,enhet=="personer",select=-enhet)
names(df)[9:13]=c("grunnskole.personer","ingen.personer","universitet_og_hogskole.personer","uoppgitt.personer","vgs.personer")
df$totalt.personer = extract_numeric(df[,9])+extract_numeric(df[,10])+extract_numeric(df[,11])+extract_numeric(df[,12])+extract_numeric(df[,13])
df$grunnskole.prosent = extract_numeric(df$grunnskole.personer)/df$totalt.personer*100
df$ingen.prosent = extract_numeric(df$ingen.personer)/df$totalt.personer*100
df$universitet_og_hogskole.prosent = extract_numeric(df$universitet_og_hogskole.personer)/df$totalt.personer*100
df$uoppgitt.prosent = extract_numeric(df$uoppgitt.personer)/df$totalt.personer*100
df$vgs.prosent = extract_numeric(df$vgs.personer)/df$totalt.personer*100
df$totalt.prosent = df[,15]+df[,16]+df[,17]+df[,18]+df[,19]
df2 = gather(df,variabler,tabellvariabel,9:20)
df2=separate(df2,variabler,c("utd5","enhet"),"\\.")
df2 = subset(df2,utd5!="totalt")
#alle NAs behandles som manglende data
sum(is.na(df2))
sum(is.na(df2$tabellvariabel))
df2$tabellvariabel[is.na(df2$tabellvariabel)==T]="."
sum(is.na(df2$tabellvariabel))
#logisk sjekk
checksum_1 == nrow(df2)
checksum_2 == sum(extract_numeric(df2$tabellvariabel[df2$enhet=="personer"]),na.rm=T)
write.csv(df2,"D:/R/imdikator-munch/data_flat_output/utdanningsniva-alle-2009.csv",row.names=F)

#2010
utdanningsniva <- read.csv("D:/R/imdikator-munch/data_flat_input/utdanningsniva-alle-2010.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
checksum_1 = nrow(utdanningsniva)
checksum_2 = sum(extract_numeric(utdanningsniva$tabellvariabel[utdanningsniva$enhet=="personer"]),na.rm=T)
#df = recast(subset(utdanningsniva,fylke_nr!="NULL"),fylke_nr~variable+enhet+innvkat_3+kjonn+utd_5,measure.var = "tabellvariabel")
df = spread(utdanningsniva,utd_5,tabellvariabel)
df = subset(df,enhet=="personer",select=-enhet)
names(df)[9:13]=c("grunnskole.personer","ingen.personer","universitet_og_hogskole.personer","uoppgitt.personer","vgs.personer")
df$totalt.personer = extract_numeric(df[,9])+extract_numeric(df[,10])+extract_numeric(df[,11])+extract_numeric(df[,12])+extract_numeric(df[,13])
df$grunnskole.prosent = extract_numeric(df$grunnskole.personer)/df$totalt.personer*100
df$ingen.prosent = extract_numeric(df$ingen.personer)/df$totalt.personer*100
df$universitet_og_hogskole.prosent = extract_numeric(df$universitet_og_hogskole.personer)/df$totalt.personer*100
df$uoppgitt.prosent = extract_numeric(df$uoppgitt.personer)/df$totalt.personer*100
df$vgs.prosent = extract_numeric(df$vgs.personer)/df$totalt.personer*100
df$totalt.prosent = df[,15]+df[,16]+df[,17]+df[,18]+df[,19]
df2 = gather(df,variabler,tabellvariabel,9:20)
df2=separate(df2,variabler,c("utd5","enhet"),"\\.")
df2 = subset(df2,utd5!="totalt")
#alle NAs behandles som manglende data
sum(is.na(df2))
sum(is.na(df2$tabellvariabel))
df2$tabellvariabel[is.na(df2$tabellvariabel)==T]="."
sum(is.na(df2$tabellvariabel))
#logisk sjekk
checksum_1 == nrow(df2)
checksum_2 == sum(extract_numeric(df2$tabellvariabel[df2$enhet=="personer"]),na.rm=T)
write.csv(df2,"D:/R/imdikator-munch/data_flat_output/utdanningsniva-alle-2010.csv",row.names=F)

#2011
utdanningsniva <- read.csv("D:/R/imdikator-munch/data_flat_input/utdanningsniva-alle-2011.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
checksum_1 = nrow(utdanningsniva)
checksum_2 = sum(extract_numeric(utdanningsniva$tabellvariabel[utdanningsniva$enhet=="personer"]),na.rm=T)
#df = recast(subset(utdanningsniva,fylke_nr!="NULL"),fylke_nr~variable+enhet+innvkat_3+kjonn+utd_5,measure.var = "tabellvariabel")
df = spread(utdanningsniva,utd_5,tabellvariabel)
df = subset(df,enhet=="personer",select=-enhet)
names(df)[9:13]=c("grunnskole.personer","ingen.personer","universitet_og_hogskole.personer","uoppgitt.personer","vgs.personer")
df$totalt.personer = extract_numeric(df[,9])+extract_numeric(df[,10])+extract_numeric(df[,11])+extract_numeric(df[,12])+extract_numeric(df[,13])
df$grunnskole.prosent = extract_numeric(df$grunnskole.personer)/df$totalt.personer*100
df$ingen.prosent = extract_numeric(df$ingen.personer)/df$totalt.personer*100
df$universitet_og_hogskole.prosent = extract_numeric(df$universitet_og_hogskole.personer)/df$totalt.personer*100
df$uoppgitt.prosent = extract_numeric(df$uoppgitt.personer)/df$totalt.personer*100
df$vgs.prosent = extract_numeric(df$vgs.personer)/df$totalt.personer*100
df$totalt.prosent = df[,15]+df[,16]+df[,17]+df[,18]+df[,19]
df2 = gather(df,variabler,tabellvariabel,9:20)
df2=separate(df2,variabler,c("utd5","enhet"),"\\.")
df2 = subset(df2,utd5!="totalt")
#alle NAs behandles som manglende data
sum(is.na(df2))
sum(is.na(df2$tabellvariabel))
df2$tabellvariabel[is.na(df2$tabellvariabel)==T]="."
sum(is.na(df2$tabellvariabel))
#logisk sjekk
checksum_1 == nrow(df2)
checksum_2 == sum(extract_numeric(df2$tabellvariabel[df2$enhet=="personer"]),na.rm=T)
write.csv(df2,"D:/R/imdikator-munch/data_flat_output/utdanningsniva-alle-2011.csv",row.names=F)

#2012
utdanningsniva <- read.csv("D:/R/imdikator-munch/data_flat_input/utdanningsniva-alle-2012.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
checksum_1 = nrow(utdanningsniva)
checksum_2 = sum(extract_numeric(utdanningsniva$tabellvariabel[utdanningsniva$enhet=="personer"]),na.rm=T)
#df = recast(subset(utdanningsniva,fylke_nr!="NULL"),fylke_nr~variable+enhet+innvkat_3+kjonn+utd_5,measure.var = "tabellvariabel")
df = spread(utdanningsniva,utd_5,tabellvariabel)
df = subset(df,enhet=="personer",select=-enhet)
names(df)[9:13]=c("grunnskole.personer","ingen.personer","universitet_og_hogskole.personer","uoppgitt.personer","vgs.personer")
df$totalt.personer = extract_numeric(df[,9])+extract_numeric(df[,10])+extract_numeric(df[,11])+extract_numeric(df[,12])+extract_numeric(df[,13])
df$grunnskole.prosent = extract_numeric(df$grunnskole.personer)/df$totalt.personer*100
df$ingen.prosent = extract_numeric(df$ingen.personer)/df$totalt.personer*100
df$universitet_og_hogskole.prosent = extract_numeric(df$universitet_og_hogskole.personer)/df$totalt.personer*100
df$uoppgitt.prosent = extract_numeric(df$uoppgitt.personer)/df$totalt.personer*100
df$vgs.prosent = extract_numeric(df$vgs.personer)/df$totalt.personer*100
df$totalt.prosent = df[,15]+df[,16]+df[,17]+df[,18]+df[,19]
df2 = gather(df,variabler,tabellvariabel,9:20)
df2=separate(df2,variabler,c("utd5","enhet"),"\\.")
df2 = subset(df2,utd5!="totalt")
#alle NAs behandles som manglende data
sum(is.na(df2))
sum(is.na(df2$tabellvariabel))
df2$tabellvariabel[is.na(df2$tabellvariabel)==T]="."
sum(is.na(df2$tabellvariabel))
#logisk sjekk
checksum_1 == nrow(df2)
checksum_2 == sum(extract_numeric(df2$tabellvariabel[df2$enhet=="personer"]),na.rm=T)
write.csv(df2,"D:/R/imdikator-munch/data_flat_output/utdanningsniva-alle-2012.csv",row.names=F)

#2013
utdanningsniva <- read.csv("D:/R/imdikator-munch/data_flat_input/utdanningsniva-alle-2013.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
checksum_1 = nrow(utdanningsniva)
checksum_2 = sum(extract_numeric(utdanningsniva$tabellvariabel[utdanningsniva$enhet=="personer"]),na.rm=T)
#df = recast(subset(utdanningsniva,fylke_nr!="NULL"),fylke_nr~variable+enhet+innvkat_3+kjonn+utd_5,measure.var = "tabellvariabel")
df = spread(utdanningsniva,utd_5,tabellvariabel)
df = subset(df,enhet=="personer",select=-enhet)
names(df)[9:13]=c("grunnskole.personer","ingen.personer","universitet_og_hogskole.personer","uoppgitt.personer","vgs.personer")
df$totalt.personer = extract_numeric(df[,9])+extract_numeric(df[,10])+extract_numeric(df[,11])+extract_numeric(df[,12])+extract_numeric(df[,13])
df$grunnskole.prosent = extract_numeric(df$grunnskole.personer)/df$totalt.personer*100
df$ingen.prosent = extract_numeric(df$ingen.personer)/df$totalt.personer*100
df$universitet_og_hogskole.prosent = extract_numeric(df$universitet_og_hogskole.personer)/df$totalt.personer*100
df$uoppgitt.prosent = extract_numeric(df$uoppgitt.personer)/df$totalt.personer*100
df$vgs.prosent = extract_numeric(df$vgs.personer)/df$totalt.personer*100
df$totalt.prosent = df[,15]+df[,16]+df[,17]+df[,18]+df[,19]
df2 = gather(df,variabler,tabellvariabel,9:20)
df2=separate(df2,variabler,c("utd5","enhet"),"\\.")
df2 = subset(df2,utd5!="totalt")
#alle NAs behandles som manglende data
sum(is.na(df2))
sum(is.na(df2$tabellvariabel))
df2$tabellvariabel[is.na(df2$tabellvariabel)==T]="."
sum(is.na(df2$tabellvariabel))
#logisk sjekk
checksum_1 == nrow(df2)
checksum_2 == sum(extract_numeric(df2$tabellvariabel[df2$enhet=="personer"]),na.rm=T)
write.csv(df2,"D:/R/imdikator-munch/data_flat_output/utdanningsniva-alle-2013.csv",row.names=F)

#2014
utdanningsniva <- read.csv("D:/R/imdikator-munch/data_flat_input/utdanningsniva-alle-2014.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
checksum_1 = nrow(utdanningsniva)
checksum_2 = sum(extract_numeric(utdanningsniva$tabellvariabel[utdanningsniva$enhet=="personer"]),na.rm=T)
#df = recast(subset(utdanningsniva,fylke_nr!="NULL"),fylke_nr~variable+enhet+innvkat_3+kjonn+utd_5,measure.var = "tabellvariabel")
df = spread(utdanningsniva,utd_5,tabellvariabel)
df = subset(df,enhet=="personer",select=-enhet)
names(df)[9:13]=c("grunnskole.personer","ingen.personer","universitet_og_hogskole.personer","uoppgitt.personer","vgs.personer")
df$totalt.personer = extract_numeric(df[,9])+extract_numeric(df[,10])+extract_numeric(df[,11])+extract_numeric(df[,12])+extract_numeric(df[,13])
df$grunnskole.prosent = extract_numeric(df$grunnskole.personer)/df$totalt.personer*100
df$ingen.prosent = extract_numeric(df$ingen.personer)/df$totalt.personer*100
df$universitet_og_hogskole.prosent = extract_numeric(df$universitet_og_hogskole.personer)/df$totalt.personer*100
df$uoppgitt.prosent = extract_numeric(df$uoppgitt.personer)/df$totalt.personer*100
df$vgs.prosent = extract_numeric(df$vgs.personer)/df$totalt.personer*100
df$totalt.prosent = df[,15]+df[,16]+df[,17]+df[,18]+df[,19]
df2 = gather(df,variabler,tabellvariabel,9:20)
df2=separate(df2,variabler,c("utd5","enhet"),"\\.")
df2 = subset(df2,utd5!="totalt")
#alle NAs behandles som manglende data
sum(is.na(df2))
sum(is.na(df2$tabellvariabel))
df2$tabellvariabel[is.na(df2$tabellvariabel)==T]="."
sum(is.na(df2$tabellvariabel))
#logisk sjekk
checksum_1 == nrow(df2)
checksum_2 == sum(extract_numeric(df2$tabellvariabel[df2$enhet=="personer"]),na.rm=T)
write.csv(df2,"D:/R/imdikator-munch/data_flat_output/utdanningsniva-alle-2014.csv",row.names=F)

#SYSSELSATTE_KJØNN_LAND
#26. januar 2016
sysselsatte_kjonn_land <- read.csv("D:/R/imdikator-munch/data_flat_input/sysselsatte_kjonn_land-alle-2013.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
sum(sysselsatte_kjonn_land$kjonn=="0")
sysselsatte_kjonn_land$kjonn[sysselsatte_kjonn_land$kjonn=="0"]="3"
sum(sysselsatte_kjonn_land$kjonn=="1")
sysselsatte_kjonn_land$kjonn[sysselsatte_kjonn_land$kjonn=="1"]="0"
sysselsatte_kjonn_land$kjonn[sysselsatte_kjonn_land$kjonn=="3"]="1"
write.csv(sysselsatte_kjonn_land,"D:/R/imdikator-munch/data_flat_output/sysselsatte_kjonn_land-alle-2013.csv",row.names=F)

#NORSK_DELTAKERE
#27. januar
#data er ikke deltakere, men antallet nye inn i personkrets i 2013. litt utdatert, og vanskelig å forklare uten kontekst
#midlertidig løsning: prikke alt.
norsk_deltakere <- read.csv("D:/R/imdikator-munch/data_flat_input/norsk_deltakere-alle-2013.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
norsk_deltakere$aar = "2015"
norsk_deltakere$tabellvariabel = "."
write.csv(norsk_deltakere,"D:/R/imdikator-munch/data_flat_output/norsk_deltakere-alle-2015.csv",row.names=F)

#BOSETTING
#kommunedata mangler tabell_navn
#2017-data fjernes midlertidig
bosetting <- read.csv("D:/R/imdikator-munch/data_flat_input/bosatt_anmodede-kommune-2015_2017.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
bosetting$tabell_navn = "bosatt_anmodede"
bosetting = subset(bosetting,aar!="2017")
#logisk sjekk
nrow(bosetting) == nlevels(as.factor(bosetting$kommune_nr))*3*2*2
write.csv(bosetting,"D:/R/imdikator-munch/data_flat_output/bosatt_anmodede-kommune-2015_2016.csv",row.names=F)

#næringsregion
bosetting <- read.csv("D:/R/imdikator-munch/data_flat_input/bosatt_anmodede-naringsregion-2015_2017.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
bosetting = subset(bosetting,aar!="2017")
#logisk sjekk
nrow(bosetting) == nlevels(as.factor(bosetting$naringsregion_nr))*3*2*2
write.csv(bosetting,"D:/R/imdikator-munch/data_flat_output/bosatt_anmodede-naringsregion-2015_2016.csv",row.names=F)

#fylke
bosetting <- read.csv("D:/R/imdikator-munch/data_flat_input/bosatt_anmodede-fylke-2015_2017.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
bosetting = subset(bosetting,aar!="2017")
#logisk sjekk
nrow(bosetting) == nlevels(as.factor(bosetting$fylke_nr))*3*2*2
write.csv(bosetting,"D:/R/imdikator-munch/data_flat_output/bosatt_anmodede-fylke-2015_2016.csv",row.names=F)

#BOSATT_BEFOLKNING
#beregnes med basis i bosatt_anmodede og befolkning_hovedgruppe
#2012-2014, kommune, næringsregion og fylke
#erstatter 2014-data med nye data
bosatt_befolkning_2012_2014 <- read.csv("D:/R/imdikator-munch/data_flat_input/bosatt_befolkning.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
levels(as.factor(bosatt_befolkning_2012_2014$aar))
bosatt_anmodede_2012_2013 = read.csv("D:/R/imdikator-munch/data_flat_input/bosatt_anmodede.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character",fileEncoding = "UTF-8-BOM")
bosatt_anmodede_2012_2013 = subset(bosatt_anmodede_2012_2013,aar=="2012"|aar=="2013")
levels(as.factor(bosatt_anmodede_2012_2013$aar))
befolkning_hovedgruppe_2012_2014 = read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_hovedgruppe.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character",fileEncoding = "UTF-8-BOM")
befolkning_hovedgruppe_2012_2014 = subset(befolkning_hovedgruppe_2012_2014,aar=="2012"|aar=="2013"|aar=="2014")
levels(as.factor(befolkning_hovedgruppe_2012_2014$aar))
befolkning_hovedgruppe_2012_2014 = subset(befolkning_hovedgruppe_2012_2014,innvkat_5=="alle"&enhet=="personer"&kjonn=="alle")

#kommune
library(tidyr)
bosatt_befolkning = subset(bosatt_befolkning_2012_2014,kommune_nr!="NULL",select=-c(fylke_nr,naringsregion_nr,bydel_nr))
df = spread(bosatt_befolkning,bosetting,tabellvariabel)
befolkning_hovedgruppe = subset(befolkning_hovedgruppe_2012_2014,kommune_nr!="NULL",select=-c(fylke_nr,naringsregion_nr,kjonn,enhet,tabell_navn))
df_2 = spread(befolkning_hovedgruppe,innvkat_5,tabellvariabel)
df3 = merge(df,df_2,all.y=F)
bosatt_anmodede = subset(bosatt_anmodede_2012_2013,kommune_nr!="NULL"&bosetting=="bosatt"&enhet=="personer",select=-c(fylke_nr,naringsregion_nr,bydel_nr,enhet,tabell_navn))
bosatt_anmodede_2014 = read.csv("D:/R/imdikator-munch/data_flat_input/bosatt_anmodede-kommune-2014.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character",fileEncoding = "UTF-8-BOM")
bosatt_anmodede_2014 = subset(bosatt_anmodede_2014,bosetting=="bosatt"&enhet=="personer",select=-c(enhet,tabell_navn))
df=rbind(bosatt_anmodede,bosatt_anmodede_2014)
df_2 = spread (df,bosetting,tabellvariabel)
df = merge(df3,df_2,all=T)
df$bosatt_per_1000_ny = round(extract_numeric(df$bosatt)/extract_numeric(df$alle)*1000,1)
df$bosatt_per_1000 = df$bosatt_per_1000_ny
df = subset(df,select=-6:-8)
df = gather(df,bosetting,tabellvariabel,bosatt_per_1000)
sum(is.na(df$aar))
sum(is.na(df$kommune_nr))
sum(is.na(df$enhet))
df$enhet="promille"
sum(is.na(df$tabell_navn))
df$tabell_navn="bosatt_befolkning"
sum(is.na(df$bosetting))
levels(as.factor(df$bosetting))
sum(is.na(df$tabellvariabel))
df$tabellvariabel[is.na(df$tabellvariabel)==T]="."
write.csv(df,"D:/R/imdikator-munch/data_flat_output/bosatt_befolkning-kommune-2012_2014.csv",row.names=F)

#FYLKE
library(tidyr)
bosatt_befolkning = subset(bosatt_befolkning_2012_2014,fylke_nr!="NULL",select=-c(kommune_nr,naringsregion_nr,bydel_nr))
df = spread(bosatt_befolkning,bosetting,tabellvariabel)
befolkning_hovedgruppe = subset(befolkning_hovedgruppe_2012_2014,fylke_nr!="NULL",select=-c(kommune_nr,naringsregion_nr,kjonn,enhet,tabell_navn))
df_2 = spread(befolkning_hovedgruppe,innvkat_5,tabellvariabel)
df3 = merge(df,df_2,all.y=F)
bosatt_anmodede = subset(bosatt_anmodede_2012_2013,fylke_nr!="NULL"&bosetting=="bosatt"&enhet=="personer",select=-c(kommune_nr,naringsregion_nr,bydel_nr,enhet,tabell_navn))
bosatt_anmodede_2014 = read.csv("D:/R/imdikator-munch/data_flat_input/bosatt_anmodede-fylke-2014.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character",fileEncoding = "UTF-8-BOM")
bosatt_anmodede_2014 = subset(bosatt_anmodede_2014,bosetting=="bosatt"&enhet=="personer",select=-c(enhet,tabell_navn))
df=rbind(bosatt_anmodede,bosatt_anmodede_2014)
df_2 = spread (df,bosetting,tabellvariabel)
df = merge(df3,df_2,all=T)
df$bosatt_per_1000_ny = round(extract_numeric(df$bosatt)/extract_numeric(df$alle)*1000,1)
df$bosatt_per_1000 = df$bosatt_per_1000_ny
df = subset(df,select=-6:-8)
df = gather(df,bosetting,tabellvariabel,bosatt_per_1000)
sum(is.na(df$aar))
sum(is.na(df$fylke_nr))
sum(is.na(df$enhet))
levels(as.factor(df$enhet))
df$enhet="promille"
sum(is.na(df$tabell_navn))
df$tabell_navn="bosatt_befolkning"
sum(is.na(df$bosetting))
levels(as.factor(df$bosetting))
sum(is.na(df$tabellvariabel))
df$tabellvariabel[is.na(df$tabellvariabel)==T]="."
sum(is.na(df))==0
write.csv(df,"D:/R/imdikator-munch/data_flat_output/bosatt_befolkning-fylke-2012_2014.csv",row.names=F)

#Næringsregion
library(tidyr)
bosatt_befolkning = subset(bosatt_befolkning_2012_2014,naringsregion_nr!="NULL",select=-c(kommune_nr,fylke_nr,bydel_nr))
df = spread(bosatt_befolkning,bosetting,tabellvariabel)
befolkning_hovedgruppe = subset(befolkning_hovedgruppe_2012_2014,naringsregion_nr!="NULL",select=-c(kommune_nr,fylke_nr,kjonn,enhet,tabell_navn))
df_2 = spread(befolkning_hovedgruppe,innvkat_5,tabellvariabel)
df3 = merge(df,df_2,all.y=F)
bosatt_anmodede = subset(bosatt_anmodede_2012_2013,naringsregion_nr!="NULL"&bosetting=="bosatt"&enhet=="personer",select=-c(kommune_nr,fylke_nr,bydel_nr,enhet,tabell_navn))
bosatt_anmodede_2014 = read.csv("D:/R/imdikator-munch/data_flat_input/bosatt_anmodede-naringsregion-2014.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character",fileEncoding = "UTF-8-BOM")
bosatt_anmodede_2014 = subset(bosatt_anmodede_2014,bosetting=="bosatt"&enhet=="personer",select=-c(enhet,tabell_navn))
df=rbind(bosatt_anmodede,bosatt_anmodede_2014)
df_2 = spread (df,bosetting,tabellvariabel)
df = merge(df3,df_2,all=T)
df$bosatt_per_1000_ny = round(extract_numeric(df$bosatt)/extract_numeric(df$alle)*1000,1)
df$bosatt_per_1000 = df$bosatt_per_1000_ny
df = subset(df,select=-6:-8)
df = gather(df,bosetting,tabellvariabel,bosatt_per_1000)
sum(is.na(df))
sum(is.na(df$aar))
sum(is.na(df$naringsregion_nr))
sum(is.na(df$enhet))
levels(as.factor(df$enhet))
df$enhet="promille"
sum(is.na(df$tabell_navn))
df$tabell_navn="bosatt_befolkning"
sum(is.na(df$bosetting))
levels(as.factor(df$bosetting))
sum(is.na(df$tabellvariabel))
df$tabellvariabel[is.na(df$tabellvariabel)==T]="."
sum(is.na(df))==0
write.csv(df,"D:/R/imdikator-munch/data_flat_output/bosatt_befolkning-naringsregion-2012_2014.csv",row.names=F)

#INTRO_STATUS_ARBUTD
intro_status_arbutd <- read.csv("D:/R/imdikator-munch/data_flat_output/intro_status_arbutd-kommune-2014.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
df = subset(intro_status_arbutd,avslutta=="ettaar"&avslstat4=="arbutd"&kjonn=="alle"&enhet=="personer")
sum(df$tabellvariabel=="."|df$tabellvariabel==":")

#VOKSNE_GRUNNSKOLE
#test av om ikke-uniformtabell fungerer
voksne_grunnskole <- read.csv("D:/R/imdikator-munch/data_flat_input/voksne_grunnskole.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character",fileEncoding = "UTF-8-BOM")
sum(voksne_grunnskole$tabellvariabel==":")
sum(voksne_grunnskole$tabellvariabel==".")
#funnet feil: prosentdesimaler, ikke prosenter, for spraak==alle
voksne_grunnskole$tabellvariabel[voksne_grunnskole$spraak=="alle"&voksne_grunnskole$enhet=="prosent"&voksne_grunnskole$undervisning_grskole!="alle"]=extract_numeric(voksne_grunnskole$tabellvariabel[voksne_grunnskole$spraak=="alle"&voksne_grunnskole$enhet=="prosent"&voksne_grunnskole$undervisning_grskole!="alle"])*100
sum(is.na(voksne_grunnskole$tabellvariabel))
sum(voksne_grunnskole$tabellvariabel==":")
sum(voksne_grunnskole$tabellvariabel==".")
voksne_grunnskole$tabellvariabel[is.na(voksne_grunnskole$tabellvariabel)==T]="."
voksne_grunnskole = subset(voksne_grunnskole,fylke_nr!="NULL",select=-8:-9)
df = subset(voksne_grunnskole, spraak="alle"|(spraak=="minoritet"&undervisning_grskole=="alle")|(spraak=="ikke_minoritet"&undervisning_grskole=="alle"))
df = subset(voksne_grunnskole, (spraak=="ikke_minoritet"&undervisning_grskole=="alle")|spraak=="alle"|(spraak=="minoritet"&undervisning_grskole=="alle"))
sum(is.na(df$tabellvariabel))
nlevels(as.factor(df$fylke_nr))
write.csv(df,"D:/R/imdikator-munch/data_flat_output/voksne_grunnskole-fylke-2013.csv",row.names=F)

#HELE DATASETTET
#test av om ikke-uniformtabell fungerer
voksne_grunnskole <- read.csv("D:/R/imdikator-munch/data_flat_input/voksne_grunnskole.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character",fileEncoding = "UTF-8-BOM")
sum(voksne_grunnskole$tabellvariabel==":")
sum(voksne_grunnskole$tabellvariabel==".")
#funnet feil: prosentdesimaler, ikke prosenter, for spraak==alle
voksne_grunnskole$tabellvariabel[voksne_grunnskole$spraak=="alle"&voksne_grunnskole$enhet=="prosent"&voksne_grunnskole$undervisning_grskole!="alle"]=extract_numeric(voksne_grunnskole$tabellvariabel[voksne_grunnskole$spraak=="alle"&voksne_grunnskole$enhet=="prosent"&voksne_grunnskole$undervisning_grskole!="alle"])*100
#sjekker for NA
sum(is.na(voksne_grunnskole$tabellvariabel))
sum(voksne_grunnskole$tabellvariabel==":")
sum(voksne_grunnskole$tabellvariabel==".")
voksne_grunnskole$tabellvariabel[is.na(voksne_grunnskole$tabellvariabel)==T]="."
#subsetter ut tom informasjon
df = subset(voksne_grunnskole, spraak="alle"|(spraak=="minoritet"&undervisning_grskole=="alle")|(spraak=="ikke_minoritet"&undervisning_grskole=="alle"))
df = subset(voksne_grunnskole, (spraak=="ikke_minoritet"&undervisning_grskole=="alle")|spraak=="alle"|(spraak=="minoritet"&undervisning_grskole=="alle"))
sum(is.na(df$tabellvariabel))
write.csv(df,"D:/R/imdikator-munch/data_flat_output/voksne_grunnskole-alle-2013.csv",row.names=F)

#9 - BOSATTE
#BOSATT_ANMODEDE
#21. januar 2016
#1. kommunedata 2014
#2. andre nivåer 2014
#3. kommunedata 2015-2017
#4. fylkesdata 2015-2017
#5. andre nivåer 2015-2017

#kommune 2014
library(tidyr)
bosatt_anmodede <- read.csv("D:/R/imdikator-munch/data_crossed_input/bosatt_anmodede-kommune-2014.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=";", dec=".",colClasses="character")
bosatt_anmodede = subset(bosatt_anmodede,select=c(1,3,4,5))
names(bosatt_anmodede)=c("kommune_nr","anmodning_personer","vedtak_personer","bosatt_personer")
bosatt_anmodede$anmodning_prosent = (as.numeric(bosatt_anmodede$anmodning_personer)/as.numeric(bosatt_anmodede$anmodning_personer))*100
bosatt_anmodede$vedtak_prosent = (as.numeric(bosatt_anmodede$vedtak_personer)/as.numeric(bosatt_anmodede$anmodning_personer))*100
bosatt_anmodede$bosatt_prosent = (as.numeric(bosatt_anmodede$bosatt_personer)/as.numeric(bosatt_anmodede$anmodning_personer))*100
df = gather(bosatt_anmodede,"variabler","tabellvariabel",2:7)
df = separate(df,variabler,c("bosetting","enhet"),sep="_")
df$tabellvariabel[is.na(df$tabellvariabel)==T]="."
#logisk sjekk
nrow(df)==429*3*2
#resterende
df$tabell_navn="bosatt_anmodede"
df$aar="2014"
write.csv(df,"D:/R/imdikator-munch/data_flat_output/bosetting_anmodede-kommune-2014.csv",row.names=F)

#næringsregion 2014
kinfo <- read.csv("D:/R/imdikator-munch/parameters/kommunesort.csv", sep=";", stringsAsFactors=FALSE)
kinfo = subset(kinfo,select=c("Nr","Naringregnr"))
names(kinfo)[2] = "naringsregion_nr"
kinfo$Nr[nchar(kinfo$Nr)==3] = paste0("0",kinfo$Nr[nchar(kinfo$Nr)==3])
kinfo$naringsregion_nr[nchar(kinfo$naringsregion_nr)==1] = paste0("0",kinfo$naringsregion_nr[nchar(kinfo$naringsregion_nr)==1])
bosatt_anmodede <- read.csv("D:/R/imdikator-munch/data_crossed_input/bosatt_anmodede-kommune-2014-tilaggregering.csv", row.names=NULL, na.strings=c("NA"), stringsAsFactors=FALSE, sep=";", dec=".")
sum(is.na(bosatt_anmodede))
sum(bosatt_anmodede==":")
sum(bosatt_anmodede==".")
#skal ikke leses inn som character, men med NA.strings ="."&":"
#logikken er at det ikke er : på dette nivået, fordi tallene skal aggregeres - det er bare missing
#og hvis det er ":", så skal disse uansett bli . på aggregert n ivå
bosatt_anmodede <- read.csv("D:/R/imdikator-munch/data_crossed_input/bosatt_anmodede-kommune-2014-tilaggregering.csv", row.names=NULL, na.strings=c("NA","."), stringsAsFactors=FALSE, sep=";", dec=".")
checksum_start = sum(bosatt_anmodede$bosatt_2014,na.rm=T)
bosatt_anmodede$kommune_nr[nchar(bosatt_anmodede$kommune_nr)==3] = paste0("0",bosatt_anmodede$kommune_nr[nchar(bosatt_anmodede$kommune_nr)==3])
#vedtak kan være ".", hvis henvendelse ikke er besvart-.
#bosetting og anmodning kan ikke være ".", skal være 0
sum(is.na(bosatt_anmodede$bosatt_2014)==T)
bosatt_anmodede$bosatt_2014[is.na(bosatt_anmodede$bosatt_2014)==T]=0
sum(is.na(bosatt_anmodede$anmodning_2014)==T)
bosatt_anmodede$anmodning_2014[is.na(bosatt_anmodede$anmodning_2014)==T]=0
sum(is.na(bosatt_anmodede$vedtak_2014)==T)
bosatt_anmodede$vedtak_2014[is.na(bosatt_anmodede$vedtak_2014)==T]=0
df = merge(bosatt_anmodede,kinfo,by.x="kommune_nr",by.y ="Nr",all.x=T,all.y=T)
library(reshape)
bosatt_anmodede_m = melt.data.frame(df,id.vars=c("kommune_nr","naringsregion_nr"),na.rm=F)
bosatt_anmodede_nareg = cast(bosatt_anmodede_m,naringsregion_nr~variable,fun.aggregate=sum,na.rm=F,add.missing=T,fill=NA)
checksum_start == sum(bosatt_anmodede_nareg$bosatt_2014)
names(bosatt_anmodede_nareg)=c("naringsregion_nr","bosatt_personer","anmodning_personer","vedtak_personer")
bosatt_anmodede_nareg$anmodning_prosent = (as.numeric(bosatt_anmodede_nareg$anmodning_personer)/as.numeric(bosatt_anmodede_nareg$anmodning_personer))*100
bosatt_anmodede_nareg$vedtak_prosent = (as.numeric(bosatt_anmodede_nareg$vedtak_personer)/as.numeric(bosatt_anmodede_nareg$anmodning_personer))*100
bosatt_anmodede_nareg$bosatt_prosent = (as.numeric(bosatt_anmodede_nareg$bosatt_personer)/as.numeric(bosatt_anmodede_nareg$anmodning_personer))*100
df = gather(bosatt_anmodede_nareg,"variabler","tabellvariabel",2:7)
df = separate(df,variabler,c("bosetting","enhet"),sep="_")
df$tabellvariabel[is.na(df$tabellvariabel)==T]="."
#logisk sjekk
nrow(df)==84*3*2
#resterende
df$tabell_navn="bosatt_anmodede"
df$aar="2014"
write.csv(df,"D:/R/imdikator-munch/data_flat_output/bosetting_anmodede-naringsregion-2014.csv",row.names=F)

#fylke 2014
kinfo = read.csv("D:/R/imdikator-munch/parameters/kommunesort.csv", sep=";", stringsAsFactors=FALSE)
kinfo = subset(kinfo,select=c("Nr","Fylkenr"))
names(kinfo)[2] = "fylke_nr"
kinfo$fylke_nr[nchar(kinfo$fylke_nr)==1] = paste0("0",kinfo$fylke_nr[nchar(kinfo$fylke_nr)==1])
#bosettingsdata spesielt og data for aggregering generelt skal ikke leses inn som character, men med NA.strings ="."&":"
#logikken er at det ikke er : på dette nivået, fordi tallene skal aggregeres - det er bare missing
#og hvis det er ":", så skal disse uansett bli . på aggregert n ivå
bosatt_anmodede <- read.csv("D:/R/imdikator-munch/data_crossed_input/bosatt_anmodede-kommune-2014-tilaggregering.csv", row.names=NULL, na.strings=c("NA","."), stringsAsFactors=FALSE, sep=";", dec=".")
checksum_start = sum(bosatt_anmodede$bosatt_2014,na.rm=T)
#vedtak kan være ".", hvis henvendelse ikke er besvart-.
#bosetting og anmodning kan ikke være ".", skal være 0
sum(is.na(bosatt_anmodede$bosatt_2014)==T)
bosatt_anmodede$bosatt_2014[is.na(bosatt_anmodede$bosatt_2014)==T]=0
sum(is.na(bosatt_anmodede$anmodning_2014)==T)
bosatt_anmodede$anmodning_2014[is.na(bosatt_anmodede$anmodning_2014)==T]=0
sum(is.na(bosatt_anmodede$vedtak_2014)==T)
bosatt_anmodede$vedtak_2014[is.na(bosatt_anmodede$vedtak_2014)==T]=0
df = merge(bosatt_anmodede,kinfo,by.x="kommune_nr",by.y ="Nr",all.x=T,all.y=T)
library(reshape)
bosatt_anmodede_m = melt.data.frame(df,id.vars=c("kommune_nr","fylke_nr"),na.rm=F)
bosatt_anmodede_fylke = cast(bosatt_anmodede_m,fylke_nr~variable,fun.aggregate=sum,na.rm=F,add.missing=T,fill=NA, margins="grand_row")
bosatt_anmodede_fylke = subset(bosatt_anmodede_fylke,select=-5)
checksum_start == sum(bosatt_anmodede_fylke$bosatt_2014)/2
names(bosatt_anmodede_fylke)=c("fylke_nr","bosatt_personer","anmodning_personer","vedtak_personer")
bosatt_anmodede_fylke$anmodning_prosent = (as.numeric(bosatt_anmodede_fylke$anmodning_personer)/as.numeric(bosatt_anmodede_fylke$anmodning_personer))*100
bosatt_anmodede_fylke$vedtak_prosent = (as.numeric(bosatt_anmodede_fylke$vedtak_personer)/as.numeric(bosatt_anmodede_fylke$anmodning_personer))*100
bosatt_anmodede_fylke$bosatt_prosent = (as.numeric(bosatt_anmodede_fylke$bosatt_personer)/as.numeric(bosatt_anmodede_fylke$anmodning_personer))*100
df = gather(bosatt_anmodede_fylke,"variabler","tabellvariabel",2:7)
df = separate(df,variabler,c("bosetting","enhet"),sep="_")
sum(is.na(df$tabellvariabel)==T)
df$tabellvariabel[is.na(df$tabellvariabel)==T]="."
#logisk sjekk
nrow(df)==21*3*2
#resterende
df$tabell_navn="bosatt_anmodede"
df$aar="2014"
#.==regexp any single character
df$fylke_nr = gsub(".all.","00",df$fylke_nr)
write.csv(df,"D:/R/imdikator-munch/data_flat_output/bosetting_anmodede-fylke-2014.csv",row.names=F)

#3. kommunedata 2015-2017
bosatt_anmodede = read.csv("D:/R/imdikator-munch/data_crossed_input/bosatt_anmodede-kommune-2015_2017.csv", row.names=NULL, na.strings=c("NA",":","."), stringsAsFactors=FALSE, sep=";", dec=",")
checksum = sum(bosatt_anmodede$vedtak_2016_personer,na.rm=T)
df = gather(bosatt_anmodede,variables,tabellvariabel,2:19)
df = separate(df,variables,into=c("bosetting","aar","enhet"),sep="_")
nlevels(as.factor(df$kommune_nr))
#i anmodnings- og vedtakstall på kommunenivå 2015-2017 er bosetting manglende, anmodning ikke : eller . men 0, og vedtak enten 0 eller NA.
df$tabellvariabel[df$bosetting=="bosatt"&is.na(df$tabellvariabel)==T]="."
df$tabellvariabel[df$bosetting=="anmodning"&is.na(df$tabellvariabel)==T]=0
df$tabellvariabel[df$bosetting=="vedtak"&is.na(df$tabellvariabel)==T]="."
checksum == sum(as.numeric(df$tabellvariabel[df$bosetting=="vedtak"&df$aar=="2016"&df$enhet=="personer"]),na.rm=T)
#logisk sjekk
nrow(df)==428*3*3*2
#resterende
df$tabell_navn="bosatt_anmodede"
df$kommune_nr[nchar(df$kommune_nr)==3] = paste0("0",df$kommune_nr[nchar(df$kommune_nr)==3])
write.csv(df,"D:/R/imdikator-munch/data_flat_output/bosetting_anmodede-kommune-2015_2017.csv",row.names=F)

#4. fylkesdata 2015-2017
bosatt_anmodede = read.csv("D:/R/imdikator-munch/data_crossed_input/bosatt_anmodede-fylke-2015_2017.csv", row.names=NULL, na.strings=c("NA",":","."), stringsAsFactors=FALSE, sep=";", dec=",")
checksum = sum(bosatt_anmodede$vedtak_2016_personer,na.rm=T)/2
df = gather(bosatt_anmodede,variables,tabellvariabel,2:19)
df = separate(df,variables,into=c("bosetting","aar","enhet"),sep="_")
nlevels(as.factor(df$fylke_nr))
#i anmodnings- og vedtakstall på fylkenivå 2015-2017 er bosetting manglende, anmodning ikke : eller . men 0, og vedtak enten 0 eller NA.
df$tabellvariabel[df$bosetting=="bosatt"&is.na(df$tabellvariabel)==T]="."
df$tabellvariabel[df$bosetting=="anmodning"&is.na(df$tabellvariabel)==T]=0
df$tabellvariabel[df$bosetting=="vedtak"&is.na(df$tabellvariabel)==T]="."
checksum == sum(as.numeric(df$tabellvariabel[df$bosetting=="vedtak"&df$aar=="2016"&df$enhet=="personer"]),na.rm=T)
#logisk sjekk
nrow(df)==20*3*3*2
#resterende
df$tabell_navn="bosatt_anmodede"
df$fylke_nr[nchar(df$fylke_nr)==1] = paste0("0",df$fylke_nr[nchar(df$fylke_nr)==1])
write.csv(df,"D:/R/imdikator-munch/data_flat_output/bosetting_anmodede-fylke-2015_2017.csv",row.names=F)

#5. næringsregion 2015-2017
bosatt_anmodede = read.csv("D:/R/imdikator-munch/data_crossed_input/bosatt_anmodede-kommune-2015_2017.csv", row.names=NULL, na.strings=c("NA",":","."), stringsAsFactors=FALSE, sep=";", dec=",")
checksum = sum(bosatt_anmodede$anmodning_2016_personer,na.rm=T)
bosatt_anmodede = subset(bosatt_anmodede,select=1:10)
kinfo <- read.csv("D:/R/imdikator-munch/parameters/kommunesort.csv", sep=";", stringsAsFactors=FALSE)
kinfo = subset(kinfo,select=c("Nr","Naringregnr"))
names(kinfo)[2] = "naringsregion_nr"
kinfo$naringsregion_nr[nchar(kinfo$naringsregion_nr)==1] = paste0("0",kinfo$naringsregion_nr[nchar(kinfo$naringsregion_nr)==1])
df = merge(bosatt_anmodede,kinfo,by.x="kommune_nr",by.y ="Nr",all.x=T,all.y=T)
#vedtak kan være ".", hvis henvendelse ikke er besvart. ved aggregering av vedtak
# settes disse lik 0. det samme gjelder NA bosetting, og NA anmodning
#bosetting og anmodning kan ikke være ".", skal være 0
df[is.na(df)==T]=0
bosatt_anmodede_m = melt.data.frame(df,id.vars=c("kommune_nr","naringsregion_nr"),na.rm=F)
bosatt_anmodede_nareg = cast(bosatt_anmodede_m,naringsregion_nr~variable,fun.aggregate=sum,na.rm=F,add.missing=T,fill=NA)

bosatt_anmodede_nareg$anmodning_2015_prosent = (as.numeric(bosatt_anmodede_nareg$anmodning_2015_personer)/as.numeric(bosatt_anmodede_nareg$anmodning_2015_personer))*100
bosatt_anmodede_nareg$vedtak_2015_prosent = (as.numeric(bosatt_anmodede_nareg$vedtak_2015_personer)/as.numeric(bosatt_anmodede_nareg$anmodning_2015_personer))*100
bosatt_anmodede_nareg$bosatt_2015_prosent = (as.numeric(bosatt_anmodede_nareg$bosatt_2015_personer)/as.numeric(bosatt_anmodede_nareg$anmodning_2015_personer))*100
bosatt_anmodede_nareg$anmodning_2016_prosent = (as.numeric(bosatt_anmodede_nareg$anmodning_2016_personer)/as.numeric(bosatt_anmodede_nareg$anmodning_2016_personer))*100
bosatt_anmodede_nareg$vedtak_2016_prosent = (as.numeric(bosatt_anmodede_nareg$vedtak_2016_personer)/as.numeric(bosatt_anmodede_nareg$anmodning_2016_personer))*100
bosatt_anmodede_nareg$bosatt_2016_prosent = (as.numeric(bosatt_anmodede_nareg$bosatt_2016_personer)/as.numeric(bosatt_anmodede_nareg$anmodning_2016_personer))*100
bosatt_anmodede_nareg$anmodning_2017_prosent = (as.numeric(bosatt_anmodede_nareg$anmodning_2017_personer)/as.numeric(bosatt_anmodede_nareg$anmodning_2017_personer))*100
bosatt_anmodede_nareg$vedtak_2017_prosent = (as.numeric(bosatt_anmodede_nareg$vedtak_2017_personer)/as.numeric(bosatt_anmodede_nareg$anmodning_2017_personer))*100
bosatt_anmodede_nareg$bosatt_2017_prosent = (as.numeric(bosatt_anmodede_nareg$bosatt_2017_personer)/as.numeric(bosatt_anmodede_nareg$anmodning_2017_personer))*100

df = gather(bosatt_anmodede_nareg,variables,tabellvariabel,2:19)
df = separate(df,variables,into=c("bosetting","aar","enhet"),sep="_")
nlevels(as.factor(df$naringsregion_nr))

#i anmodnings- og vedtakstall på naringsregion 2015-2017 er
# bosetting manglende (.), anmodning 0, og vedtak enten 0 eller ".".
df$tabellvariabel[df$bosetting=="bosatt"&df$tabellvariabel==0]="."
df$tabellvariabel[df$bosetting=="vedtak"&df$tabellvariabel=="0"]="."
#logisk sjekk
nrow(df)==84*3*3*2
#resterende
df$tabell_navn="bosatt_anmodede"
write.csv(df,"D:/R/imdikator-munch/data_flat_output/bosetting_anmodede-naringsregion-2015_2017.csv",row.names=F)

#AGGREGERING OG FLATFILERING AV 2015-DATA
#forbehandla krysstabell i excel
#26. februar
data = read.csv("D:/R/imdikator-munch/data_crossed_input/bosatt_anmodede-kommune-2015.csv", row.names=NULL, na.strings=c("NA",":","."), stringsAsFactors=FALSE, sep=";", dec=",")
library(tidyr)

#KOMMUNE 2015
bosatt_anmodede = data
names(bosatt_anmodede)=c("kommune_nr","aar", "anmodning_personer","vedtak_personer","bosatt_personer")
checksum = sum(bosatt_anmodede$vedtak_personer,na.rm=T)
bosatt_anmodede$anmodning_prosent = bosatt_anmodede$anmodning_personer/bosatt_anmodede$anmodning_personer*100
bosatt_anmodede$vedtak_prosent = bosatt_anmodede$vedtak_personer/bosatt_anmodede$anmodning_personer*100
bosatt_anmodede$bosatt_prosent = bosatt_anmodede$bosatt_personer/bosatt_anmodede$anmodning_personer*100
df = gather(bosatt_anmodede,variables,tabellvariabel,3:8)
df = separate(df,variables,into=c("bosetting","enhet"),sep="_")
nlevels(as.factor(df$kommune_nr))==429 #har med kommune 9999
#i anmodnings- og vedtakstall på kommunenivå 2015 er bosetting 0, anmodning  0, og vedtak enten 0 eller ".".
df$tabellvariabel[df$bosetting=="bosatt"&is.na(df$tabellvariabel)==T]=0
df$tabellvariabel[df$bosetting=="anmodning"&is.na(df$tabellvariabel)==T]=0
df$tabellvariabel[df$bosetting=="vedtak"&is.na(df$tabellvariabel)==T]="."
checksum == sum(as.numeric(df$tabellvariabel[df$bosetting=="vedtak"&df$enhet=="personer"]),na.rm=T)
#logisk sjekk
nrow(df)==429*3*2
#resterende
df$tabell_navn="bosatt_anmodede"
df$kommune_nr[nchar(df$kommune_nr)==3] = paste0("0",df$kommune_nr[nchar(df$kommune_nr)==3])
write.csv(df,"D:/R/imdikator-munch/data_flat_output/bosatt_anmodede-kommune-2015.csv",row.names=F)

#FYLKE 2015
kinfo = read.csv("D:/R/imdikator-munch/parameters/kommunesort.csv", sep=";", stringsAsFactors=FALSE)
kinfo = subset(kinfo,select=c("Nr","Fylkenr"))
names(kinfo)[2] = "fylke_nr"
kinfo$fylke_nr[nchar(kinfo$fylke_nr)==1] = paste0("0",kinfo$fylke_nr[nchar(kinfo$fylke_nr)==1])
#bosettingsdata spesielt og data for aggregering generelt skal ikke leses inn som character, men med NA.strings ="."&":"
#logikken er at det ikke er : på dette nivået, fordi tallene skal aggregeres - det er bare missing
#og hvis det er ":", så skal disse uansett bli . på aggregert n ivå
bosatt_anmodede = data
checksum_start = sum(bosatt_anmodede$bosetting.vedtak[bosatt_anmodede$aar==2015],na.rm=T)
sum(is.na(bosatt_anmodede$bosetting.bosatt)==T)
bosatt_anmodede$bosetting.bosatt[is.na(bosatt_anmodede$bosetting.bosatt)==T]=0
sum(is.na(bosatt_anmodede$bosetting.anmodning)==T)
bosatt_anmodede$bosetting.anmodning[is.na(bosatt_anmodede$bosetting.anmodning)==T]=0
sum(is.na(bosatt_anmodede$bosetting.vedtak)==T)
bosatt_anmodede$bosetting.vedtak[is.na(bosatt_anmodede$bosetting.vedtak)==T]=0
df = merge(bosatt_anmodede,kinfo,by.x="kommune_nr",by.y ="Nr",all.x=T,all.y=T)
library(reshape)
bosatt_anmodede_m = melt.data.frame(df,id.vars=c("kommune_nr","fylke_nr","aar"),na.rm=F)
bosatt_anmodede_fylke = cast(bosatt_anmodede_m,fylke_nr~variable,fun.aggregate=sum,na.rm=T,add.missing=T,margins="grand_row")
bosatt_anmodede_fylke = subset(bosatt_anmodede_fylke,select=-5)
checksum_start == sum(bosatt_anmodede_fylke$bosetting.vedtak)/2
names(bosatt_anmodede_fylke)=c("fylke_nr","anmodning_personer_2015","vedtak_personer_2015","bosatt_personer_2015")
bosatt_anmodede_fylke$anmodning_prosent_2015 = (as.numeric(bosatt_anmodede_fylke$anmodning_personer_2015)/as.numeric(bosatt_anmodede_fylke$anmodning_personer_2015))*100
bosatt_anmodede_fylke$vedtak_prosent_2015 = (as.numeric(bosatt_anmodede_fylke$vedtak_personer_2015)/as.numeric(bosatt_anmodede_fylke$anmodning_personer_2015))*100
bosatt_anmodede_fylke$bosatt_prosent_2015 = (as.numeric(bosatt_anmodede_fylke$bosatt_personer_2015)/as.numeric(bosatt_anmodede_fylke$anmodning_personer_2015))*100
df = gather(bosatt_anmodede_fylke,"variabler","tabellvariabel",-1)
df = separate(df,variabler,c("bosetting","enhet","aar"),sep="_")
sum(is.na(df$tabellvariabel)==T)
df$tabellvariabel[is.na(df$tabellvariabel)==T]="."
#logisk sjekk
nrow(df)==21*3*2
#.==regexp any single character
df$fylke_nr = gsub(".all.","00",df$fylke_nr)
checksum_start==df$tabellvariabel[df$fylke_nr=="00"&df$bosetting=="vedtak"&df$aar=="2015"&df$enhet=="personer"]
#resterende
df$tabell_navn="bosatt_anmodede"
write.csv(df,"D:/R/imdikator-munch/data_flat_output/bosatt_anmodede-fylke-2015.csv",row.names=F)

#Næringsregion 2015
kinfo <- read.csv("D:/R/imdikator-munch/parameters/kommunesort.csv", sep=";", stringsAsFactors=FALSE)
kinfo = subset(kinfo,select=c("Nr","Naringregnr"))
names(kinfo)[2] = "naringsregion_nr"
kinfo$naringsregion_nr[nchar(kinfo$naringsregion_nr)==1] = paste0("0",kinfo$naringsregion_nr[nchar(kinfo$naringsregion_nr)==1])
#bosettingsdata spesielt og data for aggregering generelt skal ikke leses inn som character, men med NA.strings ="."&":"
#logikken er at det ikke er : på dette nivået, fordi tallene skal aggregeres - det er bare missing
#og hvis det er ":", så skal disse uansett bli . på aggregert n ivå
bosatt_anmodede = data
checksum_start = sum(bosatt_anmodede$bosetting.vedtak[bosatt_anmodede$aar==2015],na.rm=T)
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
names(bosatt_anmodede_naringsregion)=c("naringsregion_nr","anmodning_personer_2015","vedtak_personer_2015","bosatt_personer_2015")
bosatt_anmodede_naringsregion$anmodning_prosent_2015 = (as.numeric(bosatt_anmodede_naringsregion$anmodning_personer_2015)/as.numeric(bosatt_anmodede_naringsregion$anmodning_personer_2015))*100
bosatt_anmodede_naringsregion$vedtak_prosent_2015 = (as.numeric(bosatt_anmodede_naringsregion$vedtak_personer_2015)/as.numeric(bosatt_anmodede_naringsregion$anmodning_personer_2015))*100
bosatt_anmodede_naringsregion$bosatt_prosent_2015 = (as.numeric(bosatt_anmodede_naringsregion$bosatt_personer_2015)/as.numeric(bosatt_anmodede_naringsregion$anmodning_personer_2015))*100
df = gather(bosatt_anmodede_naringsregion,"variabler","tabellvariabel",-1)
df = separate(df,variabler,c("bosetting","enhet","aar"),sep="_")
sum(is.na(df$tabellvariabel)==T)
df$tabellvariabel[is.na(df$tabellvariabel)==T]="."
#logisk sjekk
nrow(df)==84*3*2
#resterende
df$tabell_navn="bosatt_anmodede"
write.csv(df,"D:/R/imdikator-munch/data_flat_output/bosatt_anmodede-naringsregion-2015.csv",row.names=F)

#AGGREGERING OG FLATFILERING AV 2016-DATA
#forbehandla krysstabell i excel
#31. januar, 3. februar, 12. februar, 18. februar
data = read.csv("D:/R/imdikator-munch/data_crossed_input/bosatt_anmodede-kommune-2016-160218.csv", row.names=NULL, na.strings=c("NA",":","."), stringsAsFactors=FALSE, sep=";", dec=",")
library(tidyr)
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
write.csv(df,"D:/R/imdikator-munch/data_flat_output/bosatt_anmodede-kommune-2016-160218.csv",row.names=F)

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
library(reshape)
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
write.csv(df,"D:/R/imdikator-munch/data_flat_output/bosatt_anmodede-fylke-2016-160218.csv",row.names=F)

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
write.csv(df,"D:/R/imdikator-munch/data_flat_output/bosatt_anmodede-naringsregion-2016-160218.csv",row.names=F)

#DIAGNOSE AV BOSATT_BEFOLKNING
bosatt_befolkning <- read.csv("D:/R/imdikator-munch/data_flat_input/bosatt_befolkning-kommune-2015.csv", sep=",",dec=".", stringsAsFactors=FALSE,colClasses="character")
nlevels(as.factor(bosatt_befolkning[,5]))
write.csv(bosatt_befolkning,"D:/R/imdikator-munch/data_flat_output/bosatt_befolkning-kommune-2015.csv",row.names=F)

