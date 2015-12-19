#SCRIPT FOR DIAGNOSE

#RYDDEAMBISJON
#dette scriptet må dokumentere hva som er kommet inn, og hva som er gjort med disse filene.
#først: hvilke endringer og rettelser som er gjort på Benglers flatfilleveranse
#så: hva som er gjort med SSBs leveranse i 2015

#BEFOLKNING-ALDER
befolkning_alder <- read.csv2("D:/R/imdikator-munch/data_flat_input/befolkning_alder_B.csv", stringsAsFactors=FALSE,colClasses = "character")
#etter visuell inspeksjon
#1: en kolonne for mye
#2: alder_grupper-variabler har - som skilletegn, ikke _
#3: X_id, ikke X_nr
#4: bydel_nr mangler 0301 først.
#5: enhet-variabel er person, ikke personer
#6: alder_grupper-dimensjon mangler alle-variabel
#7: må ha sep=, dec=.
#8: fylke=00 og alle andre fylker kan ikke legges i ulike filer. disse må splices.
#løses i variabel_adding.R

#BARNEHAGEDELTAKELSE
test <- read.csv("D:/R/imdikator-munch/data_flat_output/barnehagedeltakelse_spraak-bydel-2013.csv", stringsAsFactors=FALSE,colClasses = "character")
write.csv(test,"data_flat_output/test.csv",row.names = F)

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

#BEFOLKNING_OPPRINNELSESLAND
befolkning_opprinnelsesland <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_opprinnelsesland_B.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE)
#1. Bydel-ID har feil lengde
#2. Bydel_nr
#3. feil sep
levels(as.factor(befolkning_opprinnelsesland$kjonn))
befolkning_opprinnelsesland = subset(befolkning_opprinnelsesland,select=c(-8,-kjonn))
befolkning_opprinnelsesland$bydel_id[nchar(befolkning_opprinnelsesland$bydel_id)==1] = paste0("0",befolkning_opprinnelsesland$bydel_id[nchar(befolkning_opprinnelsesland$bydel_id)==1])
befolkning_opprinnelsesland$bydel_id[nchar(befolkning_opprinnelsesland$bydel_id)==2] = paste0("0301",befolkning_opprinnelsesland$bydel_id[nchar(befolkning_opprinnelsesland$bydel_id)==2])
names(befolkning_opprinnelsesland)[3] = "bydel_nr"
write.csv(befolkning_opprinnelsesland,"D:/R/imdikator-munch/data_flat_output/befolkning_opprinnelsesland-bydel-2015.csv",row.names=F)

#kommune
befolkning_opprinnelsesland <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_opprinnelsesland_K.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE)
#1. kommune-ID har feil lengde
#2. kommune_nr
#3. feil sep
levels(as.factor(befolkning_opprinnelsesland$kjonn))
befolkning_opprinnelsesland = subset(befolkning_opprinnelsesland,select=c(-8,-kjonn))
befolkning_opprinnelsesland$kommune_id[nchar(befolkning_opprinnelsesland$kommune_id)==3] = paste0("0",befolkning_opprinnelsesland$kommune_id[nchar(befolkning_opprinnelsesland$kommune_id)==3])
names(befolkning_opprinnelsesland)[3] = "kommune_nr"
write.csv(befolkning_opprinnelsesland,"D:/R/imdikator-munch/data_flat_output/befolkning_opprinnelsesland-kommune-2015.csv",row.names=F)

#naringsregion
befolkning_opprinnelsesland <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_opprinnelsesland_N.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE)
#1. naringsregion-ID har feil lengde
#2. naringsregion_nr
#3. feil sep
levels(as.factor(befolkning_opprinnelsesland$kjonn))
befolkning_opprinnelsesland = subset(befolkning_opprinnelsesland,select=c(-8,-kjonn))
befolkning_opprinnelsesland$naringsregion_id[nchar(befolkning_opprinnelsesland$naringsregion_id)==1] = paste0("0",befolkning_opprinnelsesland$naringsregion_id[nchar(befolkning_opprinnelsesland$naringsregion_id)==1])
names(befolkning_opprinnelsesland)[3] = "naringsregion_nr"
write.csv(befolkning_opprinnelsesland,"D:/R/imdikator-munch/data_flat_output/befolkning_opprinnelsesland-naringsregion-2015.csv",row.names=F)

#fylke + land
befolkning_opprinnelsesland <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_opprinnelsesland_F.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE)
#1. fylke-ID har feil lengde
#2. fylke_nr
#3. feil sep
levels(as.factor(befolkning_opprinnelsesland$kjonn))
befolkning_opprinnelsesland = subset(befolkning_opprinnelsesland,select=c(-8,-kjonn))
befolkning_opprinnelsesland$fylke_id[nchar(befolkning_opprinnelsesland$fylke_id)==1] = paste0("0",befolkning_opprinnelsesland$fylke_id[nchar(befolkning_opprinnelsesland$fylke_id)==1])
names(befolkning_opprinnelsesland)[3] = "fylke_nr"
befolkning_opprinnelsesland_norge <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_opprinnelsesland_L.csv", row.names=NULL, sep=";", dec=",", na.strings="NA", stringsAsFactors=FALSE)
levels(as.factor(befolkning_opprinnelsesland_norge$kjonn))
befolkning_opprinnelsesland_norge = subset(befolkning_opprinnelsesland_norge,select=c(-8,-kjonn))
befolkning_opprinnelsesland_norge$fylke_id[nchar(befolkning_opprinnelsesland_norge$fylke_id)==1] = paste0("0",befolkning_opprinnelsesland_norge$fylke_id[nchar(befolkning_opprinnelsesland_norge$fylke_id)==1])
names(befolkning_opprinnelsesland_norge)[3] = "fylke_nr"
befolkning_opprinnelsesland = rbind(befolkning_opprinnelsesland,befolkning_opprinnelsesland_norge)
write.csv(befolkning_opprinnelsesland,"D:/R/imdikator-munch/data_flat_output/befolkning_opprinnelsesland-fylke-2015.csv",row.names=F)

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
#sjekk av alle komboer - 2009
videregaende_fullfort <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/tab34_2009.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
levels(as.factor(videregaende_fullfort$invkat_5))
#geoenheter * 5 innvkat * 3 kjønn * 2 enheter
df = subset(videregaende_fullfort,fylke_nr!="NULL")
nlevels(as.factor(df$fylke_nr))
nrow(subset(videregaende_fullfort,fylke_nr!="NULL"))==20*5*3*2
df_2=spread(df,invkat_5,tabellvariabel,drop=F)

df = subset(videregaende_fullfort,naringsregion_nr!="NULL")
nrow(df)==nlevels(as.factor(df$naringsregion_nr))*5*3*2
df_2=spread(df,invkat_5,tabellvariabel,drop=F)

df = subset(videregaende_fullfort,kommune_nr!="NULL")
nrow(df)==nlevels(as.factor(df$kommune_nr))*5*3*2
df_2=spread(df,invkat_5,tabellvariabel,drop=F)

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

#SYSSELSATTE_INNVANDRINGSGRUNN
library(tidyr)
#sjekk av alle komboer - 2011
sysselsatte_innvandringsgrunn <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/Tab_1_60a.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
nlevels(as.factor(sysselsatte_innvandringsgrunn$kommune_nr))
levels(as.factor(sysselsatte_innvandringsgrunn$innvgrunn_6))
levels(as.factor(sysselsatte_innvandringsgrunn$aar))
levels(as.factor(sysselsatte_innvandringsgrunn$enhet))
#geoenheter * 5 innvgrunn * 3 kjønn * 2 enheter * 9 år
df = subset(sysselsatte_innvandringsgrunn,aar=="2014")
nrow(df)-nlevels(as.factor(df$kommune_nr))*5*3*2

#SYSSELSATTE_INNVANDRINGSGRUNN
library(tidyr)
sysselsatte_innvkat_alder <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/Tab_9_612a.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
nlevels(as.factor(sysselsatte_innvandringsgrunn$kommune_nr))
levels(as.factor(sysselsatte_innvkat_alder$arbeidsalder_18_69))
levels(as.factor(sysselsatte_innvandringsgrunn$aar))
levels(as.factor(sysselsatte_innvandringsgrunn$enhet))
#geoenheter * 5 innvgrunn * 3 kjønn * 2 enheter * 9 år
df = subset(sysselsatte_innvandringsgrunn,aar=="2014")
nrow(df)-nlevels(as.factor(df$kommune_nr))*5*3*2

#SYSSELSATTE_BOTID
library(tidyr)
sysselsatte_botid <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/Tab_13_62a.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
nlevels(as.factor(sysselsatte_botid$kommune_nr))
levels(as.factor(sysselsatte_botid$botid_5))
levels(as.factor(sysselsatte_botid$aar))
levels(as.factor(sysselsatte_botid$enhet))
#geoenheter * 4 botid * 2 enheter * 1 år
df = subset(sysselsatte_botid,aar=="2014")
nrow(df)-nlevels(as.factor(df$kommune_nr))*4*2

#SYSSELSATTE_BOTID
library(tidyr)
sysselsatte_botid_land <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/Tab_17_631a.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
nlevels(as.factor(sysselsatte_botid_land$kommune_nr))
levels(as.factor(sysselsatte_botid_land$botid_5))
levels(as.factor(sysselsatte_botid_land$vreg_3))
levels(as.factor(sysselsatte_botid_land$aar))
levels(as.factor(sysselsatte_botid_land$enhet))
#geoenheter * 4 botid * 3 vreg * 2 enheter * 1 år
df = subset(sysselsatte_botid_land,aar=="2014")
nrow(df)-nlevels(as.factor(df$kommune_nr))*4*3*2

#SYSSELSATTE_KJONN_LAND
library(tidyr)
sysselsatte_kjonn_land <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/Tab_21_632a.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
nlevels(as.factor(sysselsatte_kjonn_land$kommune_nr))
levels(as.factor(sysselsatte_kjonn_land$vreg_3))
levels(as.factor(sysselsatte_kjonn_land$kjonn))
levels(as.factor(sysselsatte_kjonn_land$aar))
levels(as.factor(sysselsatte_kjonn_land$enhet))
#geoenheter * 3 kjønn * 3 vreg * 2 enheter * 1 år
df = subset(sysselsatte_kjonn_land,aar=="2014")
nrow(df)-nlevels(as.factor(df$kommune_nr))*3*3*2

#SYSSELSATTE_LAND
library(tidyr)
sysselsatte_land <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/Tab_25_633a.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
nlevels(as.factor(sysselsatte_land$kommune_nr))
levels(as.factor(sysselsatte_land$vreg_9))
levels(as.factor(sysselsatte_land$aar))
levels(as.factor(sysselsatte_land$enhet))
#geoenheter * 9 vreg * 2 enheter * 1 år
df = subset(sysselsatte_land,aar=="2014")
nrow(df)-nlevels(as.factor(df$kommune_nr))*9*2

#VOKSNE_VIDEREGAENDE
library(tidyr)
voksne_videregaende <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/tab36_2012.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
nlevels(as.factor(sysselsatte_land$kommune_nr))
levels(as.factor(sysselsatte_land$vreg_9))
levels(as.factor(sysselsatte_land$aar))
levels(as.factor(sysselsatte_land$enhet))
#geoenheter * 9 vreg * 2 enheter * 1 år
df = subset(sysselsatte_land,aar=="2014")
nrow(df)-nlevels(as.factor(df$kommune_nr))*9*2

#utdanningsniva
#2009
utdanningsniva <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/tab37_2009.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
names(utdanningsniva)
levels(as.factor(utdanningsniva$bydel_nr))
df=subset(utdanningsniva,utdanningsniva$bydel_nr=="1001")
df = subset(utdanningsniva,fylke_nr!="NULL")
levels(as.factor(df$invkat_3))
levels(as.factor(df$kjonn))
levels(as.factor(df$utd_5))
nrow(df)-nlevels(as.factor(df$fylke_nr))*3*3*6*2

df = subset(utdanningsniva,naringsregion_nr!="NULL")
nrow(df)-nlevels(as.factor(df$naringsregion_nr))*5*3*2

df = subset(videregaende_fullfort,kommune_nr!="NULL")
nrow(df)-nlevels(as.factor(df$kommune_nr))*5*3*2

#2010
utdanningsniva <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/tab37_2010.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
names(utdanningsniva)
levels(as.factor(utdanningsniva$bydel_nr))
df = subset(utdanningsniva,fylke_nr!="NULL")
nrow(df)-nlevels(as.factor(df$fylke_nr))*3*3*6*2

#2011
utdanningsniva <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/tab37_2011.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
names(utdanningsniva)
levels(as.factor(utdanningsniva$bydel_nr))
df = subset(utdanningsniva,fylke_nr!="NULL")
nrow(df)-nlevels(as.factor(df$fylke_nr))*3*3*6*2

#2012
utdanningsniva <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/tab37_2012.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
names(utdanningsniva)
levels(as.factor(utdanningsniva$bydel_nr))
df = subset(utdanningsniva,fylke_nr!="NULL")
nrow(df)-nlevels(as.factor(df$fylke_nr))*3*3*6*2

#2013
utdanningsniva <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/tab37_2013.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
names(utdanningsniva)
levels(as.factor(utdanningsniva$bydel_nr))
df = subset(utdanningsniva,fylke_nr!="NULL")
nrow(df)-nlevels(as.factor(df$fylke_nr))*3*3*6*2

#2014
utdanningsniva <- read.csv("D:/R/imdikator-munch/data_flat_input/diagnose/tab37_2014.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE, sep=",", dec=".",colClasses="character")
names(utdanningsniva)
levels(as.factor(utdanningsniva$bydel_nr))
df = subset(utdanningsniva,fylke_nr!="NULL")
nrow(df)-nlevels(as.factor(df$fylke_nr))*3*3*6*2

#geoenheter * 5 innvkat * 3 kjønn * 2 enheter
df = subset(videregaende_fullfort,fylke_nr!="NULL")
nrow(df)-nlevels(as.factor(df$fylke_nr))*5*3*2

df = subset(videregaende_fullfort,naringsregion_nr!="NULL")
nrow(df)-nlevels(as.factor(df$naringsregion_nr))*5*3*2

df = subset(videregaende_fullfort,kommune_nr!="NULL")
nrow(df)-nlevels(as.factor(df$kommune_nr))*5*3*2