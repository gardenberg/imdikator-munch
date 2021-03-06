#SCRIPT FOR Å FYLLE INN MANGLENDE KOMBINASJONER AV DIMENSJONER OG VARIABLER
#SCRIPTET LEGGER TIL ALLE-variabel på dimensjoner hvor denne mangler

#mønster:
#1: importer data
#2: spread() dimensjon hvor variabel skal legges til
#3: legg til alle-variabel
#4: kod evt. NA-verdier som skal være med om til "." eller ":"
#5: gather() dimensjoner til en dimensjon
#6: kontroller at alt har blitt riktig.

#biblioteker
library(tidyr)
library(dplyr)

#ALLE-KATEGORI I BEFOLKNING_ALDER
#data
befolkning_alder <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_alder.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE)
df=spread(befolkning_alder,alder_grupper,tabellvariabel)
#df$alle=extract_numeric(df[,10])+extract_numeric(df[,11])+extract_numeric(df[,12])+extract_numeric(df[,13])+extract_numeric(df[,14])+extract_numeric(df[,15])+extract_numeric(df[,16])
df$alle=apply(df[,10:16],1,
                function(x){
                        sum(extract_numeric(x))
                }
)

df$alle[is.na(df$alle)]="."
df_2=gather(df,alder_grupper,tabellvariabel,`0_5`:alle)

NA_info = data.frame(mangel=sum(befolkning_alder$tabellvariabel=="."),prikk=sum(befolkning_alder$tabellvariabel==":"),anna=sum(is.na(befolkning_alder$tabellvariabel)),row.names="original_flat")

#lager kontroll-frame av df med de nye observasjonene
df_kontroll=subset(df,select=c(aar:kommune_nr,alle))
df_kontroll=gather(df_kontroll,alder_grupper,tabellvariabel,alle)

#trenger en feilsjekk for å sikre at alt er likt, med unntak av alder_grupper.alle
#kan fungere, virker som overkill?
#library(compare)
#comparison=compare(befolkning_alder,df_2)

#trekker ut enheter som er i df_2 som ikke er i befolkning_alder - altså de nye enhetene
comparison_2=anti_join(df_2,befolkning_alder)
#trekker ut enheter som er nye i df_2 som ikke er en del av de nye observasjonene
comparison_3=anti_join(comparison_2,df_kontroll)
#comparison_3 skal være lik 0
nrow(comparison_3)==0

write.csv(df_2,"D:/R/imdikator-munch/data_flat_output/befolkning_alder.csv",row.names=F)

#ALLE-KATEGORI I BEFOLKNING_VERDENSREGION
#trenger alle-kategori for vreg_2, vreg_5 og vreg_9
library(tidyr)
library(dplyr)
befolkning_verdensregion <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_verdensregion.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE)
#Er det NA?
sum(is.na(befolkning_verdensregion$tabellvariabel))
#Er det ":"?
sum(befolkning_verdensregion$tabellvariabel==":")
#Er det "."?
sum(befolkning_verdensregion$tabellvariabel==".")

olddf=nrow(befolkning_verdensregion)
df=spread(befolkning_verdensregion,vreg_2,tabellvariabel)

#problem: spread innfører NA som verdi for de stedene hvor NULL er verdien, dvs. den
#ikke-eksisterende kombinasjonene får en verdi. 
#navnekonvensjoner er heller ikke heldig, med NULL og 1 og 2, løses med 
#names(df)[11]="reg_1", names(df)[12]="reg_2", names(df)[13]="reg_NULL"

#legger sammen vreg_2.1 og .2 til .alle
df$alle=extract_numeric(df[,11])+extract_numeric(df[,12])

#antall nye observasjoner
newobs=nrow(df)-sum(is.na(df$alle))
#skal ikke  legge inn NA som "."
#sum(is.na(df$alle))
#df$alle[is.na(df$alle)]="."
#sum(is.na(df$alle))
#sum(df$alle==".")

#samler vreg-variabler til vreg_3
df_2=gather(df,vreg_3,tabellvariabel,11:14,na.rm=T)
newdf=nrow(df_2)

#kontroll
#er ny df utvidet med riktig nrow?
newdf==olddf+newobs
#lager kontroll-frame av df med de nye observasjonene
df_kontroll=subset(df,select=c(aar:kommune_nr,alle))
df_kontroll=gather(df_kontroll,vreg_3,tabellvariabel,alle,na.rm=T)

#vreg_5->vreg_6
df=spread(df_2,vreg_5,tabellvariabel)
#legger sammen vreg_5.1->5 .alle
df$alle=extract_numeric(df[,11])+extract_numeric(df[,12])+extract_numeric(df[,13])+extract_numeric(df[,14])+extract_numeric(df[,16])
sum(is.na(df$alle))
#samler vreg-variabler til vreg_6
df_2=gather(df,vreg_6,tabellvariabel,11:17,na.rm=T)
newdf=nrow(df_2)

#kontroll
#er ny df utvidet med riktig nrow?
newdf==nrow(befolkning_verdensregion)+nrow(df)-sum(is.na(df$alle))
newdf==nrow(befolkning_verdensregion)+sum(befolkning_verdensregion$vreg_5=="afrika")
#lager kontroll-frame av df med de nye observasjonene for en kikk
df_kontroll=subset(df,select=c(aar:kommune_nr,alle))
df_kontroll=gather(df_kontroll,vreg_6,tabellvariabel,alle,na.rm=T)

#vreg_8->vreg_9
df=spread(df_2,vreg_9,tabellvariabel)
#legger sammen vreg_9.1->8 .alle
df$alle=extract_numeric(df[,11])+extract_numeric(df[,12])+extract_numeric(df[,13])+extract_numeric(df[,14])+extract_numeric(df[,15])+extract_numeric(df[,17])+extract_numeric(df[,18])+extract_numeric(df[,19])
#samler vreg-variabler til vreg_9
df_2=gather(df,vreg_9,tabellvariabel,11:20,na.rm=T)

#kontroll
#er ny df utvidet med riktig nrow?
nrow(df_2)==nrow(befolkning_verdensregion)+nrow(df)-sum(is.na(df$alle))
nrow(df_2)==nrow(befolkning_verdensregion)+sum(befolkning_verdensregion$vreg_5=="afrika")
sum(df_2$vreg_9=="alle")==sum(befolkning_verdensregion$vreg_5=="afrika")
#har et avvik på 1 her
#lager kontroll-frame av df med de nye observasjonene for en kikk
df_kontroll=subset(df,select=c(aar:kommune_nr,alle))
df_kontroll=gather(df_kontroll,vreg_9,tabellvariabel,alle,na.rm=T)

#eksporterer ferdig datasett
write.csv(df_2,"D:/R/imdikator-munch/data_flat_output/befolkning_verdensregion.csv",row.names=F)

#ALLE-KATEGORI I BEFOLKNING_VERDENSREGION_9
library(tidyr)
library(dplyr)
befolkning_verdensregion_9 <- read.csv("D:/R/imdikator-munch/data_flat_output/befolkning_verdensregion_9.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE)
#Er det NA?
sum(is.na(befolkning_verdensregion_9$tabellvariabel))
#Er det ":"?
sum(befolkning_verdensregion_9$tabellvariabel==":")
#Er det "."?
sum(befolkning_verdensregion_9$tabellvariabel==".")

olddf=nrow(befolkning_verdensregion_9)
df=spread(befolkning_verdensregion_9,vreg_9,tabellvariabel)

#vreg_8->vreg_9
#legger sammen vreg_9.1->8 .alle
df$alle=extract_numeric(df[,8])+extract_numeric(df[,9])+extract_numeric(df[,10])+extract_numeric(df[,11])+extract_numeric(df[,12])+extract_numeric(df[,13])+extract_numeric(df[,14])+extract_numeric(df[,15])
#er det innført NA?
sum(is.na(df$alle))
#koder denne om til "." manglende data
df$alle[is.na(df$alle)==T] ="."
sum(is.na(df$alle))
sum(df$alle==".")

#samler vreg-variabler til vreg_9. skal ikke være na her nå.
df_2=gather(df,vreg_9,tabellvariabel,8:16,na.rm=F)

#kontroll
#er ny df utvidet med riktig nrow?
nrow(df_2)==nrow(befolkning_verdensregion_9)+nrow(df)-sum(is.na(df$alle))
nrow(df_2)==nrow(befolkning_verdensregion_9)+sum(befolkning_verdensregion_9$vreg_9=="afrika")
sum(df_2$vreg_9=="alle")==sum(befolkning_verdensregion_9$vreg_9=="afrika")
#ingen avvik.

write.csv(df_2,"D:/R/imdikator-munch/data_flat_output/befolkning_verdensregion_9_v2.csv",row.names=F)

#BEFOLKNING_VERDENSREGION_2
befolkning_verdensregion_2 <- read.csv("D:/R/imdikator-munch/data_flat_output/befolkning_verdensregion_2.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE)
#Er det NA?
sum(is.na(befolkning_verdensregion_2$tabellvariabel))
#Er det ":"?
sum(befolkning_verdensregion_2$tabellvariabel==":")
#Er det "."?
sum(befolkning_verdensregion_2$tabellvariabel==".")

olddf=nrow(befolkning_verdensregion_2)
df=spread(befolkning_verdensregion_2,vreg_2,tabellvariabel)

#legger sammen vreg_2.1 og .2 til .alle
df$alle=extract_numeric(df[,9])+extract_numeric(df[,10])
#er det innført NA?
sum(is.na(df$alle))
#koder denne om til "." manglende data
df$alle[is.na(df$alle)==T] ="."
sum(is.na(df$alle))
sum(df$alle==".")

#samler vreg-variabler til vreg_3
df_2=gather(df,vreg_3,tabellvariabel,9:11,na.rm=F)

#kontroll
#er ny df utvidet med riktig nrow?
nrow(df_2)==nrow(befolkning_verdensregion_2)+nrow(df)-sum(is.na(df$alle))
nrow(df_2)==nrow(befolkning_verdensregion_2)+sum(befolkning_verdensregion_2$vreg_2=="1")
sum(df_2$vreg_3=="alle")==sum(befolkning_verdensregion_2$vreg_2==1)
#ingen avvik.

df <- read.csv("D:/R/imdikator-munch/data_flat_output/befolkning_verdensregion_3_v2.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE)
df$tabell_navn="befolkning_verdensregion_3"
write.csv(df,"D:/R/imdikator-munch/data_flat_output/befolkning_verdensregion_3_v2.csv",row.names=F)

#BEFOLKNING_VERDENSREGION_5 -> ALLE-kategori
befolkning_verdensregion_5 <- read.csv("D:/R/imdikator-munch/data_flat_output/befolkning_verdensregion_5.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE)
#Er det NA?
sum(is.na(befolkning_verdensregion_5$tabellvariabel))
#Er det ":"?
sum(befolkning_verdensregion_5$tabellvariabel==":")
#Er det "."?
sum(befolkning_verdensregion_5$tabellvariabel==".")

olddf=nrow(befolkning_verdensregion_5)
df=spread(befolkning_verdensregion_5,vreg_5,tabellvariabel)

#legger sammen vreg_5.1->5 .alle
df$alle=extract_numeric(df[8])+extract_numeric(df[,9])+extract_numeric(df[,10])+extract_numeric(df[,11])+extract_numeric(df[,12])

#er det innført NA?
sum(is.na(df$alle))
#koder denne om til "." manglende data
df$alle[is.na(df$alle)==T] ="."
sum(is.na(df$alle))
sum(df$alle==".")

#samler vreg-variabler til vreg_6
df_2=gather(df,vreg_6,tabellvariabel,8:13,na.rm=T)

#kontroll
#er ny df utvidet med riktig nrow?
nrow(df_2)==nrow(befolkning_verdensregion_5)+nrow(df)-sum(is.na(df$alle))
nrow(df_2)==nrow(befolkning_verdensregion_5)+sum(befolkning_verdensregion_5$vreg_5=="afrika")
sum(df_2$vreg_6=="alle")==sum(befolkning_verdensregion_5$vreg_5=="afrika")
#ingen avvik.

#eksporterer ferdig datasett
df <- read.csv("D:/R/imdikator-munch/data_flat_output/befolkning_verdensregion_6_v2.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE)
df$tabell_navn="befolkning_verdensregion_6"
write.csv(df,"D:/R/imdikator-munch/data_flat_output/befolkning_verdensregion_6_v2.csv",row.names=F)

#ALLE-KATEGORI I UTDANNINGSNIVA
library(tidyr)
library(dplyr)
utdanningsniva <- read.csv("D:/R/imdikator-munch/data_flat_input/utdanningsniva.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE)
df=spread(utdanningsniva,utd_4,tabellvariabel)
#legger sammen til alle
df$alle=extract_numeric(df[,11])+extract_numeric(df[,13])+extract_numeric(df[,14])+extract_numeric(df[,15])

#håndterer NA hvis det er NA
#I orginalt datasett:
#Antall "."
sum(utdanningsniva$tabellvariabel==".")
#Antall ":"
sum(utdanningsniva$tabellvariabel==":")
#Antall NA
sum(is.na(utdanningsniva$tabellvariabel))

#i spread df, alle
#Antall "."
sum(df$alle=="." & df$utd_6=="NULL")
#Antall ":"
sum(df$alle==":"& df$utd_6=="NULL")
#Antall NA
sum(is.na(df$alle) & df$utd_6=="NULL")

df$alle[is.na(df$alle)& df$utd_6=="NULL"]="."
sum(is.na(df$alle) & df$utd_6=="NULL")
sum(df$alle=="." & df$utd_6=="NULL")

#samler variabler til opprinnelig variabel
df_2=gather(df,utd_5,tabellvariabel,11:16,na.rm=T)
sum(is.na(df_2$tabellvariabel))
sum(df_2$tabellvariabel==".")

#kontroll
#er ny df utvidet med riktig nrow?
nrow(df_2)==nrow(utdanningsniva)+nrow(df)-sum(is.na(df$alle))
nrow(df_2)==nrow(utdanningsniva)+sum(utdanningsniva$utd_4=="grunnskole")
sum(df_2$utd_5=="alle")==sum(utdanningsniva$utd_4=="grunnskole")
sum(df_2$utd_5=="alle")==sum(df_2$utd_5=="vgs")
#har et avvik her!
#lager kontroll-frame av df med de nye observasjonene for en kikk
df_kontroll=subset(df,select=c(aar:kommune_nr,alle))
df_kontroll=gather(df_kontroll,utd_5,tabellvariabel,alle,na.rm=T)

#eksporterer ferdig datasett
write.csv(df_2,"D:/R/imdikator-munch/data_flat_output/utdanningsniva.csv",row.names=F)
