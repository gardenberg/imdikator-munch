#omkoding av 0 til missing "." i befolkning_hovedgruppe
options(scipen = 500)
befolkning_hovedgruppe <- read.csv("D:/R/imdikator-munch/data/befolkning_hovedgruppe.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE)

#antall tabellvariabler som er 0
sum(befolkning_hovedgruppe$tabellvariabel==0,na.rm=T)
temp=sum(befolkning_hovedgruppe$tabellvariabel==0,na.rm=T)
temp2=sum(befolkning_hovedgruppe$tabellvariabel==".",na.rm=T)
#antall med 0 som er personer
sum((befolkning_hovedgruppe$tabellvariabel==0&befolkning_hovedgruppe$enhet=='personer'),na.rm=T)
#antall med 0 som er prosenter
sum((befolkning_hovedgruppe$tabellvariabel==0&befolkning_hovedgruppe$enhet=='prosent'),na.rm=T)

#setter alle verdier som er 0 lik "." for manglende data
befolkning_hovedgruppe$tabellvariabel[befolkning_hovedgruppe$tabellvariabel==0]="."

#sjekk av at det ikke lenger finnes tabellvariabler som er 0
sum(befolkning_hovedgruppe$tabellvariabel==0,na.rm=T)
sum(befolkning_hovedgruppe$tabellvariabel==0,na.rm=T)==0
#sjekk av at antallet som før var 0 nå "."
sum(befolkning_hovedgruppe$tabellvariabel==".",na.rm=T)
temp+temp2==sum(befolkning_hovedgruppe$tabellvariabel==".",na.rm=T)

#skriver til csv-fil. Husk riktig csv-type
write.csv(befolkning_hovedgruppe,"D:/R/imdikator-munch/output/befolkning_hovedgruppe.csv",row.names=F)

#ALLE-KATEGORI I BEFOLKNING_ALDER
#legger til en kategori for alle i befolkning_alder
befolkning_alder <- read.csv("D:/R/imdikator-munch/data/befolkning_alder.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE)
library(tidyr)
df=spread(befolkning_alder,alder_grupper,tabellvariabel)
df$alle=extract_numeric(df[,10])+extract_numeric(df[,11])+extract_numeric(df[,12])+extract_numeric(df[,13])+extract_numeric(df[,14])+extract_numeric(df[,15])+extract_numeric(df[,16])
sum(is.na(df$alle))
df$alle[is.na(df$alle)]="."
sum(is.na(df$alle))
sum(df$alle==".")
df_2=gather(df,alder_grupper,tabellvariabel,`0_5`:alle)

#lager kontroll-frame av df med de nye observasjonene
df_kontroll=subset(df,select=c(aar:kommune_nr,alle))
df_kontroll=gather(df_kontroll,alder_grupper,tabellvariabel,alle)

#trenger en feilsjekk for å sikre at alt er likt, med unntak av alder_grupper.alle
#kan fungere, virker som overkill?
#library(compare)
#comparison=compare(befolkning_alder,df_2)

library(dplyr)
#trekker ut enheter som er i df_2 som ikke er i befolkning_alder - altså de nye enhetene
comparison_2=anti_join(df_2,befolkning_alder)
#trekker ut enheter som er nye i df_2 som ikke er en del av de nye observasjonene
comparison_3=anti_join(comparison_2,df_kontroll)
#comparison_3 skal være lik 0
nrow(comparison_3)==0

write.csv(df_2,"D:/R/imdikator-munch/output/befolkning_alder.csv",row.names=F)

#ALLE-KATEGORI I BEFOLKNING_VERDENSREGION
#trenger alle-kategori for vreg_2, vreg_5 og vreg_9
library(tidyr)
library(dplyr)
befolkning_verdensregion <- read.csv("D:/R/imdikator-munch/data/befolkning_verdensregion.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE)
df=spread(befolkning_verdensregion,vreg_2,tabellvariabel)

df$alle=extract_numeric(df[,10])+extract_numeric(df[,11])+extract_numeric(df[,12])+extract_numeric(df[,13])+extract_numeric(df[,14])+extract_numeric(df[,15])+extract_numeric(df[,16])
sum(is.na(df$alle))
df$alle[is.na(df$alle)]="."
sum(is.na(df$alle))
sum(df$alle==".")
df_2=gather(df,alder_grupper,tabellvariabel,`0_5`:alle)

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

write.csv(df_2,"D:/R/imdikator-munch/output/befolkning_alder.csv",row.names=F)


#ALLE-KATEGORI I UTDANNINGSNIVA