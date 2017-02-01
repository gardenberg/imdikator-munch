#pakker
library(dplyr)

#Pakker sammen et stort sett med filer
#oversikt over filer som finnes og andre parameter
sti = "~/Ifakta/Datasett/Azure-backup 6-desember-2016/"
temp = list.files(sti,pattern="*.csv")
navn_temp = ""
navn_tabell = ""
n = 1:length(temp)
i=1
df = data.frame()
state_fleretabellnavn = 0
state_duplikater1 = 0
state_duplikater2 = 0

t = strsplit(temp,"-",fixed=T)
t[[]]

#viktig at loopen looper over en sekvens/vektor, ikke bare ett tall - altså i in 1:length, ikke i in length
#første forsøk feilet på grunn av ett eller annet som gir gigatabell - 36 millioner observasjoner/1.5GB. problem med full_join, erstattet med bind_rows
#andre forsøk nullstilte ikke df for ny tabell. fikset
#tredje forsøk håndterte advarsler først manuelt, og la til håndtering/fjerning av duplikater (og break hvis duplikater med ulik tabellvariabel)
#fjerde versjon har state på om loopen finner tabeller med flere tabellnavn eller duplikater
#femte versjon får med siste df også.
#framover: er det mulig å skrive en innholdstest? 
# test 1: lik lengde?
# test 2: likt innhold?

#LOOP
for(i in n){
        #leser inn  fil
        df_temp <- read.csv(paste0(sti,temp[i]), row.names=NULL, stringsAsFactors=FALSE,colClasses="character",fileEncoding = "UTF-8-BOM")

        #henter tabell_navn
        if(nlevels(as.factor(df_temp$tabell_navn))>1){
                #SJEKK: bare ett tabell_navn?
                state_fleretabellnavn = state_fleretabellnavn+1
        }
        navn_temp = df_temp$tabell_navn[1]

        #sjekker om tabell_navn er likt forrige innleste tabell svakhet - hvis tabell_navn ikke ligger alfabetisk...
        if(i==1){
                df = df_temp
                navn_tabell = navn_temp
        }
        if(navn_temp==navn_tabell&i>1){
                #hvis ja: merge med bind_rows   
                df = bind_rows(df,df_temp)
                nlevels(as.factor(df$tabell_navn))==1
        }
        if(navn_temp!=navn_tabell&i>1){
                #hvis ulik: gjør NA til "NULL", lagre fila, lagre tabellnavnet. repeat
                sum(is.na(df))
                df[is.na(df)]="NULL"
                #Alle NA satt til "NULL"
                sum(is.na(df))==0
                #håndterer evt. duplikater - fjerner helt identiske rader
                if(nrow(df)!=nrow(distinct(df))){
                        df=distinct(df)
                        state_duplikater1 = state_duplikater1+1
                }
                #hvis det er helt identiske rader med ulik tabellvariabel må jeg se nærmere på det
                if(nrow(df)!=nrow(distinct(select(df,-tabellvariabel)))){
                        #duplikater
                        state_duplikater2 = 1
                        break
                }
                write.csv(df,paste0(sti,"merged/",navn_tabell,".csv"), row.names=F)
                df = df_temp
                navn_tabell=navn_temp
        }
        if(i==329){
                #hvis ulik: gjør NA til "NULL", lagre fila, lagre tabellnavnet. repeat
                sum(is.na(df))
                df[is.na(df)]="NULL"
                #Alle NA satt til "NULL"
                sum(is.na(df))==0
                #håndterer evt. duplikater - fjerner helt identiske rader
                if(nrow(df)!=nrow(distinct(df))){
                        df=distinct(df)
                        state_duplikater1 = state_duplikater1+1
                }
                #hvis det er helt identiske rader med ulik tabellvariabel må jeg se nærmere på det
                if(nrow(df)!=nrow(distinct(select(df,-tabellvariabel)))){
                        #duplikater
                        state_duplikater2 = 1
                        break
                }
                write.csv(df,paste0(sti,"merged/",navn_tabell,".csv"), row.names=F)
                df = df_temp
                navn_tabell=navn_temp
        }
}

#sjekk av feilmeldinger på befolkning_botid, befolkning_flytting og flyktning_botid_flytting
#ser ut til at fileencoding utf-8-bom ikke funker på disse.
#1: In scan(file, what, nmax, sep, dec, quote, skip, nlines, na.strings,  :
#invalid input found on input connection '~/Ifakta/Datasett/Azure-backup 29-november-2016/befolkning_botid.csv'
df <- read.csv(paste0(sti,"befolkning_botid.csv"), row.names=NULL, stringsAsFactors=FALSE,colClasses="character")
#må rydde litt
names(df)[1]="aar"
df = filter(df,enhet!="navn")
df = select(df,-Bydel,-Fylke)
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
apply(df,2, function(x){nlevels(as.factor(x))})
write.csv(df,paste0(sti,"befolkning_botid.csv"), row.names=F,fileEncoding = "UTF-8")

#befolkning_flytting
df <- read.csv(paste0(sti,"befolkning_flytting.csv"), row.names=NULL, stringsAsFactors=FALSE,colClasses="character")
#må rydde litt
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
names(df)[1]="tabell_navn"
df = filter(df,enhet!="navn")
df = select(df,-Fylke)
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
apply(df,2, function(x){nlevels(as.factor(x))})
write.csv(df,paste0(sti,"befolkning_flytting.csv"), row.names=F,fileEncoding = "UTF-8")

#flyktning_botid_flytting
df <- read.csv(paste0(sti,"flyktning_botid_flytting.csv"), row.names=NULL, stringsAsFactors=FALSE,colClasses="character")
#må rydde litt
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
names(df)[1]="tabell_navn"
df = filter(df,enhet!="navn")
df = select(df,-Fylke)
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
apply(df,2, function(x){nlevels(as.factor(x))})
write.csv(df,paste0(sti,"flyktning_botid_flytting.csv"), row.names=F,fileEncoding = "UTF-8")

#Et fåtall filer
sti = "~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 4-2016 fra SSB/9_befolkning_opprinnelsesland_botid/tilrettelagt for tolk/"
temp = list.files(sti,pattern="*.csv")

df_1 <- read.csv(paste0(sti,temp[1]), stringsAsFactors=FALSE,colClasses = "character")
df_2 <- read.csv(paste0(sti,temp[2]), stringsAsFactors=FALSE,colClasses = "character")
df_3 <- read.csv(paste0(sti,temp[3]), stringsAsFactors=FALSE,colClasses = "character")
df_4 <- read.csv(paste0(sti,temp[4]), stringsAsFactors=FALSE,colClasses = "character")

df = bind_rows(df_1,df_2,df_3,df_4)
apply(df,2, function(x){nlevels(as.factor(x))})
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})

sum(is.na(df))
df[is.na(df)]="NULL"
#Alle NA satt til "NULL"
sum(is.na(df))==0

apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})

write.csv(df,paste0(sti,"befolkning_opprinnelsesland_botid.csv"), row.names=F)

#deprecated
#datainnlesning
df <- read.csv("~/Ifakta/Datasett/28_nov_16 - fikse befolkning_botid til botid_4/befolkning_botid.csv", stringsAsFactors=FALSE,colClasses="character")

#må rydde litt
names(df)[1]="aar"
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
apply(df,2, function(x){nlevels(as.factor(x))})
df = filter(df,enhet!="navn")
df = select(df,-Bydel,-Fylke)
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
apply(df,2, function(x){nlevels(as.factor(x))})

sti = "~/Ifakta/Datasett/28_nov_16 - fikse befolkning_botid til botid_4/"
temp = list.files(sti,pattern="*.csv")
df <- read.csv(paste0(sti,temp[9]), row.names=NULL, stringsAsFactors=FALSE,colClasses="character")

df_1 <- read.csv(paste0(sti,temp[1]), row.names=NULL, stringsAsFactors=FALSE,colClasses="character")
df_2 <- read.csv(paste0(sti,temp[2]), row.names=NULL, stringsAsFactors=FALSE,colClasses="character")
df_3 <- read.csv(paste0(sti,temp[3]), row.names=NULL, stringsAsFactors=FALSE,colClasses="character")
df_4 <- read.csv(paste0(sti,temp[4]), row.names=NULL, stringsAsFactors=FALSE,colClasses="character")
df_5 <- read.csv(paste0(sti,temp[5]), row.names=NULL, stringsAsFactors=FALSE,colClasses="character")
df_6 <- read.csv(paste0(sti,temp[6]), row.names=NULL, stringsAsFactors=FALSE,colClasses="character")
df_7 <- read.csv(paste0(sti,temp[7]), row.names=NULL, stringsAsFactors=FALSE,colClasses="character")
df_8 <- read.csv(paste0(sti,temp[8]), row.names=NULL, stringsAsFactors=FALSE,colClasses="character")

#merger
t = full_join(df,df_1)
t = full_join(t,df_2)
t = full_join(t,df_3)
t = full_join(t,df_4)
t = full_join(t,df_5)
t = full_join(t,df_6)
t = full_join(t,df_7)
t = full_join(t,df_8)

apply(subset(t,select=-tabellvariabel),2, function(x){levels(as.factor(x))})

#fikser NA
sum(is.na(t))
#så lenge øvrige koder er like, bør det bare bli laget NA på regionkodene
t$bydel_nr[is.na(t$bydel_nr)]="NULL"
t$kommune_nr[is.na(t$kommune_nr)]="NULL"
t$naringsregion_nr[is.na(t$naringsregion_nr)]="NULL"
t$fylke_nr[is.na(t$fylke_nr)]="NULL"
sum(is.na(t))

apply(subset(t,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
df = t
df = filter(df,botid_4!="uoppgitt")
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
write.csv(df,paste0(sti,"befolkning_botid-2014_2016.csv"), row.names=F)


#diagnose
bosatt_anmodede <- read.csv("~/Ifakta/Datasett/Azure-backup 29-november-2016/merged/bosatt_anmodede.csv", stringsAsFactors=FALSE)

#er det duplikater?
nrow(distinct(bosatt_anmodede))==nrow(bosatt_anmodede)
#er det ukurante verdier i header eller kategorier?
apply(bosatt_anmodede,2, function(x){nlevels(as.factor(x))})
apply(subset(bosatt_anmodede,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
sum(is.na(bosatt_anmodede))
t = filter(bosatt_anmodede,aar=="NULL")
df = filter(bosatt_anmodede,aar!="NULL")
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
#riktig antall enheter?

#skriver ut.
write.csv(df,"test/bosatt_anmodede.csv", row.names=F)
