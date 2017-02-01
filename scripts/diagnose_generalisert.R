#FORSØK PÅ GENERALISERT DIAGNOSE
#biblioteker
library(tidyr)
library(dplyr)

sti = "~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 5-2016 fra SSB/36_voksne_videregaende/"
temp = list.files(path=paste0(sti,"leveranse_2/"),pattern="*.csv")

#datainnlesning 
#bruk header = FALSE ved headerkrøll
df <- read.csv(paste0(sti,"leveranse_2/",temp[3]), row.names=NULL, stringsAsFactors=FALSE,colClasses="character")
tabell_navn=df$tabell_navn[1]
if(length(strsplit(names(df)[grep("_nr",names(df))],"_",fixed=T))>1){geonivå="alle"}
if(length(strsplit(names(df)[grep("_nr",names(df))],"_",fixed=T))==1){geonivå=strsplit(names(df)[grep("_nr",names(df))],"_",fixed=T)[[1]][[1]]}

kombinasjoner=3*3*2
#spørsmål 1: er alle variabler her? er de kodet riktig?
#trenger da kunnskap om antallet variabler
#spørsmål 2: er alle kategorier her? er de kodet riktig?
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
apply(df,2, function(x){nlevels(as.factor(x))})
#kan hekte på tidsserie
df_2012 = read.csv(paste0(sti,"leveranse_1/",temp[17]), row.names=NULL, stringsAsFactors=FALSE,colClasses="character")
df_2013 = read.csv(paste0(sti,"leveranse_1/",temp[18]), row.names=NULL, stringsAsFactors=FALSE,colClasses="character")
df_2014 = read.csv(paste0(sti,"leveranse_1/",temp[19]), row.names=NULL, stringsAsFactors=FALSE,colClasses="character")
df_2015 = read.csv(paste0(sti,"leveranse_1/",temp[20]), row.names=NULL, stringsAsFactors=FALSE,colClasses="character")
df = rbind(df,df_2012,df_2013,df_2014,df_2015)
#mangler nasjonale tall
df_L <- read.csv(paste0(sti,"leveranse_1/",temp[4]), row.names=NULL, stringsAsFactors=FALSE,colClasses="character")
df = rbind(df,df_L)
#en kolonne for mye
df = select(df,-X)
df = select(df,-kjonn)
#en kategori for mye i en variabel
df = filter(df,innvgrunn_6!=" ")
#enhet er kodet feil
df$enhet[df$enhet=="person"]="personer"
#ved feilkoding, fiks den
#feilkodet kategori 0-4 for 0_4
df$botid_3[df$botid_3=="0-4"] = "0_4"
#feilkodet kjønn 2 for alle
df$kjonn[df$kjonn=="2"] = "alle"

#feilkodet bydel_nr
df$bydel_nr = paste0("0301",df$bydel_nr)
#feilkodet tabell_navn
df$tabell_navn = tabell_navn
#feilkodet fylke_nr
df$fylke_nr[df$fylke_nr=="0"]="00"

#header-krøll
df = select(df,-V9)
names(df)=df[1,]
df = df[2:nrow(df),]

#hvilke kommuner/bydeler mangler
t = distinct(df,kommune_nr)
kommuner = select(filter(read.csv("parameters/kommuneegenskaper.csv", row.names=NULL, stringsAsFactors=FALSE,colClasses="character"),standard_utgave=="2014"),kommune_nr)
t = full_join(t,kommuner)
t1 = filter(t,is.na(t$tabell_navn)==T)

#er det avrunding eller prikking?
apply(subset(df,select=tabellvariabel),2, function(x){levels(as.factor(x))})

#manglende utvalg av kommuner
#tabell 34/videregaende_fullfort = alle kommuner med mer enn 2000 innvandrere blant sine registrerte innbyggere 1.1.20XX
utvalg = read.csv("parameters/befolkning_hovedgruppe-kommune-2015.csv", row.names=NULL, stringsAsFactors=FALSE,colClasses="character")
utvalg$tabellvariabel = as.numeric(utvalg$tabellvariabel)
utvalg = filter(utvalg,aar=="2015",innvkat_5=="innvandrere",kjonn=="alle",enhet=="personer")
utvalg = filter(utvalg,tabellvariabel>1999)
df = semi_join(df,utvalg,by="kommune_nr")

#sjekker på nytt at det ser riktig ut
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
apply(df,2, function(x){nlevels(as.factor(x))})

#spørsmål 3: er alle observasjoner her?
if(geonivå=="alle"){nrow(df)==(21*kombinasjoner+18*kombinasjoner+428*kombinasjoner+84*kombinasjoner)}
if(geonivå=="fylke"){nrow(df)==20*kombinasjoner}
if(geonivå=="bydel"){nrow(df)==19*kombinasjoner}
if(geonivå=="kommune"){nrow(df)==428*kombinasjoner}
if(geonivå=="naringsregion"){nrow(df)==83*kombinasjoner}
#hvis ikke alle observasjoner er her, muter til du finner mangel
        t = spread(df,fylke_nr,tabellvariabel,drop=FALSE)
        df = gather(t,botid_3,tabellvariabel,6:8)
        isna = sum(is.na(barn)==T)
        t = spread(df,forste_bosettingskommune,tabellvariabel,drop=FALSE)
        t = gather(t,forste_bosettingskommune,tabellvariabel,6:9)
        df = spread(barn,enhet,tabellvariabel,drop=FALSE)
        df = gather(df,enhet,tabellvariabel,6:7)
#substansielt, ser det rimelig ut?
if(geonivå=="alle"){t=filter(df,fylke_nr=="00")}
if(geonivå=="fylke"){t=filter(df,fylke_nr=="00")}
if(geonivå=="bydel"){t=filter(df,bydel_nr=="030101")}
if(geonivå=="kommune"){t=filter(df,kommune_nr=="0101")}
if(geonivå=="naringsregion"){t=filter(df,naringsregion_nr=="05")}
#legg ferdig fil i output-mappa hvis den er klar til test i testmiljøet
write.csv(df,paste0(sti,tabell_navn,"-",geonivå,".csv"), row.names=F,fileEncoding = "UTF-8")
