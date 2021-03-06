#OMKODING AV VERDIER

#omkoding av 0 til missing "." i befolkning_hovedgruppe
options(scipen = 500)
befolkning_hovedgruppe <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_hovedgruppe.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE)

#feil i år=2001, f=03
#rettet 20.10.2015
#henter nye tall fra SSBs statistikkbank tabell 05182
df=subset(befolkning_hovedgruppe,fylke_nr=='03'&aar=="2001"&innvkat_5=="norskfodte_m_innvf"&kjonn==1)
befolkning_hovedgruppe$tabellvariabel[befolkning_hovedgruppe$fylke_nr=='03'&befolkning_hovedgruppe$aar=="2001"&befolkning_hovedgruppe$innvkat_5=="norskfodte_m_innvf"&befolkning_hovedgruppe$kjonn==1&befolkning_hovedgruppe$enhet=="personer"]=11454
befolkning_hovedgruppe$tabellvariabel[befolkning_hovedgruppe$fylke_nr=='03'&befolkning_hovedgruppe$aar=="2001"&befolkning_hovedgruppe$innvkat_5=="norskfodte_m_innvf"&befolkning_hovedgruppe$kjonn==1&befolkning_hovedgruppe$enhet=="prosent"] = (as.numeric(befolkning_hovedgruppe$tabellvariabel[befolkning_hovedgruppe$fylke_nr=='03'&befolkning_hovedgruppe$aar=="2001"&befolkning_hovedgruppe$innvkat_5=="norskfodte_m_innvf"&befolkning_hovedgruppe$kjonn==1&befolkning_hovedgruppe$enhet=="personer"]) / as.numeric(befolkning_hovedgruppe$tabellvariabel[befolkning_hovedgruppe$fylke_nr=='03'&befolkning_hovedgruppe$aar=="2001"&befolkning_hovedgruppe$innvkat_5=="alle"&befolkning_hovedgruppe$kjonn=="alle"&befolkning_hovedgruppe$enhet=="personer"]))*100

#antall tabellvariabler som er 0
#måte å sjekke på om enheter har kun en/to 0-verdier?
#df=subset(befolkning_hovedgruppe,tabellvariabel==0)
#write.csv2(df,"training_files/befolkning_hovedgruppe_0.csv",row.names = F)
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
write.csv(befolkning_hovedgruppe,"D:/R/imdikator-munch/data_flat_output/befolkning_hovedgruppe.csv",row.names=F)

#29. juli
#omkoding av intro_status_arbutd til å ha kohort-kolonne i stedet for avslutta-kolonne
#bydel
#steg 1: legg til observasjoner i ei fil per geonivå
df1 <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranser selvhenta fra statbank 2015/intro_status-arbutd/steg 5 - test av omkoding til kohort/intro_status_arbutd-bydel-2011_2013.csv", fileEncoding = "UTF-8-BOM", stringsAsFactors=FALSE,colClasses="character")
df2 <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranser selvhenta fra statbank 2015/intro_status-arbutd/steg 5 - test av omkoding til kohort/intro_status_arbutd-bydel-2014.csv", fileEncoding = "UTF-8-BOM", stringsAsFactors=FALSE,colClasses="character")
df = rbind(df1,df2)
#steg 2: kod om avslutta til året de avslutta
df$temp = df$avslutta
df$temp[df$temp=="ettaar"]=1
df$temp[df$temp=="toaar"]=2
df$temp[df$temp=="treaar"]=3
df$temp[df$temp=="fireaar"]=4
df$temp[df$temp=="femaar"]=5
levels(as.factor(df$temp))
df$kohort=as.numeric(df$aar)-as.numeric(df$temp)
#3: fjern overflødig info og eksporter
df = subset(df,select=-c(temp,avslutta))
write.csv(df,"~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranser selvhenta fra statbank 2015/intro_status-arbutd/steg 5 - test av omkoding til kohort/intro_status_arbutd-bydel-2011_2014.csv",row.names = F)

#kommune
df1 <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranser selvhenta fra statbank 2015/intro_status-arbutd/steg 5 - test av omkoding til kohort/intro_status_arbutd-kommune-2011_2013.csv", fileEncoding = "UTF-8-BOM", stringsAsFactors=FALSE,colClasses="character")
df2 <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranser selvhenta fra statbank 2015/intro_status-arbutd/steg 5 - test av omkoding til kohort/intro_status_arbutd-kommune-2014.csv", fileEncoding = "UTF-8-BOM", stringsAsFactors=FALSE,colClasses="character")
df = rbind(df1,df2)
#steg 2: kod om avslutta til året de avslutta
df$temp = df$avslutta
df$temp[df$temp=="ettaar"]=1
df$temp[df$temp=="toaar"]=2
df$temp[df$temp=="treaar"]=3
df$temp[df$temp=="fireaar"]=4
df$temp[df$temp=="femaar"]=5
levels(as.factor(df$temp))
df$kohort=as.numeric(df$aar)-as.numeric(df$temp)
#3: fjern overflødig info og eksporter
df = subset(df,select=-c(temp,avslutta))
write.csv(df,"~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranser selvhenta fra statbank 2015/intro_status-arbutd/steg 5 - test av omkoding til kohort/intro_status_arbutd-kommune-2011_2014.csv",row.names = F)

#fylke
df1 <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranser selvhenta fra statbank 2015/intro_status-arbutd/steg 5 - test av omkoding til kohort/intro_status_arbutd-fylke-2011_2013.csv", fileEncoding = "UTF-8-BOM", stringsAsFactors=FALSE,colClasses="character")
df2 <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranser selvhenta fra statbank 2015/intro_status-arbutd/steg 5 - test av omkoding til kohort/intro_status_arbutd-fylke-2014.csv", fileEncoding = "UTF-8-BOM", stringsAsFactors=FALSE,colClasses="character")
df = rbind(df1,df2)
#steg 2: kod om avslutta til året de avslutta
df$temp = df$avslutta
df$temp[df$temp=="ettaar"]=1
df$temp[df$temp=="toaar"]=2
df$temp[df$temp=="treaar"]=3
df$temp[df$temp=="fireaar"]=4
df$temp[df$temp=="femaar"]=5
levels(as.factor(df$temp))
df$kohort=as.numeric(df$aar)-as.numeric(df$temp)
#3: fjern overflødig info og eksporter
df = subset(df,select=-c(temp,avslutta))
write.csv(df,"~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranser selvhenta fra statbank 2015/intro_status-arbutd/steg 5 - test av omkoding til kohort/intro_status_arbutd-fylke-2011_2014.csv",row.names = F)

#næringsregion
df1 <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranser selvhenta fra statbank 2015/intro_status-arbutd/steg 5 - test av omkoding til kohort/intro_status_arbutd-naringsregion-2011_2013.csv", fileEncoding = "UTF-8-BOM", stringsAsFactors=FALSE,colClasses="character")
df2 <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranser selvhenta fra statbank 2015/intro_status-arbutd/steg 5 - test av omkoding til kohort/intro_status_arbutd-naringsregion-2014.csv", fileEncoding = "UTF-8-BOM", stringsAsFactors=FALSE,colClasses="character")
df = rbind(df1,df2)
#steg 2: kod om avslutta til året de avslutta
df$temp = df$avslutta
df$temp[df$temp=="ettaar"]=1
df$temp[df$temp=="toaar"]=2
df$temp[df$temp=="treaar"]=3
df$temp[df$temp=="fireaar"]=4
df$temp[df$temp=="femaar"]=5
levels(as.factor(df$temp))
df$kohort=as.numeric(df$aar)-as.numeric(df$temp)
#3: fjern overflødig info og eksporter
df = subset(df,select=-c(temp,avslutta))
write.csv(df,"~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranser selvhenta fra statbank 2015/intro_status-arbutd/steg 5 - test av omkoding til kohort/intro_status_arbutd-naringsregion-2011_2014.csv",row.names = F)