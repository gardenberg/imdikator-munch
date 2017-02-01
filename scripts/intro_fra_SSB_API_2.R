#Bearbeiding av SSBs statistikk om tidligere introduksjonsdeltakere 1-5 år etter
#forsøk på å hente data fra SSB sitt API
#spørringa er bygd i konsollet på http://data.ssb.no/api/v0/no/console
#metadata er tilgjengelig på http://data.ssb.no/api/v0/no/console/meta/table/10824/
#bruksanvisning på https://www.ssb.no/omssb/om-oss/nyheter-om-ssb/_attachment/248256?_ts=157046caac8

#globale settinger
options(encoding="UTF-8")

#pakker
library(httr)
library(rjstat)
library(dplyr)

url <- "http://data.ssb.no/api/v0/no/table/10824"
data <- '{
        "query": [
{
        "code": "Region",
        "selection": {
        "filter": "all",
        "values": [
        "*"
        ]
        }
},
        {
        "code": "Kjonn",
        "selection": {
        "filter": "all",
        "values": [
        "*"
        ]
        }
        },
        {
        "code": "IntroAvbrudd",
        "selection": {
        "filter": "all",
        "values": [
        "*"
        ]
        }
        },
        {
        "code": "Arbeidsmarkedsstatus",
        "selection": {
        "filter": "item",
        "values": [
        "00",
        "1",
        "2",
        "3"
        ]
        }
        },
        {
        "code": "ContentsCode",
        "selection": {
        "filter": "item",
        "values": [
        "DeltakereProg1",
        "DeltakereProg"
        ]
        }
        },
        {
        "code": "Tid",
        "selection": {
        "filter": "item",
        "values": [
        "2012",
        "2013",
        "2014",
        "2015"
        ]
        }
        }
        ],
        "response": {
        "format": "json-stat"
        }
}'

#ved bruk 19. desember 2016 fikk jeg 404 (not found)-feil på forsøk å hente alle år
        
d.tmp <- POST(url , body = data, encode = "json", verbose())

# Henter ut innholdet fra d.tmp som tekst deretter bearbeides av fromJSONstat
#naming-argumentet bestemmer naming, default er "labels" - at regionkodene blitt bytta til tekstlige beskrivelser
#naming = id beholder koder
sbtabell <- fromJSONstat(content(d.tmp, "text"), naming="id")
ds <- sbtabell[[1]] # Henter ut kun datasettet fra sbtabell

# Viser første seks rader av datasettet
head(ds)

#antallet kombinasjoner bør være 570 regioner * 3 kjønn * 4 arbeidsmarkedsstatuser * 2 contentscode * 4 år * 5 kohorter 
nrow(ds) == 570*4*2*3*5*4 #bør være riktig

#sjekk av NA
sum(is.na(ds[,1:6])) #NA i ikke-value 
sum(is.na(ds[,7])) #NA i value

#koder om fra SSB-koder til IMDi-koder
names(ds)[7] = "tabellvariabel"
names(ds)[4] = "avslstat4"
ds$avslstat4[ds$avslstat4=="00"] = "alle"
ds$avslstat4[ds$avslstat4=="1"] = "arbutd"
ds$avslstat4[ds$avslstat4=="2"] = "ledig_tiltak"
ds$avslstat4[ds$avslstat4=="3"] = "annet"
levels(as.factor(ds$avslstat4))
names(ds)[6] = "aar"
levels(as.factor(ds$aar))
names(ds)[3] = "avslutta"
ds$avslutta[ds$avslutta=="01"] = "ettaar"
ds$avslutta[ds$avslutta=="02"] = "toaar"
ds$avslutta[ds$avslutta=="03"] = "treaar"
ds$avslutta[ds$avslutta=="04"] = "fireaar"
ds$avslutta[ds$avslutta=="05"] = "femaar"
levels(as.factor(ds$avslutta))
ds$temp = ds$avslutta
ds$temp[ds$temp=="ettaar"]=1
ds$temp[ds$temp=="toaar"]=2
ds$temp[ds$temp=="treaar"]=3
ds$temp[ds$temp=="fireaar"]=4
ds$temp[ds$temp=="femaar"]=5
levels(as.factor(ds$temp))
ds$kohort=as.numeric(ds$aar)-as.numeric(ds$temp)
levels(as.factor(ds$kohort))
ds = select(ds,-temp)
names(ds)[5] = "enhet"
ds$enhet[ds$enhet=="DeltakereProg"]="personer"
ds$enhet[ds$enhet=="DeltakereProg1"]="prosent"
levels(as.factor(ds$enhet))
names(ds)[2] = "kjonn"
ds$kjonn[ds$kjonn=="0"]="alle"
ds$kjonn[ds$kjonn=="1"]="1"
ds$kjonn[ds$kjonn=="2"]="0"
levels(as.factor(ds$kjonn))

#håndtering av missing
#fra Jan Brusgaard: json-stat bruker status for å angi tegnet for prikking. 
#spm: betyr det at det er mulig å finne prikkene i d.tmp-objektet et sted?
#...fra rjstat-vignette: "extensive metadata is not supported"

sum(is.na(ds$tabellvariabel))
#det ser ikke ut til å være noe skille mellom manglende data og undertrykte data. 
#gitt at data som vises ut er fulltelling, og at bare eksisterende kommuner per 2014 vises koder jeg alle NA som skjult av personvernhensyn ":"
ds$tabellvariabel[is.na(ds$tabellvariabel)==T]=":"
levels(as.factor(ds$tabellvariabel))

#legger til tabellnavn
ds$tabell_navn ="intro_status_arbutd"

#koder om kohort==2006 fra 0 til manglende "." (kun relevant for aar=2011 eller før)
t = filter(ds,kohort=="2006")
sum(as.numeric(t$tabellvariabel,na.rm=T))==0 #summen av alle verdier er lik 0 - altså meningsløs
ds$tabellvariabel[ds$kohort=="2006"]="."

#lager ett sett med avslutta og ett sett med kohort, og slår de sammen
t_avslutta = ds
t_kohort = ds
t_avslutta$kohort = "NULL"
t_kohort$avslutta = "NULL"
t_kohort$kohort = as.character(t_kohort$kohort)
t = bind_rows(t_avslutta,t_kohort)

#omkoding av regionale koder og oppsplitting i ulike deler
t_k = filter(t,nchar(Region)==4)
nlevels(as.factor(t_k$Region)) #full kommuneliste
names(t_k)[1] = "kommune_nr"
#fylke
t_f = filter(t,nchar(Region)<3)
levels(as.factor(t_f$Region))
names(t_f)[1] = "fylke_nr"
t_f$fylke_nr[t_f$fylke_nr=="0"]="00"
#næringsregion
t_n = filter(t,nchar(Region)==3)
levels(as.factor(t_n$Region))
t_n$Region = gsub("N","",t_n$Region)
levels(as.factor(t_n$Region))
names(t_n)[1] = "naringsregion_nr"
#bydel
t_b = filter(t,nchar(Region)>4)
levels(as.factor(t_b$Region))
t_b$Region = gsub("a","",t_b$Region)
levels(as.factor(t_b$Region))
names(t_b)[1] = "bydel_nr"

#sjekk av kombinasjoner
#bydel
apply(t_b,2, function(x){nlevels(as.factor(x))})
apply(t_b,2, function(x){levels(as.factor(x))})
nrow(t_b)==19*3*6*4*2*4*9

#kommune
apply(t_k,2, function(x){nlevels(as.factor(x))})
nrow(t_k)==447*4*2*3*5*4 #det er flere observasjoner enn jeg hadde antatt her?
sum(duplicated(t_k)==T) #det er med duplikater ut?
t_k = unique(t_k) #hvis dette er et problem med outputen fra SSB, så kan det løses lenger opp?
nrow(t_k)==447*4*2*3*5*4 #bør være riktig

#næringsregion
apply(t_n,2, function(x){nlevels(as.factor(x))})
nrow(t_n)==84*4*2*3*5*4 #det er flere observasjoner enn jeg hadde antatt her?
sum(duplicated(t_n)==T) #det er med duplikater ut?
t_n = unique(t_n) #hvis dette er et problem med outputen fra SSB, så kan det løses lenger opp?
nrow(t_n)==84*4*2*3*5*4 #bør være riktig

#fylke
apply(t_f,2, function(x){nlevels(as.factor(x))})
nrow(t_f)==20*4*2*3*5*4 #det er flere observasjoner enn jeg hadde antatt her?
sum(duplicated(t_f)==T) #det er med duplikater ut?
t_f = unique(t_f) #hvis dette er et problem med outputen fra SSB, så kan det løses lenger opp?
nrow(t_f)==20*4*2*3*5*4

#sjekk av NA
sum(is.na(t_b))
sum(is.na(t_k))
sum(is.na(t_n))
sum(is.na(t_f))

#en dataframe?
t = bind_rows(t_b,t_f,t_k,t_n)
sum(is.na(t))
t[is.na(t)==T] = "NULL"
sum(is.na(t))

apply(t,2, function(x){levels(as.factor(x))})

#output
write.csv(t_b,"data_flat_output/intro_status_arbutd-bydel-2012_2015-utenavslutta.csv", row.names=F)
write.csv(t_k,"data_flat_output/intro_status_arbutd-kommune-2012_2015-utenavslutta.csv", row.names=F)
write.csv(t_n,"data_flat_output/intro_status_arbutd-naringsregion-2012_2015-utenavslutta.csv", row.names=F)
write.csv(t_f,"data_flat_output/intro_status_arbutd-fylke-2012_2015-utenavslutta.csv", row.names=F)
write.csv(t,"data_flat_output/intro_status_arbutd-2012_2015.csv", row.names=F)

#sjekk av trondheim
t2 = filter(t_k,kommune_nr=="1601")

