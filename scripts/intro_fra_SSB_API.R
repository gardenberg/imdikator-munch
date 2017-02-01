#Bearbeiding av SSBs statistikk om tidligere introduksjonsdeltakere 1-5 år etter

#pakker
library(dplyr)

#datainnlesning
intro_status <- read.csv("~/R/imdikator-munch/data_flat_input/intro_status_arbutd-kommune-2011_2014.csv", stringsAsFactors=FALSE)
intro_status <- read.csv("~/R/imdikator-munch/data_flat_input/intro_status_arbutd-bydel-2011_2014.csv", stringsAsFactors=FALSE)
df1 <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranser selvhenta fra statbank 2015/intro_status-arbutd/steg 5 - test av omkoding til kohort/intro_status_arbutd-kommune-2011_2013.csv", fileEncoding = "UTF-8-BOM", stringsAsFactors=FALSE,colClasses="character")

#forsøk på å hente data fra SSB sitt API
#spørringa er bygd i konsollet på http://data.ssb.no/api/v0/no/console
#metadata er tilgjengelig på http://data.ssb.no/api/v0/no/console/meta/table/10824/
#bruksanvisning på https://www.ssb.no/omssb/om-oss/nyheter-om-ssb/_attachment/248256?_ts=157046caac8
options(encoding="UTF-8")

library(httr)
library(rjstat)

url <- "http://data.ssb.no/api/v0/no/table/10824"
data <- '{
        "query": [
                {
                        "code": "Region",
                        "selection": {
                                "filter": "item",
                                "values": [
                                        "0",
                                        "01",
                                        "0101",
                                        "0104",
                                        "0105",
                                        "0106",
                                        "0111",
                                        "0118",
                                        "0119",
                                        "0121",
                                        "0122",
                                        "0123",
                                        "0124",
                                        "0125",
                                        "0127",
                                        "0128",
                                        "0135",
                                        "0136",
                                        "0137",
                                        "0138",
                                        "02",
                                        "0211",
                                        "0213",
                                        "0214",
                                        "0215",
                                        "0216",
                                        "0217",
                                        "0219",
                                        "0220",
                                        "0221",
                                        "0226",
                                        "0227",
                                        "0228",
                                        "0229",
                                        "0230",
                                        "0231",
                                        "0233",
                                        "0234",
                                        "0235",
                                        "0236",
                                        "0237",
                                        "0238",
                                        "0239",
                                        "03",
                                        "0301",
                                        "030100aa",
                                        "030101a",
                                        "030102a",
                                        "030103a",
                                        "030104a",
                                        "030105a",
                                        "030106a",
                                        "030107a",
                                        "030108a",
                                        "030109a",
                                        "030110a",
                                        "030111a",
                                        "030112a",
                                        "030113a",
                                        "030114a",
                                        "030115a",
                                        "030116a",
                                        "030117a",
                                        "030199a",
                                        "04",
                                        "0402",
                                        "0403",
                                        "0412",
                                        "0415",
                                        "0417",
                                        "0418",
                                        "0419",
                                        "0420",
                                        "0423",
                                        "0425",
                                        "0426",
                                        "0427",
                                        "0428",
                                        "0429",
                                        "0430",
                                        "0432",
                                        "0434",
                                        "0436",
                                        "0437",
                                        "0438",
                                        "0439",
                                        "0441",
                                        "05",
                                        "0501",
                                        "0502",
                                        "0511",
                                        "0512",
                                        "0513",
                                        "0514",
                                        "0515",
                                        "0516",
                                        "0517",
                                        "0519",
                                        "0520",
                                        "0521",
                                        "0522",
                                        "0528",
                                        "0529",
                                        "0532",
                                        "0533",
                                        "0534",
                                        "0536",
                                        "0538",
                                        "0540",
                                        "0541",
                                        "0542",
                                        "0543",
                                        "0544",
                                        "0545",
                                        "06",
                                        "0602",
                                        "0604",
                                        "0605",
                                        "0612",
                                        "0615",
                                        "0616",
                                        "0617",
                                        "0618",
                                        "0619",
                                        "0620",
                                        "0621",
                                        "0622",
                                        "0623",
                                        "0624",
                                        "0625",
                                        "0626",
                                        "0627",
                                        "0628",
                                        "0631",
                                        "0632",
                                        "0633",
                                        "07",
                                        "0701",
                                        "0702",
                                        "0704",
                                        "0706",
                                        "0709",
                                        "0711",
                                        "0713",
                                        "0714",
                                        "0716",
                                        "0718",
                                        "0719",
                                        "0720",
                                        "0722",
                                        "0723",
                                        "0728",
                                        "08",
                                        "0805",
                                        "0806",
                                        "0807",
                                        "0811",
                                        "0814",
                                        "0815",
                                        "0817",
                                        "0819",
                                        "0821",
                                        "0822",
                                        "0826",
                                        "0827",
                                        "0828",
                                        "0829",
                                        "0830",
                                        "0831",
                                        "0833",
                                        "0834",
                                        "09",
                                        "0901",
                                        "0904",
                                        "0906",
                                        "0911",
                                        "0912",
                                        "0914",
                                        "0919",
                                        "0926",
                                        "0928",
                                        "0929",
                                        "0935",
                                        "0937",
                                        "0938",
                                        "0940",
                                        "0941",
                                        "10",
                                        "1001",
                                        "1002",
                                        "1003",
                                        "1004",
                                        "1014",
                                        "1017",
                                        "1018",
                                        "1021",
                                        "1026",
                                        "1027",
                                        "1029",
                                        "1032",
                                        "1034",
                                        "1037",
                                        "1046",
                                        "11",
                                        "1101",
                                        "1102",
                                        "1103",
                                        "1106",
                                        "1111",
                                        "1112",
                                        "1114",
                                        "1119",
                                        "1120",
                                        "1121",
                                        "1122",
                                        "1124",
                                        "1127",
                                        "1129",
                                        "1130",
                                        "1133",
                                        "1134",
                                        "1135",
                                        "1141",
                                        "1142",
                                        "1144",
                                        "1145",
                                        "1146",
                                        "1149",
                                        "1151",
                                        "1154",
                                        "1159",
                                        "1160",
                                        "12",
                                        "1201",
                                        "1211",
                                        "1214",
                                        "1216",
                                        "1219",
                                        "1221",
                                        "1222",
                                        "1223",
                                        "1224",
                                        "1227",
                                        "1228",
                                        "1231",
                                        "1232",
                                        "1233",
                                        "1234",
                                        "1235",
                                        "1238",
                                        "1241",
                                        "1242",
                                        "1243",
                                        "1244",
                                        "1245",
                                        "1246",
                                        "1247",
                                        "1251",
                                        "1252",
                                        "1253",
                                        "1256",
                                        "1259",
                                        "1260",
                                        "1263",
                                        "1264",
                                        "1265",
                                        "1266",
                                        "14",
                                        "1401",
                                        "1411",
                                        "1412",
                                        "1413",
                                        "1416",
                                        "1417",
                                        "1418",
                                        "1419",
                                        "1420",
                                        "1421",
                                        "1422",
                                        "1424",
                                        "1426",
                                        "1428",
                                        "1429",
                                        "1430",
                                        "1431",
                                        "1432",
                                        "1433",
                                        "1438",
                                        "1439",
                                        "1441",
                                        "1443",
                                        "1444",
                                        "1445",
                                        "1449",
                                        "15",
                                        "1502",
                                        "1503",
                                        "1504",
                                        "1505",
                                        "1511",
                                        "1514",
                                        "1515",
                                        "1516",
                                        "1517",
                                        "1519",
                                        "1520",
                                        "1523",
                                        "1524",
                                        "1525",
                                        "1526",
                                        "1528",
                                        "1529",
                                        "1531",
                                        "1532",
                                        "1534",
                                        "1535",
                                        "1539",
                                        "1543",
                                        "1545",
                                        "1546",
                                        "1547",
                                        "1548",
                                        "1551",
                                        "1554",
                                        "1556",
                                        "1557",
                                        "1560",
                                        "1563",
                                        "1566",
                                        "1567",
                                        "1569",
                                        "1571",
                                        "1572",
                                        "1573",
                                        "1576",
                                        "16",
                                        "1601",
                                        "1612",
                                        "1613",
                                        "1617",
                                        "1620",
                                        "1621",
                                        "1622",
                                        "1624",
                                        "1627",
                                        "1630",
                                        "1632",
                                        "1633",
                                        "1634",
                                        "1635",
                                        "1636",
                                        "1638",
                                        "1640",
                                        "1644",
                                        "1648",
                                        "1653",
                                        "1657",
                                        "1662",
                                        "1663",
                                        "1664",
                                        "1665",
                                        "17",
                                        "1702",
                                        "1703",
                                        "1711",
                                        "1714",
                                        "1717",
                                        "1718",
                                        "1719",
                                        "1721",
                                        "1723",
                                        "1724",
                                        "1725",
                                        "1729",
                                        "1736",
                                        "1738",
                                        "1739",
                                        "1740",
                                        "1742",
                                        "1743",
                                        "1744",
                                        "1748",
                                        "1749",
                                        "1750",
                                        "1751",
                                        "1755",
                                        "1756",
                                        "18",
                                        "1804",
                                        "1805",
                                        "1811",
                                        "1812",
                                        "1813",
                                        "1815",
                                        "1816",
                                        "1818",
                                        "1820",
                                        "1822",
                                        "1824",
                                        "1825",
                                        "1826",
                                        "1827",
                                        "1828",
                                        "1832",
                                        "1833",
                                        "1834",
                                        "1835",
                                        "1836",
                                        "1837",
                                        "1838",
                                        "1839",
                                        "1840",
                                        "1841",
                                        "1842",
                                        "1845",
                                        "1848",
                                        "1849",
                                        "1850",
                                        "1851",
                                        "1852",
                                        "1853",
                                        "1854",
                                        "1856",
                                        "1857",
                                        "1859",
                                        "1860",
                                        "1865",
                                        "1866",
                                        "1867",
                                        "1868",
                                        "1870",
                                        "1871",
                                        "1874",
                                        "19",
                                        "1901",
                                        "1902",
                                        "1903",
                                        "1911",
                                        "1913",
                                        "1915",
                                        "1917",
                                        "1919",
                                        "1920",
                                        "1922",
                                        "1923",
                                        "1924",
                                        "1925",
                                        "1926",
                                        "1927",
                                        "1928",
                                        "1929",
                                        "1931",
                                        "1933",
                                        "1936",
                                        "1938",
                                        "1939",
                                        "1940",
                                        "1941",
                                        "1942",
                                        "1943",
                                        "20",
                                        "2002",
                                        "2003",
                                        "2004",
                                        "2011",
                                        "2012",
                                        "2014",
                                        "2015",
                                        "2017",
                                        "2018",
                                        "2019",
                                        "2020",
                                        "2021",
                                        "2022",
                                        "2023",
                                        "2024",
                                        "2025",
                                        "2027",
                                        "2028",
                                        "2030",
                                        "2111",
                                        "2121",
                                        "2131",
                                        "2211",
                                        "2311",
                                        "2321",
                                        "N01",
                                        "N02",
                                        "N03",
                                        "N04",
                                        "N05",
                                        "N06",
                                        "N07",
                                        "N08",
                                        "N09",
                                        "N10",
                                        "N11",
                                        "N12",
                                        "N13",
                                        "N14",
                                        "N15",
                                        "N16",
                                        "N17",
                                        "N18",
                                        "N19",
                                        "N20",
                                        "N21",
                                        "N22",
                                        "N23",
                                        "N24",
                                        "N25",
                                        "N26",
                                        "N27",
                                        "N28",
                                        "N29",
                                        "N30",
                                        "N31",
                                        "N32",
                                        "N33",
                                        "N34",
                                        "N35",
                                        "N36",
                                        "N37",
                                        "N38",
                                        "N39",
                                        "N40",
                                        "N41",
                                        "N42",
                                        "N43",
                                        "N44",
                                        "N45",
                                        "N46",
                                        "N47",
                                        "N48",
                                        "N49",
                                        "N50",
                                        "N51",
                                        "N52",
                                        "N53",
                                        "N54",
                                        "N55",
                                        "N56",
                                        "N57",
                                        "N58",
                                        "N59",
                                        "N60",
                                        "N61",
                                        "N62",
                                        "N63",
                                        "N64",
                                        "N65",
                                        "N66",
                                        "N67",
                                        "N68",
                                        "N69",
                                        "N70",
                                        "N71",
                                        "N72",
                                        "N73",
                                        "N74",
                                        "N75",
                                        "N76",
                                        "N77",
                                        "N78",
                                        "N79",
                                        "N80",
                                        "N81",
                                        "N82",
                                        "N83",
                                        "N99"
                                        ]
                        }
                },
                {
                        "code": "Kjonn",
                        "selection": {
                                "filter": "item",
                                "values": [
                                        "0",
                                        "1",
                                        "2"
                                        ]
                        }
                },
                {
                        "code": "IntroAvbrudd",
                        "selection": {
                                "filter": "item",
                                "values": [
                                        "01",
                                        "02",
                                        "03",
                                        "04",
                                        "05"
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
                                        "DeltakereProg",
                                        "DeltakereProg1"
                                        ]
                        }
                },
                {
                        "code": "Tid",
                        "selection": {
                                "filter": "item",
                                "values": [
                                        "2011",
                                        "2012",
                                        "2013",
                                        "2014"
                                        ]
                        }
                }
                ],
        "response": {
                "format": "json-stat"
        }
}'

d.tmp <- POST(url , body = data, encode = "json", verbose())

# Henter ut innholdet fra d.tmp som tekst deretter bearbeides av fromJSONstat
sbtabell <- fromJSONstat(content(d.tmp, "text"))
ds <- sbtabell[[1]] # Henter ut kun datasettet fra sbtabell

34201*4*2

# Viser datasettet
head(ds)

#her har regionkodene blitt bytta til tekstlige beskrivelser
#library(plyr)
#t = ds
#t = mapvalues(ds$region,from=json_data$variables$valueText[1],to=json_data$variables$values[1])
#t$region = mapvalues(ds$region,from=koder_region$tekst,to=koder_region$verdier)
#dette funker dårlig - hva med database-tilnærminger som left_join?
t = ds
t = left_join(t,koder_region,by=c("region" = "region_tekst"))
t = left_join(t,koder_avslstatus,by=c("arbeidsmarkedsstatus" = "avslstatus_tekst"))
t = left_join(t,koder_avsluttet,by=c("avsluttet eller avbrutt introduksjonsprogram" = "avsluttet_tekst"))
t = left_join(t,koder_enhet,by=c("statistikkvariabel" = "enhet_tekst"))
names(t)[2]="kjonn"
names(t)[6]="aar"
t = left_join(t,koder_kjonn,by=c("kjonn" = "kjonn_tekst")) #feil
t = left_join(t,koder_aar,by=c("aar" = "aar_tekst"))
sum(is.na(t[,8:13]))

#rensker ut de interessante delene av data
t = select(t,value:aar_kode)

#koder om fra SSB-koder til IMDi-koder
#dette kunne vært en del av metadata-bearbeidinga, for evt. å kunne mappe direkte som over.
names(t)[1] = "tabellvariabel"
names(t)[3] = "avslstat4"
t$avslstat4 = as.character(t$avslstat4)
t$avslstat4[t$avslstat4=="00"] = "alle"
t$avslstat4[t$avslstat4=="1"] = "arbutd"
t$avslstat4[t$avslstat4=="2"] = "ledig_tiltak"
t$avslstat4[t$avslstat4=="3"] = "annet"
levels(as.factor(t$avslstat4))
t$aar_kode = as.character(t$aar_kode)
levels(as.factor(t$aar_kode))
names(t)[7]="aar"
t$avsluttet_kode = as.character(t$avsluttet_kode)
t$avsluttet_kode[t$avsluttet_kode=="01"] = "ettaar"
t$avsluttet_kode[t$avsluttet_kode=="02"] = "toaar"
t$avsluttet_kode[t$avsluttet_kode=="03"] = "treaar"
t$avsluttet_kode[t$avsluttet_kode=="04"] = "fireaar"
t$avsluttet_kode[t$avsluttet_kode=="05"] = "femaar"
names(t)[4]="avslutta"
t$temp = t$avslutta
t$temp[t$temp=="ettaar"]=1
t$temp[t$temp=="toaar"]=2
t$temp[t$temp=="treaar"]=3
t$temp[t$temp=="fireaar"]=4
t$temp[t$temp=="femaar"]=5
levels(as.factor(t$temp))
t$kohort=as.numeric(t$aar)-as.numeric(t$temp)
levels(as.factor(t$kohort))
t = select(t,-temp)
t$enhet_kode=as.character(t$enhet_kode)
t$enhet_kode[t$enhet_kode=="DeltakereProg"]="personer"
t$enhet_kode[t$enhet_kode=="DeltakereProg1"]="prosent"
levels(as.factor(t$enhet_kode))
names(t)[5] = "enhet"
t$kjonn_kode = as.character(t$kjonn_kode)
t$kjonn_kode[t$kjonn_kode=="0"]="alle"
t$kjonn_kode[t$kjonn_kode=="1"]="1"
t$kjonn_kode[t$kjonn_kode=="2"]="0"
levels(as.factor(t$kjonn_kode))
names(t)[6] = "kjonn"

#håndtering av missing
sum(is.na(t$tabellvariabel))
#det ser ikke ut til å være noe skille mellom manglende data og undertrykte data. 
#gitt at data som vises ut er fulltelling, og at bare eksisterende kommuner per 2014 vises koder jeg alle NA som missing ":"
t$tabellvariabel[is.na(t$tabellvariabel)==T]=":"
levels(as.factor(t$tabellvariabel))

#legger til tabellnavn
t$tabell_navn ="intro_status_arbutd"

#fjerner avslutta, har utelukkende kohort
t = select(t,-avslutta)

#koder om kohort==2006 fra 0 til manglende "."
t2 = filter(t,kohort=="2006")
sum(as.numeric(t2$tabellvariabel,na.rm=T))==0
t$tabellvariabel[t$kohort=="2006"]="."

#omkoding av regionale koder og oppsplitting i ulike deler
t$region_kode = as.character(t$region_kode)
t_k = filter(t,nchar(region_kode)==4)
nlevels(as.factor(t_k$region_kode)) #full kommuneliste
names(t_k)[2] = "kommune_nr"
#fylke
t_f = filter(t,nchar(region_kode)<3)
levels(as.factor(t_f$region_kode))
names(t_f)[2] = "fylke_nr"
t_f$fylke_nr[t_f$fylke_nr=="0"]="00"
#næringsregion
t_n = filter(t,nchar(region_kode)==3)
levels(as.factor(t_n$region_kode))
t_n$region_kode = gsub("N","",t_n$region_kode)
levels(as.factor(t_n$region_kode))
names(t_n)[2] = "naringsregion_nr"
#bydel
t_b = filter(t,nchar(region_kode)>4)
levels(as.factor(t_b$region_kode))
t_b$region_kode = gsub("a","",t_b$region_kode)
levels(as.factor(t_b$region_kode))
names(t_b)[2] = "bydel_nr"

#sjekk av kombinasjoner
#bydel
apply(t_b,2, function(x){nlevels(as.factor(x))})
nrow(t_b)==19*4*2*3*5*4

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

#output
write.csv(t_b,"data_flat_output/intro_status_arbutd-bydel-2011_2014-test.csv", row.names=F)
write.csv(t_k,"data_flat_output/intro_status_arbutd-kommune-2011_2014-test.csv", row.names=F)
write.csv(t_n,"data_flat_output/intro_status_arbutd-naringsregion-2011_2014-test.csv", row.names=F)
write.csv(t_f,"data_flat_output/intro_status_arbutd-fylke-2011_2014-test.csv", row.names=F)

#sjekk av trondheim
t2 = filter(t_k,kommune_nr=="1601")

