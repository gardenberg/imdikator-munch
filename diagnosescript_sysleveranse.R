#diagnosescript
#tabell 60
tabell <- read.csv("~/R/datamunch_indikator/diagnose/TAB_1_60A.csv", stringsAsFactors=FALSE, colClasses="character")
levels(as.factor(tabell[,1]))
levels(as.factor(tabell[,2]))
levels(as.factor(tabell[,3]))
levels(as.factor(tabell[,4]))
levels(as.factor(tabell[,5]))
levels(as.factor(tabell[,6]))
levels(as.factor(tabell[,7]))
#logisk sjekk
df=subset(tabell,aar=="2014")
nrow(df)==nlevels(as.factor(df[,2]))*6*3*2
#substanssjekk - populasjonen
df=subset(tabell,aar=="2014"&kommune_nr=="0000"&kjonn=="alle"&enhet=="personer")
write.csv(df,"tab_1_60a_uttrekk.csv",row.names=F)

tabell <- read.csv("~/R/datamunch_indikator/diagnose/TAB_2_60b.csv", stringsAsFactors=FALSE, colClasses="character")
levels(as.factor(tabell[,1]))
levels(as.factor(tabell[,2]))
levels(as.factor(tabell[,3]))
levels(as.factor(tabell[,4]))
levels(as.factor(tabell[,5]))
levels(as.factor(tabell[,6]))
levels(as.factor(tabell[,7]))
#logisk sjekk
df=subset(tabell,aar=="2014")
nrow(df)==nlevels(as.factor(df[,2]))*6*3*2

tabell <- read.csv("~/R/datamunch_indikator/diagnose/TAB_3_60c.csv", stringsAsFactors=FALSE, colClasses="character")
levels(as.factor(tabell[,1]))
levels(as.factor(tabell[,2]))
levels(as.factor(tabell[,3]))
levels(as.factor(tabell[,4]))
levels(as.factor(tabell[,5]))
levels(as.factor(tabell[,6]))
levels(as.factor(tabell[,7]))
#logisk sjekk
df=subset(tabell,aar=="2014")
nrow(df)==nlevels(as.factor(df[,2]))*6*3*2
#substanssjekk - populasjonen
df=subset(tabell,aar=="2014"&fylke_nr=="00"&kjonn=="alle"&enhet=="personer")


tabell <- read.csv("~/R/datamunch_indikator/diagnose/TAB_4_60d.csv", stringsAsFactors=FALSE, colClasses="character")
levels(as.factor(tabell[,1]))
levels(as.factor(tabell[,2]))
levels(as.factor(tabell[,3]))
levels(as.factor(tabell[,4]))
levels(as.factor(tabell[,5]))
levels(as.factor(tabell[,6]))
levels(as.factor(tabell[,7]))
#logisk sjekk
df=subset(tabell,aar=="2014")
nrow(df)==nlevels(as.factor(df[,2]))*6*3*2

#tabell 61.1
tabell <- read.csv("~/R/datamunch_indikator/diagnose/TAB_5_611a.csv", stringsAsFactors=FALSE, colClasses="character")
levels(as.factor(tabell[,1]))
levels(as.factor(tabell[,2]))
levels(as.factor(tabell[,3]))
levels(as.factor(tabell[,4]))
levels(as.factor(tabell[,5]))
levels(as.factor(tabell[,6]))
#logisk sjekk
df=subset(tabell,aar=="2014")
nrow(df)==nlevels(as.factor(df[,2]))*3*2
#substanssjekk - populasjonen
df=subset(tabell,aar=="2014"&kommune_nr=="0000"&innvkat_2=="alle")

tabell <- read.csv("~/R/datamunch_indikator/diagnose/TAB_6_611b.csv", stringsAsFactors=FALSE, colClasses="character")
levels(as.factor(tabell[,1]))
levels(as.factor(tabell[,2]))
levels(as.factor(tabell[,3]))
levels(as.factor(tabell[,4]))
levels(as.factor(tabell[,5]))
levels(as.factor(tabell[,6]))
#logisk sjekk
df=subset(tabell,aar=="2014")
nrow(df)==nlevels(as.factor(df[,2]))*3*2

tabell <- read.csv("~/R/datamunch_indikator/diagnose/TAB_7_611c.csv", stringsAsFactors=FALSE, colClasses="character")
levels(as.factor(tabell[,1]))
levels(as.factor(tabell[,2]))
levels(as.factor(tabell[,3]))
levels(as.factor(tabell[,4]))
levels(as.factor(tabell[,5]))
levels(as.factor(tabell[,6]))
#logisk sjekk
df=subset(tabell,aar=="2014")
nrow(df)==nlevels(as.factor(df[,2]))*3*2

tabell <- read.csv("~/R/datamunch_indikator/diagnose/TAB_8_611d.csv", stringsAsFactors=FALSE, colClasses="character")
levels(as.factor(tabell[,1]))
levels(as.factor(tabell[,2]))
levels(as.factor(tabell[,3]))
levels(as.factor(tabell[,4]))
levels(as.factor(tabell[,5]))
levels(as.factor(tabell[,6]))
#logisk sjekk
df=subset(tabell,aar=="2014")
nrow(df)==nlevels(as.factor(df[,2]))*3*2

#tabell 61.2 - sysselsetting etter alder og innvkat
tabell <- read.csv("~/R/datamunch_indikator/diagnose/TAB_9_612a.csv", stringsAsFactors=FALSE, colClasses="character")
levels(as.factor(tabell[,1]))
levels(as.factor(tabell[,2]))
levels(as.factor(tabell[,3]))
levels(as.factor(tabell[,4]))
levels(as.factor(tabell[,5]))
levels(as.factor(tabell[,6]))
levels(as.factor(tabell[,7]))
#logisk sjekk
df=subset(tabell,aar=="2014")
nrow(df)==nlevels(as.factor(df[,2]))*3*4*2
#substanssjekk - populasjonen
df=subset(tabell,aar=="2014"&kommune_nr=="0000"&innvkat_2=="alle"&arbeidsalder_18_69=="alle")

tabell <- read.csv("~/R/datamunch_indikator/diagnose/TAB_10_612b.csv", stringsAsFactors=FALSE, colClasses="character")
levels(as.factor(tabell[,1]))
levels(as.factor(tabell[,2]))
levels(as.factor(tabell[,3]))
levels(as.factor(tabell[,4]))
levels(as.factor(tabell[,5]))
levels(as.factor(tabell[,6]))
levels(as.factor(tabell[,7]))
#logisk sjekk
df=subset(tabell,aar=="2014")
nrow(df)==nlevels(as.factor(df[,2]))*3*4*2

tabell <- read.csv("~/R/datamunch_indikator/diagnose/TAB_11_612c.csv", stringsAsFactors=FALSE, colClasses="character")
levels(as.factor(tabell[,1]))
levels(as.factor(tabell[,2]))
levels(as.factor(tabell[,3]))
levels(as.factor(tabell[,4]))
levels(as.factor(tabell[,5]))
levels(as.factor(tabell[,6]))
levels(as.factor(tabell[,7]))
#logisk sjekk
df=subset(tabell,aar=="2014")
nrow(df)==nlevels(as.factor(df[,2]))*3*4*2

tabell <- read.csv("~/R/datamunch_indikator/diagnose/TAB_12_612d.csv", stringsAsFactors=FALSE, colClasses="character")
levels(as.factor(tabell[,1]))
levels(as.factor(tabell[,2]))
levels(as.factor(tabell[,3]))
levels(as.factor(tabell[,4]))
levels(as.factor(tabell[,5]))
levels(as.factor(tabell[,6]))
levels(as.factor(tabell[,7]))
#logisk sjekk
df=subset(tabell,aar=="2014")
nrow(df)==nlevels(as.factor(df[,2]))*3*4*2

#tabell 62 - sysselsetting innvandrere etter botid 
tabell <- read.csv("~/R/datamunch_indikator/diagnose/TAB_13_62a.csv", stringsAsFactors=FALSE, colClasses="character")
levels(as.factor(tabell[,1]))
levels(as.factor(tabell[,2]))
levels(as.factor(tabell[,3]))
levels(as.factor(tabell[,4]))
levels(as.factor(tabell[,5]))
levels(as.factor(tabell[,6]))
#logisk sjekk
df=subset(tabell,aar=="2014")
nrow(df)==nlevels(as.factor(df[,2]))*5*2
#substanssjekk - populasjonen
df=subset(tabell,aar=="2014"&kommune_nr=="0000"&botid_5=="alle")
#substanssjekk - uoppgiott
df=subset(tabell,aar=="2014"&kommune_nr=="0000"&botid_5=="uoppgitt")

tabell <- read.csv("~/R/datamunch_indikator/diagnose/TAB_14_62b.csv", stringsAsFactors=FALSE, colClasses="character")
levels(as.factor(tabell[,1]))
levels(as.factor(tabell[,2]))
levels(as.factor(tabell[,3]))
levels(as.factor(tabell[,4]))
levels(as.factor(tabell[,5]))
levels(as.factor(tabell[,6]))
#logisk sjekk
df=subset(tabell,aar=="2014")
nrow(df)==nlevels(as.factor(df[,2]))*5*2

tabell <- read.csv("~/R/datamunch_indikator/diagnose/TAB_15_62c.csv", stringsAsFactors=FALSE, colClasses="character")
levels(as.factor(tabell[,1]))
levels(as.factor(tabell[,2]))
levels(as.factor(tabell[,3]))
levels(as.factor(tabell[,4]))
levels(as.factor(tabell[,5]))
levels(as.factor(tabell[,6]))
#logisk sjekk
df=subset(tabell,aar=="2014")
nrow(df)==nlevels(as.factor(df[,2]))*5*2

tabell <- read.csv("~/R/datamunch_indikator/diagnose/TAB_16_62d.csv", stringsAsFactors=FALSE, colClasses="character")
levels(as.factor(tabell[,1]))
levels(as.factor(tabell[,2]))
levels(as.factor(tabell[,3]))
levels(as.factor(tabell[,4]))
levels(as.factor(tabell[,5]))
levels(as.factor(tabell[,6]))
#logisk sjekk
df=subset(tabell,aar=="2014")
nrow(df)==nlevels(as.factor(df[,2]))*5*2

#tabell 63.1 - sysselsetting innvandrere etter botid og landbakgrunn 
tabell <- read.csv("~/R/datamunch_indikator/diagnose/TAB_17_631a.csv", stringsAsFactors=FALSE, colClasses="character")
levels(as.factor(tabell[,1]))
levels(as.factor(tabell[,2]))
levels(as.factor(tabell[,3]))
levels(as.factor(tabell[,4]))
rlevels(as.factor(tabell[,5]))
levels(as.factor(tabell[,6]))
#logisk sjekk
df=subset(tabell,aar=="2014")
nrow(df)==nlevels(as.factor(df[,2]))*5*3*2
#substanssjekk - populasjonen
df=subset(tabell,aar=="2014"&kommune_nr=="0000"&botid_5=="alle")
#substanssjekk - uoppgiott
df=subset(tabell,aar=="2014"&kommune_nr=="0000"&botid_5=="uoppgitt")

tabell <- read.csv("~/R/datamunch_indikator/diagnose/TAB_18_631b.csv", stringsAsFactors=FALSE, colClasses="character")
levels(as.factor(tabell[,1]))
levels(as.factor(tabell[,2]))
levels(as.factor(tabell[,3]))
levels(as.factor(tabell[,4]))
levels(as.factor(tabell[,5]))
levels(as.factor(tabell[,6]))
#logisk sjekk
df=subset(tabell,aar=="2014")
nrow(df)==nlevels(as.factor(df[,2]))*5*3*2

tabell <- read.csv("~/R/datamunch_indikator/diagnose/TAB_19_631c.csv", stringsAsFactors=FALSE, colClasses="character")
levels(as.factor(tabell[,1]))
levels(as.factor(tabell[,2]))
levels(as.factor(tabell[,3]))
levels(as.factor(tabell[,4]))
levels(as.factor(tabell[,5]))
levels(as.factor(tabell[,6]))
#logisk sjekk
df=subset(tabell,aar=="2014")
nrow(df)==nlevels(as.factor(df[,2]))*5*3*2

tabell <- read.csv("~/R/datamunch_indikator/diagnose/TAB_20_631d.csv", stringsAsFactors=FALSE, colClasses="character")
levels(as.factor(tabell[,1]))
levels(as.factor(tabell[,2]))
levels(as.factor(tabell[,3]))
levels(as.factor(tabell[,4]))
levels(as.factor(tabell[,5]))
levels(as.factor(tabell[,6]))
#logisk sjekk
df=subset(tabell,aar=="2014")
nrow(df)==nlevels(as.factor(df[,2]))*5*3*2

#tabell 63.2 - sysselsetting innvandrere etter kjønn og landbakgrunn 
tabell <- read.csv("~/R/datamunch_indikator/diagnose/TAB_21_632a.csv", stringsAsFactors=FALSE, colClasses="character")
levels(as.factor(tabell[,1]))
levels(as.factor(tabell[,2]))
levels(as.factor(tabell[,3]))
levels(as.factor(tabell[,4]))
levels(as.factor(tabell[,5]))
levels(as.factor(tabell[,6]))
#logisk sjekk
df=subset(tabell,aar=="2014")
nrow(df)==nlevels(as.factor(df[,2]))*3*3*2
#substanssjekk - populasjonen
df=subset(tabell,aar=="2014"&kommune_nr=="0000"&vreg_3=="alle"&kjonn=="alle")
#substanssjekk - uoppgiott
df=subset(tabell,aar=="2014"&kommune_nr=="0000"&botid_5=="uoppgitt")

tabell <- read.csv("~/R/datamunch_indikator/diagnose/TAB_18_631b.csv", stringsAsFactors=FALSE, colClasses="character")
levels(as.factor(tabell[,1]))
levels(as.factor(tabell[,2]))
levels(as.factor(tabell[,3]))
levels(as.factor(tabell[,4]))
levels(as.factor(tabell[,5]))
levels(as.factor(tabell[,6]))
#logisk sjekk
df=subset(tabell,aar=="2014")
nrow(df)==nlevels(as.factor(df[,2]))*5*3*2

tabell <- read.csv("~/R/datamunch_indikator/diagnose/TAB_19_631c.csv", stringsAsFactors=FALSE, colClasses="character")
levels(as.factor(tabell[,1]))
levels(as.factor(tabell[,2]))
levels(as.factor(tabell[,3]))
levels(as.factor(tabell[,4]))
levels(as.factor(tabell[,5]))
levels(as.factor(tabell[,6]))
#logisk sjekk
df=subset(tabell,aar=="2014")
nrow(df)==nlevels(as.factor(df[,2]))*5*3*2

tabell <- read.csv("~/R/datamunch_indikator/diagnose/TAB_20_631d.csv", stringsAsFactors=FALSE, colClasses="character")
levels(as.factor(tabell[,1]))
levels(as.factor(tabell[,2]))
levels(as.factor(tabell[,3]))
levels(as.factor(tabell[,4]))
levels(as.factor(tabell[,5]))
levels(as.factor(tabell[,6]))
#logisk sjekk
df=subset(tabell,aar=="2014")
nrow(df)==nlevels(as.factor(df[,2]))*5*3*2