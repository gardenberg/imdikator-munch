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

#DIAGNOSESCRIPT LEVERANSE 2016 - 28. JULI
#tabell 60a
#1. lese inn data
df <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 2-2016/sysselsetting/DIAGNOSE/Tab_1_60a.csv", stringsAsFactors=FALSE,colClasses="character")
#2. har vi fått variablene vi har bedt om? er variablene og kategoriene kodet riktig?
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
apply(subset(df,select=-tabellvariabel),2, function(x){nlevels(as.factor(x))})
#3. er det riktig antall observasjoner?
nrow(df)==429*7*3*2
#4. er dette substansielt riktige tall?
t = subset(df,kommune_nr=='0000'&kjonn=='alle'&enhet=='personer')
t = subset(df,kommune_nr=='0101'&kjonn=='alle'&enhet=='personer')
t = subset(df,kommune_nr=='0101'&innvgrunn_6=='alle'&enhet=='personer')

#tabell 60b
#1. lese inn data
df <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 2-2016/sysselsetting/DIAGNOSE/Tab_2_60b.csv", stringsAsFactors=FALSE,colClasses="character")
#2. har vi fått variablene vi har bedt om? er variablene og kategoriene kodet riktig?
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
apply(subset(df,select=-tabellvariabel),2, function(x){nlevels(as.factor(x))})
#3. er det riktig antall observasjoner?
nrow(df)==16*7*3*2
#4. er dette substansielt riktige tall?
t = subset(df,kommune_nr=='0000'&kjonn=='alle'&enhet=='personer')
t = subset(df,kommune_nr=='0101'&kjonn=='alle'&enhet=='personer')
t = subset(df,kommune_nr=='0101'&innvgrunn_6=='alle'&enhet=='personer')

#tabell 60c
#1. lese inn data
df <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 2-2016/sysselsetting/DIAGNOSE/Tab_3_60c.csv", stringsAsFactors=FALSE,colClasses="character")
#2. har vi fått variablene vi har bedt om? er variablene og kategoriene kodet riktig?
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
apply(subset(df,select=-tabellvariabel),2, function(x){nlevels(as.factor(x))})
#3. er det riktig antall observasjoner?
nrow(df)==20*7*3*2
#4. er dette substansielt riktige tall?
t = subset(df,kommune_nr=='0000'&kjonn=='alle'&enhet=='personer')
t = subset(df,kommune_nr=='0101'&kjonn=='alle'&enhet=='personer')
t = subset(df,kommune_nr=='0101'&innvgrunn_6=='alle'&enhet=='personer')

#tabell 60d
#1. lese inn data
df <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 2-2016/sysselsetting/DIAGNOSE/Tab_4_60d.csv", stringsAsFactors=FALSE,colClasses="character")
#2. har vi fått variablene vi har bedt om? er variablene og kategoriene kodet riktig?
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
apply(subset(df,select=-tabellvariabel),2, function(x){nlevels(as.factor(x))})
#3. er det riktig antall observasjoner?
nrow(df)==83*7*3*2
#4. er dette substansielt riktige tall?
t = subset(df,naringsregion_nr=='00'&kjonn=='alle'&enhet=='personer')
t = subset(df,naringsregion_nr=='01'&kjonn=='alle'&enhet=='personer')
t = subset(df,naringsregion_nr=='00'&innvgrunn_6=='alle'&enhet=='personer')

#tabell 611a
#1. lese inn data
df <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 2-2016/sysselsetting/DIAGNOSE/Tab_5_611a.csv", stringsAsFactors=FALSE,colClasses="character")
#2. har vi fått variablene vi har bedt om? er variablene og kategoriene kodet riktig?
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
apply(subset(df,select=-tabellvariabel),2, function(x){nlevels(as.factor(x))})
#3. er det riktig antall observasjoner?
nrow(df)==429*3*2
#4. er dette substansielt riktige tall?
t = subset(df,kommune_nr=='0000'&enhet=='personer')
t = subset(df,kommune_nr=='0101'&kjonn=='alle'&enhet=='personer')

#tabell 611b
#1. lese inn data
df <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 2-2016/sysselsetting/DIAGNOSE/Tab_6_611b.csv", stringsAsFactors=FALSE,colClasses="character")
#2. har vi fått variablene vi har bedt om? er variablene og kategoriene kodet riktig?
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
apply(subset(df,select=-tabellvariabel),2, function(x){nlevels(as.factor(x))})
#3. er det riktig antall observasjoner?
nrow(df)==15*3*2
#4. er dette substansielt riktige tall?
t = subset(df,kommune_nr=='0000'&enhet=='personer')
t = subset(df,kommune_nr=='0101'&kjonn=='alle'&enhet=='personer')

#tabell 611c
#1. lese inn data
df <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 2-2016/sysselsetting/DIAGNOSE/Tab_7_611c.csv", stringsAsFactors=FALSE,colClasses="character")
#2. har vi fått variablene vi har bedt om? er variablene og kategoriene kodet riktig?
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
apply(subset(df,select=-tabellvariabel),2, function(x){nlevels(as.factor(x))})
#3. er det riktig antall observasjoner?
nrow(df)==20*3*2
#4. er dette substansielt riktige tall?
t = subset(df,kommune_nr=='0000'&kjonn=='alle'&enhet=='personer')
t = subset(df,kommune_nr=='0101'&kjonn=='alle'&enhet=='personer')
t = subset(df,kommune_nr=='0101'&innvgrunn_6=='alle'&enhet=='personer')

#tabell 60d
#1. lese inn data
df <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 2-2016/sysselsetting/DIAGNOSE/Tab_8_611d.csv", stringsAsFactors=FALSE,colClasses="character")
#2. har vi fått variablene vi har bedt om? er variablene og kategoriene kodet riktig?
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
apply(subset(df,select=-tabellvariabel),2, function(x){nlevels(as.factor(x))})
#3. er det riktig antall observasjoner?
nrow(df)==83*7*3*2
#4. er dette substansielt riktige tall?
t = subset(df,naringsregion_nr=='00'&kjonn=='alle'&enhet=='personer')
t = subset(df,naringsregion_nr=='01'&kjonn=='alle'&enhet=='personer')
t = subset(df,naringsregion_nr=='00'&innvgrunn_6=='alle'&enhet=='personer')

#tabell 612a
#1. lese inn data
df <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 2-2016/sysselsetting/DIAGNOSE/Tab_9_612a.csv", stringsAsFactors=FALSE,colClasses="character")
#2. har vi fått variablene vi har bedt om? er variablene og kategoriene kodet riktig?
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
apply(subset(df,select=-tabellvariabel),2, function(x){nlevels(as.factor(x))})
#3. er det riktig antall observasjoner?
nrow(df)==429*3*4*2
#4. er dette substansielt riktige tall?
t = subset(df,kommune_nr=='0000'&enhet=='personer')
t = subset(df,kommune_nr=='0101'&enhet=='personer')

#tabell 612b
#1. lese inn data
df <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 2-2016/sysselsetting/DIAGNOSE/Tab_10_612b.csv", stringsAsFactors=FALSE,colClasses="character")
#2. har vi fått variablene vi har bedt om? er variablene og kategoriene kodet riktig?
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
apply(subset(df,select=-tabellvariabel),2, function(x){nlevels(as.factor(x))})
#3. er det riktig antall observasjoner?
nrow(df)==15*3*4*2
#4. er dette substansielt riktige tall?
t = subset(df,bydel_nr=='00'&enhet=='personer')
t = subset(df,bydel_nr=='00'&kjonn=='alle'&enhet=='personer')

#tabell 612c
#1. lese inn data
df <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 2-2016/sysselsetting/DIAGNOSE/Tab_11_612c.csv", stringsAsFactors=FALSE,colClasses="character")
#2. har vi fått variablene vi har bedt om? er variablene og kategoriene kodet riktig?
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
apply(subset(df,select=-tabellvariabel),2, function(x){nlevels(as.factor(x))})
#3. er det riktig antall observasjoner?
nrow(df)==20*3**42
#4. er dette substansielt riktige tall?
t = subset(df,fylke_nr=='00'&kjonn=='alle'&enhet=='personer')
t = subset(df,fylke_nr=='01'&kjonn=='alle'&enhet=='personer')
t = subset(df,fylke_nr=='01'&innvgrunn_6=='alle'&enhet=='personer')

#tabell 612d
#1. lese inn data
df <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 2-2016/sysselsetting/DIAGNOSE/Tab_12_612d.csv", stringsAsFactors=FALSE,colClasses="character")
#2. har vi fått variablene vi har bedt om? er variablene og kategoriene kodet riktig?
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
apply(subset(df,select=-tabellvariabel),2, function(x){nlevels(as.factor(x))})
#3. er det riktig antall observasjoner?
nrow(df)==83*3*4*2
#4. er dette substansielt riktige tall?
t = subset(df,naringsregion_nr=='01'&kjonn=='alle'&enhet=='personer')
t = subset(df,naringsregion_nr=='01'&kjonn=='alle'&enhet=='personer')
t = subset(df,naringsregion_nr=='01'&innvgrunn_6=='alle'&enhet=='personer')

#tabell 62a
#1. lese inn data
df <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 2-2016/sysselsetting/DIAGNOSE/Tab_13_62a.csv", stringsAsFactors=FALSE,colClasses="character")
#2. har vi fått variablene vi har bedt om? er variablene og kategoriene kodet riktig?
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
apply(subset(df,select=-tabellvariabel),2, function(x){nlevels(as.factor(x))})
#3. er det riktig antall observasjoner?
nrow(df)==429*4*2
#4. er dette substansielt riktige tall?
t = subset(df,kommune_nr=='0000'&enhet=='personer')
t = subset(df,kommune_nr=='0101'&enhet=='personer')
t = subset(df,kommune_nr=='0000'&enhet=='prosent')
t = subset(df,kommune_nr=='0101'&enhet=='prosent')

#tabell 62b
#1. lese inn data
df <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 2-2016/sysselsetting/DIAGNOSE/Tab_14_62b.csv", stringsAsFactors=FALSE,colClasses="character")
#2. har vi fått variablene vi har bedt om? er variablene og kategoriene kodet riktig?
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
apply(subset(df,select=-tabellvariabel),2, function(x){nlevels(as.factor(x))})
#3. er det riktig antall observasjoner?
nrow(df)==15*4*2
#4. er dette substansielt riktige tall?
t = subset(df,bydel_nr=='00'&enhet=='personer')
t = subset(df,bydel_nr=='030101'&enhet=='personer')
t = subset(df,bydel_nr=='00'&enhet=='prosent')
t = subset(df,bydel_nr=='030101'&enhet=='prosent')

#tabell 62c
#1. lese inn data
df <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 2-2016/sysselsetting/DIAGNOSE/Tab_15_62c.csv", stringsAsFactors=FALSE,colClasses="character")
#2. har vi fått variablene vi har bedt om? er variablene og kategoriene kodet riktig?
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
apply(subset(df,select=-tabellvariabel),2, function(x){nlevels(as.factor(x))})
#3. er det riktig antall observasjoner?
nrow(df)==20*4*2
#4. er dette substansielt riktige tall?
t = subset(df,fylke_nr=='00'&enhet=='personer')
t = subset(df,fylke_nr=='01'&enhet=='personer')
t = subset(df,fylke_nr=='00'&enhet=='prosent')
t = subset(df,fylke_nr=='01'&enhet=='prosent')

#tabell 62d
#1. lese inn data
df <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 2-2016/sysselsetting/DIAGNOSE/Tab_16_62d.csv", stringsAsFactors=FALSE,colClasses="character")
#2. har vi fått variablene vi har bedt om? er variablene og kategoriene kodet riktig?
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
apply(subset(df,select=-tabellvariabel),2, function(x){nlevels(as.factor(x))})
#3. er det riktig antall observasjoner?
nrow(df)==83*4*2
#4. er dette substansielt riktige tall?
t = subset(df,naringsregion_nr=='01'&enhet=='personer')
t = subset(df,naringsregion_nr=='02'&enhet=='personer')
t = subset(df,naringsregion_nr=='01'&enhet=='prosent')
t = subset(df,naringsregion_nr=='02'&enhet=='prosent')

#tabell 63.1a
#1. lese inn data
df <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 2-2016/sysselsetting/DIAGNOSE/Tab_17_631a.csv", stringsAsFactors=FALSE,colClasses="character")
#2. har vi fått variablene vi har bedt om? er variablene og kategoriene kodet riktig?
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
apply(subset(df,select=-tabellvariabel),2, function(x){nlevels(as.factor(x))})
#3. er det riktig antall observasjoner?
nrow(df)==429*3*4*2
#4. er dette substansielt riktige tall?
t = subset(df,kommune_nr=='0000'&enhet=='personer')
t = subset(df,kommune_nr=='0101'&enhet=='personer')
t = subset(df,kommune_nr=='0000'&enhet=='prosent')
t = subset(df,kommune_nr=='0101'&enhet=='prosent')

#tabell 631b
#1. lese inn data
df <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 2-2016/sysselsetting/DIAGNOSE/Tab_18_631b.csv", stringsAsFactors=FALSE,colClasses="character")
#2. har vi fått variablene vi har bedt om? er variablene og kategoriene kodet riktig?
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
apply(subset(df,select=-tabellvariabel),2, function(x){nlevels(as.factor(x))})
#3. er det riktig antall observasjoner?
nrow(df)==15*3*4*2
#4. er dette substansielt riktige tall?
t = subset(df,bydel_nr=='00'&enhet=='personer')
t = subset(df,bydel_nr=='030101'&enhet=='personer')
t = subset(df,bydel_nr=='00'&enhet=='prosent')
t = subset(df,bydel_nr=='030101'&enhet=='prosent')

#tabell 631c
#1. lese inn data
df <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 2-2016/sysselsetting/DIAGNOSE/Tab_19_631c.csv", stringsAsFactors=FALSE,colClasses="character")
#2. har vi fått variablene vi har bedt om? er variablene og kategoriene kodet riktig?
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
apply(subset(df,select=-tabellvariabel),2, function(x){nlevels(as.factor(x))})
#3. er det riktig antall observasjoner?
nrow(df)==20*3*4*2
#4. er dette substansielt riktige tall?
t = subset(df,fylke_nr=='00'&enhet=='personer')
t = subset(df,fylke_nr=='01'&enhet=='personer')
t = subset(df,fylke_nr=='00'&enhet=='prosent')
t = subset(df,fylke_nr=='01'&enhet=='prosent')

#tabell 631d
#1. lese inn data
df <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 2-2016/sysselsetting/DIAGNOSE/Tab_20_631d.csv", stringsAsFactors=FALSE,colClasses="character")
#2. har vi fått variablene vi har bedt om? er variablene og kategoriene kodet riktig?
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
apply(subset(df,select=-tabellvariabel),2, function(x){nlevels(as.factor(x))})
#3. er det riktig antall observasjoner?
nrow(df)==83*3*4*2
#4. er dette substansielt riktige tall?
t = subset(df,naringsregion_nr=='01'&enhet=='personer')
t = subset(df,naringsregion_nr=='02'&enhet=='personer')
t = subset(df,naringsregion_nr=='01'&enhet=='prosent')
t = subset(df,naringsregion_nr=='02'&enhet=='prosent')

#tabell 63.2a
#1. lese inn data
df <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 2-2016/sysselsetting/DIAGNOSE/Tab_21_632a.csv", stringsAsFactors=FALSE,colClasses="character")
#2. har vi fått variablene vi har bedt om? er variablene og kategoriene kodet riktig?
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
apply(subset(df,select=-tabellvariabel),2, function(x){nlevels(as.factor(x))})
#3. er det riktig antall observasjoner?
nrow(df)==429*3*3*2
#4. er dette substansielt riktige tall?
t = subset(df,kommune_nr=='0000'&enhet=='personer')
t = subset(df,kommune_nr=='0101'&enhet=='personer')
t = subset(df,kommune_nr=='0000'&enhet=='prosent')
t = subset(df,kommune_nr=='0101'&enhet=='prosent')

#tabell 632b
#1. lese inn data
df <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 2-2016/sysselsetting/DIAGNOSE/Tab_22_632b.csv", stringsAsFactors=FALSE,colClasses="character")
#2. har vi fått variablene vi har bedt om? er variablene og kategoriene kodet riktig?
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
apply(subset(df,select=-tabellvariabel),2, function(x){nlevels(as.factor(x))})
#3. er det riktig antall observasjoner?
nrow(df)==15*3*3*2
#4. er dette substansielt riktige tall?
t = subset(df,bydel_nr=='00'&enhet=='personer')
t = subset(df,bydel_nr=='030101'&enhet=='personer')
t = subset(df,bydel_nr=='00'&enhet=='prosent')
t = subset(df,bydel_nr=='030101'&enhet=='prosent')

#tabell 632c
#1. lese inn data
df <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 2-2016/sysselsetting/DIAGNOSE/Tab_23_632c.csv", stringsAsFactors=FALSE,colClasses="character")
#2. har vi fått variablene vi har bedt om? er variablene og kategoriene kodet riktig?
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
apply(subset(df,select=-tabellvariabel),2, function(x){nlevels(as.factor(x))})
#3. er det riktig antall observasjoner?
nrow(df)==20*3*3*2
#4. er dette substansielt riktige tall?
t = subset(df,fylke_nr=='00'&enhet=='personer')
t = subset(df,fylke_nr=='01'&enhet=='personer')
t = subset(df,fylke_nr=='00'&enhet=='prosent')
t = subset(df,fylke_nr=='01'&enhet=='prosent')

#tabell 632d
#1. lese inn data
df <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 2-2016/sysselsetting/DIAGNOSE/Tab_24_632d.csv", stringsAsFactors=FALSE,colClasses="character")
#2. har vi fått variablene vi har bedt om? er variablene og kategoriene kodet riktig?
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
apply(subset(df,select=-tabellvariabel),2, function(x){nlevels(as.factor(x))})
#3. er det riktig antall observasjoner?
nrow(df)==83*3*3*2
#4. er dette substansielt riktige tall?
t = subset(df,naringsregion_nr=='01'&enhet=='personer')
t = subset(df,naringsregion_nr=='02'&enhet=='personer')
t = subset(df,naringsregion_nr=='01'&enhet=='prosent')
t = subset(df,naringsregion_nr=='02'&enhet=='prosent')

#tabell 63.3a
#1. lese inn data
df <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 2-2016/sysselsetting/DIAGNOSE/Tab_25_633a.csv", stringsAsFactors=FALSE,colClasses="character")
#2. har vi fått variablene vi har bedt om? er variablene og kategoriene kodet riktig?
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
apply(subset(df,select=-tabellvariabel),2, function(x){nlevels(as.factor(x))})
#3. er det riktig antall observasjoner?
nrow(df)==429*9*2
#4. er dette substansielt riktige tall?
t = subset(df,kommune_nr=='0000'&enhet=='personer')
t = subset(df,kommune_nr=='0101'&enhet=='personer')
t = subset(df,kommune_nr=='0000'&enhet=='prosent')
t = subset(df,kommune_nr=='0101'&enhet=='prosent')

#tabell 633b
#1. lese inn data
df <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 2-2016/sysselsetting/DIAGNOSE/Tab_26_633b.csv", stringsAsFactors=FALSE,colClasses="character")
#2. har vi fått variablene vi har bedt om? er variablene og kategoriene kodet riktig?
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
apply(subset(df,select=-tabellvariabel),2, function(x){nlevels(as.factor(x))})
#3. er det riktig antall observasjoner?
nrow(df)==15*9*2
#4. er dette substansielt riktige tall?
t = subset(df,bydel_nr=='00'&enhet=='personer')
t = subset(df,bydel_nr=='030101'&enhet=='personer')
t = subset(df,bydel_nr=='00'&enhet=='prosent')
t = subset(df,bydel_nr=='030101'&enhet=='prosent')

#tabell 633c
#1. lese inn data
df <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 2-2016/sysselsetting/DIAGNOSE/Tab_27_633c.csv", stringsAsFactors=FALSE,colClasses="character")
#2. har vi fått variablene vi har bedt om? er variablene og kategoriene kodet riktig?
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
apply(subset(df,select=-tabellvariabel),2, function(x){nlevels(as.factor(x))})
#3. er det riktig antall observasjoner?
nrow(df)==20*9*2
#4. er dette substansielt riktige tall?
t = subset(df,fylke_nr=='00'&enhet=='personer')
t = subset(df,fylke_nr=='01'&enhet=='personer')
t = subset(df,fylke_nr=='00'&enhet=='prosent')
t = subset(df,fylke_nr=='01'&enhet=='prosent')

#tabell 633d
#1. lese inn data
df <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 2-2016/sysselsetting/DIAGNOSE/Tab_28_633d.csv", stringsAsFactors=FALSE,colClasses="character")
#2. har vi fått variablene vi har bedt om? er variablene og kategoriene kodet riktig?
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
apply(subset(df,select=-tabellvariabel),2, function(x){nlevels(as.factor(x))})
#3. er det riktig antall observasjoner?
nrow(df)==83*9*2
#4. er dette substansielt riktige tall?
t = subset(df,naringsregion_nr=='01'&enhet=='personer')
t = subset(df,naringsregion_nr=='02'&enhet=='personer')
t = subset(df,naringsregion_nr=='01'&enhet=='prosent')
t = subset(df,naringsregion_nr=='02'&enhet=='prosent')
