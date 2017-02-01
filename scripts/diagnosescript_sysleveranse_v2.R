#DIAGNOSESCRIPT LEVERANSE 2016 - 28. JULI
#versjon 2.

options("scipen"=999)

library(dplyr)

#tabell 60a
#1. lese inn data
df <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 2-2016 fra SSB/sysselsetting/DIAGNOSE/Tab_1_60a.csv", stringsAsFactors=FALSE,colClasses="character")
#2. har vi fått variablene vi har bedt om? er variablene og kategoriene kodet riktig?
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
apply(subset(df,select=-tabellvariabel),2, function(x){nlevels(as.factor(x))})
#3. er det riktig antall observasjoner?
nrow(df)==429*7*3*2
#4. er dette substansielt riktige tall?
#foreløpig kun visuell inspeksjon her, ingen automatisering
t = subset(df,kommune_nr=='0000'&kjonn=='alle'&enhet=='personer')
t = subset(df,kommune_nr=='0101'&kjonn=='alle'&enhet=='personer')
t = subset(df,kommune_nr=='0101'&innvgrunn_7=='alle'&enhet=='personer')
t = subset(df,kommune_nr=='0000'&kjonn=='alle'&enhet=='prosent')
t = subset(df,kommune_nr=='0101'&kjonn=='alle'&enhet=='prosent')
t = subset(df,kommune_nr=='0101'&innvgrunn_7=='alle'&enhet=='prosent')
#5. manglende eller skjulte data?
sum(df$tabellvariabel==":") #undertrykket
sum(df$tabellvariabel==".") #manglende

#tabell 60b
#1. lese inn data
df <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 2-2016 fra SSB/sysselsetting/DIAGNOSE/Tab_2_60b.csv", stringsAsFactors=FALSE,colClasses="character")
#2. har vi fått variablene vi har bedt om? er variablene og kategoriene kodet riktig?
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
apply(subset(df,select=-tabellvariabel),2, function(x){nlevels(as.factor(x))})
#3. er det riktig antall observasjoner?
nrow(df)==19*7*3*2
#4. er dette substansielt riktige tall?
t = subset(df,bydel_nr=='00'&kjonn=='alle'&enhet=='personer')
t = subset(df,innvgrunn_7=="alle"&kjonn=='alle'&enhet=='personer')
t = subset(df,bydel_nr=='030101'&kjonn=='alle'&enhet=='personer')
t = subset(df,bydel_nr=='030101'&innvgrunn_7=='alle'&enhet=='personer')
t = subset(df,bydel_nr=='00'&kjonn=='alle'&enhet=='prosent')
t = subset(df,bydel_nr=='030101'&kjonn=='alle'&enhet=='prosent')
t = subset(df,bydel_nr=='030101'&innvgrunn_7=='alle'&enhet=='prosent')
#5. manglende eller skjulte data?
sum(df$tabellvariabel==":") #undertrykket
sum(df$tabellvariabel==".") #manglende

#tabell 60c
#1. lese inn data
df <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 2-2016 fra SSB/sysselsetting/DIAGNOSE/Tab_3_60c.csv", stringsAsFactors=FALSE,colClasses="character")
#2. har vi fått variablene vi har bedt om? er variablene og kategoriene kodet riktig?
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
apply(subset(df,select=-tabellvariabel),2, function(x){nlevels(as.factor(x))})
#3. er det riktig antall observasjoner?
nrow(df)==20*7*3*2
#4. er dette substansielt riktige tall?
t = subset(df,fylke_nr=='00'&kjonn=='alle'&enhet=='personer')
t = subset(df,fylke_nr=='01'&kjonn=='alle'&enhet=='personer')
t = subset(df,fylke_nr=='01'&innvgrunn_7=='alle'&enhet=='personer')
t = subset(df,fylke_nr=='00'&kjonn=='alle'&enhet=='prosent')
t = subset(df,fylke_nr=='01'&kjonn=='alle'&enhet=='prosent')
t = subset(df,fylke_nr=='01'&innvgrunn_7=='alle'&enhet=='prosent')
#5. manglende eller skjulte data?
sum(df$tabellvariabel==":") #undertrykket
sum(df$tabellvariabel==".") #manglende

#tabell 60d
#1. lese inn data
df <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 2-2016 fra SSB/sysselsetting/DIAGNOSE/Tab_4_60d.csv", stringsAsFactors=FALSE,colClasses="character")
#2. har vi fått variablene vi har bedt om? er variablene og kategoriene kodet riktig?
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
apply(subset(df,select=-tabellvariabel),2, function(x){nlevels(as.factor(x))})
#3. er det riktig antall observasjoner?
nrow(df)==83*7*3*2
#4. er dette substansielt riktige tall?
t = subset(df,naringsregion_nr=='01'&kjonn=='alle'&enhet=='personer')
t = subset(df,naringsregion_nr=='01'&innvgrunn_7=='alle'&enhet=='personer')
t = subset(df,innvgrunn_7=='alle'&kjonn=='alle'&enhet=='personer')
sum(as.numeric(t$tabellvariabel))
t = subset(df,naringsregion_nr=='02'&kjonn=='alle'&enhet=='prosent')
t = subset(df,naringsregion_nr=='01'&kjonn=='alle'&enhet=='prosent')
t = subset(df,naringsregion_nr=='02'&innvgrunn_7=='alle'&enhet=='prosent')
#5. manglende eller skjulte data?
sum(df$tabellvariabel==":") #undertrykket
sum(df$tabellvariabel==".") #manglende

#tabell 611a
#1. lese inn data
df <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 2-2016 fra SSB/sysselsetting/DIAGNOSE/Tab_5_611a.csv", stringsAsFactors=FALSE,colClasses="character")
#2. har vi fått variablene vi har bedt om? er variablene og kategoriene kodet riktig?
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
apply(subset(df,select=-tabellvariabel),2, function(x){nlevels(as.factor(x))})
#3. er det riktig antall observasjoner?
nrow(df)==429*3*2
#4. er dette substansielt riktige tall?
t = subset(df,kommune_nr=='0000'&enhet=='personer')
t = subset(df,kommune_nr=='0101'&enhet=='personer')
t = subset(df,kommune_nr=='0000'&enhet=='prosent')
t = subset(df,kommune_nr=='0101'&enhet=='prosent')
#5. manglende eller skjulte data?
sum(df$tabellvariabel==":") #undertrykket
sum(df$tabellvariabel==".") #manglende

#tabell 611b
#1. lese inn data
df <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 2-2016 fra SSB/sysselsetting/DIAGNOSE/Tab_6_611b.csv", stringsAsFactors=FALSE,colClasses="character")
#2. har vi fått variablene vi har bedt om? er variablene og kategoriene kodet riktig?
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
apply(subset(df,select=-tabellvariabel),2, function(x){nlevels(as.factor(x))})
#3. er det riktig antall observasjoner?
nrow(df)==19*3*2
#4. er dette substansielt riktige tall?
t = subset(df,bydel_nr=='00'&enhet=='personer')
t = subset(df,innvkat_3=="alle"&enhet=='personer')
t = subset(df,bydel_nr=='030101'&enhet=='personer')
t = subset(df,bydel_nr=='00'&enhet=='prosent')
t = subset(df,bydel_nr=='030101'&enhet=='prosent')
#5. manglende eller skjulte data?
sum(df$tabellvariabel==":") #undertrykket
sum(df$tabellvariabel==".") #manglende

#tabell 611c
#1. lese inn data
df <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 2-2016 fra SSB/sysselsetting/DIAGNOSE/Tab_7_611c.csv", stringsAsFactors=FALSE,colClasses="character")
#2. har vi fått variablene vi har bedt om? er variablene og kategoriene kodet riktig?
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
apply(subset(df,select=-tabellvariabel),2, function(x){nlevels(as.factor(x))})
#3. er det riktig antall observasjoner?
nrow(df)==20*3*2
#4. er dette substansielt riktige tall?
t = subset(df,fylke_nr=='00'&enhet=='personer')
t = subset(df,fylke_nr=='01'&enhet=='personer')
t = subset(df,fylke_nr=='00'&enhet=='prosent')
t = subset(df,fylke_nr=='01'&enhet=='prosent')
#5. manglende eller skjulte data?
sum(df$tabellvariabel==":") #undertrykket
sum(df$tabellvariabel==".") #manglende

#tabell 611d
#1. lese inn data
df <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 2-2016 fra SSB/sysselsetting/DIAGNOSE/Tab_8_611d.csv", stringsAsFactors=FALSE,colClasses="character")
#2. har vi fått variablene vi har bedt om? er variablene og kategoriene kodet riktig?
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
apply(subset(df,select=-tabellvariabel),2, function(x){nlevels(as.factor(x))})
#3. er det riktig antall observasjoner?
nrow(df)==83*3*2
#4. er dette substansielt riktige tall?
t = subset(df,naringsregion_nr=='01'&enhet=='personer')
t = subset(df,naringsregion_nr=='02'&enhet=='personer')
t = subset(df,innvkat_3=='alle'&enhet=='personer')
sum(as.integer(t$tabellvariabel))
t = subset(df,naringsregion_nr=='01'&enhet=='prosent')
t = subset(df,naringsregion_nr=='02'&enhet=='prosent')
#5. manglende eller skjulte data?
sum(df$tabellvariabel==":") #undertrykket
sum(df$tabellvariabel==".") #manglende

#tabell 612a

#1. lese inn data
df <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 2-2016 fra SSB/sysselsetting/DIAGNOSE/Tab_9_612a.csv", stringsAsFactors=FALSE,colClasses="character")
#2. har vi fått variablene vi har bedt om? er variablene og kategoriene kodet riktig?
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
apply(subset(df,select=-tabellvariabel),2, function(x){nlevels(as.factor(x))})
#3. er det riktig antall observasjoner?
nrow(df)==429*3*6*2
#4. er dette substansielt riktige tall?
t = subset(df,kommune_nr=='0000'&enhet=='personer')
t = subset(df,kommune_nr=='0101'&enhet=='personer')
t = subset(df,kommune_nr=='0000'&enhet=='prosent')
t = subset(df,kommune_nr=='0101'&enhet=='prosent')
#må legge til en beregning av riktig "alle"


#5. manglende eller skjulte data?
sum(df$tabellvariabel==":") #undertrykket
sum(df$tabellvariabel==".") #manglende

#tabell 612b
#1. lese inn data
df <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 2-2016 fra SSB/sysselsetting/DIAGNOSE/Tab_10_612b.csv", stringsAsFactors=FALSE,colClasses="character")
#2. har vi fått variablene vi har bedt om? er variablene og kategoriene kodet riktig?
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
apply(subset(df,select=-tabellvariabel),2, function(x){nlevels(as.factor(x))})
#3. er det riktig antall observasjoner?
nrow(df)==19*3*6*2
#4. er dette substansielt riktige tall?
t = subset(df,bydel_nr=='00'&enhet=='personer')
t = subset(df,innvkat_3=="alle"&enhet=='personer'&arbeidsalder_18_69=="alle")
t = subset(df,bydel_nr=='00'&enhet=='prosent')
#5. manglende eller skjulte data?
sum(df$tabellvariabel==":") #undertrykket
sum(df$tabellvariabel==".") #manglende

#tabell 612c
#1. lese inn data
df <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 2-2016 fra SSB/sysselsetting/DIAGNOSE/Tab_11_612c.csv", stringsAsFactors=FALSE,colClasses="character")
#2. har vi fått variablene vi har bedt om? er variablene og kategoriene kodet riktig?
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
apply(subset(df,select=-tabellvariabel),2, function(x){nlevels(as.factor(x))})
#3. er det riktig antall observasjoner?
nrow(df)==20*3*6*2
#4. er dette substansielt riktige tall?
t = subset(df,fylke_nr=='00'&enhet=='personer')
t = subset(df,fylke_nr=='00'&enhet=='prosent')
#5. manglende eller skjulte data?
sum(df$tabellvariabel==":") #undertrykket
sum(df$tabellvariabel==".") #manglende

#tabell 612d
#1. lese inn data
df <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 2-2016 fra SSB/sysselsetting/DIAGNOSE/Tab_12_612d.csv", stringsAsFactors=FALSE,colClasses="character")
#2. har vi fått variablene vi har bedt om? er variablene og kategoriene kodet riktig?
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
apply(subset(df,select=-tabellvariabel),2, function(x){nlevels(as.factor(x))})
#3. er det riktig antall observasjoner?
nrow(df)==83*3*6*2
#4. er dette substansielt riktige tall?
t = subset(df,naringsregion_nr=='01'&enhet=='personer')
t = subset(df,naringsregion_nr=='01'&enhet=='prosent')
#5. manglende eller skjulte data?
sum(df$tabellvariabel==":") #undertrykket
sum(df$tabellvariabel==".") #manglende

#tabell 62a
#1. lese inn data
df <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 2-2016 fra SSB/sysselsetting/DIAGNOSE/Tab_13_62a.csv", stringsAsFactors=FALSE,colClasses="character")
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
#5. manglende eller skjulte data?
sum(df$tabellvariabel==":") #undertrykket
sum(df$tabellvariabel==".") #manglende

#tabell 62b
#1. lese inn data
df <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 2-2016 fra SSB/sysselsetting/DIAGNOSE/Tab_14_62b.csv", stringsAsFactors=FALSE,colClasses="character")
#2. har vi fått variablene vi har bedt om? er variablene og kategoriene kodet riktig?
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
apply(subset(df,select=-tabellvariabel),2, function(x){nlevels(as.factor(x))})
#3. er det riktig antall observasjoner?
nrow(df)==19*4*2
#4. er dette substansielt riktige tall?
t = subset(df,bydel_nr=='00'&enhet=='personer')
t = subset(df,botid_4=="alle"&enhet=='personer')
t = subset(df,bydel_nr=='030101'&enhet=='personer')
t = subset(df,bydel_nr=='00'&enhet=='prosent')
t = subset(df,bydel_nr=='030101'&enhet=='prosent')
#5. manglende eller skjulte data?
sum(df$tabellvariabel==":") #undertrykket
sum(df$tabellvariabel==".") #manglende

#tabell 62c
#1. lese inn data
df <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 2-2016 fra SSB/sysselsetting/DIAGNOSE/Tab_15_62c.csv", stringsAsFactors=FALSE,colClasses="character")
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
#5. manglende eller skjulte data?
sum(df$tabellvariabel==":") #undertrykket
sum(df$tabellvariabel==".") #manglende

#tabell 62d
#1. lese inn data
df <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 2-2016 fra SSB/sysselsetting/DIAGNOSE/Tab_16_62d.csv", stringsAsFactors=FALSE,colClasses="character")
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
#5. manglende eller skjulte data?
sum(df$tabellvariabel==":") #undertrykket
sum(df$tabellvariabel==".") #manglende

#tabell 63.1a
#1. lese inn data
df <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 2-2016 fra SSB/sysselsetting/DIAGNOSE/Tab_17_631a.csv", stringsAsFactors=FALSE,colClasses="character")
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
#5. manglende eller skjulte data?
sum(df$tabellvariabel==":") #undertrykket
sum(df$tabellvariabel==".") #manglende

#tabell 631b
#1. lese inn data
df <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 2-2016 fra SSB/sysselsetting/DIAGNOSE/Tab_18_631b.csv", stringsAsFactors=FALSE,colClasses="character")
#2. har vi fått variablene vi har bedt om? er variablene og kategoriene kodet riktig?
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
apply(subset(df,select=-tabellvariabel),2, function(x){nlevels(as.factor(x))})
#3. er det riktig antall observasjoner?
nrow(df)==19*3*4*2
#4. er dette substansielt riktige tall?
t = subset(df,vreg_3=="alle"&botid_4=="alle"&enhet=='personer')
t = subset(df,bydel_nr=='00'&enhet=='personer')
t = subset(df,bydel_nr=='030101'&enhet=='personer')
t = subset(df,bydel_nr=='00'&enhet=='prosent')
t = subset(df,bydel_nr=='030101'&enhet=='prosent')
#5. manglende eller skjulte data?
sum(df$tabellvariabel==":") #undertrykket
sum(df$tabellvariabel==".") #manglende

#tabell 631c
#1. lese inn data
df <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 2-2016 fra SSB/sysselsetting/DIAGNOSE/Tab_19_631c.csv", stringsAsFactors=FALSE,colClasses="character")
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
#5. manglende eller skjulte data?
sum(df$tabellvariabel==":") #undertrykket
sum(df$tabellvariabel==".") #manglende

#tabell 631d
#1. lese inn data
df <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 2-2016 fra SSB/sysselsetting/DIAGNOSE/Tab_20_631d.csv", stringsAsFactors=FALSE,colClasses="character")
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
#5. manglende eller skjulte data?
sum(df$tabellvariabel==":") #undertrykket
sum(df$tabellvariabel==".") #manglende

#tabell 63.2a
#1. lese inn data
df <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 2-2016 fra SSB/sysselsetting/DIAGNOSE/Tab_21_632a.csv", stringsAsFactors=FALSE,colClasses="character")
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
#5. manglende eller skjulte data?
sum(df$tabellvariabel==":") #undertrykket
sum(df$tabellvariabel==".") #manglende

#tabell 632b
#1. lese inn data
df <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 2-2016 fra SSB/sysselsetting/DIAGNOSE/Tab_22_632b.csv", stringsAsFactors=FALSE,colClasses="character")
#2. har vi fått variablene vi har bedt om? er variablene og kategoriene kodet riktig?
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
apply(subset(df,select=-tabellvariabel),2, function(x){nlevels(as.factor(x))})
#3. er det riktig antall observasjoner?
nrow(df)==19*3*3*2
#4. er dette substansielt riktige tall?
t = subset(df,bydel_nr=='00'&enhet=='personer')
t = subset(df,vreg_3=="alle"&kjonn=="alle"&enhet=='personer')
t = subset(df,bydel_nr=='030101'&enhet=='personer')
t = subset(df,bydel_nr=='00'&enhet=='prosent')
t = subset(df,bydel_nr=='030101'&enhet=='prosent')
#5. manglende eller skjulte data?
sum(df$tabellvariabel==":") #undertrykket
sum(df$tabellvariabel==".") #manglende

#tabell 632c
#1. lese inn data
df <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 2-2016 fra SSB/sysselsetting/DIAGNOSE/Tab_23_632c.csv", stringsAsFactors=FALSE,colClasses="character")
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
#5. manglende eller skjulte data?
sum(df$tabellvariabel==":") #undertrykket
sum(df$tabellvariabel==".") #manglende

#tabell 632d
#1. lese inn data
df <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 2-2016 fra SSB/sysselsetting/DIAGNOSE/Tab_24_632d.csv", stringsAsFactors=FALSE,colClasses="character")
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
#5. manglende eller skjulte data?
sum(df$tabellvariabel==":") #undertrykket
sum(df$tabellvariabel==".") #manglende

#tabell 63.3a
#1. lese inn data
df <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 2-2016 fra SSB/sysselsetting/DIAGNOSE/Tab_25_633a.csv", stringsAsFactors=FALSE,colClasses="character")
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
#5. manglende eller skjulte data?
sum(df$tabellvariabel==":") #undertrykket
sum(df$tabellvariabel==".") #manglende

#tabell 633b
#1. lese inn data
df <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 2-2016 fra SSB/sysselsetting/DIAGNOSE/Tab_26_633b.csv", stringsAsFactors=FALSE,colClasses="character")
#2. har vi fått variablene vi har bedt om? er variablene og kategoriene kodet riktig?
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
apply(subset(df,select=-tabellvariabel),2, function(x){nlevels(as.factor(x))})
#3. er det riktig antall observasjoner?
nrow(df)==19*9*2
#4. er dette substansielt riktige tall?
t = subset(df,bydel_nr=='00'&enhet=='personer')
t = subset(df,bydel_nr=='030101'&enhet=='personer')
t = subset(df,bydel_nr=='00'&enhet=='prosent')
t = subset(df,bydel_nr=='030101'&enhet=='prosent')
#5. manglende eller skjulte data?
sum(df$tabellvariabel==":") #undertrykket
sum(df$tabellvariabel==".") #manglende

#tabell 633c
#1. lese inn data
df <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 2-2016 fra SSB/sysselsetting/DIAGNOSE/Tab_27_633c.csv", stringsAsFactors=FALSE,colClasses="character")
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
#5. manglende eller skjulte data?
sum(df$tabellvariabel==":") #undertrykket
sum(df$tabellvariabel==".") #manglende

#tabell 633d
#1. lese inn data
df <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 2-2016 fra SSB/sysselsetting/DIAGNOSE/Tab_28_633d.csv", stringsAsFactors=FALSE,colClasses="character")
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
#5. manglende eller skjulte data?
sum(df$tabellvariabel==":") #undertrykket
sum(df$tabellvariabel==".") #manglende

#tabell 64a
#1. lese inn data
df <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 2-2016 fra SSB/sysselsetting/DIAGNOSE/Tab_29_64a.csv", stringsAsFactors=FALSE,colClasses="character")
#2. har vi fått variablene vi har bedt om? er variablene og kategoriene kodet riktig?
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
apply(subset(df,select=-tabellvariabel),2, function(x){nlevels(as.factor(x))})
#3. er det riktig antall observasjoner?
nrow(df)==(429*1*3*2)+(429*2*1*2)
nrow(df)==(429*3*3*2)
#4. er dette substansielt riktige tall?
t = subset(df,kommune_nr=='0000'&enhet=='personer')
t = subset(df,kommune_nr=='0101'&enhet=='personer')
t = subset(df,kommune_nr=='0000'&enhet=='prosent')
t = subset(df,kommune_nr=='0101'&enhet=='prosent')
#5. manglende eller skjulte data?
sum(df$tabellvariabel==":") #undertrykket
sum(df$tabellvariabel==".") #manglende

#tabell 64b
#1. lese inn data
df <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 2-2016 fra SSB/sysselsetting/DIAGNOSE/Tab_30_64b.csv", stringsAsFactors=FALSE,colClasses="character")
#2. har vi fått variablene vi har bedt om? er variablene og kategoriene kodet riktig?
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
apply(subset(df,select=-tabellvariabel),2, function(x){nlevels(as.factor(x))})
#3. er det riktig antall observasjoner?
nrow(df)==(18*1*3*2)+(18*2*1*2)
nrow(df)==(19*3*3*2)
#4. er dette substansielt riktige tall?
t = subset(df,bydel_nr=='00'&enhet=='personer')
t = subset(df,innvkat_3=="innvandrere"&vreg_3=="alle"&enhet=='personer')
t = subset(df,bydel_nr=='030101'&enhet=='personer')
t = subset(df,bydel_nr=='00'&enhet=='prosent')
t = subset(df,bydel_nr=='030101'&enhet=='prosent')
#5. manglende eller skjulte data?
sum(df$tabellvariabel==":") #undertrykket
sum(df$tabellvariabel==".") #manglende

#tabell 64c
#1. lese inn data
df <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 2-2016 fra SSB/sysselsetting/DIAGNOSE/Tab_31_64c.csv", stringsAsFactors=FALSE,colClasses="character")
#2. har vi fått variablene vi har bedt om? er variablene og kategoriene kodet riktig?
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
apply(subset(df,select=-tabellvariabel),2, function(x){nlevels(as.factor(x))})
#3. er det riktig antall observasjoner?
nrow(df)==(20*1*3*2)+(20*2*1*2)
nrow(df)==(20*3*3*2)
#4. er dette substansielt riktige tall?
t = subset(df,fylke_nr=='00'&enhet=='personer')
t = subset(df,fylke_nr=='01'&enhet=='personer')
t = subset(df,fylke_nr=='00'&enhet=='prosent')
t = subset(df,fylke_nr=='01'&enhet=='prosent')
#5. manglende eller skjulte data?
sum(df$tabellvariabel==":") #undertrykket
sum(df$tabellvariabel==".") #manglende

#tabell 64d
#1. lese inn data
df <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 2-2016 fra SSB/sysselsetting/DIAGNOSE/Tab_32_64d.csv", stringsAsFactors=FALSE,colClasses="character")
#2. har vi fått variablene vi har bedt om? er variablene og kategoriene kodet riktig?
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
apply(subset(df,select=-tabellvariabel),2, function(x){nlevels(as.factor(x))})
#3. er det riktig antall observasjoner?
nrow(df)==(83*1*3*2)+(83*2*1*2)
nrow(df)==(83*3*3*2)
#4. er dette substansielt riktige tall?
t = subset(df,naringsregion_nr=='01'&enhet=='personer')
t = subset(df,naringsregion_nr=='02'&enhet=='personer')
t = subset(df,naringsregion_nr=='01'&enhet=='prosent')
t = subset(df,naringsregion_nr=='02'&enhet=='prosent')
#5. manglende eller skjulte data?
sum(df$tabellvariabel==":") #undertrykket
sum(df$tabellvariabel==".") #manglende

#tabell 65a
#1. lese inn data
df <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 2-2016 fra SSB/sysselsetting/DIAGNOSE/Tab_33_65a.csv", stringsAsFactors=FALSE,colClasses="character")
#2. har vi fått variablene vi har bedt om? er variablene og kategoriene kodet riktig?
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
apply(subset(df,select=-tabellvariabel),2, function(x){nlevels(as.factor(x))})
#3. er det riktig antall observasjoner?
nrow(df)==429*3*3*3*2
#4. er dette substansielt riktige tall?
t = subset(df,kommune_nr=='0000'&innvkat_3=="innvandrere"&enhet=='personer')
t = subset(df,kommune_nr=='0000'&kjonn=="alle"&enhet=='personer')
t = subset(df,kommune_nr=='0101'&innvkat_3=="innvandrere"&enhet=='personer')
t = subset(df,kommune_nr=='0000'&innvkat_3=="innvandrere"&enhet=='prosent')
t = subset(df,kommune_nr=='0101'&innvkat_3=="innvandrere"&enhet=='prosent')
#5. manglende eller skjulte data?
sum(df$tabellvariabel==":") #undertrykket
sum(df$tabellvariabel==".") #manglende

#tabell 65b
#1. lese inn data
df <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 2-2016 fra SSB/sysselsetting/DIAGNOSE/Tab_34_65b.csv", stringsAsFactors=FALSE,colClasses="character")
#2. har vi fått variablene vi har bedt om? er variablene og kategoriene kodet riktig?
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
apply(subset(df,select=-tabellvariabel),2, function(x){nlevels(as.factor(x))})
#3. er det riktig antall observasjoner?
nrow(df)==19*3*3*3*2
#4. er dette substansielt riktige tall?
t = subset(df,bydel_nr=='00'&innvkat_3=="innvandrere"&enhet=='personer')
t = subset(df,innvkat_3=="innvandrere"&vreg_3=="alle"&kjonn=="alle"&enhet=='personer')
t = subset(df,bydel_nr=='030101'&innvkat_3=="innvandrere"&enhet=='personer')
t = subset(df,bydel_nr=='00'&innvkat_3=="innvandrere"&enhet=='prosent')
t = subset(df,bydel_nr=='030101'&innvkat_3=="innvandrere"&enhet=='prosent')
#5. manglende eller skjulte data?
sum(df$tabellvariabel==":") #undertrykket
sum(df$tabellvariabel==".") #manglende

#tabell 65c
#1. lese inn data
df <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 2-2016 fra SSB/sysselsetting/DIAGNOSE/Tab_35_65c.csv", stringsAsFactors=FALSE,colClasses="character")
#2. har vi fått variablene vi har bedt om? er variablene og kategoriene kodet riktig?
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
apply(subset(df,select=-tabellvariabel),2, function(x){nlevels(as.factor(x))})
#3. er det riktig antall observasjoner?
nrow(df)==20*3*3*3*2
#4. er dette substansielt riktige tall?
t = subset(df,fylke_nr=='00'&innvkat_3=="innvandrere"&enhet=='personer')
t = subset(df,fylke_nr=='01'&innvkat_3=="innvandrere"&enhet=='personer')
t = subset(df,fylke_nr=='00'&innvkat_3=="innvandrere"&enhet=='prosent')
t = subset(df,fylke_nr=='01'&innvkat_3=="innvandrere"&enhet=='prosent')
#5. manglende eller skjulte data?
sum(df$tabellvariabel==":") #undertrykket
sum(df$tabellvariabel==".") #manglende

#tabell 65d
#1. lese inn data
df <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 2-2016 fra SSB/sysselsetting/DIAGNOSE/Tab_36_65d.csv", stringsAsFactors=FALSE,colClasses="character")
#2. har vi fått variablene vi har bedt om? er variablene og kategoriene kodet riktig?
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
apply(subset(df,select=-tabellvariabel),2, function(x){nlevels(as.factor(x))})
#3. er det riktig antall observasjoner?
nrow(df)==83*3*3*3*2
#4. er dette substansielt riktige tall?
t = subset(df,naringsregion_nr=='01'&innvkat_3=="innvandrere"&enhet=='personer')
t = subset(df,naringsregion_nr=='02'&innvkat_3=="innvandrere"&enhet=='personer')
t = subset(df,naringsregion_nr=='01'&innvkat_3=="innvandrere"&enhet=='prosent')
t = subset(df,naringsregion_nr=='02'&innvkat_3=="innvandrere"&enhet=='prosent')
#5. manglende eller skjulte data?
sum(df$tabellvariabel==":") #undertrykket
sum(df$tabellvariabel==".") #manglende

#tabell 66a
#1. lese inn data
df <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 2-2016 fra SSB/sysselsetting/DIAGNOSE/Tab_37_66a.csv", stringsAsFactors=FALSE,colClasses="character")
#2. har vi fått variablene vi har bedt om? er variablene og kategoriene kodet riktig?
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
apply(subset(df,select=-tabellvariabel),2, function(x){nlevels(as.factor(x))})
#3. er det riktig antall observasjoner?
nrow(df)==429*3*3*3*2
#4. er dette substansielt riktige tall?
t = subset(df,kommune_nr=='0000'&innvkat_3=="innvandrere"&enhet=='personer')
t = subset(df,kommune_nr=='0000'&kjonn=="alle"&enhet=='personer')
t = subset(df,kommune_nr=='0101'&innvkat_3=="innvandrere"&enhet=='personer')
t = subset(df,kommune_nr=='0000'&innvkat_3=="innvandrere"&enhet=='prosent')
t = subset(df,kommune_nr=='0101'&innvkat_3=="innvandrere"&enhet=='prosent')
#5. manglende eller skjulte data?
sum(df$tabellvariabel==":") #undertrykket
sum(df$tabellvariabel==".") #manglende

#tabell 66b
#1. lese inn data
df <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 2-2016 fra SSB/sysselsetting/DIAGNOSE/Tab_38_66b.csv", stringsAsFactors=FALSE,colClasses="character")
#2. har vi fått variablene vi har bedt om? er variablene og kategoriene kodet riktig?
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
apply(subset(df,select=-tabellvariabel),2, function(x){nlevels(as.factor(x))})
#3. er det riktig antall observasjoner?
nrow(df)==19*3*3*3*2
#4. er dette substansielt riktige tall?
t = subset(df,bydel_nr=='00'&innvkat_3=="innvandrere"&enhet=='personer')
t = subset(df,innvkat_3=="innvandrere"&aldersinndeling_etter_grskole=="16_25"&kjonn=="alle"&enhet=='personer')
t = subset(df,bydel_nr=='030101'&innvkat_3=="innvandrere"&enhet=='personer')
t = subset(df,bydel_nr=='00'&innvkat_3=="innvandrere"&enhet=='prosent')
t = subset(df,bydel_nr=='030101'&innvkat_3=="innvandrere"&enhet=='prosent')
#5. manglende eller skjulte data?
sum(df$tabellvariabel==":") #undertrykket
sum(df$tabellvariabel==".") #manglende

#tabell 66c
#1. lese inn data
df <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 2-2016 fra SSB/sysselsetting/DIAGNOSE/Tab_39_66c.csv", stringsAsFactors=FALSE,colClasses="character")
#2. har vi fått variablene vi har bedt om? er variablene og kategoriene kodet riktig?
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
apply(subset(df,select=-tabellvariabel),2, function(x){nlevels(as.factor(x))})
#3. er det riktig antall observasjoner?
nrow(df)==20*3*3*3*2
#4. er dette substansielt riktige tall?
t = subset(df,fylke_nr=='00'&innvkat_3=="innvandrere"&enhet=='personer')
t = subset(df,fylke_nr=='00'&kjonn=="alle"&enhet=='personer')
t = subset(df,fylke_nr=='01'&innvkat_3=="innvandrere"&enhet=='personer')
t = subset(df,fylke_nr=='00'&innvkat_3=="innvandrere"&enhet=='prosent')
t = subset(df,fylke_nr=='01'&innvkat_3=="innvandrere"&enhet=='prosent')
#5. manglende eller skjulte data?
sum(df$tabellvariabel==":") #undertrykket
sum(df$tabellvariabel==".") #manglende

#tabell 66d
#1. lese inn data
df <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 2-2016 fra SSB/sysselsetting/DIAGNOSE/Tab_40_66d.csv", stringsAsFactors=FALSE,colClasses="character")
#2. har vi fått variablene vi har bedt om? er variablene og kategoriene kodet riktig?
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
apply(subset(df,select=-tabellvariabel),2, function(x){nlevels(as.factor(x))})
#3. er det riktig antall observasjoner?
nrow(df)==83*3*3*3*2
#4. er dette substansielt riktige tall?
t = subset(df,naringsregion_nr=='01'&innvkat_3=="innvandrere"&enhet=='personer')
t = subset(df,naringsregion_nr=='02'&innvkat_3=="innvandrere"&enhet=='personer')
t = subset(df,naringsregion_nr=='01'&innvkat_3=="innvandrere"&enhet=='prosent')
t = subset(df,naringsregion_nr=='02'&innvkat_3=="innvandrere"&enhet=='prosent')
#5. manglende eller skjulte data?
sum(df$tabellvariabel==":") #undertrykket
sum(df$tabellvariabel==".") #manglende