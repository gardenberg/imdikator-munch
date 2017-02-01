#datasett med næringsregioninndelinger

#biblioteker
library(dplyr)
library(tidyr)

#Telemarksforskning sin standard
temp <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Naringsregioninndelinger/Vareide 2015 - Kommunetabell.csv", sep=";", stringsAsFactors=FALSE)
names(temp)=c("knr_gammel","Knrtxt","knavn_gammel","knr_gjeldende","knavn_gjeldende","rnr_telemark","rnavn_telemark")
df = select(temp,-Knrtxt)
#standarden som IMDI bruker per 28. august 2016
temp <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Naringsregioninndelinger/IMDi - Regioninndeling_v3_kommuner.csv", sep=";", stringsAsFactors=FALSE)
temp = select(temp,Kommunenr,Næringsregionnr,Næringsregion_.navn)
names(temp)=c("knr","rnr_gjeldende","rnavn_gjeldende")
df = full_join(df,temp,by=c("knr_gjeldende"="knr"))
#SSBs økonomiske regioner
temp <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Naringsregioninndelinger/SSB_okonomiske_regioner.csv", sep=";", stringsAsFactors=FALSE)
temp = select(temp,-idnr,-knavn)
df = full_join(df,temp,by=c("knr_gjeldende"="knr"))
#NIBRs bo- og arbeidsmarkedsregioner
temp <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Naringsregioninndelinger/NIBR_2012_boarb.csv", sep=";", stringsAsFactors=FALSE)
temp = select(temp,-knavn)
df = full_join(df,temp,by=c("knr_gammel"="knr"))

#eksport
write.csv2(df,"~/Indikatorprosjektet/Indikatorer og datagrunnlag/Naringsregioninndelinger/regioninndelinger.csv",row.names=F)
