#Script for norskfeltet

#Laster data
norskprove_kommune <- read.csv("~/R/datamerge_indikator/40_1-norsk_prover-kommuner-halvar_1_2014_v1.csv", sep=";", dec=",", na.strings=":")

#legger inn identifikator for prikker i stedet for NA og kopierer kommunenummer slik jeg har tolket de
norskprove_kommune$prikker = 1
norskprove_kommune$Nr_kopi = norskprove_kommune$Nr

#legger inn all annen informasjon om kommunen som ligger i kommunesett-fila
kommunesett <- read.csv("H:/My Documents/R/datamerge_indikator/kommunesort.csv", sep=";")
#fjerne noen unødvenige rader i kommunesettet, og renavner disse med opprinnelige navn
k_navn = names(kommunesett)
kommunesett = data.frame(kommunesett[,1],kommunesett[,2],kommunesett[,4],kommunesett[,6],kommunesett[,7])
names(kommunesett) = c(k_navn[1],k_navn[2],k_navn[4],k_navn[6],k_navn[7])

#generelt script for å legge til kommuner som ikke er med i settet
#NB! Husk at by.y og by.x er viktig å stille riktig!
norskprove_kommune_alle = merge(kommunesett,norskprove_kommune,by.y="Nr",by.x="Nr",all.x=T,all.y=T)

write.csv2(norskprove_kommune_alle,file="norskprove_kommune_alle.csv")
