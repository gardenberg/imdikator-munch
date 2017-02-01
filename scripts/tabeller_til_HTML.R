#Script for HTML - oppdatering av nettsida

#første jobb er å konvertere til html
#http://stackoverflow.com/questions/17748566/how-can-i-turn-an-r-data-frame-into-a-simple-unstyled-html-table

#pakker
library(dplyr)
library(xtable)

#datainnlesning
df_2016 <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Bosettingsdata 2016/Kopi av Bosetting_kommuneoversikt 2014-2017. Per 04 10 16.csv", sep=";", stringsAsFactors=FALSE)
df_2017 <- read.csv("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Bosettingsdata 2016/anmodning_2017_161006_2.csv", sep=";", stringsAsFactors=FALSE)
df <- full_join(df_2016,df_2017,by="Kommune")

#forbehandling av data
df$kommune_navn = gsub(paste0("[012][0123456789][0123456789][0123456789]"),"",df$Kommune)
df$anmodning_2016_inkl_em = NA
df$vedtak_2016_inkl_em = NA
df$anmodning_2017_inkl_em = NA
df$vedtak_2017_inkl_em = NA
df$anmodning_2016_inkl_em[df$Anmodning_2016_EM>=0] = paste0(df$Anmodning_2016[df$Anmodning_2016_EM>=0]," (",df$Anmodning_2016_EM[df$Anmodning_2016_EM>=0],")")
df$anmodning_2016_inkl_em[is.na(df$Anmodning_2016_EM)==T] = df$Anmodning_2016[is.na(df$Anmodning_2016_EM)==T]
df$vedtak_2016_inkl_em[df$Vedtak_2016_EM>=0] = paste0(df$Vedtak_2016[df$Vedtak_2016_EM>=0]," (",df$Vedtak_2016_EM[df$Vedtak_2016_EM>=0],")")
df$vedtak_2016_inkl_em[df$Vedtak_2016_EM==""] = df$Vedtak_2016[df$Vedtak_2016_EM==""]
sum(is.na(df$Anmodning.totalt)) #Kommuner som ikke anmodes
sum(is.na(df$Hvorav.anmodning.EM)) #Kommuner som ikke anmodes om EM
df$anmodning_2017_inkl_em[is.na(df$Hvorav.anmodning.EM)==F] = paste0(df$Anmodning.totalt[is.na(df$Hvorav.anmodning.EM)==F]," (",df$Hvorav.anmodning.EM[is.na(df$Hvorav.anmodning.EM)==F],")")
df$anmodning_2017_inkl_em[is.na(df$Hvorav.anmodning.EM)==T] = paste0(df$Anmodning.totalt[is.na(df$Hvorav.anmodning.EM)==T]," (0)")
df$vedtak_2017_inkl_em = gsub("()","",df$Vedtak_2017_EM,fixed=T)

df = select(df,Kommune,kommune_navn,anmodning_2016_inkl_em,vedtak_2016_inkl_em,anmodning_2017_inkl_em,vedtak_2017_inkl_em)
sum(is.na(df))==0 #Ingen gjenværende NA
names(df)=c("kode","Kommune","Anmodning 2016 (herav enslige mindreårige)","Vedtak 2016 (herav enslige mindreårige)","Anmodning 2017 (herav enslige mindreårige)","Vedtak 2017 (herav enslige mindreårige)")

#fylkesoversikt
df_fylke = df[-grep("[012]",df$kode),2:6]
names(df_fylke)=c("Fylke","Anmodning 2016 (herav enslige mindreårige)","Vedtak 2016 (herav enslige mindreårige)","Anmodning 2017 (herav enslige mindreårige)","Vedtak 2017 (herav enslige mindreårige)")
print(xtable(df_fylke, caption="Fylkestall", label="label"), type="html", file="test/fylker.html",include.rownames=FALSE)

#Kommunefiler
fylkenr = seq(01,20)
for(i in fylkenr){
        if(i>9){t = as.character(fylkenr[i])}
        if(i<10){t = paste0("0",fylkenr[i])}
        t=strsplit(t,split="")
        print(xtable(df[grep(paste0("[",t[[1]][[1]],"][",t[[1]][[2]],"][0123456789][0123456789]"),df$kode),2:6]), type="html", file=paste0("test/",fylkenr[i],".html"),include.rownames=FALSE)
}