#SPLITTING AV FLATFILER

#BEFOLKNING_VERDENSREGION
#data inn
befolkning_verdensregion <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_verdensregion.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE)

#nidelt
befolkning_verdensregion_9 = subset(befolkning_verdensregion,vreg_9!="NULL",select=c(-vreg_2,-vreg_5,-kjonn))
befolkning_verdensregion_9$tabell_navn="befolkning_verdensregion_9"
write.csv(befolkning_verdensregion_9,"D:/R/imdikator-munch/data_flat_output/befolkning_verdensregion_9.csv",row.names=F)

#femdelt
befolkning_verdensregion_5 = subset(befolkning_verdensregion,vreg_5!="NULL",select=c(-vreg_2,-vreg_9,-kjonn))
befolkning_verdensregion_5$tabell_navn="befolkning_verdensregion_5"
write.csv(befolkning_verdensregion_5,"D:/R/imdikator-munch/data_flat_output/befolkning_verdensregion_5.csv",row.names=F)

#todelt
befolkning_verdensregion_2 = subset(befolkning_verdensregion,vreg_2!="NULL",select=c(-vreg_5,-vreg_9))
befolkning_verdensregion_2$tabell_navn="befolkning_verdensregion_2"
write.csv(befolkning_verdensregion_2,"D:/R/imdikator-munch/data_flat_output/befolkning_verdensregion_2.csv",row.names=F)

#SPLITTING AV FLYTTEFIL
#22.10.2015
#splitter ut vreg3 til en egen flyttefil.
#rader hvor innvkat_3 ==innvandrere legges i en egen fil, befolkning_flytting_vreg
#rader hvor vreg 3 = alle erstatter befolkning_flytting
#vreg_3 fjernes fra befolkning_flytting, innvakt_3 fjernes fra befolkning_flytting_vreg
befolkning_flytting <- read.csv("D:/R/imdikator-munch/data_flat_input/befolkning_flytting.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE)

#befolkning_flytting_vreg
befolkning_flytting_vreg = subset(befolkning_flytting,innvkat_3 =='innvandrere',select=-innvkat_3)
befolkning_flytting_vreg$tabell_navn ="befolkning_flytting_vreg"
write.csv(befolkning_flytting_vreg,"D:/R/imdikator-munch/data_flat_output/befolkning_flytting_verdensregion.csv",row.names=F)
df=subset(befolkning_flytting,vreg_3 =='alle',select=-vreg_3)
write.csv(df,"D:/R/imdikator-munch/data_flat_output/befolkning_flytting.csv",row.names=F)

#SPLITTING AV UTDANNINGSNIVA
#ta ut verdier med kort og lang uni- og høyskoleutd
#kode om utd6 til utd4 og fjerne utd4
utdanningsniva <- read.csv("D:/R/imdikator-munch/data_flat_input/utdanningsniva.csv", row.names=NULL, na.strings="NA", stringsAsFactors=FALSE)
df=subset(utdanningsniva,utd_6!="universitet_og_hogskole_kort"&utd_6!="universitet_og_hogskole_lang")
df$utd_ny = df$utd_6
df$utd_ny[df$utd_4!="NULL"] = df$utd_4[df$utd_4!="NULL"]
df$utd_4=df$utd_ny
df=subset(df,select=-c(utd_ny,utd_6))
write.csv(df,"D:/R/imdikator-munch/data_flat_output/utdanningsniva.csv",row.names=F)

#SPLITTING AV KRYSSTABELLER (PREFLAT)
#forsøk på script som splitter en lang tidsserie
kommune.1986_2015 <- read.table("~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 10 fra prosjektet til frontend/kommune-1986_2015_v2.csv", sep=";", quote="\"", na.strings="", stringsAsFactors=FALSE)
data = kommune.1986_2015

lengde=length(data)
forrige_ar=1985
i=30
for (i in lengde){
  i=i+1
  na_ar=1985+ceiling(i/30)
  kolonne=data.frame(data[,i],stringsAsFactors=F)
  if(forrige_ar==na_ar){
    data_split=data.frame(data_split,kolonne,stringsAsFactors=F)
  }
  if(forrige_ar<na_ar){
    write.csv2(data_split,paste0("data_",forrige_ar,".csv"))
    data_split=data.frame(data[,1],stringsAsFactors=F)
  }
  forrige_ar=na_ar
}

paste(split_dataset=data[,1])
      tekst=data[1,2]
      a=strsplit(tekst,'\\.')[[1]]
      a[2]
      b=as.numeric(a[2])
      
split_dataset=data[,1]

antall_ar=(lengde-1)/30
i=61
ar=1985+ceiling(i/30)
1985+ceiling(1/30)
forrige_ar=1985

paste0("data_",forrige_ar,".csv")

ar=1985+ceiling(i/lengde)
tekst=data[1,i]
a=strsplit(tekst,'\\.')[[1]]
b=as.numeric(a[2])
if(b==forrige_ar){
  split_dataset=data.frame(split_dataset,data[,i])
}
else split_dataset=data[,1]
b=forrige_ar



kommune