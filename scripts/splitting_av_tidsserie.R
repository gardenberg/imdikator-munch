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