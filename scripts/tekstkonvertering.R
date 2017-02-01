#TEKSTKOnVERTERER
#konverterer csv-dokumenter til txt-dokumenter for arkivformål

#lister alle filer som skal telles
sti = "~/Indikatorprosjektet/Indikatorer og datagrunnlag/Dataleveranser/Leveranse 2-2016 fra SSB/7-flytting/"
temp = list.files(path=paste0(sti,""),pattern="*.csv")
dir.create(paste0(sti,"output_txt"))
#kommentert versjon
for (i in 1:length(temp)){
        temp_dataset = read.csv(paste0(sti,"",temp[i]), stringsAsFactors=FALSE,fileEncoding = "UTF-8-BOM",colClasses="character")
        #skriver ut fil
        write.csv(temp_dataset,paste0(sti,"output_txt/",temp[i],".txt"),row.names=F)
}