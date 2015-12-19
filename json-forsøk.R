#Eksperiment for å se om variabel + kategorier kan kryssjekkes fra dimensions-fil

#JSON (javascript object notation)
library(jsonlite)
jsonData <- fromJSON("parameters/dimensions.json")
variables = toJSON(jsonData$variables)
df_variables = fromJSON(variables,flatten=T)

a = unlist(jsonData$variables,recursive=F)
names(jsonData)
names(jsonData$variables)
jsonData$owner$login
myjson=toJSON(iris,pretty=T)
cat(myjson)
iris2=fromJSON(myjson)
head(iris2)
#http://www.r-bloggers.com/new-package-jsonlite-a-smarter-json-encoderdecoder/

library(jsonlite)
#cardDescriptions
jsonData <- fromJSON("D:/R/imdikator-kode/imdikator/data/cardDescriptions.json",flatten=T)
write.csv2(jsonData[,1:6],"df_cD.csv",row.names=F)
df = toJSON(jsonData,pretty=T)
write(df,file="df.json")

#cardPages
jsonData <- fromJSON("D:/R/imdikator-kode/imdikator/data/cardPages.json",flatten=T)
#for nesta til å konverteres    
write.csv2(jsonData[,1:6],"df_cD.csv",row.names=F)
df = toJSON(jsonData,pretty=T)
write(df,file="df.json")

names(jsonData)
str(jsonData$cards,max.level=2)

df_befolkning=jsonData$cards[[1]]
df_utdanning=jsonData$cards[[2]]
df_arbeid=jsonData$cards[[3]]
df_levekaar=jsonData$cards[[4]]
df_tilskudd=jsonData$cards[[5]]

write.csv2(df_befolkning,"df_b.csv")
