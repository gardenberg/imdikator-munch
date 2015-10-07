library(jsonlite)
jsonData <- fromJSON("D:/R/imdikator/data/cardPages.json")
names(jsonData)
str(jsonData$cards,max.level=2)

df_befolkning=jsonData$cards[[1]]
df_utdanning=jsonData$cards[[2]]
df_arbeid=jsonData$cards[[3]]
df_levekaar=jsonData$cards[[4]]
df_tilskudd=jsonData$cards[[5]]

write.csv2(df_befolkning,"df_b.csv")
