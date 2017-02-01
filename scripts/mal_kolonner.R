#script som erstatter første kolonne med tre kolonner fra en mal.

mal_kom <- read.table("~/R/datamunch_indikator/mal-kommune-2013.csv", sep=";", quote="\"", stringsAsFactors=FALSE)
mal_fyl <- read.table("~/R/datamunch_indikator/mal-fylke-2013.csv", sep=";", quote="\"", stringsAsFactors=FALSE)
mal_nar <- read.table("~/R/datamunch_indikator/mal-naringsregion-2013.csv", sep=";", quote="\"", stringsAsFactors=FALSE)

#import skal følge dette mønsteret
`01.befolkning_hovedgruppe.kommune.1986` <- read.table("~/R/datamunch_indikator/kommune/01-befolkning_hovedgruppe-kommune-1986.csv", sep=";", quote="\"", stringsAsFactors=FALSE)
kommune_1986 = data.frame(mal_kom,`01.befolkning_hovedgruppe.kommune.1986`[2:31], stringsAsFactors=FALSE)
write.table(kommune_1986,"output/01-befolkning_hovedgruppe-kommune-1986.csv",sep=";",dec=",",row.names=F,col.names=F)

#KOMMUNE
temp = list.files(path="kommune",pattern="*.csv")
for (i in 1:length(temp)){
  temp_dataset = read.table(paste0("kommune/",temp[i]), sep=";", quote="\"", stringsAsFactors=FALSE)
  data = data.frame(mal_kom,temp_dataset[2:31], stringsAsFactors=FALSE)
  write.table(data,paste0("output/",temp[i]),sep=";",dec=",",row.names=F,col.names=F)
    #assign(temp[i], read.table(paste0("kommune/",temp[i]), sep=";", quote="\"", stringsAsFactors=FALSE))
} 


#FYLKE
temp = list.files(path="fylke",pattern="*.csv")
for (i in 1:length(temp)){
  temp_dataset = read.table(paste0("fylke/",temp[i]), sep=";", quote="\"", stringsAsFactors=FALSE)
  data = data.frame(mal_fyl,temp_dataset[2:31], stringsAsFactors=FALSE)
  write.table(data,paste0("output/",temp[i]),sep=";",dec=",",row.names=F,col.names=F)
  #assign(temp[i], read.table(paste0("kommune/",temp[i]), sep=";", quote="\"", stringsAsFactors=FALSE))
}

#NÆRINGSREGION
temp = list.files(path="naringsregion",pattern="*.csv")
for (i in 1:length(temp)){
  temp_dataset = read.table(paste0("naringsregion/",temp[i]), sep=";", quote="\"", stringsAsFactors=FALSE)
  data = data.frame(mal_nar,temp_dataset[2:31], stringsAsFactors=FALSE)
  write.table(data,paste0("output/",temp[i]),sep=";",dec=",",row.names=F,col.names=F)
  #assign(temp[i], read.table(paste0("kommune/",temp[i]), sep=";", quote="\"", stringsAsFactors=FALSE))
}


for (i in 1:length(temp)){
  data = mal_kom
  data = 
  data = data.frame(mal_kom,temp2[i][2:31], stringsAsFactors=FALSE)
  assign(temp2[1],data.frame(mal_kom,temp2[1], stringsAsFactors=FALSE))
}

data = data.frame(mal_kom,temp2[1], stringsAsFactors=FALSE)
data = cbind(mal_kom,temp2[1], stringsAsFactors=FALSE)
for (i in 1:length(temp2)){
  data = data.frame(mal_kom,temp2[i][2:31], stringsAsFactors=FALSE)
  assign(temp2[1],data.frame(mal_kom,temp2[1], stringsAsFactors=FALSE))
}