data <- read.csv2("D:/R/innvandring/innvandrerandel og flyktninger.csv",na.strings=c("NA"," NA","NA "))

#default plotting parameters
par(mfrow=c(1,1),mar=c(5.1, 4.1, 4.1, 2.1))

plot(data$flyktninger_2012,data$inv_andel_2014)

#hierarchical clustering
dist_data <- dist(data[,-1])
hClustering <- hclust(dist_data)
plot(hClustering) 

#kmeans
data_nona = data
data_nona[is.na(data_nona)]=0
kmeansObj <- kmeans(data_nona, centers = 3)
plot(data_nona$flyktninger_2012, data_nona$inv_andel_2014, col = kmeansObj$cluster, pch = 19, cex = 2)
points(kmeansObj$centers, col = 1:3, pch = 3, cex = 3, lwd = 3)
?kmeans

#Bengler-modell for kommunelikhet

#1. les inn data for kommuner: befolkningstall, flyktninger, innvandrere
data <- read.csv("D:/R/imdikator-munch/training_files/data_cluster_2014.csv", sep=";", na.strings=c("NA",".",":"), stringsAsFactors=FALSE)
sum(is.na(data))==0
#3. beregn andel innvandrere og flyktninger av befolkningen
data$innvandrere_andel = data$innvandrere/data$alle
data$flyktninger_andel = data$flyktninger/data$alle
#4. normaliser og legg til vekt
data$alle_norm = (data$alle-min(data$alle))/(max(data$alle)-min(data$alle))
data$innv_andel_norm = (data$innvandrere_andel-min(data$innvandrere_andel))/(max(data$innvandrere_andel)-min(data$innvandrere_andel))
data$flukt_andel_norm = (data$flyktninger_andel-min(data$flyktninger_andel))/(max(data$flyktninger_andel)-min(data$flyktninger_andel))
#5. beregn avstand mellom alle punkter. 
a=data$alle_norm[1]
b=data$innv_andel_norm[1]
c=data$flukt_andel_norm[1]
data$test = sqrt((a-data$alle_norm)^2+(b-data$innv_andel_norm)^2+(c-data$flukt_andel_norm)^2)
#reproduserer nesten resultatet av Benglers kode på https://github.com/bengler/imdikator/blob/master/bin/calculateSimilar.js
#vekta euklidisk avstand
data$test_2 = sqrt(0.25*(a-data$alle_norm)^2+0.5*(b-data$innv_andel_norm)^2+0.5*(c-data$flukt_andel_norm)^2)
data$test_3 = sqrt(0.60*(a-data$alle_norm)^2+0.20*(b-data$innv_andel_norm)^2+0.20*(c-data$flukt_andel_norm)^2)
cor(data$test,data$test_3)
plot(data$test,data$test_3)

df = subset(data,select=10:12)
dm = dist(df)

#-> likner på k-nearest-neighbour?