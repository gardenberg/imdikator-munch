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


