#Diverse feilretting i eksisterende datasett

#befolkning_hovedgruppe
df <- read.csv("~/Ifakta/Datasett/Azure-backup 6-desember-2016/befolkning_hovedgruppe.csv", row.names=NULL, stringsAsFactors=FALSE,colClasses="character",fileEncoding = "UTF-8-BOM")
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
apply(df,2, function(x){nlevels(as.factor(x))})

#feilkodet fylke_nr
df$fylke_nr[nchar(df$fylke_nr)==1] = paste0("0",df$fylke_nr[nchar(df$fylke_nr)==1])

write.csv(df,"~/Ifakta/Datasett/Azure-backup 6-desember-2016/bugfix/befolkning_hovedgruppe.csv", row.names=F,fileEncoding = "UTF-8")

#befolkning_opprinnelsesland
df <- read.csv("~/Ifakta/Datasett/Azure-backup 6-desember-2016/merged/befolkning_opprinnelsesland.csv", row.names=NULL, stringsAsFactors=FALSE,colClasses="character")
apply(subset(df,select=-tabellvariabel),2, function(x){levels(as.factor(x))})
apply(df,2, function(x){nlevels(as.factor(x))})

#feilkodet landbakgrunn
t = filter(df,landbakgrunn=="1")
df = filter(df,landbakgrunn!="1")

write.csv(df,"~/Ifakta/Datasett/Azure-backup 6-desember-2016/bugfix/befolkning_opprinnelsesland.csv", row.names=F,fileEncoding = "UTF-8")
