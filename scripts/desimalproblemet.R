#fil for felles praksis på desimaler

#funksjon for telling av desimalplasser
#teller desimaler
decimalplaces(as.numeric(temp_dataset$tabellvariabel))
decimalplaces(as.numeric("Inf"))
              
#denne tar kun det første elementet, fordi if bare kan vurdere en logisk vektor med lengde 1
#apply over kun en kolonne gir ikke mening
#nb: forutsetter numerisk input. har en svakhet for ikke-numeriske tall(?)/konsepter, som Inf
decimalplaces <- function(x) {
        if (is.na(x)==T) {
                return(NA)
        }
        else if ((x %% 1) != 0) {
                nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][[2]])
        } else {
                return(0)
        }
}

#lister alle filer som skal telles
temp = list.files(path="test",pattern="*.csv")

#kommentert versjon
for (i in 1:length(temp)){
        temp_dataset = read.csv(paste0("test/",temp[i]), stringsAsFactors=FALSE,fileEncoding = "UTF-8-BOM",colClasses="character")
        #noe som fikser desimalproblemet her
        #variant 1: bruteforce alle prosenter som ikke er :/. til 1 desimal
        #temp_dataset$test1[temp_dataset$enhet=="prosent"&temp_dataset$tabellvariabel!=":"&temp_dataset$tabellvariabel!="."] = round(as.numeric(temp_dataset$tabellvariabel[temp_dataset$enhet=="prosent"&temp_dataset$tabellvariabel!=":"&temp_dataset$tabellvariabel!="."]),1)
        #variant 2: teller desimaler
        for (j in 1:nrow(temp_dataset)){
                if (is.na(decimalplaces(as.numeric(temp_dataset$tabellvariabel[j])))==T){
                        temp_dataset$tabellvariabel[j] = temp_dataset$tabellvariabel[j]
                }
                else if (decimalplaces(as.numeric(temp_dataset$tabellvariabel[j]))>0) {
                        temp_dataset$tabellvariabel[j] = round(as.numeric(temp_dataset$tabellvariabel[j]),1)
                } else {
                        temp_dataset$tabellvariabel[j] = temp_dataset$tabellvariabel[j]
                }        
        }
        if(sum(is.na(temp_dataset$tabellvariabel)==0))"ingen NA"
        #skriver ut fil
        write.csv(temp_dataset,paste0("test/output/",temp[i]),row.names=F)
}

#spesialvariant for barnehagedeltakelsesfiler som har feil kommategn
temp = list.files(path="data_flat_input/barnehagedeltakelse1",pattern="*.csv")
for (i in 1:length(temp)){
        temp_dataset = read.csv(paste0("data_flat_input/barnehagedeltakelse1/",temp[i]), stringsAsFactors=FALSE,fileEncoding = "UTF-8-BOM",colClasses="character")
        temp_dataset$tabellvariabel = gsub("\\,","\\.",temp_dataset$tabellvariabel)
        for (j in 1:nrow(temp_dataset)){
                if (is.na(decimalplaces(as.numeric(temp_dataset$tabellvariabel[j])))==T){
                        temp_dataset$tabellvariabel[j] = temp_dataset$tabellvariabel[j]
                }
                else if (decimalplaces(as.numeric(temp_dataset$tabellvariabel[j]))>0) {
                        temp_dataset$tabellvariabel[j] = round(as.numeric(temp_dataset$tabellvariabel[j]),1)
                } else {
                        temp_dataset$tabellvariabel[j] = temp_dataset$tabellvariabel[j]
                }        
        }
        if(sum(is.na(temp_dataset$tabellvariabel)==0))"ingen NA"
        write.csv(temp_dataset,paste0("test/output/",temp[i]),row.names=F)
} 
