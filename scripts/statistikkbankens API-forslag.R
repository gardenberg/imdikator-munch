options(encoding="UTF-8")
library(httr)
library(rjstat)

url <- "http://data.ssb.no/api/v0/no/table/10824"
data <- '{
        "query": [
                {
                        "code": "Region",
                        "selection": {
                                "filter": "all",
                                "values": [
                                        "*"
                                        ]
                        }
                },
                {
                        "code": "Kjonn",
                        "selection": {
                                "filter": "all",
                                "values": [
                                        "*"
                                        ]
                        }
                },
                {
                        "code": "IntroAvbrudd",
                        "selection": {
                                "filter": "all",
                                "values": [
                                        "*"
                                        ]
                        }
                },
                {
                        "code": "Arbeidsmarkedsstatus",
                        "selection": {
                                "filter": "item",
                                "values": [
                                        "00",
                                        "1",
                                        "2",
                                        "3"
                                        ]
                        }
                },
                {
                        "code": "ContentsCode",
                        "selection": {
                                "filter": "item",
                                "values": [
                                        "DeltakereProg1",
                                        "DeltakereProg"
                                        ]
                        }
                },
                {
                        "code": "Tid",
                        "selection": {
                                "filter": "all",
                                "values": [
                                        "*"
                                        ]
                        }
                }
                ],
        "response": {
                "format": "json-stat"
        }
}'

d.tmp <- POST(url , body = data, encode = "json", verbose())
# Henter ut innholdet fra d.tmp som tekst deretter bearbeides av fromJSONstat
sbtabell <- fromJSONstat(content(d.tmp, "text"), naming="id")
# Henter ut kun datasettet fra sbtabell
ds <- sbtabell[[1]]
# Viser første rader av datasettet
head(ds)

#sjekk av antallet kombinasjoner
apply(ds,2, function(x){nlevels(as.factor(x))})


