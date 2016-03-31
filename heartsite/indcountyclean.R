library(data.table)

counties <- read.table("heartsite/data/counties_list_18.txt",sep="\t", header=TRUE)
counties$NAME <- as.character.factor(counties$NAME)
indi <- read.csv("heartsite/data/indiana.csv")

countyname <- counties$NAME
countyname <- strsplit(countyname, split = " ")
countyname <- unlist(countyname)
countyindex <- grepl("County",countyname)
countyname <- countyname[!(countyindex)]
countyname[71] = "St. Joseph"
countyname <- countyname[-72]

counties$NAME <- countyname

counties <- counties[,c(4,11,12)]

colnames(counties) <- c("county","latitude","longitude")

indicounty <- merge(x = indi, y = counties, by = "county")

