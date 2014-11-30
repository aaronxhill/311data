wd <- "/Users/aaron/rdata/311"
aggwd <- "/Users/aaron/rdata/311/aggData"
shape_fp <- "/Users/aaron/rdata/311/tl_2010_36_zcta510"
shape_fn <- "tl_2010_36_zcta510nyc.shp"


# packages
library(ggplot2)
library(maptools)
library(rgdal)
library(rgeos)


setwd(wd)

too <- read.csv("311_Service_Requests_from_2010_to_Present.csv")

summary(too)

aggTotal <- aggregate(too$Incident.Zip, by=list(too$Incident.Zip), FUN=length)

summary(aggTotal)

table(too$Agency)
table(too$Complaint.Type)
table(too$Location.Type)
table(too$Borough)

aggAgency <- aggregate(too$Incident.Zip, by=list(too$Incident.Zip, too$Agency), FUN=length)
aggComplaintType <- aggregate(too$Incident.Zip, by=list(too$Incident.Zip, too$Complaint.Type), FUN=length)
aggLocationType <- aggregate(too$Incident.Zip, by=list(too$Incident.Zip, too$Location.Type), FUN=length)
aggBorough <- aggregate(too$Incident.Zip, by=list(too$Incident.Zip, too$Borough), FUN=length)

rm(too)

popBronx <- 1418733
popBrooklyn <- 2592149
popManhattan <- 1626159
popQueens <- 2296175
popSI <- 472621

1321576 / popBronx
2256990 / popBrooklyn
1519250 / popManhattan
1759192 / popQueens
406789 / popSI

nycZips <- c(10453, 10457, 10460, 10458, 10467, 10468, 10451, 10452, 10456, 10454, 10455, 10459, 10474, 10463, 10471, 10466, 10469, 10470, 10475, 10461, 10462,10464, 10465, 10472, 10473, 11212, 11213, 11216, 11233, 11238, 11209, 11214, 11228, 11204, 11218, 11219, 11230, 11234, 11236, 11239, 11223, 11224,11229, 11235, 11201, 11205, 11215, 11217, 11231, 11203, 11210, 11225, 11226, 11207, 11208, 11211, 11222, 11220, 11232, 11206, 11221, 11237, 10026, 10027, 10030, 10037, 10039, 10001, 10011, 10018, 10019, 10020, 10036, 10029, 10035, 10010, 10016, 10017, 10022, 10012, 10013, 10014, 10004,10005, 10006, 10007, 10038, 10280, 10002, 10003, 10009, 10021, 10028, 10044, 10128, 10023, 10024, 10025, 10031, 10032, 10033, 10034, 10040, 11361, 11362, 11363, 11364, 11354, 11355, 11356, 11357, 11358, 11359, 11360, 11365, 11366, 11367, 11412, 11423, 11432, 11433, 11434, 11435, 11436, 11101, 11102, 11103, 11104, 11105, 11106, 11374, 11375, 11379, 11385, 11691, 11692, 11693, 11694, 11695, 11697, 11004, 11005, 11411, 11413, 11422, 11426, 11427, 11428, 11429, 11414, 11415, 11416, 11417, 11418, 11419, 11420, 11421, 11368, 11369, 11370, 11372, 11373, 11377, 11378, 10302, 10303, 10310, 10306, 10307, 10308, 10309, 10312, 10301, 10304, 10305, 10314)
table(nycZips)
nycZipsDfN <- data.frame(nycZips)
summary(nycZipsDfN)
nrow(nycZipsDfN)

nycZipsC <- as.character(nycZips)

nycZipsDf <- data.frame(nycZipsC)
summary(nycZipsDf)

popByZip <- read.csv("2010CensusPopulationByZipcodeZCTA.csv")
summary(popByZip)

cleanPopByZip <- merge(x=nycZipsDfN, y=popByZip, by.x="nycZips" , by.y= "Zip.Code.ZCTA")
cleanPopByZip <- cleanPopByZip[cleanPopByZip$X2010.Census.Population > 0 , ]
nrow(cleanPopByZip)
cleanPopByZip$Zip <- as.character(cleanPopByZip$nycZips)
summary(cleanPopByZip)
summary(aggTotal)
aggTotalByZip <- merge(x= cleanPopByZip, y=aggTotal, by.x="Zip", by.y="Group.1", all.x=TRUE)
aggTotalByZip$threeOneOneRate <- aggTotalByZip$x / aggTotalByZip$X2010.Census.Population
aggTotalByZip
hist(aggTotalByZip$threeOneOneRate)

sdThreeOneOneRate <- sd(aggTotalByZip$threeOneOneRate, na.rm=TRUE)
avThreeOneOneRate <- mean(aggTotalByZip$threeOneOneRate, na.rm=TRUE)

aggTotalByZip$threeOneOneRateZDiff <- (aggTotalByZip$threeOneOneRate - avThreeOneOneRate) / sdThreeOneOneRate
hist(aggTotalByZip$threeOneOneRateZDiff)
hist(aggTotalByZip$threeOneOneRate)
aggTotalByZip

aggTotalByZip$threeOneOneRateZDiffCat[aggTotalByZip$threeOneOneRateZDiff <= -1.8] <- 11
aggTotalByZip$threeOneOneRateZDiffCat[aggTotalByZip$threeOneOneRateZDiff > -1.8 & aggTotalByZip$threeOneOneRateZDiff <= -1.4] <- 10
aggTotalByZip$threeOneOneRateZDiffCat[aggTotalByZip$threeOneOneRateZDiff > -1.4 & aggTotalByZip$threeOneOneRateZDiff <= -1] <- 9
aggTotalByZip$threeOneOneRateZDiffCat[aggTotalByZip$threeOneOneRateZDiff > -1 & aggTotalByZip$threeOneOneRateZDiff <= -0.6] <- 8
aggTotalByZip$threeOneOneRateZDiffCat[aggTotalByZip$threeOneOneRateZDiff > -0.6 & aggTotalByZip$threeOneOneRateZDiff <= -0.2] <- 7
aggTotalByZip$threeOneOneRateZDiffCat[aggTotalByZip$threeOneOneRateZDiff > -0.2 & aggTotalByZip$threeOneOneRateZDiff <= 0.2] <- 6
aggTotalByZip$threeOneOneRateZDiffCat[aggTotalByZip$threeOneOneRateZDiff > 0.2 & aggTotalByZip$threeOneOneRateZDiff <= 0.6] <- 5
aggTotalByZip$threeOneOneRateZDiffCat[aggTotalByZip$threeOneOneRateZDiff > 0.6 & aggTotalByZip$threeOneOneRateZDiff <= 1] <- 4
aggTotalByZip$threeOneOneRateZDiffCat[aggTotalByZip$threeOneOneRateZDiff > 1 & aggTotalByZip$threeOneOneRateZDiff <= 1.4] <- 3
aggTotalByZip$threeOneOneRateZDiffCat[aggTotalByZip$threeOneOneRateZDiff > 1.4 & aggTotalByZip$threeOneOneRateZDiff <= 1.8] <- 2
aggTotalByZip$threeOneOneRateZDiffCat[aggTotalByZip$threeOneOneRateZDiff > 1.8] <- 1
table(aggTotalByZip$threeOneOneRateZDiffCat)
aggTotalByZip$threeOneOneRateZDiffCat <- factor(aggTotalByZip$threeOneOneRateZDiffCat)




aggAgencyZip <- merge(x= cleanPopByZip, y= aggAgency, by.x="Zip", by.y="Group.1", all.x=TRUE)
aggComplaintTypeZip <- merge(x= cleanPopByZip, y= aggComplaintType, by.x="Zip", by.y="Group.1", all.x=TRUE)
aggLocationTypeZip <- merge(x= cleanPopByZip, y= aggLocationType, by.x="Zip", by.y="Group.1", all.x=TRUE)
aggBoroughZip <- merge(x= cleanPopByZip, y= aggBorough, by.x="Zip", by.y="Group.1", all.x=TRUE)






setwd(aggwd)
write.csv(aggTotalByZip, file="aggTotalByZip.csv")
write.csv(aggAgencyZip, file="aggAgencyZip.csv")
write.csv(aggComplaintTypeZip, file="aggComplaintTypeZip.csv")
write.csv(aggLocationTypeZip, file="aggLocationTypeZip.csv")
write.csv(aggBoroughZip, file="aggBoroughZip.csv")



aggAgency <- aggregate(too$Incident.Zip, by=list(too$Incident.Zip, too$Agency), FUN=length)
aggComplaintType <- aggregate(too$Incident.Zip, by=list(too$Incident.Zip, too$Complaint.Type), FUN=length)
aggLocationType <- aggregate(too$Incident.Zip, by=list(too$Incident.Zip, too$Location.Type), FUN=length)
aggBorough <- aggregate(too$Incident.Zip, by=list(too$Incident.Zip, too$Borough), FUN=length)


############# map ################

###################### GET MAP PROJECTION (SHAPE) FOR NYC ZIP CODES ################################################

# read shape file
setwd(shape_fp)
ogrListLayers(shape_fn)
nyc_shp <- readOGR(shape_fn, layer="tl_2010_36_zcta510nyc")
nycs <- fortify(nyc_shp, region= "ZCTA5CE10")
head(nycs)

formap <- merge(nycs, aggTotalByZip, by.x="id", by.y="Zip", all.x = TRUE)
formap <- formap[order(formap$order), ]
head(formap)
getBounds <- formap[is.na(formap$x) == FALSE , ]
head(getBounds)

maxlong <- max(getBounds$long)
minlong <- min(getBounds$long)
offsetlong <- (max(getBounds$long) - min(getBounds$long)) * 0.05 
maxlong
maxlong + offsetlong
minlong - offsetlong
minlong

maxlat <- max(getBounds$lat)
minlat <- min(getBounds$lat)
offsetlat <- (max(getBounds$lat) - min(getBounds$lat)) * 0.05 
maxlat
maxlat + offsetlat
minlat - offsetlat
minlat

# draw map
head(formap)

ggplot(formap, aes(x = long, y = lat, group=group)) +
  #+#, fill= Consumption..kWh.) +
  geom_polygon(aes(fill= threeOneOneRateZDiffCat), color="grey") +
  geom_path(color="grey", size=0.05) +
  #coord_cartesian(xlim=c(-74.28365, -73.67308), ylim=c(40.47496, 40.93625)) +
  scale_fill_manual(values = c("#67001f", "#b2182b", "#d6604d", "#f4a582", "#fddbc7", "#f7f7f7", "#d1e5f0", "#92c5de", "#4393c3", "#2166ac", "#053061"), na.value="grey")  +
  theme(line = element_blank()) +
  theme(rect = element_blank()) +
  theme(text = element_blank()) +
  theme(title = element_blank()) + 
  theme(plot.margin = unit(c(0,0,0,0), "in")) +
  theme(legend.position="none") +
  labs(x=NULL, y=NULL) +
  scale_x_continuous(expand=c(0,0))  
