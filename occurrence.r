#2. Species occurrence records
#2.1 Download occuren data from IDigBio, GBIF, obIS
##Notolepis coatsorum
library(dismo)
library(dplyr)
library(tidyr)
library(mapview)
library(spocc)
library(tidyverse)
noc=occ(query = 'Notolepis coatsi', date = c('2000-01-01', '2020-12-31'), from = c("idigbio", 'gbif','obis'))
dat <- occ2df(noc)
dat <- subset(dat, !is.na(date))
dat <- subset(dat, name !="BOLD:AAC0164")
write.csv(dat,"/Users/menggeduan/Documents/考氏背鳞鱼/考氏/data/database1224.csv")
dat <- read.csv("/Users/menggeduan/Documents/考氏背鳞鱼/考氏/data/database1224.csv")
GBIF <- dat[which(dat$prov=="gbif"),]
idigbio <- dat[which(dat$prov=="idigbio"),]
obis <- dat[which(dat$prov=="obis"),]
GBIF=GBIF %>% select(x='longitude',y='latitude')
idigbio=idigbio %>% select(x='longitude',y='latitude')
obis=obis %>% select(x='longitude',y='latitude')
GBIF=GBIF%>%dplyr::distinct(x,y)
idigbio=idigbio%>%dplyr::distinct(x,y)
obis=obis%>%dplyr::distinct(x,y)
mydata2 <- rbind(GBIF, idigbio, obis)
#去掉重复
mydata2=mydata2%>%dplyr::distinct(x,y)
#文献数据
coll <- read.csv("/Users/menggeduan/Documents/考氏背鳞鱼/考氏/data/coll.csv")
coll <- coll[,-1]
mydata2 <- rbind(coll,mydata2)
mydata2=mydata2%>%dplyr::distinct(x,y)
#2.2 The occurrence point data is processed in ArcGIS
occs <- mydata2[which(mydata2$y<=-35),]
mask<-raster("/Users/menggeduan/Documents/考氏背鳞鱼/相同分辨率环境因子/BO2_curvelmean_ss.tif")
mask <- raster::crop(mask, extent(-180, 180, -90, -35))
values(mask)[!is.na(values(mask))]<-c(1:length(values(mask)[!is.na(values(mask))]))
plot(mask)
occs$index<-raster::extract(mask, occs[, c("x", "y")])
occs <- data.table(occs)
occs<-occs[!is.na(index)]
dim(occs)
plot(occs$x, occs$y, pch=".")
dups <- duplicated(occs[, "index"])
occs <- occs[!dups,]
occs <- occs%>% select(x='x',y='y')
write.csv(occs,"/Users/menggeduan/Documents/考氏背鳞鱼/考氏/data/occs1224.csv")
plot(occs$x, occs$y, pch=".",col="red")