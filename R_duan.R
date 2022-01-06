setwd("/Users/menggeduan/Documents/考氏背鳞鱼/")
#第一步，加载环境变量
library(raster)
library(dismo)
library(sdmpredictors)
library(zoon)
library(tidyverse)
library(data.table)
library(sdm)
####1.1已下载环境变量时使用
env=stack(c("/Users/menggeduan/Documents/考氏整理/整理二/气候和地图/相同分辨率环境因子/BO2_curvelmean_ss.tif",
            '/Users/menggeduan/Documents/考氏整理/整理二/气候和地图/相同分辨率环境因子/BO2_icethickmean_ss.tif',
            '/Users/menggeduan/Documents/考氏整理/整理二/气候和地图/相同分辨率环境因子/BO2_salinitymean_ss.tif',
            '/Users/menggeduan/Documents/考氏整理/整理二/气候和地图/相同分辨率环境因子/BO2_tempmean_ss.tif',
            '/Users/menggeduan/Documents/考氏整理/整理二/气候和地图/相同分辨率环境因子/gb_depth.tif',
            '/Users/menggeduan/Documents/考氏整理/整理二/气候和地图/相同分辨率环境因子/gb_land_distance.tif'))
antarctica <- raster::crop(env, extent(-180, 180, -90, -35))

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

#3. Pearson correlation coefficient (r) 
per=pearson_correlation_matrix(antarctica)
antarctica
view(per)
# to check the Pearson correlation coefficient 
library(usdm)
ex=vif(antarctica) #Variance Inflation Factor and test for multicollinearity
v=vifstep(antarctica)


#生成伪分布点(背景点)
library(dismo)
library(dplyr)
library(tidyr)
library(mapview)
library(mopa)
library(spatstat)
#(dismo)background sample
occs <- read.csv("/Users/menggeduan/Documents/考氏背鳞鱼/考氏/data/occs1224.csv")
source(pseudoAbsences_duan.R)
occ <- occs%>% select(x='x',y='y')
bg1 <- OCSVMprofiling(xy =occ, varstack = antarctica)
bg_df<-as.data.frame(bg1$absence)
Absences_duan <- pseudoAbsences_duan(xy=occ, background=bg_df, 
                        realizations = 1, exclusion.buffer = 0.083*7,
                        prevalence=0.5,kmeans = FALSE, varstack = antarctica)
write.table(Absences_duan,"/Users/menggeduan/Documents/考氏背鳞鱼/考氏/data/考氏背景点1224.csv",row.names=FALSE,col.names=TRUE,sep=",")



 #建模
setwd("/Users/menggeduan/Documents/考氏背鳞鱼/考氏/model")
library(dismo)##maxent
library(mgcv)##GAMs
library(MuMIn)
library(sdm)
library(mapview)
library(parallel)
library(usdm)
library(rJava)
Absences_duan <- read.csv("/Users/menggeduan/Documents/考氏背鳞鱼/考氏/data/考氏背景点1224.csv")
Absences2=as.data.frame(Absences_duan)
ab1=Absences2 %>% select('species1.PA01.x','species1.PA01.y','species1.PA01.v')
ab2<-ab1%>%drop_na()
coordinates(ab2)<-c('species1.PA01.x','species1.PA01.y')
proj4string(ab2)<-projection(raster())
d=sdmData(species1.PA01.v~.,ab2,predictors = antarctica)
model <- sdm(species1.PA01.v~.,d,methods = c("brt","mda","rf","svm",
                                             "maxent","domainD","fda","gam",
                                             "glm","mars"),
             modelSettings=list(brt=list(n.trees=1000,train.fraction=1),
                                mda=list(eps=2),
                                rf=list(ntree=500,mtry=1),
                                svm=list(kernel="vanilladot",nu=0.2),
                                maxent=list(args=c(sprintf("betamultiplier=%.2f", 2),
                                                   "autofeature=false",
                                                   sprintf("linear=%s", "true"),
                                                   sprintf("quadratic=%s", "true"),
                                                   sprintf("product=%s", "false"),
                                                   sprintf("threshold=%s", "false"),
                                                   sprintf("hinge=%s", "false"),
                                                   sprintf("doclamp=%s", "false")))),
             replication='boot',test.p=0,n=1,
             parallelSetting=list(ncore=8,method='parallel'))
saveRDS(model, "/Users/menggeduan/Documents/考氏背鳞鱼/考氏/model1225.rda")
model <- readRDS("/Users/menggeduan/Documents/考氏背鳞鱼/考氏/model1225.rda")
library(kuenm)
setwd("/Users/menggeduan/Documents/考氏背鳞鱼/考氏/model25")
occ <- read.csv("/Users/menggeduan/Documents/考氏背鳞鱼/考氏/data/occs1224.csv")
occ <- occ[,-1]
cur<-predict(model,antarctica,filename='cur_proc1225.img',overwrite=TRUE)
#cur<-brick("/Users/menggeduan/Documents/考氏背鳞鱼/model/sdm_prediction_o4yxlc.gri")
n<-names(cur)[1]
model_names<-c("brt","mda","rf","svm",
               "maxent","domainD","fda","gam",
               "glm","mars")
result_proc<-NULL
for (i in c(1:length(names(cur)))){
  print(i)
  n<-names(cur)[i]
  model_name<-model_names[i]
  cur_item<-cur[[n]]
  pauc<-kuenm_proc(occ[, c("x", "y")], cur_item)
  proc_v<-pauc$pROC_summary[1]
  item<-data.frame(model=model_name, proc=proc_v)
  if (is.null(result_proc)){
    result_proc<-item
  }else{
    result_proc<-rbind(result_proc, item)
  }
}
write.csv(result_proc,"/Users/menggeduan/Documents/考氏背鳞鱼/考氏/model25/result_proc.csv")
for (i in c(1:length(names(cur)))){
  print(i)
  values(cur[[names(cur)[i]]])<-values(cur[[names(cur)[i]]]) * 
    result_proc[i, "proc"]
}
final<-sum(cur)
final<-final/sum(result_proc$proc)
pauc<-kuenm_proc(occ[, c("x", "y")], final)
PROC <- data.frame(pauc$pROC_summary[1],period="current")
crs(final)
writeRaster(final, "cur_proc1225.tif")
final2<-projectRaster(from=final, 
                      crs="+proj=laea +lat_0=-90 +lon_0=0 +ellps=GRS80 +units=m +no_defs")
plot(final2)
values(final2)[which(values(final2)<0)]<-0
writeRaster(final2, "current1225.tif")

#未来预测
env_base<-"/Users/menggeduan/Documents/考氏背鳞鱼/相同分辨率环境因子"
RCPS<-c("RCP26","RCP45","RCP60","RCP85")
ESM <- "BO2"
year <-c(2050,2100)
vars<-c("curvelmean_ss", "icethickmean_ss","salinitymean_ss", "tempmean_ss")
stack <- stack( '/Users/menggeduan/Documents/考氏背鳞鱼/相同分辨率环境因子/gb_depth.tif',
                '/Users/menggeduan/Documents/考氏背鳞鱼/相同分辨率环境因子/gb_land_distance.tif')


for (j in 1:2){
  for (z in 1:4){
  print(RCPS[z])
  print(year[j])
  stacked_rasters_future <- stack(sprintf("%s/%s_%s_%s_%s.tif", 
                                          env_base,ESM,RCPS[z],year[j],vars))
  stacked_rasters_future <- stack(stacked_rasters_future, stack)
  south_raster_future <- raster::crop(stacked_rasters_future, extent(-180, 180, -90, -35))
  names(south_raster_future)<-names(antarctica)
  cur <- predict(model,south_raster_future,filename=sprintf("%s_%s.img",RCPS[z],year[j]))
    for (i in c(1:length(names(cur)))){
      print(i)
      values(cur[[names(cur)[i]]])<-values(cur[[names(cur)[i]]]) * 
      result_proc[i, "proc"]
    }
  final<-sum(cur)
  final<-final/sum(result_proc$proc)
  pauc<-kuenm_proc(occ[, c("x", "y")], final)
  PROC <- rbind(PROC,data.frame(pauc$pROC_summary[1],period=sprintf("future_%s_%s",RCPS[z],year[j])))
  crs(final)
  writeRaster(final2, filename=sprintf("%s_%s.tif",RCPS[z],year[j]))
  final2<-projectRaster(from=final, 
                        crs="+proj=laea +lat_0=-90 +lon_0=0 +ellps=GRS80 +units=m +no_defs")
  plot(final2)
  values(final2)[which(values(final2)<0)]<-0
  writeRaster(final2, filename=sprintf("future_%s_%s.tif",RCPS[z],year[j]))
  }
}

########计算阈值=0.2963542  
#thr <- 5%
setwd("/Users/menggeduan/Documents/考氏背鳞鱼/考氏/model25/")
raster <- raster("/Users/menggeduan/Documents/考氏背鳞鱼/考氏/model25/cur_proc1225.tif")
occs <- read.csv("/Users/menggeduan/Documents/考氏背鳞鱼/考氏/data/occs1224.csv")
occ <- data.frame(x=occs$x,y=occs$y)
all_training_v<-raster::extract(raster, occ)
training_present<-quantile(all_training_v, c(0.05, 0.1, 0.15, 0.2), na.rm=T)
thr <- training_present[1]#阈值
all <- data.frame(v = all_training_v)
thr_occ <- cbind(occ,all)
thr_occ$bin<-ifelse(thr_occ$v>=thr, 1, 0)
write.csv(thr_occ, "/Users/menggeduan/Documents/考氏背鳞鱼/考氏/data25/thr_occ.csv")
#(raster)Compute quantiles for the cell values of a RasterLayer
current <- raster("current1225.tif")
points<-data.table(rasterToPoints(current))
points$bin<-ifelse(points$current1225>=thr, 1, 0)
A <- points[which(points$bin==1),]
nrow(A)
Change <- data.frame()
item <- data.frame()

#2050
for (z in 1:4){
  j=1
  print(RCPS[z])
  print(year[j])
  pred <- raster(sprintf("future_%s_%s.tif",RCPS[z],year[j]))
  points_pred<-data.table(rasterToPoints(pred))
  names(points_pred) <- c("x", "y", "layer")
  points_pred$bin<-ifelse(points_pred$layer>=thr, 1, 0)
  B <- points_pred[which(points_pred$bin==1),]
  nrow(B)
  pred_2100 <- raster(sprintf("future_%s_%s.tif",RCPS[z],year[j+1]))
  points_2100<-data.table(rasterToPoints(pred_2100))
  names(points_2100) <- c("x", "y", "layer")
  points_2100$bin<-ifelse(points_2100$layer>=thr, 1, 0)
  C <- points_2100[which(points_2100$bin==1),]
  nrow(C)
  if(z==1){
    Change <- data.frame(item=sprintf("future_%s_%s",RCPS[z],year[j]),
                         change=nrow(B)/nrow(A)-1)
    item2100 <- c(item=sprintf("future_%s_%s",RCPS[z],year[j+1]),
                  change=nrow(C)/nrow(A)-1)
    Change <- rbind(Change, item2100)
  }
  else{
    item <- c(item=sprintf("future_%s_%s",RCPS[z],year[j]),
              change=nrow(B)/nrow(A)-1)
    Change <- rbind(Change, item)
    item2100 <- c(item=sprintf("future_%s_%s",RCPS[z],year[j+1]),
              change=nrow(C)/nrow(A)-1)
    Change <- rbind(Change, item2100)
  }
}
write.csv(Change, "/Users/menggeduan/Documents/考氏背鳞鱼/考氏/data25/Change1225.csv")


######模型评估
library(tidyverse)
library(ggplot2)
et<-getEvaluation(model,stat = c('AUC','COR','TSS','Kappa','sensitivity','specificity'),opt =2)
et$Method <- c("brt","domainD","fda","gam",
              "glm","mars","maxent","mda",
              "rf","svm")
write.csv(et, "/Users/menggeduan/Documents/考氏背鳞鱼/performance.csv")
Perform <- data.frame(Value=et[,2],Method=et$Method,index="AUC")
Perform <- rbind(Perform, data.frame(Value=et[,7],Method=et$Method,index="specificity"))

ggplot(Perform, aes(x=Method, y=Value))+
  geom_col(position="dodge", aes(fill=index))+
  theme_bw()


result<-raster("/Users/menggeduan/Documents/考氏背鳞鱼/考氏/model/current1224.tif")
env_proj<-projectRaster(env, crs=crs(result))
result_p<-data.frame(rasterToPoints(result))
result_p<-result_p[which(result_p$current1214>0.2),]
result_p_sample<-result_p[sample(nrow(result_p), 1e4),]
var<-names(env_proj)[1]
result_list<-list()
for (var in names(env_proj)){
  print(var)
  item<-result_p_sample
  item$var<-var
  item$v<-raster::extract(env_proj[[var]], item[, c("x", "y")])
  result_list[[var]]<-item
}
result_list<-rbindlist(result_list)
result_list[, max_v:=max(v, na.rm = T), by=var]
result_list[, min_v:=min(v, na.rm = T), by=var]
result_list$v_int<-round(((result_list$v-result_list$min_v)/
                           (result_list$max_v-result_list$min_v))*10)

result_list_se<-result_list[, .(mean=mean(current1214, na.rm=T),
                                sd=sd(current1214, na.rm = T)),
                            by=list(var, v_int)]
result_list_se[is.na(sd)]$sd<-0
ggplot(result_list_se)+geom_line(aes(x=v_int/100, y=mean))+
  facet_wrap(~var)

ggplot(result_list[sample(nrow(result_list), 1e4)])+
  #geom_point(aes(x=v, y=current1214))+
  geom_smooth(aes(x=v, y=current1214), method="gam")+
  theme_bw()+
  facet_wrap(~var, scale="free")


plot(getVarImp(model))
cl = colorRampPalette(c('#3E49BB','#3498DB','yellow','#FF6347','red'))
niche(antarctica,en1,n=c("BO2_tempmean_ss",  "BO2_icethickmean_ss"),col=cl(200))
niche(antarctica,en1,n=c("BO2_salinitymean_ss","BO2_tempmean_ss"),col=cl(200))

##