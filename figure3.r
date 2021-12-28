library(rgdal)
library(raster)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(ggspatial)
mask_continent <- raster("/Users/menggeduan/Documents/考氏背鳞鱼/南极底图/底图/0.08分辨率35s.tif")
mask_continent2<-projectRaster(from=mask_continent, 
                        crs="+proj=laea +lat_0=-90 +lon_0=0 +ellps=GRS80 +units=m +no_defs")
plot(mask_continent2)
mask_continent2_p<-data.frame(rasterToPoints(mask_continent2))
colnames(mask_continent2_p)[3]<-"v"

model<-raster("/Users/menggeduan/Documents/考氏背鳞鱼/考氏/model25/current1225.tif")
if(F){
  v<-values(model)
  na<-v[is.na(v)]
  length(na)
  presence<-length(v[v>=thr])
  absence<-length(v[v<thr])
  area_presence<-presence * (res(model)[1] * res(model)[2]/1e6)
  area_absence<-absence * (res(model)[1] * res(model)[2]/1e6)
  area_total<-area_presence+area_absence
  area_proportion<-area_presence/area_total
  area_na<-length(na) * (res(model)[1] * res(model)[2]/1e6)
  area_total+area_na
}
model_p<-data.frame(rasterToPoints(model))
colnames(model_p)[3]<-"v"

model_p_2<-model_p[which(model_p$v<thr), ]


#lines<-st_read("/Users/menggeduan/Documents/考氏背鳞鱼/antarctic_circumpolar_current_fronts/shapefile/antarctic_circumpolar_current_fronts.shp")
lines <- st_read("/Users/menggeduan/Documents/考氏背鳞鱼/南极底图/antarctic_circumpolar_current_fronts/shapefile/antarctic_circumpolar_current_fronts.shp")
ll_str<-"+proj=longlat +datum=WGS84 +no_defs"
laea_str<-"+proj=laea +lat_0=-90 +lon_0=0 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"
lines_laea<-st_transform(lines, laea_str)
lines_laea$NAME_2<-c("Subantarctic Front",
                     "Polar Front",
                     "Southern boundary",
                     "Southern Antarctic Circumpolar Current Front",
                     "Southern Antarctic Circumpolar Current Front",
                     "Southern Antarctic Circumpolar Current Front",
                     "Subtropical Front",
                     "Subtropical Front")
lines_color<-c("Subantarctic Front"="#000000",
               "Polar Front"="#d7191c",
               "Southern boundary"="#000000",
               "Southern Antarctic Circumpolar Current Front"="#315a89",
               "Subtropical Front"="#fdbf6f")

lines_type<-c("Subantarctic Front"=2,
               "Polar Front"=1,
               "Southern boundary"=1,
               "Southern Antarctic Circumpolar Current Front"=1,
               "Subtropical Front"=2)
lines_laea$linetype<-c(2, 1, 1, 1, 1, 1, 2, 2)
occs<-read.csv("/Users/menggeduan/Documents/考氏背鳞鱼/考氏/data25/thr_occ.csv", stringsAsFactors = F)
occ1 <- occs[which(occs$bin==1),]
sp<-st_multipoint(as.matrix(occ1[, c("x", "y")]))
sp<-st_sfc(sp, crs=st_crs(ll_str))
sp<-st_transform(sp, crs=laea_str)
occ0 <- occs[which(occs$bin==0),]
sm<-st_multipoint(as.matrix(occ0[, c("x", "y")]))
sm<-st_sfc(sm, crs=st_crs(ll_str))
sm<-st_transform(sm, crs=laea_str)

model_p_3<-model_p
v<-model_p_3$v
v[which(v>=thr)]<-((v[which(v>=thr)]-thr)/(max(v)-thr))/2+0.5
v[which(v<thr)]<-0.5-((thr-v[which(v<thr)])/(thr-min(v)))/2
model_p_3$v<-v
p<-ggplot()+
  geom_tile(data=mask_continent2_p, 
            aes(x=x, y=y), colour="gray")+
  geom_tile(data=model_p_3, 
            aes(x=x, y=y, fill=v))+
  geom_sf(data=lines_laea, aes(color=NAME_2, linetype=factor(linetype)), 
          alpha=0.5, size=0.5)+
  geom_sf(data=sp, color="#33a02c", size=0.8)+
  geom_sf(data=sm, color="#9932CC", size=0.8)+
  scale_color_manual(values=lines_color)+
  scale_fill_gradient2(low="#447cb0", mid = "#ffefd3", high="#FF0000", 
                       midpoint = 0.5, breaks=c(0, 0.5, 1),
                       labels=c(0, round(thr, digits=2), 1))+
  labs(fill="Habitat suitability index", 
       color="Antarctic circumpolar current fronts")+
  theme_bw()+
  theme(axis.title   = element_blank(),
        legend.position = "bottom")+
  guides(linetype="none",
         color=guide_legend(title.position = "top", ncol=2),
         fill=guide_colorbar(title.position = "top", 
                           title.hjust = 0.5,
                           label.position="bottom"))
p
ggsave(p, filename="/Users/menggeduan/Documents/考氏背鳞鱼/考氏/results25/pdf/Figure3.pdf", 
       width = 8, height=8, units="in")  
ggsave(p, filename="/Users/menggeduan/Documents/考氏背鳞鱼/考氏/results25/tif/Figure3.tiff", 
       width = 8, height=8, units="in")  
  
