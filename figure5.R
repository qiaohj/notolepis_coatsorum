setwd("/Users/menggeduan/Documents/考氏背鳞鱼/考氏/")
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

model<-raster("/Users/menggeduan/Documents/考氏背鳞鱼/考氏/model25/future_RCP85_2100.tif")
model_p<-data.frame(rasterToPoints(model))
colnames(model_p)[3]<-"v"
model_p_3<-model_p
v<-model_p_3$v
v[which(v>=thr)]<-((v[which(v>=thr)]-thr)/(max(v)-thr))/2+0.5
v[which(v<thr)]<-0.5-((thr-v[which(v<thr)])/(thr-min(v)))/2
model_p_3$v<-v

lines<-st_read("/Users/menggeduan/Documents/考氏背鳞鱼/南极底图/antarctic_circumpolar_current_fronts/shapefile/antarctic_circumpolar_current_fronts.shp")
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

p1<-ggplot()+
  geom_tile(data=mask_continent2_p, 
            aes(x=x, y=y), fill="grey")+
  geom_tile(data=model_p_3, 
            aes(x=x, y=y, fill=v))+
  geom_sf(data=lines_laea, aes(color=NAME_2, linetype=factor(linetype)), 
          alpha=0.5, size=0.5)+
  scale_color_manual(values=lines_color)+
  scale_fill_gradient2(low="#447cb0", mid = "#ffefd3", high="#FF0000", 
                       midpoint = 0.5, breaks=c(0, 0.5, 1),
                       labels=c(0, round(thr, digits=2), 1))+
  labs(fill="Habitat suitability index", 
       color="Antarctic circumpolar current fronts")+
  theme_bw()+
  theme(axis.title   = element_blank(),
        legend.position = "bottom")+
  guides(color="none",linetype="none",
         fill=guide_colorbar(title.position = "top", 
                             title.hjust = 0.5,
                             label.position="bottom"))

###########################
mask_continent2<-raster("/Users/menggeduan/Documents/考氏背鳞鱼/continent_map.tif")
mask_continent2_p<-data.frame(rasterToPoints(mask_continent2))
colnames(mask_continent2_p)[3]<-"v"

model<-raster("/Users/menggeduan/Documents/考氏背鳞鱼/考氏/model25/current1225.tif")
model_p<-data.frame(rasterToPoints(model))
colnames(model_p)[3]<-"v"
lines<-st_read("/Users/menggeduan/Documents/考氏背鳞鱼/南极底图/antarctic_circumpolar_current_fronts/shapefile/antarctic_circumpolar_current_fronts.shp")
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
lines_laea$linetype<-c(2, 1, 1, 1, 1, 1, 2, 2)

#thr <- 0.206204
thr <- 0.2963542
RCP_year<-list()
a<-raster("/Users/menggeduan/Documents/考氏背鳞鱼/考氏/model25/future_RCP85_2100.tif")
p_a<-data.frame(rasterToPoints(a))
colnames(p_a)[3]<-"v_2"
p_a<-merge(p_a, model_p, by=c("x", "y"))
p_a$bin_v<-ifelse(p_a$v>=thr, 1, 0)
p_a$bin_v2<-ifelse(p_a$v_2>=thr, 1, 0)
p_a$type<- -2
p_a[which((p_a$bin_v==1)&(p_a$bin_v2==1)), "type"]<-0
p_a[which((p_a$bin_v==1)&(p_a$bin_v2==0)), "type"]<--1
p_a[which((p_a$bin_v==0)&(p_a$bin_v2==1)), "type"]<-1

cols <- c("1" = "#FF0000", "0" = "#EEE8CD", "-1" = "#447cb0")
p2<-ggplot()+
  geom_tile(data=mask_continent2_p, 
            aes(x=x, y=y), fill="grey")+
  geom_tile(data=p_a[which(p_a$type!=-2),], 
            aes(x=x, y=y, fill=factor(type)))+
  geom_sf(data=lines_laea, aes(color=NAME_2, linetype=factor(linetype)), 
          alpha=0, size=0.5)+
  scale_color_manual(values=lines_color)+
  scale_fill_manual(values=cols,
                     labels=c("Gain","Refuge","Loss"))+
  labs(fill="   ", 
       color="Antarctic circumpolar current fronts")+
  theme_bw()+
  theme(axis.title   = element_blank(),
        legend.position = "bottom")+
  guides(color="none",linetype="none",
         fill=guide_legend(title.position = "top", 
                             title.hjust = 0.5,
                             label.position="bottom"))
library(ggpubr)  
pp<-ggarrange(p1, p2, nrow=1, ncol=2,labels="auto")
ggsave(pp, filename="/Users/menggeduan/Documents/考氏背鳞鱼/考氏/results25/pdf/Figure5.pdf", 
       width = 12, height=8,units="in") 
ggsave(pp, filename="/Users/menggeduan/Documents/考氏背鳞鱼/考氏/results25/tif/Figure5.tiff", 
       width = 12, height=8,units="in")

