library(dismo)
library(ggplot2)
library(ggpubr)
library(data.table)
library(raster)
library(rgdal)
library("rJava")
raster <- raster("/Users/menggeduan/Documents/考氏背鳞鱼/南极底图/底图/0.08分辨率35s.tif")
plot(raster)
#水域
water_area <- readOGR(dsn="/Users/menggeduan/Documents/考氏背鳞鱼/南极底图/南极地区水域（标注海域名称时使用）/",layer = "南极地区水域")
water_area_df <- fortify(water_area)
#五条线
antarctic <- readOGR(dsn="/Users/menggeduan/Documents/考氏背鳞鱼/南极底图/antarctic_circumpolar_current_fronts/shapefile/",layer = "antarctic_circumpolar_current_fronts")
antarctic_df <- fortify(antarctic)
Polar <- readOGR(dsn="/Users/menggeduan/Documents/考氏背鳞鱼/南极底图/antarctic_circumpolar_current_fronts/shapefile/",layer = "Polar Front")
Polar_df <- fortify(Polar)
#经纬度
LAT <- readOGR(dsn="/Users/menggeduan/Documents/考氏背鳞鱼/南极底图/自制经纬网/",layer = "纬线")
LAT_df <- fortify(LAT)
LON <- readOGR(dsn="/Users/menggeduan/Documents/考氏背鳞鱼/南极底图/自制经纬网/",layer = "经线")
LON_df <- fortify(LON)
ggplot()+
  geom_path(data = Polar_df, 
            aes(x = long, y = lat, group = group),
            color = 'gray',fill="blue")
#########################

raster_1<-projectRaster(from=raster, 
                      crs="+proj=laea +lat_0=-90 +lon_0=0 +ellps=GRS80 +units=m +no_defs")
mask_continent2_p<-data.frame(rasterToPoints(raster_1))
colnames(mask_continent2_p)[3]<-"v"
model<-raster("/Users/menggeduan/Documents/考氏背鳞鱼/model/current1214.tif")
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

lines_type<-c("Subantarctic Front"=2,
              "Polar Front"=1,
              "Southern boundary"=1,
              "Southern Antarctic Circumpolar Current Front"=1,
              "Subtropical Front"=2)
lines_laea$linetype<-c(2, 1, 1, 1, 1, 1, 2, 2)
occs<-read.csv("/Users/menggeduan/Documents/考氏背鳞鱼/考氏缓冲0905.csv", stringsAsFactors = F)
sp<-st_multipoint(as.matrix(occs[, c("lon", "lat")]))
sp<-st_sfc(sp, crs=st_crs(ll_str))
sp<-st_transform(sp, crs=laea_str)

p<-ggplot()+
  geom_tile(data=mask_continent2_p, 
            aes(x=x, y=y, fill=v))+
  scale_fill_gradient2(low="#000000",mid="#A9A9A9",high="#FFFFFF",
                       midpoint = 100)

p<-p+
  geom_tile(data=model_p, 
            aes(x=x, y=y, fill=v))+
  geom_sf(data=lines_laea, aes(color=NAME_2, linetype=factor(linetype)), 
          alpha=0.5, size=0.5)+
  geom_sf(data=sp, color="#33a02c", size=1)+
  scale_color_manual(values=lines_color)+
  scale_fill_gradient2(low="#447cb0", mid = "#ffefd3", high="#FF0000", 
                       midpoint = 0.5)+
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
