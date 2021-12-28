library(raster)
library(ggplot2)
library(data.table)
library(rgdal)
library(rgdal)

if (F){
  continent<-readOGR(dsn="/Users/menggeduan/Documents/考氏背鳞鱼/map",
                     layer="map")
  mask<-raster("/Users/menggeduan/Documents/考氏背鳞鱼/考氏/model/future_RCP26_2050.tif")
  continent_laea<-spTransform(continent, 
                              CRS(proj4string(mask)))
  
  values(mask)<-0
  mask_continent<-mask(mask, continent_laea)
  
  
  continent<-readOGR(dsn="/Users/menggeduan/Documents/Data/Shape/TM_WORLD_BORDERS-0.3",
                     layer="TM_WORLD_BORDERS-0.3")
  mask<-raster("/Users/menggeduan/Documents/考氏背鳞鱼/考氏/model/RCP26_2050.img")
  continent_laea<-spTransform(continent, 
                              CRS(proj4string(mask)))
  
  values(mask)<-0
  mask_continent<-mask(mask, continent_laea)
  mask_continent2<-projectRaster(from=mask_continent, 
                                 crs="+proj=laea +lat_0=-90 +lon_0=0 +ellps=GRS80 +units=m +no_defs")
  
  writeRaster(mask_continent2,"/Users/menggeduan/Documents/考氏背鳞鱼/continent_map.tif",overwrite=TRUE)
}

mask_continent2<-raster("/Users/menggeduan/Documents/考氏背鳞鱼/南极底图/底图/0.08分辨率35s.tif")
mask_continent2<-projectRaster(from=mask_continent, 
                               crs="+proj=laea +lat_0=-90 +lon_0=0 +ellps=GRS80 +units=m +no_defs")
mask_continent2_p<-data.frame(rasterToPoints(mask_continent2))
colnames(mask_continent2_p)[3]<-"v"

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

a<-raster("/Users/menggeduan/Documents/考氏背鳞鱼/考氏/model25/future_RCP26_2050.tif")
p_a<-data.frame(rasterToPoints(a))
colnames(p_a)[3]<-"v"
v<-p_a$v
v[which(v>=thr)]<-((v[which(v>=thr)]-thr)/(max(v)-thr))/2+0.5
v[which(v<thr)]<-0.5-((thr-v[which(v<thr)])/(thr-min(v)))/2
p_a$v<-v
continent_a<-mask_continent2_p
continent_a$year<-2050
continent_a$rcp<-"RCP26"
p_a$year<-2050
p_a$rcp<-"RCP26"

b<-raster("/Users/menggeduan/Documents/考氏背鳞鱼/考氏/model25/future_RCP45_2050.tif")
p_b<-data.frame(rasterToPoints(b))
colnames(p_b)[3]<-"v"
v<-p_b$v
v[which(v>=thr)]<-((v[which(v>=thr)]-thr)/(max(v)-thr))/2+0.5
v[which(v<thr)]<-0.5-((thr-v[which(v<thr)])/(thr-min(v)))/2
p_b$v<-v
p_b$year<-2050
p_b$rcp<-"RCP45"
continent_b<-mask_continent2_p
continent_b$year<-2050
continent_b$rcp<-"RCP45"

c<-raster("/Users/menggeduan/Documents/考氏背鳞鱼/考氏/model25/future_RCP60_2050.tif")
p_c<-data.frame(rasterToPoints(c))
colnames(p_c)[3]<-"v"
v<-p_c$v
v[which(v>=thr)]<-((v[which(v>=thr)]-thr)/(max(v)-thr))/2+0.5
v[which(v<thr)]<-0.5-((thr-v[which(v<thr)])/(thr-min(v)))/2
p_c$v<-v
p_c$year<-2050
p_c$rcp<-"RCP60"
continent_c<-mask_continent2_p
continent_c$year<-2050
continent_c$rcp<-"RCP60"

d<-raster("/Users/menggeduan/Documents/考氏背鳞鱼/考氏/model25/future_RCP85_2050.tif")
p_d<-data.frame(rasterToPoints(d))
colnames(p_d)[3]<-"v"
v<-p_d$v
v[which(v>=thr)]<-((v[which(v>=thr)]-thr)/(max(v)-thr))/2+0.5
v[which(v<thr)]<-0.5-((thr-v[which(v<thr)])/(thr-min(v)))/2
p_d$v<-v
p_d$year<-2050
p_d$rcp<-"RCP85"
continent_d<-mask_continent2_p
continent_d$year<-2050
continent_d$rcp<-"RCP85"

e<-raster("/Users/menggeduan/Documents/考氏背鳞鱼/考氏/model25/future_RCP26_2100.tif")
p_e<-data.frame(rasterToPoints(e))
colnames(p_e)[3]<-"v"
v<-p_e$v
v[which(v>=thr)]<-((v[which(v>=thr)]-thr)/(max(v)-thr))/2+0.5
v[which(v<thr)]<-0.5-((thr-v[which(v<thr)])/(thr-min(v)))/2
p_e$v<-v
p_e$year<-2100
p_e$rcp<-"RCP26"
continent_e<-mask_continent2_p
continent_e$year<-2100
continent_e$rcp<-"RCP26"

f<-raster("/Users/menggeduan/Documents/考氏背鳞鱼/考氏/model25/future_RCP45_2100.tif")
p_f<-data.frame(rasterToPoints(f))
colnames(p_f)[3]<-"v"
v<-p_f$v
v[which(v>=thr)]<-((v[which(v>=thr)]-thr)/(max(v)-thr))/2+0.5
v[which(v<thr)]<-0.5-((thr-v[which(v<thr)])/(thr-min(v)))/2
p_f$v<-v
p_f$year<-2100
p_f$rcp<-"RCP45"
continent_f<-mask_continent2_p
continent_f$year<-2100
continent_f$rcp<-"RCP45"

g<-raster("/Users/menggeduan/Documents/考氏背鳞鱼/考氏/model25/future_RCP60_2100.tif")
p_g<-data.frame(rasterToPoints(g))
colnames(p_g)[3]<-"v"
v<-p_g$v
v[which(v>=thr)]<-((v[which(v>=thr)]-thr)/(max(v)-thr))/2+0.5
v[which(v<thr)]<-0.5-((thr-v[which(v<thr)])/(thr-min(v)))/2
p_g$v<-v
p_g$year<-2100
p_g$rcp<-"RCP60"
continent_g<-mask_continent2_p
continent_g$year<-2100
continent_g$rcp<-"RCP60"

h<-raster("/Users/menggeduan/Documents/考氏背鳞鱼/考氏/model25/future_RCP85_2100.tif")
p_h<-data.frame(rasterToPoints(h))
colnames(p_h)[3]<-"v"
v<-p_h$v
v[which(v>=thr)]<-((v[which(v>=thr)]-thr)/(max(v)-thr))/2+0.5
v[which(v<thr)]<-0.5-((thr-v[which(v<thr)])/(thr-min(v)))/2
p_h$v<-v
p_h$year<-2100
p_h$rcp<-"RCP85"
continent_h<-mask_continent2_p
continent_h$year<-2100
continent_h$rcp<-"RCP85"

p_df<-rbindlist(list(p_a, p_b, p_c, p_d, p_e, p_f, p_g, p_h))
continent_df<-rbindlist(list(continent_a, continent_b, 
                             continent_c, continent_d, 
                             continent_e, continent_f, 
                             continent_g, continent_h))


#p_df<-p_df[sample(nrow(p_df), 10000),]
#continent_df<-continent_df[sample(nrow(continent_df), 10000),]


p<-ggplot()+
  geom_tile(data=mask_continent2_p, 
            aes(x=x, y=y), colour="gray")+
  geom_tile(data=p_df, aes(x=x, y=y, fill=v))+
  geom_sf(data=lines_laea, aes(color=NAME_2, linetype=factor(linetype)), 
          alpha=0, size=0.5)+
  scale_fill_gradient2(low="#447cb0", mid = "#ffefd3", high="#FF0000", 
                       midpoint = 0.5, breaks=c(0, 0.5, 1),
                       labels=c(0, round(thr, digits=2), 1))+
  labs(fill="Habitat suitability index")+
  theme_bw()+
  theme(axis.title   = element_blank(),
        legend.position = "bottom")+
  guides(color="none", linetype="none",
    fill=guide_colorbar(title.position = "top", 
                             title.hjust = 0.5,
                             label.position="bottom"))+
  facet_grid(year~rcp)
  #coord_fixed()+
#p
ggsave(p, 
       filename="/Users/menggeduan/Documents/考氏背鳞鱼/考氏/results25/pdf/Figure4.pdf",
       width=12, height=5.5, units="in")
ggsave(p, 
       filename="/Users/menggeduan/Documents/考氏背鳞鱼/考氏/results25/tif/Figure4.tiff",
       width=12, height=5.5, units="in")
