setwd("/Users/menggeduan/Documents/考氏背鳞鱼/考氏/")
library(rgdal)
library(raster)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(ggspatial)
library(data.table)
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
thr<-0.2963542
RCP_year<-list()
a<-raster("/Users/menggeduan/Documents/考氏背鳞鱼/考氏/model25/future_RCP26_2050.tif")
p_a<-data.frame(rasterToPoints(a))
colnames(p_a)[3]<-"v_2"
p_a<-merge(p_a, model_p, by=c("x", "y"))
p_a$bin_v<-ifelse(p_a$v>=thr, 1, 0)
p_a$bin_v2<-ifelse(p_a$v_2>=thr, 1, 0)
p_a$type<- -2
p_a[which((p_a$bin_v==1)&(p_a$bin_v2==1)), "type"]<-0
p_a[which((p_a$bin_v==1)&(p_a$bin_v2==0)), "type"]<--1
p_a[which((p_a$bin_v==0)&(p_a$bin_v2==1)), "type"]<-1
if (F){
  p<-ggplot(p_a[which(p_a$type!=-2),])+
    geom_tile(aes(x=x, y=y, fill=factor(type)))
  ggsave(p, filename="/Users/menggeduan/Documents/考氏背鳞鱼/temp.png")
}

continent_a<-mask_continent2_p
continent_a$year<-2050
continent_a$rcp<-"RCP26"
p_a$year<-2050
p_a$rcp<-"RCP26"

b<-raster("/Users/menggeduan/Documents/考氏背鳞鱼/考氏/model25/future_RCP45_2050.tif")
p_b<-data.frame(rasterToPoints(b))
colnames(p_b)[3]<-"v_2"
p_b<-merge(p_b, model_p, by=c("x", "y"))
p_b$bin_v<-ifelse(p_b$v>=thr, 1, 0)
p_b$bin_v2<-ifelse(p_b$v_2>=thr, 1, 0)
p_b$type<- -2
p_b[which((p_b$bin_v==1)&(p_b$bin_v2==1)), "type"]<-0
p_b[which((p_b$bin_v==1)&(p_b$bin_v2==0)), "type"]<--1
p_b[which((p_b$bin_v==0)&(p_b$bin_v2==1)), "type"]<-1
p_b$year<-2050
p_b$rcp<-"RCP45"
continent_b<-mask_continent2_p
continent_b$year<-2050
continent_b$rcp<-"RCP45"

c<-raster("/Users/menggeduan/Documents/考氏背鳞鱼/考氏/model25/future_RCP60_2050.tif")
p_c<-data.frame(rasterToPoints(c))
colnames(p_c)[3]<-"v_2"
p_c<-merge(p_c, model_p, by=c("x", "y"))
p_c$bin_v<-ifelse(p_c$v>=thr, 1, 0)
p_c$bin_v2<-ifelse(p_c$v_2>=thr, 1, 0)
p_c$type<- -2
p_c[which((p_c$bin_v==1)&(p_c$bin_v2==1)), "type"]<-0
p_c[which((p_c$bin_v==1)&(p_c$bin_v2==0)), "type"]<--1
p_c[which((p_c$bin_v==0)&(p_c$bin_v2==1)), "type"]<-1
p_c$year<-2050
p_c$rcp<-"RCP60"
continent_c<-mask_continent2_p
continent_c$year<-2050
continent_c$rcp<-"RCP60"

d<-raster("/Users/menggeduan/Documents/考氏背鳞鱼/考氏/model25/future_RCP85_2050.tif")
p_d<-data.frame(rasterToPoints(d))
colnames(p_d)[3]<-"v_2"
p_d<-merge(p_d, model_p, by=c("x", "y"))
p_d$bin_v<-ifelse(p_d$v>=thr, 1, 0)
p_d$bin_v2<-ifelse(p_d$v_2>=thr, 1, 0)
p_d$type<- -2
p_d[which((p_d$bin_v==1)&(p_d$bin_v2==1)), "type"]<-0
p_d[which((p_d$bin_v==1)&(p_d$bin_v2==0)), "type"]<--1
p_d[which((p_d$bin_v==0)&(p_d$bin_v2==1)), "type"]<-1
p_d$year<-2050
p_d$rcp<-"RCP85"
continent_d<-mask_continent2_p
continent_d$year<-2050
continent_d$rcp<-"RCP85"

e<-raster("/Users/menggeduan/Documents/考氏背鳞鱼/考氏/model25/future_RCP26_2100.tif")
p_e<-data.frame(rasterToPoints(e))
colnames(p_e)[3]<-"v_2"
p_e<-merge(p_e, model_p, by=c("x", "y"))
p_e$bin_v<-ifelse(p_e$v>=thr, 1, 0)
p_e$bin_v2<-ifelse(p_e$v_2>=thr, 1, 0)
p_e$type<- -2
p_e[which((p_e$bin_v==1)&(p_e$bin_v2==1)), "type"]<-0
p_e[which((p_e$bin_v==1)&(p_e$bin_v2==0)), "type"]<--1
p_e[which((p_e$bin_v==0)&(p_e$bin_v2==1)), "type"]<-1
p_e$year<-2100
p_e$rcp<-"RCP26"
continent_e<-mask_continent2_p
continent_e$year<-2100
continent_e$rcp<-"RCP26"

f<-raster("/Users/menggeduan/Documents/考氏背鳞鱼/考氏/model25/future_RCP45_2100.tif")
p_f<-data.frame(rasterToPoints(f))
colnames(p_f)[3]<-"v_2"
p_f<-merge(p_f, model_p, by=c("x", "y"))
p_f$bin_v<-ifelse(p_f$v>=thr, 1, 0)
p_f$bin_v2<-ifelse(p_f$v_2>=thr, 1, 0)
p_f$type<- -2
p_f[which((p_f$bin_v==1)&(p_f$bin_v2==1)), "type"]<-0
p_f[which((p_f$bin_v==1)&(p_f$bin_v2==0)), "type"]<--1
p_f[which((p_f$bin_v==0)&(p_f$bin_v2==1)), "type"]<-1
p_f$year<-2100
p_f$rcp<-"RCP45"
continent_f<-mask_continent2_p
continent_f$year<-2100
continent_f$rcp<-"RCP45"

g<-raster("/Users/menggeduan/Documents/考氏背鳞鱼/考氏/model25/future_RCP60_2100.tif")
p_g<-data.frame(rasterToPoints(g))
colnames(p_g)[3]<-"v_2"
p_g<-merge(p_g, model_p, by=c("x", "y"))
p_g$bin_v<-ifelse(p_g$v>=thr, 1, 0)
p_g$bin_v2<-ifelse(p_g$v_2>=thr, 1, 0)
p_g$type<- -2
p_g[which((p_g$bin_v==1)&(p_g$bin_v2==1)), "type"]<-0
p_g[which((p_g$bin_v==1)&(p_g$bin_v2==0)), "type"]<--1
p_g[which((p_g$bin_v==0)&(p_g$bin_v2==1)), "type"]<-1
p_g$year<-2100
p_g$rcp<-"RCP60"
continent_g<-mask_continent2_p
continent_g$year<-2100
continent_g$rcp<-"RCP60"

h<-raster("/Users/menggeduan/Documents/考氏背鳞鱼/考氏/model25/future_RCP85_2100.tif")
p_h<-data.frame(rasterToPoints(h))
colnames(p_h)[3]<-"v_2"
p_h<-merge(p_h, model_p, by=c("x", "y"))
p_h$bin_v<-ifelse(p_h$v>=thr, 1, 0)
p_h$bin_v2<-ifelse(p_h$v_2>=thr, 1, 0)
p_h$type<- -2
p_h[which((p_h$bin_v==1)&(p_h$bin_v2==1)), "type"]<-0
p_h[which((p_h$bin_v==1)&(p_h$bin_v2==0)), "type"]<--1
p_h[which((p_h$bin_v==0)&(p_h$bin_v2==1)), "type"]<-1
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



cols <- c("1" = "#FF0000", "0" = "#EEE8CD", "-1" = "#447cb0")

p<-ggplot()+
  geom_tile(data=mask_continent2_p, 
            aes(x=x, y=y), fill="grey")+
  geom_tile(data=p_df[which(p_df$type!=-2),], 
            aes(x=x, y=y, fill=factor(type)))+
  geom_sf(data=lines_laea, aes(color=NAME_2, linetype=factor(linetype)), 
          alpha=0, size=1)+
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
                           label.position="bottom"))+
  facet_grid(year~rcp)
ggsave(p, 
       filename="/Users/menggeduan/Documents/考氏背鳞鱼/考氏/results25/pdf/Figures2.pdf",
       width=12, height=5.5, units="in")
ggsave(p, 
       filename="/Users/menggeduan/Documents/考氏背鳞鱼/考氏/results25/tif/Figures2.tiff",
       width=12, height=5.5, units="in") 



