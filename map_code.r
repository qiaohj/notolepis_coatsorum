library(rgdal)
library(sp)
lon<-180
lat<-90
## from the sp vignette:
lines<-list()

for (lat in seq(-80, 80, by=10)){
  id<-sprintf("lat%d", lat)
  l<-matrix(c(-180, 180, lat, lat), nrow=2)
  Sl <- Line(l)
  S <- Lines(list(Sl), ID = id)
  lines[[id]]<-S
}

for (lon in seq(-180, 180, by=45)){
  id<-sprintf("%d", lon)
  l<-matrix(c(lon, lon, -80, 80), nrow=2)
  Sl <- Line(l)
  S <- Lines(list(Sl), ID = id)
  lines[[id]]<-S
}
ll_df<-data.frame(id=c(1:26))
Sl <- SpatialLines(lines, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))
plot(Sl)
Sl_df<-SpatialLinesDataFrame(Sl, ll_df, match.ID = F)
writeOGR(Sl_df, dsn="~/Downloads/ll", layer="ll", driver = "ESRI Shapefile")
Sl_df_laea<-spTransform(Sl_df, 
                        CRS("+proj=laea +lat_0=-90 +lon_0=0 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"))
writeOGR(Sl_df_laea, dsn="~/Downloads/ll", layer="ll_laea", driver = "ESRI Shapefile")

## sample data: line lengths
library(rgeos)
df <- data.frame(len = sapply(1:length(Sl), function(i) gLength(Sl[i, ])))
rownames(df) <- sapply(1:length(Sl), function(i) Sl@lines[[i]]@ID)


## SpatialLines to SpatialLinesDataFrame
Sldf <- SpatialLinesDataFrame(Sl, data = df)

plot(Sldf, col = c("red", "blue"))
text(labels = paste0("length = ", round(Sldf@data$len, 2)), 
     x = gCentroid(Sldf, byid = TRUE)$x,
     y = gCentroid(Sldf, byid = TRUE)$y)


library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(ggplot2)
library(ggspatial)
world <- ne_countries(scale = "medium", returnclass = "sf")
ggplot(data = world) +
  geom_sf(aes(fill = continent)) + 
  coord_sf(crs = st_crs("+proj=laea +lat_0=-90 +lon_0=0 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"))+
  theme(panel.grid.major = element_line(color = gray(.2), 
                                        linetype = "dashed", size = 0.1), 
        panel.background = element_rect(fill = "aliceblue"))
st_crs(3035)
