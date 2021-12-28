result<-raster("/Users/menggeduan/Documents/考氏背鳞鱼/考氏/model25/current1225.tif")
env_proj<-projectRaster(env, crs=crs(result))
result_p<-data.frame(rasterToPoints(result))
result_p<-result_p[which(result_p$current1225>thr),]
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

result_list_se<-result_list[, .(mean=mean(current1224, na.rm=T),
                                sd=sd(current1224, na.rm = T)),
                            by=list(var, v_int)]
result_list_se[is.na(sd)]$sd<-0
ggplot(result_list_se)+geom_line(aes(x=v_int/100, y=mean))+
  facet_wrap(~var)

p <- ggplot(result_list[sample(nrow(result_list), 1e4)])+
  #geom_point(aes(x=v, y=current1214))+
  geom_smooth(aes(x=v, y=current1225), method="gam")+
  theme_bw()+
  facet_wrap(~var, scale="free")
p
ggsave(p, filename = "/Users/menggeduan/Documents/考氏背鳞鱼/考氏/results25/pdf/Figure2.pdf",width = 8,height = 4,units = "in")
ggsave(p, filename = "/Users/menggeduan/Documents/考氏背鳞鱼/考氏/results25/tif/Figure2.tiff",width = 8,height = 4,units = "in")
