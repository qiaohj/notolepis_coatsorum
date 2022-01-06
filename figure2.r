library(raster)
library(dismo)
library(ggplot2)
occ <- read.csv("/Users/menggeduan/Documents/考氏整理/整理二/第三次/data25/occs1224.csv")
occ$dep <- raster::extract(env$gb_depth, occ[,c("x", "y")])
hist(occ$dep)
occ$dep_th<-ifelse(occ$dep>=2, 1, 0)
ggplot(occ)+geom_point(aes(x=x, y=y, color=factor(dep_th)))

result <- raster("/Users/menggeduan/Documents/考氏整理/整理二/第三次/model25/current1225.tif")
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
result_list2 <- result_list
result_list<-rbindlist(result_list)
result_list[, max_v:=max(v, na.rm = T), by=var]
result_list[, min_v:=min(v, na.rm = T), by=var]
result_list$v_int<-round(((result_list$v-result_list$min_v)/
                            (result_list$max_v-result_list$min_v))*10)

result_list_se<-result_list[, .(mean=mean(current1225, na.rm=T),
                                sd=sd(current1225, na.rm = T)),
                            by=list(var, v_int)]
result_list_se[is.na(sd)]$sd<-0
ggplot(result_list_se)+geom_line(aes(x=v_int/100, y=mean))+
  facet_wrap(~var)
saveRDS(result_list,"/Users/menggeduan/Documents/考氏整理/整理二/第三次/data25/result_list.rda")
p <- ggplot(result_list)+
  #geom_point(aes(x=v, y=current1225))+
  geom_smooth(aes(x=v, y=current1225), method="gam")+
  labs(x = "", y = "Response")+
  theme_bw()+
  facet_wrap(~var, scale="free")
p
vars<-unique(result_list$var)
var_i<-vars[4]
library(gam)
result_v<-list()
for (var_i in vars){
  item<-result_list[var==var_i]
  ml<-mgcv::gam(current1225~s(v, bs = "cs"), data=item)
  pred_hsi<-predict(ml, item)
  max_hsi<-max(pred_hsi, na.rm = T)
  #plot(item$current1225, pred_v)
  v_item<-item[pred_hsi==max_hsi]$v
  result_v[[var_i]]<-data.frame(var=var_i, max_hsi=max_hsi, v=v_item)
}
result_v<-rbindlist(result_v)
write.csv(result_v, "/Users/menggeduan/Documents/考氏整理/整理二/第三次/data25/result_v.csv")
ggsave(p, filename = "/Users/menggeduan/Documents/考氏整理/整理二/第三次/results25/figure2/Figure2.pdf",width = 8,height = 4,units = "in")
ggsave(p, filename = "/Users/menggeduan/Documents/考氏整理/整理二/第三次/results25/figure2/Figure2.tiff",width = 8,height = 4,units = "in")
