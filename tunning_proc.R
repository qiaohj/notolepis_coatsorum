setwd("/Users/menggeduan/Documents/考氏背鳞鱼/考氏/tunning")
#建模
library(dismo)##maxent
library(mgcv)##GAMs
library(MuMIn)
library(sdm)
library(mapview)
library(parallel)
library(usdm)
library(rJava)
library(dplyr)
library(tidyr)
library(kuenm)
Absences_duan <- read.csv("/Users/menggeduan/Documents/考氏背鳞鱼/考氏/data/考氏背景点1224.csv")
Absences2=as.data.frame(Absences_duan)
ab1=Absences2 %>% select('species1.PA01.x','species1.PA01.y','species1.PA01.v')
ab2<-ab1%>%drop_na()
coordinates(ab2)<-c('species1.PA01.x','species1.PA01.y')
proj4string(ab2)<-projection(raster())
d=sdmData(species1.PA01.v~.,ab2,predictors = antarctica)
#model
methods<-c(rep("brt", 6), rep("mda", 3), rep("rf", 10), rep("svm", 8), rep("maxent", 18), "domainD", "fda", "gam", "glm","mars")
paras<-list()
paras[[1]]<-list(brt=list(n.trees=500,train.fraction=0.1))
paras[[2]]<-list(brt=list(n.trees=500,train.fraction=0.5))
paras[[3]]<-list(brt=list(n.trees=500,train.fraction=1))
paras[[4]]<-list(brt=list(n.trees=1000,train.fraction=0.1))
paras[[5]]<-list(brt=list(n.trees=1000,train.fraction=0.5))
paras[[6]]<-list(brt=list(n.trees=1000,train.fraction=1))
paras[[7]]<-list(mda=list(eps=2))
paras[[8]]<-list(mda=list(eps=3))
paras[[9]]<-list(mda=list(eps=4))
paras[[10]]<-list(rf=list(ntree=500,mtry=1))
paras[[11]]<-list(rf=list(ntree=500,mtry=2))
paras[[12]]<-list(rf=list(ntree=500,mtry=3))
paras[[13]]<-list(rf=list(ntree=500,mtry=4))
paras[[14]]<-list(rf=list(ntree=500,mtry=5))
paras[[15]]<-list(rf=list(ntree=1000,mtry=1))
paras[[16]]<-list(rf=list(ntree=1000,mtry=2))
paras[[17]]<-list(rf=list(ntree=1000,mtry=3))
paras[[18]]<-list(rf=list(ntree=1000,mtry=4))
paras[[19]]<-list(rf=list(ntree=1000,mtry=5))
paras[[20]]<-list(svm=list(kernel="vanilladot",gamma=0.2))#default
paras[[21]]<-list(svm=list(kernel="vanilladot",gamma=0))
paras[[22]]<-list(svm=list(kernel="vanilladot",gamma=0.5))
paras[[23]]<-list(svm=list(kernel="vanilladot",gamma=1))
paras[[24]]<-list(svm=list(kernel="polydot",gamma=0.2))#default
paras[[25]]<-list(svm=list(kernel="polydot",gamma=0))
paras[[26]]<-list(svm=list(kernel="polydot",gamma=0.5))
paras[[27]]<-list(svm=list(kernel="polydot",gamma=1))

paras[[28]]<-list(maxent=list(args=c(sprintf("betamultiplier=%.2f", 0.01),
                                     "autofeature=false",
                                     sprintf("linear=%s", "true"),
                                     sprintf("quadratic=%s", "false"),
                                     sprintf("product=%s", "false"),
                                     sprintf("threshold=%s", "false"),
                                     sprintf("hinge=%s", "false"),
                                     sprintf("doclamp=%s", "false"))))
paras[[29]]<-list(maxent=list(args=c(sprintf("betamultiplier=%.2f", 1),
                                     "autofeature=false",
                                     sprintf("linear=%s", "true"),
                                     sprintf("quadratic=%s", "false"),
                                     sprintf("product=%s", "false"),
                                     sprintf("threshold=%s", "false"),
                                     sprintf("hinge=%s", "false"),
                                     sprintf("doclamp=%s", "false"))))
paras[[30]]<-list(maxent=list(args=c(sprintf("betamultiplier=%.2f", 2),
                                     "autofeature=false",
                                     sprintf("linear=%s", "true"),
                                     sprintf("quadratic=%s", "false"),
                                     sprintf("product=%s", "false"),
                                     sprintf("threshold=%s", "false"),
                                     sprintf("hinge=%s", "false"),
                                     sprintf("doclamp=%s", "false"))))
paras[[31]]<-list(maxent=list(args=c(sprintf("betamultiplier=%.2f", 3),
                                     "autofeature=false",
                                     sprintf("linear=%s", "true"),
                                     sprintf("quadratic=%s", "false"),
                                     sprintf("product=%s", "false"),
                                     sprintf("threshold=%s", "false"),
                                     sprintf("hinge=%s", "false"),
                                     sprintf("doclamp=%s", "false"))))
paras[[32]]<-list(maxent=list(args=c(sprintf("betamultiplier=%.2f", 4),
                                     "autofeature=false",
                                     sprintf("linear=%s", "true"),
                                     sprintf("quadratic=%s", "false"),
                                     sprintf("product=%s", "false"),
                                     sprintf("threshold=%s", "false"),
                                     sprintf("hinge=%s", "false"),
                                     sprintf("doclamp=%s", "false"))))
paras[[33]]<-list(maxent=list(args=c(sprintf("betamultiplier=%.2f", 5),
                                     "autofeature=false",
                                     sprintf("linear=%s", "true"),
                                     sprintf("quadratic=%s", "false"),
                                     sprintf("product=%s", "false"),
                                     sprintf("threshold=%s", "false"),
                                     sprintf("hinge=%s", "false"),
                                     sprintf("doclamp=%s", "false"))))
paras[[34]]<-list(maxent=list(args=c(sprintf("betamultiplier=%.2f", 0.01),
                                     "autofeature=false",
                                     sprintf("linear=%s", "false"),
                                     sprintf("quadratic=%s", "true"),
                                     sprintf("product=%s", "false"),
                                     sprintf("threshold=%s", "false"),
                                     sprintf("hinge=%s", "false"),
                                     sprintf("doclamp=%s", "false"))))
paras[[35]]<-list(maxent=list(args=c(sprintf("betamultiplier=%.2f", 1),
                                     "autofeature=false",
                                     sprintf("linear=%s", "false"),
                                     sprintf("quadratic=%s", "true"),
                                     sprintf("product=%s", "false"),
                                     sprintf("threshold=%s", "false"),
                                     sprintf("hinge=%s", "false"),
                                     sprintf("doclamp=%s", "false"))))
paras[[36]]<-list(maxent=list(args=c(sprintf("betamultiplier=%.2f", 2),
                                     "autofeature=false",
                                     sprintf("linear=%s", "false"),
                                     sprintf("quadratic=%s", "true"),
                                     sprintf("product=%s", "false"),
                                     sprintf("threshold=%s", "false"),
                                     sprintf("hinge=%s", "false"),
                                     sprintf("doclamp=%s", "false"))))
paras[[37]]<-list(maxent=list(args=c(sprintf("betamultiplier=%.2f", 3),
                                     "autofeature=false",
                                     sprintf("linear=%s", "false"),
                                     sprintf("quadratic=%s", "true"),
                                     sprintf("product=%s", "false"),
                                     sprintf("threshold=%s", "false"),
                                     sprintf("hinge=%s", "false"),
                                     sprintf("doclamp=%s", "false"))))
paras[[38]]<-list(maxent=list(args=c(sprintf("betamultiplier=%.2f", 4),
                                     "autofeature=false",
                                     sprintf("linear=%s", "false"),
                                     sprintf("quadratic=%s", "true"),
                                     sprintf("product=%s", "false"),
                                     sprintf("threshold=%s", "false"),
                                     sprintf("hinge=%s", "false"),
                                     sprintf("doclamp=%s", "false"))))
paras[[39]]<-list(maxent=list(args=c(sprintf("betamultiplier=%.2f", 5),
                                     "autofeature=false",
                                     sprintf("linear=%s", "false"),
                                     sprintf("quadratic=%s", "true"),
                                     sprintf("product=%s", "false"),
                                     sprintf("threshold=%s", "false"),
                                     sprintf("hinge=%s", "false"),
                                     sprintf("doclamp=%s", "false"))))
paras[[40]]<-list(maxent=list(args=c(sprintf("betamultiplier=%.2f", 0.01),
                                     "autofeature=false",
                                     sprintf("linear=%s", "true"),
                                     sprintf("quadratic=%s", "true"),
                                     sprintf("product=%s", "false"),
                                     sprintf("threshold=%s", "false"),
                                     sprintf("hinge=%s", "false"),
                                     sprintf("doclamp=%s", "false"))))
paras[[41]]<-list(maxent=list(args=c(sprintf("betamultiplier=%.2f", 1),
                                     "autofeature=false",
                                     sprintf("linear=%s", "true"),
                                     sprintf("quadratic=%s", "true"),
                                     sprintf("product=%s", "false"),
                                     sprintf("threshold=%s", "false"),
                                     sprintf("hinge=%s", "false"),
                                     sprintf("doclamp=%s", "false"))))
paras[[42]]<-list(maxent=list(args=c(sprintf("betamultiplier=%.2f", 2),
                                     "autofeature=false",
                                     sprintf("linear=%s", "true"),
                                     sprintf("quadratic=%s", "true"),
                                     sprintf("product=%s", "false"),
                                     sprintf("threshold=%s", "false"),
                                     sprintf("hinge=%s", "false"),
                                     sprintf("doclamp=%s", "false"))))
paras[[43]]<-list(maxent=list(args=c(sprintf("betamultiplier=%.2f", 3),
                                     "autofeature=false",
                                     sprintf("linear=%s", "true"),
                                     sprintf("quadratic=%s", "true"),
                                     sprintf("product=%s", "false"),
                                     sprintf("threshold=%s", "false"),
                                     sprintf("hinge=%s", "false"),
                                     sprintf("doclamp=%s", "false"))))
paras[[44]]<-list(maxent=list(args=c(sprintf("betamultiplier=%.2f", 4),
                                     "autofeature=false",
                                     sprintf("linear=%s", "true"),
                                     sprintf("quadratic=%s", "true"),
                                     sprintf("product=%s", "false"),
                                     sprintf("threshold=%s", "false"),
                                     sprintf("hinge=%s", "false"),
                                     sprintf("doclamp=%s", "false"))))
paras[[45]]<-list(maxent=list(args=c(sprintf("betamultiplier=%.2f", 5),
                                     "autofeature=false",
                                     sprintf("linear=%s", "true"),
                                     sprintf("quadratic=%s", "true"),
                                     sprintf("product=%s", "false"),
                                     sprintf("threshold=%s", "false"),
                                     sprintf("hinge=%s", "false"),
                                     sprintf("doclamp=%s", "false"))))

paras[[46]] <- NA
paras[[47]] <- NA
paras[[48]] <- NA
paras[[49]] <- NA
paras[[50]] <- NA


for (i in c(1:length(methods))){
  print(i)
  filename=sprintf("/Users/menggeduan/Documents/考氏背鳞鱼/考氏/tunning/%d.rda", i)
 # if (file.exists(filename)){
  #  next()
  #}
  ###Among the 19 algorithms, only those with excellent performance (AUC>0.90; TSS>0.8; Kappa>0.8)were selected for further work.
  if (is.na(paras[[i]])){
    m2=sdm(species1.PA01.v~.,d,methods = methods[i],
           replication='boot',test.p=20,n=10,
           parallelSetting=list(ncore=8,method='parallel'))
  }else{
    m2=sdm(species1.PA01.v~.,d,methods = methods[i],
           modelSettings=paras[[i]],
           replication='boot',test.p=20,n=10,
           parallelSetting=list(ncore=8,method='parallel'))
  }
  saveRDS(m2, filename)
}

#tunning
env=stack(c("/Users/menggeduan/Documents/考氏背鳞鱼/相同分辨率环境因子/BO2_curvelmean_ss.tif",
            '/Users/menggeduan/Documents/考氏背鳞鱼/相同分辨率环境因子/BO2_icethickmean_ss.tif',
            '/Users/menggeduan/Documents/考氏背鳞鱼/相同分辨率环境因子/BO2_salinitymean_ss.tif',
            '/Users/menggeduan/Documents/考氏背鳞鱼/相同分辨率环境因子/BO2_tempmean_ss.tif',
            '/Users/menggeduan/Documents/考氏背鳞鱼/相同分辨率环境因子/gb_depth.tif',
            '/Users/menggeduan/Documents/考氏背鳞鱼/相同分辨率环境因子/gb_land_distance.tif'))
antarctica <- raster::crop(env, extent(-180, 180, -90, -35))
csv_file<-sprintf("/Users/menggeduan/Documents/考氏背鳞鱼/考氏/data/occs1224.csv")
occ<-read.table(csv_file, head=T, sep=",")
occ <- occ[,-1]
##study area
model<-c(rep("brt", 6), rep("mda", 3), rep("rf", 10), rep("svm", 8), 
           rep("maxent", 18), "domainD", "fda", "gam", "glm","mars")
result_proc <- list(NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
                    NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
                    NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
                    NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
                    NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL)
Eval <- data.frame()
for (j in c(1:length(model))){
  print(j)
  filename=sprintf("/Users/menggeduan/Documents/考氏背鳞鱼/考氏/tunning/%d.rda", j)
  m2<-readRDS(filename)
  cur=predict(m2,antarctica,filename=sprintf('cur_%s.img',j))

  for (i in c(1:length(names(cur)))){
    print(i)
    n<-names(cur)[i]
    order<-j
    model_name<-model[j]
    cur_item<-cur[[n]]
    pauc<-kuenm_proc(occ[, c("x", "y")], cur_item)
    proc_v<-pauc$pROC_summary[1]
    item<-data.frame( order= order, model=model_name, proc=proc_v)
    if (is.null(result_proc[[j]])){
      result_proc[[j]]<-item
    }else{
      result_proc[[j]]<-rbind(result_proc[[j]], item)
    }
    Eval[j,1] <- j
    Eval[j,2] <- model[j]
    Eval[j,3]<- mean(result_proc[[j]]$proc)
    Eval[j,4] <- sd(result_proc[[j]]$proc)
  }
}
names(Eval) <- c("order","model","mean_proc","SD_proc")
write.csv(Eval, "/Users/menggeduan/Documents/考氏背鳞鱼/考氏/tunning/Eval.csv")
E_l <- list()
for (i in c(1:length(model))){
  E_l[[model[i]]] <- Eval[which(Eval$model==model[i]),]
}
select <- list()
#methods<-c(rep("brt", 6), rep("mda", 3), rep("rf", 10), rep("svm", 8), rep("maxent", 18), "domainD", "fda", "gam", "glm","mars", "mlp" )
select[[1]] <- E_l$brt[which(E_l$brt$mean_proc==max(E_l$brt$mean_proc)),]
select[[2]] <- E_l$mda[which(E_l$mda$mean_proc==max(E_l$mda$mean_proc)),]
select[[3]] <- E_l$rf[which(E_l$rf$mean_proc==max(E_l$rf$mean_proc)),]
select[[4]] <- E_l$svm[which(E_l$svm$mean_proc==max(E_l$svm$mean_proc)),]
select[[5]] <- E_l$maxent[which(E_l$maxent$mean_proc==max(E_l$maxent$mean_proc)),]
select[[6]] <- E_l$domain
select[[7]] <- E_l$fda
select[[8]] <- E_l$gam
select[[9]] <- E_l$glm
select[[10]] <- E_l$mars
select <- rbindlist(select)
write.csv(select , "/Users/menggeduan/Documents/考氏背鳞鱼/考氏/tunning/Eval_select.csv")
