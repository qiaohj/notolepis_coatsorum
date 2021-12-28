library(sdm)
library(ggpubr)
model <- readRDS("/Users/menggeduan/Documents/考氏背鳞鱼/考氏/model1225.rda")

model_names<-c("BRT","MDA","RF","SVM","MAXENT","DOMAIND","FDA","GAM",
               "GLM","MARS")
p=list()
for (i in c(1:10)){
  vi <- getVarImp(model,id=i) 
  B <-data.frame(vi@varImportance)
  p[[i]] <- ggplot(B, aes(x=corTest,y=variables))+
    geom_bar(stat="identity", position="dodge",fill="#6A3D9AFF")+
    labs(x = "Relative Variable Importance", y = "Variables",
         title =model_names[i])+
    theme_bw()+
    theme(axis.text=element_text(size=6),
          axis.title.x =element_text(size=7),
          axis.title.y =element_text(size=7),
          title =element_text(size=7))
}
vi <- getVarImp(model) 
B <-data.frame(vi@varImportanceMean)
p[[11]] <- ggplot(B, aes(x=corTest.corTest,y=corTest.variables))+
  geom_bar(stat="identity", position="dodge",fill="#6A3D9AFF")+
  labs(x = "Relative Variable Importance", y = "Variables",
       title ="ensemble")+
  theme_bw()+
  theme(axis.text=element_text(size=6),
        axis.title.x =element_text(size=7),
        axis.title.y =element_text(size=7),
        title =element_text(size=7))
pp<-ggarrange(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]],p[[6]],
              p[[7]],p[[8]], p[[9]],p[[10]], p[[11]],nrow=2, ncol=6)
ggsave(pp, filename="/Users/menggeduan/Documents/考氏背鳞鱼/考氏/results25/pdf/Figures1.pdf", 
       width=15, height=5, units = "in")
ggsave(pp, filename="/Users/menggeduan/Documents/考氏背鳞鱼/考氏/results25/tif/Figures1.tiff", 
       width=15, height=5, units = "in")

######
B$importance <- B$corTest.corTest/sum(B$corTest.corTest)
write.csv(B,"/Users/menggeduan/Documents/考氏背鳞鱼/考氏/data25/importance.csv")
