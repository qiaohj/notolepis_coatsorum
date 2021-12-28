model1<-c(rep("BRT", 6), rep("MDA", 3), rep("RF", 10), rep("SVM", 8), 
         rep("MAXENT", 18), "DOMAIND", "FDA", "GAM", "GLM","MARS")
E <- read.csv("/Users/menggeduan/Documents/考氏背鳞鱼/考氏/tunning/Eval.csv")
E <- E[1:50,]
E_M <- cbind(E,model1)

  parastrs<-c(rep("A", 6),
              rep("B",3),
              rep("C",10),
              rep("D",8),
              rep("E",18),
              "F","G","H","I","J")
  Exx<-data.table(E_M)
  E_sd<-Exx[, .(max_proc=max(mean_proc)),
            by=list(model1)]
  E_sd<-Exx[mean_proc %in% E_sd$max_proc]
  E_M$model1<-as.factor(E_M$model1)
  library(viridis)
  p <- ggplot(E_M)+
    geom_point(aes(x=X, y=mean_proc, color=model1), 
               position = position_dodge(width = -0.5))+
    scale_color_viridis(alpha=1,end=0.9,direction=-1,discrete = TRUE)+
    geom_errorbar(aes(x=X, ymin=mean_proc-SD_proc, 
                      ymax=mean_proc+SD_proc, color=model1), 
                  width=0.7, 
                  position=position_dodge(width = -0.5))+
    labs(x = "", y = "partial AUC", color="")+
    #geom_point(data=E_sd, aes(x=model, y=mean_proc),color="#FF0000",size=1)+
    geom_text(data=E_sd, aes(x=X, y=mean_proc+SD_proc, 
                             label=parastrs[order]),
              color="#000000",vjust=-0.2, hjust=0.5)+
    theme_bw()+
    ylim(1.3, 2)+
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold"),
          axis.title.x = element_blank(),
          axis.text.x = element_blank())
  p
  ggsave(p,filename = "/Users/menggeduan/Documents/考氏背鳞鱼/考氏/results25/pdf/Figure1.pdf",width=8, height=4.5,units="in")
  ggsave(p,filename = "/Users/menggeduan/Documents/考氏背鳞鱼/考氏/results25/tif/Figure1.tiff",width=8, height=4.5,units="in")
  