library("ggplot2")
library("openxlsx")
library(reshape)
library(reshape2)
library(cowplot)
library(gdata)
library(ggpubr)

bch_manual <- read.xlsx("/Users/amandava/Desktop/HIPC_Flow/BCH-UBC/BCH-UBC_manual.xlsx",sheet=2,rowNames = TRUE)
bch_dafi <- read.xlsx("/Users/amandava/Desktop/HIPC_Flow/BCH-UBC/BCH-UBC_props_2.xlsx",sheet=2,rowNames = TRUE)
bch_dafi_h2 <- read.xlsx("/Users/amandava/Desktop/HIPC_Flow/BCH-UBC/BCH-UBC_v2_hierarchy/BCH-UBC_v2_hierarchy.xlsx",sheet=2,rowNames = TRUE)
bch_dafi_melt <- melt(t(bch_dafi))
bch_dafi_melt_h2 <- melt(t(bch_dafi_h2))
bch_manual_melt <- melt(t(bch_manual))

man_dafi <- cbind(bch_dafi_melt,bch_manual_melt[,3])
man_dafi_dafi_h2 <- cbind(bch_dafi_melt,bch_dafi_melt_h2[,3],bch_manual_melt[,3])
colnames(man_dafi) <- c("Sample","CellPopulation","DAFi","Manual")
colnames(man_dafi_dafi_h2) <- c("Sample","CellPopulation","DAFi","DAFi_h2","Manual")
man_dafi_melt <- melt(man_dafi)
man_dafi_melt_h2 <- melt(man_dafi_dafi_h2)
 
quartz()
ggplot(data = na.omit(man_dafi_melt),aes(x=CellPopulation,y=value)) + facet_wrap(~Sample,ncol=3) +
  geom_bar(aes(fill=variable),stat = "identity",position = "dodge")+
  scale_fill_discrete(guide=guide_legend(reverse = T))+xlab("Cell Population") + ylab("Population proportion with live singlets as parent")+theme_bw()+
  ggtitle("Cell populations between manual and DAFi across the samples")+coord_flip()

quartz()
ggplot(data = na.omit(man_dafi_melt_h2),aes(x=CellPopulation,y=value)) + facet_wrap(~Sample,ncol=3) +
  geom_bar(aes(fill=variable),stat = "identity",position = "dodge")+
  scale_fill_discrete(guide=guide_legend(reverse = T))+xlab("Cell Population") + ylab("Population proportion")+theme_bw()+
  ggtitle("Cell popualtions for DAFi, DAFi_2 and manual across the samples")+coord_flip()

quartz()
ggplot(data = na.omit(man_dafi_melt),aes(x=CellPopulation,y=value,fill=variable))+
  geom_boxplot()+xlab("Cell Population") + ylab("Population proportion")+
  scale_fill_discrete(guide=guide_legend(reverse = T))+theme_bw()+
  ggtitle("Boxplot of cell popualtions between manual and DAFi")+coord_flip()

  quartz()
  ggplot(data = na.omit(man_dafi_melt),aes(x=variable,y=value,fill=variable))+
    geom_boxplot()+xlab("Cell Population") + ylab("Population proportion")+
    scale_fill_discrete(guide=guide_legend(reverse = T))+facet_grid(.~CellPopulation,scales = "free")
  ggtitle("Boxplot of cell popualtions between manual and DAFi")+coord_flip()

  
  #Correlation analysis for BCH-UBC Manual vs DAFi 
  plots_1 <- list()
  for(i in 1:10){
    my_data <- as.data.frame(cbind(t(bch_dafi)[,i],t(bch_manual)[,i]))
    plots_1[[i]] <- ggscatter(my_data,x="V1",y="V2", add="reg.line", conf.int = TRUE, 
                              cor.coef = TRUE, cor.method = "pearson",xlab="DAFi", ylab="Manual",title=paste(colnames(t(bch_dafi))[i]))
  }
  quartz()
  title <- ggdraw() + draw_label("Pearson correlation for BCH-UBC", fontface='bold')
  p <- plot_grid(plots_1[[1]],plots_1[[2]],plots_1[[3]],plots_1[[4]],plots_1[[5]],plots_1[[6]],plots_1[[7]],plots_1[[8]],plots_1[[9]],plots_1[[10]])
  plot_grid(title,p,ncol=1,rel_heights=c(0.1,1))

  #Taking an average of emory results for manual
  emory_m <- c()
  emory_m <- (emorya_manual+emoryb_manual)/2
  
  emory <- c()
  emory <- (emorya_dafi+emoryb_dafi)/2
  
  #Monocytes
  mono_dafi <- as.data.frame(cbind(lji_dafi[,1], emory[,1],mtsinai_dafi[,1],bch_ubc_dafi[,1]),row.names = rownames(lji_dafi),colnames = TRUE)
  colnames(mono_dafi)<-c("lji","emory","mtsinai","BCH-UBC")
  mono_manual <- as.data.frame(cbind(lji_manual[,1], emory_m[,1],mtsinai_manual[,1],bch_ubc_manual[,1]),row.names = rownames(lji_dafi),colnames = TRUE)
  colnames(mono_manual)<-c("lji","emory","mtsinai","BCH-UBC")
  mono_avg <- data.frame(melt(t(mono_manual)),melt(t(mono_dafi))[3])
  names(mono_avg)<-c("Center","Sample","Manual","DAFi")
  monocytes <- melt(mono_avg)
  
  quartz()
  ggplot(data = monocytes,aes(x=Center,y=value,fill=Center))+facet_wrap(~Sample,ncol=3)+
    geom_bar(aes(fill=variable),stat = "identity",position = "dodge",width=0.5)+
    scale_fill_discrete(guide=guide_legend(reverse = T))+xlab("Center") +
    ylab("Population proportion with live singlets as the parent")+theme_bw()+ 
    ggtitle("Monocytes population proportion between manual and DAFi across the centers")+coord_flip()

  Tcells_dafi <- as.data.frame(cbind(lji_dafi[,4], emory[,4],mtsinai_dafi[,4],bch_ubc_dafi[,8]),row.names = rownames(lji_dafi),colnames = c("lji","emory","mtsiani","Columbia"))
  colnames(Tcells_dafi)<-c("lji","emory","mtsinai","BCH-UBC")
  Tcells_manual <- as.data.frame(cbind(lji_manual[,4], emory_m[,4],mtsinai_manual[,4],bch_ubc_manual[,8]),row.names = rownames(lji_dafi),colnames = TRUE)
  colnames(Tcells_manual)<-c("lji","emory","mtsinai","BCH-UBC")
  Tcells_avg <- data.frame(melt(t(Tcells_manual)),melt(t(Tcells_dafi))[3])
  names(Tcells_avg)<-c("Center","Sample","Manual","DAFi")
  Tcells <- melt(Tcells_avg)
  
  quartz()
  ggplot(data = Tcells,aes(x=Center,y=value,fill=Center))+facet_wrap(~Sample,ncol=3)+
    geom_bar(aes(fill=variable),stat = "identity",position = "dodge",width=0.5)+
    scale_fill_discrete(guide=guide_legend(reverse = T))+xlab("Center") +
    ylab("Population proportion with live singlets as the parent")+theme_bw()+ 
    ggtitle("Tcells population proportion between manual and DAFi across the centers")+coord_flip()
  
  
  