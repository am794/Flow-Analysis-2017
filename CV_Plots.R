install.packages("reshape2")
install.packages("ggplot2")
library("reshape2")
library("ggplot2")
require(gdata)

################################
# proportions for DAFi results #
################################

lji_dafi <- read.xls("/Users/amandava/Desktop/HIPC_IOF_amandava/IOF_new/props_DAFi_new_2/LJI_props_4.xlsx",header= TRUE, sep = ",", sheet = 2)
rownames(lji_dafi)<-lji_dafi[,1]
lji_dafi<-lji_dafi[,2:12]
lji_dafi <- as.data.frame(t(lji_dafi))

emorya_dafi <- read.xls("/Users/amandava/Desktop/HIPC_IOF_amandava/IOF_new/props_DAFi_new_2/emoryA_props_2.xlsx",header= TRUE, sep = ",", sheet=2)
rownames(emorya_dafi)<-emorya_dafi[,1]
emorya_dafi<-emorya_dafi[,2:12]
emorya_dafi <- as.data.frame(t(emorya_dafi))

emoryb_dafi <- read.xls("/Users/amandava/Desktop/HIPC_IOF_amandava/IOF_new/props_DAFi_new_2/emoryB_props_2.xlsx",header= TRUE, sep = ",", sheet=2)
rownames(emoryb_dafi)<-emoryb_dafi[,1]
emoryb_dafi<-emoryb_dafi[,2:12]
emoryb_dafi <- as.data.frame(t(emoryb_dafi))

#mtsinai_dafi <- read.xls("/Users/amandava/Desktop/HIPC_IOF_amandava/IOF_new/props_DAFi_new/Mtsinai_props_2.xlsx",header= TRUE, sep = ",")
mtsinai_dafi <- read.xls("/Users/amandava/Desktop/HIPC_IOF_amandava/IOF_new/props_DAFi_new_2/Mtsinai_props_4.xlsx",header= TRUE, sep = ",",sheet=2)
rownames(mtsinai_dafi)<-mtsinai_dafi[,1]
mtsinai_dafi<-mtsinai_dafi[,2:12]
mtsinai_dafi <- as.data.frame(t(mtsinai_dafi))

#Take an average of emory results for DAFi
r_cv <- row.names(emorya_dafi)
c_cv <- colnames(emorya_dafi)
emory <- c()
emory <- (emorya_dafi+emoryb_dafi)/2
cv_new <- c()
cv_new <- data.frame(matrix(ncol = 6,nrow = 11))
rownames(cv_new) <- r_cv
colnames(cv_new) <- c_cv
i=1
j=1
for (i in 1:9){
  for (j in 1:6){
    cv_new[i,j]<-sd(c(emory[i,j],lji_dafi[i,j],mtsinai_dafi[i,j])/mean(c(emory[i,j],lji_dafi[i,j],mtsinai_dafi[i,j])))
  }
}


  for (j in 1:6){
    cv_new[10,j]<-sd(c(emory[10,j],lji_dafi[10,j])/mean(c(emory[10,j],lji_dafi[10,j])))
  }


  for (j in 1:6){
    cv_new[11,j]<-sd(c(emory[11,j],lji_dafi[11,j],mtsinai_dafi[11,j])/mean(c(emory[11,j],lji_dafi[11,j],mtsinai_dafi[11,j])))
  }
#####################################################################################################################
#without taking average#
cv_dafi <- data.frame(matrix(ncol = 6,nrow = 11))
r_cv <- row.names(emorya_dafi)
c_cv <- colnames(emorya_dafi)
rownames(cv_dafi) <- r_cv
colnames(cv_dafi) <- c_cv

i=1
j=1
for (i in 1:9) {
  for (j in 1:6){
    cv_dafi[i,j] <- sd(c(emorya_dafi[i,j],lji_dafi[i,j],emoryb_dafi[i,j]))/mean(c(emorya_dafi[i,j],lji_dafi[i,j],emoryb_dafi[i,j]))
  }
}


#####################################################################################################################
##################################
# Manual gating cell proportions #
##################################

lji_manual <- read.xls("/Users/amandava/Desktop/HIPC_IOF_amandava/IOF_new/props_manual/lji_manual.csv",header= TRUE, sep = ",", sheet = 2)
rownames(lji_manual)<-lji_manual[,1]
lji_manual<-lji_manual[,2:12]
lji_manual <- as.data.frame(t(lji_manual))

emorya_manual <- read.xls("/Users/amandava/Desktop/HIPC_IOF_amandava/IOF_new/props_manual/emorya_manual.csv",header= TRUE, sep = ",", sheet = 2)
rownames(emorya_manual)<-emorya_manual[,1]
emorya_manual<-emorya_manual[,2:12]
emorya_manual <- as.data.frame(t(emorya_manual))

emoryb_manual <- read.xls("/Users/amandava/Desktop/HIPC_IOF_amandava/IOF_new/props_manual/emoryb_manual.csv",header= TRUE, sep = ",", sheet = 2)
rownames(emoryb_manual)<-emoryb_manual[,1]
emoryb_manual<-emoryb_manual[,2:12]
emoryb_manual <- as.data.frame(t(emoryb_manual))

mtsinai_manual <- read.xls("/Users/amandava/Desktop/HIPC_IOF_amandava/IOF_new/props_manual/MtSinai_manual.xlsx",header=TRUE, sep = ",", sheet = 2)
rownames(mtsinai_manual)<-mtsinai_manual[,1]
mtsinai_manual<-mtsinai_manual[,2:12]
mtsinai_manual <- as.data.frame(t(mtsinai_manual))

#Taking an average of emory results for manual
emory_m <- c()
emory_m <- (emorya_manual+emoryb_manual)/2
cv_m <- c()
cv_m <- data.frame(matrix(ncol = 6,nrow = 11))
rownames(cv_m) <- r_cv
colnames(cv_m) <- c_cv
i=1
j=1
for (i in 1:9){
  for (j in 1:6){
    cv_m[i,j]<-sd(c(emory_m[i,j],lji_manual[i,j],mtsinai_manual[i,j])/mean(c(emory_m[i,j],lji_manual[i,j],mtsinai_manual[i,j])))
  }
}

for (j in 1:6){
  cv_m[10,j]<-sd(c(emory_m[10,j],lji_manual[10,j])/mean(c(emory_m[10,j],lji_manual[10,j])))
}

for (j in 1:6){
  cv_m[11,j]<-sd(c(emory_m[11,j],lji_manual[11,j],mtsinai_manual[11,j])/mean(c(emory_m[11,j],lji_manual[11,j],mtsinai_manual[11,j])))
}

#Plotting
cv_avg <- data.frame(melt(cv_m),melt(cv_new)[2])
names(cv_avg) <- c("population", "Manual" , "DAFi")
df2_new <- melt(cv_avg)
quartz()
ggplot(data = df2_new)+geom_boxplot(aes(x=population,y=value,fill=variable))+ggtitle("Coefficient of variability")+theme_bw()+theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),text = element_text(size = 12, family = "Tahoma"),
       axis.title = element_text(face="bold"),axis.text.x=element_text(size = 11)) +scale_fill_brewer(palette = "Accent")  

#####################################################################################################################
#without taking average##
cv_manual <- data.frame(matrix(ncol = 6,nrow = 11))
r_cv_m <- row.names(emorya_manual)
c_cv_m <- colnames(emorya_manual)
rownames(cv_manual) <- r_cv_m
colnames(cv_manual) <- c_cv_m

i=1
j=1
for (i in 1:11) {
  for (j in 1:6){
    cv_manual[i,j] <- sd(c(emorya_manual[i,j],lji_manual[i,j],emoryb_manual[i,j]))/mean(c(emorya_manual[i,j],lji_manual[i,j],emoryb_manual[i,j]))
  }
}

cv <- data.frame(melt(cv_dafi),melt(cv_manual)[2])
names(cv) <- c("population", "DAFi" , "Manual")
df2 <- melt(cv)
quartz()
ggplot(data = df2)+geom_boxplot(aes(x=population,y=value,fill=variable))+ggtitle("Coefficient of variability")+theme_bw()+theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),text = element_text(size = 12, family = "Tahoma"),
       axis.title = element_text(face="bold"),axis.text.x=element_text(size = 11)) +scale_fill_brewer(palette = "Accent")  

##############################################################################################################################################

plot(lji_dafi[,1],mtsinai_dafi[,1])
abline(lm(mtsinai_dafi[,1]~lji_dafi[,1]))
lji_man <- read.xls("/Users/amandava/Desktop/HIPC_IOF_Analysis/Compare_centers/Manual_prop/stats_LJI_manual.xlsx")
emory_man <- read.xls("/Users/amandava/Desktop/HIPC_IOF_Analysis/Compare_centers/Manual_prop/stats_pB_manual.xlsx")
mtsinai_man <- read.xls("/Users/amandava/Desktop/HIPC_IOF_Analysis/Compare_centers/Manual_prop/stats_MtSinai_manual.xlsx")
df$x <- lji_dafi[1,]
df$y <- mtsinai_dafi[1,]
ggplot(aes(lji_dafi[,1],emory_dafi[,1]), geom_point(shape=1))

#########################################################################################################################################
cv1 <- data.frame(replicate(6,rnorm(11,mean = 0, sd =1)))
cv2 <- data.frame(replicate(6,rnorm(11,mean = 0, sd =1)))
View(cv2)
View(lji_dafi)
rownames(cv1) <- colnames(lji_dafi)
colnames(cv1) <- rownames(lji_dafi)
rownames(cv2) <- colnames(lji_dafi)
colnames(cv2) <- rownames(lji_dafi)
cv <- data.frame(melt(cv1),melt(cv2)[2])
names(cv) <- c("population", "cv1" , "cv2")
df2 <- melt(cv)
quartz()
ggplot(data = df2)+geom_boxplot(aes(x=population,y=value,fill=variable),alpha=0.7)+ggtitle("Coefficient of variability")+theme_bw()+theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),text = element_text(size = 12, family = "Tahoma"),
axis.title = element_text(face="bold"),axis.text.x=element_text(size = 11)) +scale_fill_brewer(palette = "Accent")  
####################
 

####################
#comparing the proportions across the institutes for DAFi and manual

#Monocytes
mono_dafi <- as.data.frame(cbind(lji_dafi[,1], emoryb_dafi[,1],mtsinai_dafi[,1]),row.names = TRUE,colnames = TRUE)
colnames(mono_dafi)<-c("lji","emory","mtsinai")
mono_manual <- as.data.frame(cbind(lji_manual[,1], emoryb_manual[,1],mtsinai_manual[,1]),row.names = TRUE,colnames = TRUE)
colnames(mono_manual)<-c("lji","emory","mtsinai")
mono_avg <- data.frame(melt(mono_manual),melt(mono_dafi)[2])
names(mono_avg)<-c("Center","Manual","DAFi")
monocytes <- melt(mono_avg)
quartz()
ggplot(data = na.omit(monocytes))+geom_boxplot(aes(x=Center,y=value,fill=variable),alpha=0.7)+ggtitle("Monocytes Population proportion ")+theme_bw()+theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),text = element_text(size = 12, family = "Tahoma"),
                                                                                                                                                          axis.title = element_text(face="bold"),axis.text.x=element_text(size = 11)) +scale_fill_brewer(palette = "Accent") 
#Bcells
bcells_dafi <- as.data.frame(cbind(lji_dafi[,2], emoryb_dafi[,2],mtsinai_dafi[,2]),row.names = TRUE,colnames = TRUE)
colnames(bcells_dafi)<-c("lji","emory","mtsinai")
bcells_manual <- as.data.frame(cbind(lji_manual[,2], emoryb_manual[,2],mtsinai_manual[,2]),row.names = TRUE,colnames = TRUE)
colnames(bcells_manual)<-c("lji","emory","mtsinai")
bcells_avg <- data.frame(melt(bcells_manual),melt(bcells_dafi)[2])
names(bcells_avg)<-c("Center","Manual","DAFi")
bcells <- melt(bcells_avg)
quartz()
ggplot(data = bcells)+geom_boxplot(aes(x=Center,y=value,fill=variable),alpha=0.7)+ggtitle("B-cell Population proportion ")+theme_bw()+theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),text = element_text(size = 12, family = "Tahoma"),
                                                                                                                                                          axis.title = element_text(face="bold"),axis.text.x=element_text(size = 11)) +scale_fill_brewer(palette = "Accent") 

#comparing the NK cell population across the institutes for DAFi and manual
NKcell_dafi <- as.data.frame(cbind(lji_dafi[,3], emory[,3],mtsinai_dafi[,3]),row.names = TRUE,colnames = TRUE)
colnames(NKcell_dafi)<-c("lji","emory","mtsinai")
NKcell_manual <- as.data.frame(cbind(lji_manual[,3], emory_m[,3],mtsinai_manual[,3]),row.names = TRUE,colnames = TRUE)
colnames(NKcell_manual)<-c("lji","emory","mtsinai")
NKcell_avg <- data.frame(melt(NKcell_manual),melt(NKcell_dafi)[2])
names(NKcell_avg)<-c("Center","Manual","DAFi")
NKcell <- melt(NKcell_avg)
quartz()
ggplot(data = na.omit(NKcell))+geom_boxplot(aes(x=Center,y=value,fill=variable),alpha=0.7)+ggtitle("NK cell Population proportion ")+theme_bw()+theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),text = element_text(size = 12, family = "Tahoma"),
                                                                                                                                                      axis.title = element_text(face="bold"),axis.text.x=element_text(size = 11)) +scale_fill_brewer(palette = "Accent") 

#comparing the T cell population across the institutes for DAFi and manual
Tcell_dafi <- as.data.frame(cbind(lji_dafi[,4], emory[,4],mtsinai_dafi[,4]),row.names = TRUE,colnames = TRUE)
colnames(Tcell_dafi)<-c("lji","emory","mtsinai")
Tcell_manual <- as.data.frame(cbind(lji_manual[,4], emory_m[,4],mtsinai_manual[,4]),row.names = TRUE,colnames = TRUE)
colnames(Tcell_manual)<-c("lji","emory","mtsinai")
Tcell_avg <- data.frame(melt(Tcell_manual),melt(Tcell_dafi)[2])
names(Tcell_avg)<-c("Center","Manual","DAFi")
Tcell <- melt(Tcell_avg)
quartz()
ggplot(data = na.omit(Tcell))+geom_boxplot(aes(x=Center,y=value,fill=variable),alpha=0.7)+ggtitle("T cell Population proportion ")+theme_bw()+theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),text = element_text(size = 12, family = "Tahoma"),
                                                                                                                                                    axis.title = element_text(face="bold"),axis.text.x=element_text(size = 11)) +scale_fill_brewer(palette = "Accent") 

#CD4+ T cell
mydata_dafi <- as.data.frame(cbind(lji_dafi[,5], emoryb_dafi[,5],mtsinai_dafi[,5]),row.names = TRUE,colnames = TRUE)
colnames(mydata_dafi)<-c("lji","emory","mtsinai")
mydata_manual <- as.data.frame(cbind(lji_manual[,5], emoryb_manual[,5],mtsinai_manual[,5]),row.names = TRUE,colnames = TRUE)
colnames(mydata_manual)<-c("lji","emory","mtsinai")
mydata_avg <- data.frame(melt(mydata_manual),melt(mydata_dafi)[2])
names(mydata_avg)<-c("Center","Manual","DAFi")
mydata <- melt(mydata_avg)
quartz()
ggplot(data = na.omit(mydata))+geom_boxplot(aes(x=Center,y=value,fill=variable),alpha=0.7)+ggtitle("CD4+ T cell Population proportion ")+theme_bw()+theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),text = element_text(size = 12, family = "Tahoma"),
       axis.title = element_text(face="bold"),axis.text.x=element_text(size = 11)) +scale_fill_brewer(palette = "Accent") 

#comparing the CD8+ T cell population across the institutes for DAFi and manual
T8cell_dafi <- as.data.frame(cbind(lji_dafi[,6], emoryb_dafi[,6],mtsinai_dafi[,6]),row.names = TRUE,colnames = TRUE)
colnames(T8cell_dafi)<-c("lji","emory","mtsinai")
T8cell_manual <- as.data.frame(cbind(lji_manual[,6], emoryb_manual[,6],mtsinai_manual[,6]),row.names = TRUE,colnames = TRUE)
colnames(T8cell_manual)<-c("lji","emory","mtsinai")
T8cell_avg <- data.frame(melt(T8cell_manual),melt(T8cell_dafi)[2])
names(T8cell_avg)<-c("Center","Manual","DAFi")
T8cell <- melt(T8cell_avg)
quartz()
ggplot(data = na.omit(T8cell))+geom_boxplot(aes(x=Center,y=value,fill=variable),alpha=0.7)+ggtitle("CD8+ T cell Population proportion ")+theme_bw()+theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),text = element_text(size = 12, family = "Tahoma"),
                                                                                                                                                     axis.title = element_text(face="bold"),axis.text.x=element_text(size = 11)) +scale_fill_brewer(palette = "Accent") 



################################
#Previous results
################################
lji_pre <- read.csv("/Users/amandava/Desktop/HIPC_IOF_amandava/HIPC_IOF_Analysis/Compare_centers/DAFi_prop/DAFi_LJI_prop.csv",header= TRUE, sep = ",")
rownames(lji_pre)<-lji_pre[,1]
lji_pre<-lji_pre[,2:12]
lji_pre <- as.data.frame(t(lji_pre))

emorya_pre <- read.csv("/Users/amandava/Desktop/HIPC_IOF_amandava/HIPC_IOF_Analysis/Compare_centers/DAFi_prop/DAFi_pA_prop.csv",header= TRUE, sep = ",")
rownames(emorya_pre)<-emorya_pre[,1]
emorya_pre<-emorya_pre[,2:12]
emorya_pre <- as.data.frame(t(emorya_pre))

emoryb_pre <- read.csv("/Users/amandava/Desktop/HIPC_IOF_amandava/HIPC_IOF_Analysis/Compare_centers/DAFi_prop/DAFi_pB_prop.csv",header= TRUE, sep = ",")
rownames(emoryb_pre)<-emoryb_pre[,1]
emoryb_pre<-emoryb_pre[,2:12]
emoryb_pre <- as.data.frame(t(emoryb_pre))

mtsinai_pre <- read.csv("/Users/amandava/Desktop/HIPC_IOF_amandava/HIPC_IOF_Analysis/Compare_centers/DAFi_prop/DAFi_MtSinai_prop.csv",header=TRUE, sep = ",")
rownames(mtsinai_pre)<-mtsinai_pre[,1]
mtsinai_pre<-mtsinai_pre[,2:12]
mtsinai_pre <- as.data.frame(t(mtsinai_pre))

#Taking an average of emory results for manual
emory_p <- c()
emory_p <- (emorya_pre+emoryb_pre)/2
cv_p <- c()
cv_p <- data.frame(matrix(ncol = 6,nrow = 11))
rownames(cv_p) <- r_cv
colnames(cv_p) <- c_cv
i=1
j=1
for (i in 1:9){
  for (j in 1:6){
    cv_p[i,j]<-sd(c(emory_p[i,j],lji_pre[i,j],mtsinai_pre[i,j])/mean(c(emory_p[i,j],lji_pre[i,j],mtsinai_pre[i,j])))
  }
}

for (j in 1:6){
  cv_p[10,j]<-sd(c(emory_p[10,j],lji_pre[10,j])/mean(c(emory_p[10,j],lji_pre[10,j])))
}

for (j in 1:6){
  cv_p[11,j]<-sd(c(emory_p[11,j],lji_pre[11,j],mtsinai_pre[11,j])/mean(c(emory_p[11,j],lji_pre[11,j],mtsinai_pre[11,j])))
}

#Plotting
cv_avg <- data.frame(melt(cv_m),melt(cv_new)[2],melt(cv_p)[2])
names(cv_avg) <- c("population", "Manual" , "DAFi","previous_DAFi")
df2_new <- melt(cv_avg)
quartz()
ggplot(data = df2_new)+geom_boxplot(aes(x=population,y=value,fill=variable))+ggtitle("Coefficient of variability")+theme_bw()+theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),text = element_text(size = 12, family = "Tahoma"),
                                                                                                                                    axis.title = element_text(face="bold"),axis.text.x=element_text(size = 11)) +scale_fill_brewer(palette = "Accent")  
############################################################
# Linear regression analysis between DAFi and Manual gating
############################################################
par(mfrow = c(3,3))
quartz()
for(i in 1:6){
  qplot(lji_dafi[,i],emory[,i])+stat_smooth(method="lm", col="red") +   
    scale_y_continuous(breaks = c(0.2,0.4,0.6,0.8,1.0),minor_breaks = NULL) +
    scale_x_continuous(breaks = c(0.2,0.4,0.6,0.8,1.0),minor_breaks = NULL)
}

lm(lji_dafi$Monocytes~emory$Monocytes)
qplot(lji_dafi$Monocytes,emory$Monocytes)+ stat_smooth(method="lm", col="red")+
  scale_y_continuous(breaks = c(0.2,0.4,0.6,0.8,1.0),minor_breaks = NULL) +
  scale_x_continuous(breaks = c(0.2,0.4,0.6,0.8,1.0),minor_breaks = NULL)

lm(lji_dafi$`NK cells`~emory$`NK cells`)
qplot(lji_dafi$`NK cells`, emory$`NK cells`) + stat_smooth(method="lm", col="red")+
  scale_y_continuous(breaks = c(0.2,0.4,0.6,0.8,1.0),minor_breaks = NULL) +
  scale_x_continuous(breaks = c(0.2,0.4,0.6,0.8,1.0),minor_breaks = NULL)

####Correlation snslysis
quartz(title="LJI")
par(mfrow=c(3,3))
#layout.show(6)
for(i in 1:6){
  my_data <- as.data.frame(cbind(mtsinai_dafi[,5],mtsinai_manual[,5]))
  ggscatter(my_data,x="V1",y="V2", add="reg.line", conf.int = TRUE, 
            cor.coef = TRUE, cor.method = "pearson",xlab="DAFi", ylab="Manual",title="T cells")
}
dev.off()
