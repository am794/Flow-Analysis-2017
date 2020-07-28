###########################################
# Arcsinh Transformation Yale CyTOF files #
# Event length - no transformation        #
# DNA1 and DNA2 - linear transformation   #
###########################################
require("flowCore")
library("gdata")
library(ggplot2)
library(colorspace)
library(gridExtra)

setwd("/Users/amandava/Desktop/HIPC_CyTOF/Yale/FCS/")
file_list <- list.files(path = "/Users/amandava/Desktop/HIPC_CyTOF/Yale/FCS/")
fcs_raw <- read.flowSet(file_list,transformation = FALSE, truncate_max_range = FALSE)
#pData(parameters(fcs_raw[[1]]))
cols_keep_trans <- c(3,18,20,21,23,24,25,26,27,29,30,31,32,33,35,36,
                     39,40,41,43,44,46,47,48,49,50,51,52,60)
exp <- asinh((exprs(fcs_raw[[5]])[,cols_keep_trans])/5)
new_exp <- as.data.frame(cbind(exprs(fcs_raw[[5]])[,c(1,2,57,58)],exp))
marker_names <- c("Time","Event_Length","Ir191Di_DNA1", "Ir193Di_DNA2",
                  " Y89Di_CD45","Pr141Di_CD27","Nd142Di_CD19","Nd143Di_CD45RA",
                  "Nd145Di_CD16","Nd146Di_CD8a","Sm147Di_HLA-DR","Nd148Di_CCR4","Sm149Di_CD25",
                  "Eu151Di_CD123","Sm152Di_CD14","Eu153Di_CD69","Sm154Di_CD185(CXCR5)","Gd155Di_CD4",
                  "Gd158Di_CD3","Tb159Di_CD11c","Dy162Di_CD56","Dy163Di_CD183","Dy164Di_CD45RO",
                  "Er166Di_CD24","Er167Di_CD38","Tm169Di_TCRgd","Er170Di_CCR7","Yb171Di_CCR6",
                  "Yb172Di_IgM","Yb173Di_CD57","Yb174Di_CD86","Lu175Di_CD279(PD-1)","Pt195Di_Viability")
colnames(new_exp) <- marker_names
i=33
j=3
#trans <- as.data.frame(exprs(datatransform[[2]]))
quartz()
ggplot(data=as.data.frame(new_exp),aes(x=new_exp[,i],y=new_exp[,j])) + geom_point(size=0.1,alpha=0.2, colour="purple") + 
  xlab(colnames(new_exp)[i])+ylab(colnames(new_exp)[j])+ggtitle(paste0(colnames(new_exp)[i]," vs ",colnames(new_exp)[j]))+ylim(0,400)

live_log <- log10(new_exp$Pt195Di_Viability)
quartz()
ggplot(data=as.data.frame(new_exp),aes(x=live_log,y=new_exp[,j])) + geom_point(size=0.1,alpha=0.2, colour="purple") + 
  xlab(colnames(new_exp)[i])+ylab(colnames(new_exp)[j])+ggtitle(paste0(colnames(new_exp)[i]," vs ",colnames(new_exp)[j]))
##########################################################

logtrans <- logTransform(transformationId="defaultLogTransform", logbase=10, r=1, d=1)
cols_list <- fcs_raw[[1]]@parameters@data$name[60]
trans_list <- transformList(cols_list,logtrans)
datatransform <- transform(fcs_raw[[i]],trans_list)

quartz()
ggplot(data=as.data.frame(new_exp),aes(x=live_log,y=new_exp[,j])) + geom_point(size=0.1,alpha=0.2, colour="purple") + 
  xlab(colnames(new_exp)[i])+ylab(colnames(new_exp)[j])+ggtitle(paste0(colnames(new_exp)[i]," vs ",colnames(new_exp)[j]))

quartz()
ggplot(data=as.data.frame(new_exp),aes(x=new_exp[,4],y=new_exp[,2])) + geom_point(size=0.5,alpha=0.2, colour="purple") + 
  xlab(paste0(colnames(new_exp)[4]))+ylab(paste0(colnames(new_exp)[2]))

quartz()
ggplot(data=as.data.frame(new_exp),aes(x=new_exp[,33],y=new_exp[,17])) + geom_point(size=0.5,alpha=0.2, colour="purple") + 
  xlab("Viabitlity")+ylab("CD3")

quartz()
ggplot(data=as.data.frame(new_exp),aes(x=new_exp[,8],y=new_exp[,16])) + geom_point(size=0.5,alpha=0.2, colour="purple") + 
  xlab("CD8")+ylab("CD4")+ylim(0,1)

a=c(0,0.5,1,2,5,8,10)
for(i in a){
  arc_transform <- arcsinhTransform(transformationId="defaultArcsinhTransform", a=i, b=0.2, c=0)
  cols_keep_trans <- c(3,18,20,21,23,24,25,26,27,29,30,31,32,33,35,36,
                       39,40,41,43,44,46,47,48,49,50,51,52,60)
  cols_list <- fcs_raw[[5]]@parameters@data$name[cols_keep_trans]
  trans_list <- transformList(cols_list,arc_transform)
  datatransform <- transform(fcs_raw[[5]],trans_list)
  new_exp_2 <- as.data.frame(cbind(exprs(fcs_raw[[1]])[,c(1,2,57,58)],exprs(datatransform)[,cols_keep_trans]))
  marker_names <- c("Time","Event_Length","Ir191Di_DNA1", "Ir193Di_DNA2",
                    " Y89Di_CD45","Pr141Di_CD27","Nd142Di_CD19","Nd143Di_CD45RA",
                    "Nd145Di_CD16","Nd146Di_CD8a","Sm147Di_HLA-DR","Nd148Di_CCR4","Sm149Di_CD25",
                    "Eu151Di_CD123","Sm152Di_CD14","Eu153Di_CD69","Sm154Di_CD185(CXCR5)","Gd155Di_CD4",
                    "Gd158Di_CD3","Tb159Di_CD11c","Dy162Di_CD56","Dy163Di_CD183","Dy164Di_CD45RO",
                    "Er166Di_CD24","Er167Di_CD38","Tm169Di_TCRgd","Er170Di_CCR7","Yb171Di_CCR6",
                    "Yb172Di_IgM","Yb173Di_CD57","Yb174Di_CD86","Lu175Di_CD279(PD-1)","Pt195Di_Viability")
  colnames(new_exp_2) <- marker_names
  p=8
  q=27
  quartz()
  ggplot(data=as.data.frame(new_exp_2),aes(x=new_exp_2[,p],y=new_exp_2[,q])) + geom_point(size=0.1,alpha=0.2, colour="purple") + 
    xlab(colnames(new_exp)[p])+ylab(colnames(new_exp)[q])+ggtitle(paste0("a is ",i," and b is 0.2"))
}

cofactor=c(1,5,10,50,100,150)
for(i in cofactor){
  b=(1/cofactor[2])
  #b=4
  arc_transform <- arcsinhTransform(transformationId="defaultArcsinhTransform", a=0, b=(1/cofactor[2]), c=0)
  cols_keep_trans <- c(3,18,20,21,23,24,25,26,27,29,30,31,32,33,35,36,
                       39,40,41,43,44,46,47,48,49,50,51,52,60)
  cols_list <- fcs_raw[[1]]@parameters@data$name[cols_keep_trans]
  trans_list <- transformList(cols_list,arc_transform)
  datatransform <- transform(fcs_raw[[5]],trans_list)
  new_exp_2 <- as.data.frame(cbind(exprs(fcs_raw[[5]])[,c(1,2,57,58)],exprs(datatransform)[,cols_keep_trans]))
  marker_names <- c("Time","Event_Length","Ir191Di_DNA1", "Ir193Di_DNA2",
                    " Y89Di_CD45","Pr141Di_CD27","Nd142Di_CD19","Nd143Di_CD45RA",
                    "Nd145Di_CD16","Nd146Di_CD8a","Sm147Di_HLA-DR","Nd148Di_CCR4","Sm149Di_CD25",
                    "Eu151Di_CD123","Sm152Di_CD14","Eu153Di_CD69","Sm154Di_CD185(CXCR5)","Gd155Di_CD4",
                    "Gd158Di_CD3","Tb159Di_CD11c","Dy162Di_CD56","Dy163Di_CD183","Dy164Di_CD45RO",
                    "Er166Di_CD24","Er167Di_CD38","Tm169Di_TCRgd","Er170Di_CCR7","Yb171Di_CCR6",
                    "Yb172Di_IgM","Yb173Di_CD57","Yb174Di_CD86","Lu175Di_CD279(PD-1)","Pt195Di_Viability")
  colnames(new_exp_2) <- marker_names
  p=10
  q=18
  quartz()
  ggplot(data=as.data.frame(new_exp_2),aes(x=new_exp_2[,p],y=new_exp_2[,q])) + geom_point(size=0.1,alpha=0.2, colour="purple") + 
    xlab(colnames(new_exp_2)[p])+ylab(colnames(new_exp_2)[q])+ggtitle(paste0("a is 0  and b is ", b))
}

