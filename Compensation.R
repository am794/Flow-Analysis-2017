######################################
# Compensation for BCH-UBC FCS files #
######################################
library("devtools")
devtools::install_github("ParkerICI/premessa")
library("premessa")
require("flowCore")
require("flowDensity")
library("openxlsx")
source("/Users/amandava/Desktop/amandava/FCSTransP3.0.R")

# FCS files
setwd("/Users/amandava/Desktop/HIPC_Flow/BCH-UBC/")
BCH_files <- list.files("/Users/amandava/Desktop/HIPC_Flow/BCH-UBC/FCS", full.names=T,recursive=FALSE)
compensation_matrix <- as.matrix(read.xlsx("/Users/amandava/Desktop/HIPC_Flow/BCH-UBC/Comp_Matrix_1.xlsx",
                                    rowNames = TRUE,colNames = TRUE))
comp_inverse_matrix <- solve(compensation_matrix/100)
flow_files <- lapply(BCH_files, read.FCS,transformation=FALSE,truncate_max_range = FALSE)
flow_set <- as(flow_files,"flowSet")
#p <- read.FCS(BCH_files[[1]],transformation = TRUE,truncate_max_range = FALSE)

#Compensation by multiplying the inverse of compensaiton matrix to the flow files
#Compensation function(below) gives better results
comp_frame <- list(list())
for(i in 1:10){
fcs <- exprs(flow_files[[i]])
compensation <- exprs(flow_files[[i]])[,7:15] %*% comp_inverse_matrix
compensated <- cbind(fcs[,1:6],compensation,fcs[,16])
colnames(compensated)[16] <- "Time"
comp_frame[i] <- as_flowFrame(compensated)
#scatt <- transformList(colnames(compensated)[1:6],scatterTransform())
#lgcl <- transformList(colnames(compensated)[7:15],FCSTransTransform())
#timetrans <- transformList(colnames(compensated)[16],timeTransform())
#fcs_scatt[i] <- lapply(comp_frame[i],transform,scatt)
#fcs_lgcl[i] <- lapply(fcs_scatt[i],transform,lgcl)
#fcs_transform[i] <- lapply(fcs_lgcl[i],transform,timetrans)
}
comp_flow <- as(comp_frame,"flowSet")


#Transformation
scatt <- transformList(comp_flow@colnames[1:6],scatterTransform())
lgcl <- transformList(comp_flow@colnames[7:15],FCSTransTransform())
#lgcl_scatt <- transformList(comp_flow@colnames[7:15],linearTransform())
timetrans <- transformList(comp_flow@colnames[16],timeTransform())
fcs_scatt <- lapply(comp_frame,transform,scatt)
fcs_lgcl <- lapply(fcs_scatt,transform,lgcl)
#fcs_lgcl_scatt <- lapply(fcs_lgcl,transform, lgcl_scatt)
fcs_transform <- lapply(fcs_lgcl,transform,timetrans)
#fcs_transform_2 <- lapply(fcs_lgcl_scatt,transform,timetrans)
cofactor <- 500
cbind(exprs(fcs_transform[[1]])[,1:6],asinh(exprs(fcs_transform[[1]])[,7:15]/cofactor))

expr_1 <- exprs(fcs_transform[[6]])
#expr_1 <- exprs(comp_flow[[1]])
marker_names <- c("FSC-A","FSC-H","FSC-W","SSC-A","SSC-H","SSC-W","CD11c","Viability","CD16","HLA-DR","CD56","CD3","CD123","CD14","gd","Time")
colnames(expr_1) <- marker_names

quartz()
ggplot(data=as.data.frame(expr_1),aes(x=expr_1[,10],y=expr_1[,14])) + geom_point(size=0.3,alpha=0.2, colour="purple") +
  xlab("HLA-DR")+ylab("CD14")+xlim(0,4096)+ylim(0,4096)

###############################################
# Compensation function from flowCore package #
###############################################
compOut <- list(list())
comp_Out <- list(list())
compout_frame <- list(list())
##################################################
complist <- colnames(compensation_matrix)
comp <- compensation(compensation_matrix/100,compensationID="defaultCompensation")
comp_par <- compensatedParameter(complist,"comp",searchEnv = .GlobalEnv)
for(i in 1:10){
fcs <- exprs(flow_files[[i]])
compOut[[i]] <- eval(comp_par)(fcs)
comp_Out[[i]] <- cbind(fcs[,1:6],compOut[[i]]),fcs[,16])
colnames(comp_Out[[i]])[16] <- "Time"
compout_frame[[i]] <- as_flowFrame(as.matrix(comp_Out[[i]]))
}
compOut_flow <- as(compout_frame,"flowSet")
scatt_fc <- transformList(compOut_flow@colnames[1:6],scatterTransform())
lgcl_fc <- transformList(compOut_flow@colnames[7:15],FCSTransTransform())
lgcl_scatt_fc <- transformList(compOut_flow@colnames[7:15],linearTransform())
timetrans_fc <- transformList(compOut_flow@colnames[16],timeTransform())

fcs_scatt_fc <- lapply(compout_frame,transform,scatt_fc)
fcs_lgcl_fc <- lapply(fcs_scatt_fc,transform,lgcl_fc)
#fcs_lgcl_scatt <- lapply(fcs_lgcl,transform, lgcl_scatt)
fcs_transform_fc <- lapply(fcs_lgcl_fc,transform,timetrans)
#fcs_transform_2 <- lapply(fcs_lgcl_scatt,transform,timetrans)

for(i in 1:10){
  expr_1_fc <- exprs(fcs_transform_fc[[i]])
  marker_names <- c("FSC-A","FSC-H","FSC-W","SSC-A","SSC-H","SSC-W","CD11c","Viability","CD16","HLA-DR","CD56","CD3","CD123","CD14","gd","Time")
  colnames(expr_1_fc) <- marker_names
  write.table(expr_1_fc[,1:15],file = paste0("Preprocessed_00",i,".txt"),quote=FALSE,sep = "\t",col.names=TRUE,row.names=FALSE)
}
list_exprs <- lapply(fcs_transform_fc, exprs)
expr_1_fc <- exprs(fcs_transform_fc[[1]])
marker_names <- c("FSC-A","FSC-H","FSC-W","SSC-A","SSC-H","SSC-W","CD11c","Viability","CD16","HLA-DR","CD56","CD3","CD123","CD14","gd","Time")
colnames(expr_1_fc) <- marker_names

quartz()
ggplot(data=as.data.frame(expr_1_fc),aes(x=expr_1_fc[,10],y=expr_1_fc[,14])) + geom_point(size=0.3,alpha=0.2, colour="purple") +
  xlab(colnames(expr_1_fc)[10])+ylab(colnames(expr_1_fc)[14])


compensated_frame <- compensate(flow_set,compensation_matrix,com)
                                c("APC-A","APC-eF780-A","BV605-A","BV650-A","FITC-A","PE-A","PE-CF594-A","PE-Cy7-A","V500-A" ))
com <- transformList(comp_flow@colnames[7:15],compensate)
