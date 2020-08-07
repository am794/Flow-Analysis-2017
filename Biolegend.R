###################
# BioLegend files #
###################

raw_fcs_path <- "/Volumes/Samsung_T5/BioLegend/datashare_JCVI_Updated/"
files <- list.files(raw_fcs_path,pattern=".fcs")
library(flowCore)
source("/Users/amandava/Desktop/amandava/FCS_trans/FCSTrans_July19.R")

exp2 <- c(c())
#exp_aria <- c(c())
for(i in 1:length(files)){
fcs_header <- read.FCS(paste0("/Volumes/Samsung_T5/BioLegend/datashare_JCVI_Updated/",files[i]),transformation = FALSE)
#ff <- convertfcs(fcs_header)
exp <- exprs(fcs_header)
#exp <-  cbind(exp,rep(strsplit(files,"_")[[i]][2],dim(exp)[1]),rep(strsplit(files,"_")[[i]][3],dim(exp)[1]),stringsAsFactors = FALSE)
#exp <- rep(strsplit(files,"_")[[i]][3],dim(exp)[2])
#colnames(exp)[c(203,204)] <- c("titration","treatment")
exp2[[i]] <- apply(exp,2,function(x) {(x-min(x))/(max(x)-min(x))*4095})
exp2[[i]] <-  data.frame(cbind(exp2[[i]],rep(strsplit(files[i],"_")[2],dim(exp2[[i]])[1]),rep(strsplit(files[i],"_")[3],dim(exp2[[i]])[1])))
colnames(exp2[[i]])[c(203,204)] <- c("Titration","Treatment")
quartz()
plot(exp2[[i]][,125],exp2[[i]][,151],cex=0.1,col="blue",xlab="CD4",ylab="CD8")
write.table(as.matrix(exp2[[i]]),paste0("/Volumes/Samsung_T5/BioLegend/TXT_Updated/TXT_2/",files[i]),sep = "\t",quote=FALSE,row.names = FALSE)
}

library(dplyr)
bind <- bind_rows(exp2)
write.xlsx(bind,"/Volumes/Samsung_T5/BioLegend/TXT_Updated/Merged_all.xlsx")
write.table(bind,"/Volumes/Samsung_T5/BioLegend/TXT_Updated/Merged_all.txt",sep="\t",quote=FALSE,row.names=FALSE)
cnames <- colnames(exp2[[1]])
write.table(cnames,"/Volumes/Samsung_T5/BioLegend/TXT_Updated/TXT/marker_names.txt",quote = FALSE, row.names = FALSE)
