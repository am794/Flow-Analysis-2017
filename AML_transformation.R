#-----------------------#
# Linear transformation #
# for FCS 2.0 files     #
# AML data              #
# After FCS Trans       #
#-----------------------#

#Transformation of FCS files
library(ggplot2)
library("flowCore")
setwd("/Volumes/Samsung_T5/AML/7/")
filelst <- list.files("./FCS",full.names = TRUE,recursive = FALSE)
#fcs_raw <- read.flowSet(filelst,transformation = FALSE, truncate_max_range = FALSE)
fcs_raw <- lapply(filelst,read.FCS,transformation=FALSE)
flow_set <- as(fcs_raw,"flowSet")

scatt <- transformList(flow_set@colnames[1:2],scatterTransform())
lgcl <- transformList(flow_set@colnames[3:7],FCSTransTransform())
fcs_scatt <- lapply(fcs_raw,transform,scatt)
fcs_lgcl <- lapply(fcs_scatt,transform,lgcl)

quartz()
ggplot(data=as.data.frame(exprs(fcs_lgcl[[1]])))+geom_point(aes(x=`SS Log`,y=`FS Lin`),size=0.2)+xlim(0,4096)+ylim(0,4096)

quartz()
ggplot(data=as.data.frame(exprs(fcs_lgcl[[1]])))+geom_point(aes(x=`FL3 Log`,y=`FL4 Log`),size=0.2)+xlim(0,4096)+ylim(0,4096)

#Linear transformation of TXT files
min_max_trans <- function(x){
  return((x * (4096)/ (1023)))
}
setwd("/Volumes/Samsung_T5/AML/2/TXT/")
filelst <- list.files("/Volumes/Samsung_T5/AML/2/TXT",".txt",full.names = TRUE,recursive = FALSE)
for(i in 1:length(filelst)){
table <- read.table(filelst[i],header=TRUE)
tab <- as.data.frame(apply(table,2, min_max_trans))
write.table(tab,paste0("/Volumes/Samsung_T5/AML/2/2_Linear_TXT/lrs_",strsplit(filelst[i],"/")[[1]][7]),sep="\t",quote=FALSE,row.names = FALSE)
}
quartz()
ggplot(data=as.data.frame(tab))+geom_point(aes(x=`FSC`,y=`SSC`),size=0.2)+xlim(0,4096)+ylim(0,4096)

quartz()
ggplot(data=as.data.frame(tab))+geom_point(aes(x=`CD45`,y=`CD19`),size=0.2)+xlim(0,4096)+ylim(0,4096)

############################ FUNCTIONS #########################
# set output to 0 when input is less than cutoff value
ipfloor <- function (x, cutoff = 0, target = 0) {
  y = x
  if (x <= cutoff)
    y = target
  y
}
# set output to 0 when input is less than cutoff value
ipceil <- function (x, cutoff = 0, target = 0) {
  y = x
  if (x >= cutoff)
    y = target
  y
}
# immport linear function - convert scatter values to channel output
# linear transformation
ipscatter <-
  function (x,
            range = 4096.0,
            cutoff = 4096,
            channelrange = 262144) {
    y = range * x / channelrange
    y = sapply(y, ipfloor)
    y = sapply(y, ipceil, cutoff = cutoff, target = range)
    y
  }
# immport time function - convert time values to channel output
# linear transformation
iptime <- function (x, channelrange) {
  # use simple cutoff for now
  y = sapply(x, ipfloor)
  y
}
scatterTransform <-
  function(transformationId = "defaultScatterTransform",
           channelrange = 262144,
           range = 4096,
           cutoff = 4096,
           rescale = TRUE) {
    t <- new(
      "transform",
      .Data = function(x) {
        x <- ipscatter(x, channelrange = channelrange)
      }
    )
    t
  }
timeTransform <- function(transformationId = "defaultTimeTransform",
                          channelrange = 262144,
                          range = 4096,
                          rescale = TRUE) {
  t <- new(
    "transform",
    .Data = function(x) {
      x <- iptime(x, channelrange = channelrange)
    }
  )
  t
}

