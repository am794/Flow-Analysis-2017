#-----------------------#
# Linear transformation #
# for FCS 2.0 files     #
# AML data              #
# After FCS Trans       #
#-----------------------#

library("flowCore")
setwd("/Volumes/Samsung_T5/AML/6/")
filelst <- list.files("./FCS",full.names = TRUE,recursive = FALSE)
#fcs_raw <- read.flowSet(filelst,transformation = FALSE, truncate_max_range = FALSE)
fcs_raw <- lapply(filelst,read.FCS,transformation=FALSE)
flow_set <- as(fcs_raw,"flowSet")

linearTrans <- linearTransform(transformationId="Linear-transformation", a=2, b=0)
scatt <- transformList(flow_set@colnames[1:2],scatterTransform())
lgcl <- transformList(flow_set@colnames[3:7],FCSTransTransform())
fcs_lgcl <- lapply(fcs_raw,transform,lgcl)
fcs_scatt <- lapply(fcs_lgcl,transform,scatt)

quartz()
ggplot(data=as.data.frame(exprs(fcs_lgcl[[1]])))+geom_point(aes(x=`SS Log`,y=`FS Lin`),size=0.2)+xlim(0,4096)+ylim(0,4096)


min_max_trans <- function(x){
  return((x-min(x)) / (max(x)-min(x)))
}

tab <- as.data.frame(lapply(exprs(fcs_lgcl[[1]]), min_max_trans))
quartz()
ggplot(data=as.data.frame(tab))+geom_point(aes(x=`SS Log`,y=`FS Lin`),size=0.2)+xlim(0,4096)+ylim(0,4096)
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

