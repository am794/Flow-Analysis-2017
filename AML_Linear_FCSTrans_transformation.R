#-----------------------#
# Linear transformation #
# for FCS 2.0 files     #
# AML data              #
# After FCS Trans       #
#-----------------------#

library("flowCore")
setwd("/Volumes/Samsung_T5/AML/6/")
filelst <- list.files("./FCS",full.names = TRUE,recursive = FALSE)
fcs_raw <- read.flowSet(filelst,transformation = FALSE, truncate_max_range = FALSE)



