#############HVP UBC Sept 20###############
hvp <- read.xlsx("/Users/amandava/Desktop/UBC_Aug/12thSept/mDC_FLOCK_phenotype_percentages_19Sept2018.xlsx",sheet=2,rowNames = TRUE)[-1,]
events <- read.xlsx("/Users/amandava/Desktop/UBC_Aug/12thSept/mDC_FLOCK_phenotype_percentages_19Sept2018.xlsx",sheet=3,rowNames = TRUE)
data <- c()
for(i in 1:nrow(hvp)){
  func <- function(x){((x*events[i,1])/events[i,2])}
  dat <- func(hvp[i,])
  data <- rbind(data,as.data.frame(dat))
}

write.xlsx(data,file="/Users/amandava/Desktop/UBC_Aug/12thSept/mDC_FLOCK_phenotype_percentages_19Sept2018.xlsx",sheetName="Final_percentages",append=TRUE,rownames=TRUE)
write.xlsx(hvp,file="/Users/amandava/Desktop/UBC_Aug/12thSept/mDC_FLOCK_percentages.xlsx",sheetName="Percentages",append=TRUE,rownames=TRUE)
write.xlsx(events,file="/Users/amandava/Desktop/UBC_Aug/12thSept/mDC_events.xlsx",sheetName="Final_percentages",append=TRUE,rownames=TRUE)



