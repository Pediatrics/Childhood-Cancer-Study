rm(list=ls()) # clear environment
cat("\014")   # clear console
setwd("~/OneDrive - uni-duisburg-essen.de/Mastercode")

####################### Master Complete Code #########################  
Version <- "Master Descriptive Statistics"

options("scipen"=999, "digits"=3) # normale Zahlen


# Load libraries
library (spdep) # for adjacency matrix
library(openxlsx)
library (data.table) # merge lists
require (tidyverse)
require(gplots)
library (pastecs) # for descriptive statistics


Datum <- Sys.Date()
start_time <- Sys.time()

######################################
# get population data Table 12411-0018 from www-genesis.destatis.de - with population etc.  
# defined clusters also added in the xlsx file
# Cluster TAbelle: Stichtag 31.12.2016
ClusterID = read.xlsx("Data/Cluster.xlsx")  


ClusterID$Mbis3 <- as.numeric (ClusterID$Mbis3)
ClusterID [,3] <- as.numeric (ClusterID [,3] )
for (i in 1:10) 
  {
  ClusterID [,i] <- as.numeric (ClusterID [,i] )
  }

DesStat <- c (ClusterID$Fläche, ClusterID$Population)

#ClusterID [,c(3,4)]
#ClusterID [,c(3,4)]<- as.numeric (ClusterID [,c(3, 4)]) # geht nciht 



DesStat <- as.data.frame( sapply(ClusterID, mean , na.rm=TRUE))
format(DesStat, digits=0, nsmall=1)


Descript.Statistics<- as.data.table (stat.desc(ClusterID[,c(11,12,13,14,15,32,36,38,39,40)]), p =0.95 )

wb <- createWorkbook()
addWorksheet(wb,"Descript.Statistics")
writeDataTable( wb, "Descript.Statistics", x = Descript.Statistics, startCol = 1, startRow = 1)
saveWorkbook(wb, "Data/Descript.Statistics.xlsx", overwrite = T)


mean (ClusterID$Anzahl.Fälle1)
sd (ClusterID$Anzahl.Fälle1)
options(digits=8)
Descript.Statistics2 <- as.data.frame( summary (ClusterID[,c(11,12,13,14,15,32,36,38,39,40)]))
wb <- createWorkbook()
addWorksheet(wb,"Descript.Statistics2")
writeDataTable( wb, "Descript.Statistics2", x = Descript.Statistics2, startCol = 1, startRow = 1)
saveWorkbook(wb, "Data/Descript.Statistics2.xlsx", overwrite = T)


#mean(ClusterID$Anzahl.Fälle1)-(sd(ClusterID$Anzahl.Fälle1)/sqrt(402))
#mean(ClusterID$Anzahl.Fälle1)+(sd(ClusterID$Anzahl.Fälle1)/sqrt(402))
#mean(ClusterID$Anzahl.Fälle1)-(4.68/2)
