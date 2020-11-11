######################################################################
########## Source Code Childhood Cancer Cluster Simulation  ##########
########## "November 2020" ###########################################
######################################################################
rm(list=ls()) # clear environment
cat("\014")   # clear console
# set user specific wd 
setwd("~/OneDrive - uni-duisburg-essen.de/Mastercode")
# files needed (in directory /Mastercode/data) 
# gadm36_DEU_2.cpg; gadm36_DEU_2.dbf; gadm36_DEU_2.prj; gadm36_DEU_2.shp; gadm36_DEU_2.shx
# cluster.xlsx 
Version <- "November 2020"

# Load libraries
library('raster')
library('geosphere')
library('mapview') # visualization
library (spdep) # for adjacency matrix
library(openxlsx) # to save excel files
library (SpatialEpi) # Spatial Epi functions
library (sp) # Spatial Epi misc
library (data.table) # merge lists
library(INLA) # for INLA models
library (maptools)  # visualizaton
library(rgdal) # für readOGR
require(dataAnalysisMisc)# by Maximilian Knoll 
# to get an update of the above library: 
#if (!"devtools" %in% installed.packages()) { install.packages("devtools") }
#devtools::install_github("mknoll/dataAnalysisMisc", force=T) #force=T 
require (tidyverse) # tidyerse
require(gplots) # gplots

Datum <- Sys.Date()
start_time <- Sys.time()
set.seed (999999999)

##################################################################
#Presets for all methods
##################################################################
Method <- "BN" # "BN" , "Kulldorff", "INLA"
simulations <- 20# 
k <-   5 # or e.g. 50/ALL 5/Wilms  for Besag Newell
alpha.level <- 0.05#  0.01/ Besag, 0.05/ Kullforf  (NA for INLA)
pop.upper.bound <- 0.05 # NA or 0.1 for Kulldorf
n.simulations <- 99 # NA or 99  for Kulldorf
plot <- F # Plot Histograms of Monte Carlo Distribution of Lambda (Kulldorff only )
ALLorWILMS <- "WILMS"
lambda = 14/100000 # 14/100000 für all neoplasms, 0.7/100000 for Wilms Tumors  (15.99/0.86 for real data )
# relative risk in risk clusters
# ClusterRR <- c(1,1.1,1.2, 1.3,1.4, 1.5, 2, 5,10,100)
ClusterRR <- c(1,2) # for shorter Test runs
# ClusterRR <- c(seq(1,50, length=10)) # for display "xxx over RR"
# aggregation period (years)
aggregate <- 10

###### which kind of risk-clusters is being formed (random size/shape or defined)
RANDOMorDEFINED <- "random" # type "random" for random clusters "defined" for defined clusters. 
### For defined clusters: Clustertype must set as seen two lines below. c("Cluster1".......). The defined clusters are found in the excel table in the respective cluster´s colum (0= normal risk polygon /1 0 high risk polygon)
### For random clusters: Clustertype <- c(1,2,3,...) (n=number of combined polygons in one high risk cluster)
#Clustertype <-c ("Cluster1", "Cluster2")  ### short for test
#Clustertype <-c ("Cluster1", "Cluster2", "Cluster3","Cluster4","Cluster5", "Cluster6", "Cluster7")  # COMPLETE SIMULATION ALL CLUSTERS
Clustertype <-c(5)  ## short for random test run
#Clustertype <-c(1,2,5,10,20,50) # complete set of  randomly sized/shaped clusters

clustervector <- c (1:402) # needed several times (number of polygons)
CRR <- length (ClusterRR) # for Loop over Relative Risiken
CLT <- length (Clustertype) # for Loop over Random Clusters 

##################################################################
# OPERATING Characteristics for Performance     ##################
##################################################################

sensitivity = 0 
specificity =  0
PPV = 0 # positive predictive Value 
EP =0 # exact power
MP = 0 # minimum Power
NPV = 0 # Negative Predictive VAlue 
CC =0  # mean correct classification 
PRVC = 0 # Rate of true Positives of all detected clusters
FPR = 0 # False Positive Rate
FNR = 0 # False Negative Rate 
RRSimGes = 0  # simulated relative risk (main outcome of simulation) for Monte Carlo Error 
RRTrueGes = 0 # Expected overall RR over 402 Districts
RRBIAS =0 # für RR bias
MCE = 0 # for Monte Carlo Error
Performance <- vector(mode = "list",length = CRR)
PerformanceComplete <- vector (mode = "list",length = CLT)
# zum testen, ob die Risk und Randomvektoren übereinstimmen (kann hinterher raus)
list_of_riskvectors   <- list() # from random/defined list
list_of_riskvectorsCR <- list()
list_of_collvectors   <- list() # from Random "generator"
list_of_collvectorsCR <- list()
list_of_RandomCluster <- list() # # The random Generator may pull ´donut shaped polygons´ the polygon inside the donut is then added into the cluster
list_of_RandomClusterQ <- list()  
list_of_RandomClusterCR <- list() # Cumulated, nested, complete List for all simulated scenarios (Risk Level and Clustertype )
list_of_clustervectors <- list() # detected clusters 
list_of_clustervectorsCR <- list()
list_of_SimCases <- list() # # Simulated Cases in 402 Districts of each scenario
list_of_SimCasesQ <- list() # # Simulated Cases in 402 Districts of each Risk Level  scenario
list_of_SimCasesCR <- list() # Cumulated, nested, complete List for all simulated scenarios (Risk Level and Clustertype), corresponds to list_of_RandomClusterCR
DLRPOS <- 0 # Positive Diagnostic Likelihood Ratio
DLRNEG <- 0 # Negative Diagnostic Likelihood Ratio


######################################################################
#Get SpatialPolygonsDataFrame
polygons <-shapefile("Data/gadm36_DEU_2.shp") #Polygons of districts. GADM version 3.6. 6th May 2018
# mapview (polygons)  
# omit Lake Constance ("large water body" )
polygons <- polygons[!is.na(polygons$CC_2), ] 

######################################################################
# get population data Table 12411-0018 from www-genesis.destatis.de - with population etc.  
# defined clusters also added in the xlsx file
# Cluster TAbelle: Stichtag 31.12.2016
ClusterID = read.xlsx("Data/Cluster.xlsx")  

######################################################################
# Merge population and geographic data  (left join from tidyverse!! (merge gets the wrong ID order!!!!))  
polygons@data <- polygons@data %>%  left_join(ClusterID,     by ="CC_2") %>% mutate(ID = 1:402)
# delete unwanted columns
polygons@data[c("GID_0","NAME_0","GID_1","NAME_1" ,"NL_NAME_1","ENGTYPE_2" , "HASC_2","VARNAME_2","NL_NAME_2","TYPE_2", "ID.ab.Flensburg", "ID.ab.Alb.Donau", "Case","Mbis3" ,"Mbis6","Mbis10","Mbis15","Fbis3", "Fbis6", "Fbis10", "Fbis15")] <- NULL

### calculate Expected Cases per District
polygons@data$Erwartet <- polygons@data$Population*lambda*aggregate 


######################################################################
# Get polygons centroids
centroids <- as.data.frame(centroid(polygons))
colnames(centroids) <- c("lon", "lat")
centroids <- data.frame("ID" = 1:nrow(centroids), centroids)

# Create SpatialPointsDataFrame object
coordinates(centroids) <- c("lon", "lat")
proj4string(centroids) <- proj4string(polygons) # assign projection

# Adjacency Matrix 
adjacency <- (poly2nb (polygons, row.names = polygons@data$CC_2))

# Adjacency - Plot (complete Matrix) 
# plot (polygons, col='gray', border ='blue', lwd=1)
# plot (adjacency, centroids@coords, col='red', lwd=1, add=T)

# Transform into spatial weight matrix
adjacencyweight <- nb2mat (adjacency, style='B')

# Adjacency matrix
adj <- adjacencyweight

# Plot Polygons + Centroids
geo2 <- centroids@coords[,1:2]
geo2 <- latlong2grid(geo2)

# Adjaceny from Blangiardo Book page 181:
adjacencyweight <- as (adjacencyweight, "dgTMatrix")
nb2INLA ("adjacency.graph", adjacency )
adjacency.adj <- paste (getwd (), "/adjacency.graph", sep="")

# Plot Adjacency Matrix 
# adjmatrix <- inla.read.graph(filename="adjacency.graph")
# image (inla.graph2matrix(adjmatrix), xlab="", ylab="")


######################################################################
### loops start here 
######################################################################

########## Loop for Cluster-Type    #################################
for (cr in 1: CLT)
{
########## Loop for RR-Risk Levels  #################################
for(q in 1:CRR)
{
# Number of simulations per RR-Level
set.seed(888888888) # set this seed for various "CAS10" if only 1 x drawn
for (m in 1:simulations){ 

## Statement , not random: = defined --> 2. Statement 
if (RANDOMorDEFINED == "random") {
######################################################################
  #### Start of RANDOM Cluster Formation     #########################
######################################################################

seed <- sample(1:999999999, 1)

# Draw Risk-Cluster of connected Polygons (star=1: all Polygons around the first one (does not work with larger than 8), type=1: Exact numnber of polygons is drawn ("Clustertype")
mRev <- sampleAdj(adj,  Clustertype [cr], n=1, maxTry=10000, star=0, type=1, seed=seed)

#### find ids of clusters
nCl <- unique(names(table(mRev)))
nCl <- nCl[-which(nCl == 0)]
coll <-list()
for (i in nCl) {
  #ids <- unique(unlist(apply(m, 1, function(x) which(x == i))))
  ids <- apply(mRev, 1, function(x) any(x == i))
  ids2 <- apply(mRev, 2, function(x) any(x == i))
  
  if (length(ids) > 0 || length(ids2) > 0) {
    coll[[length(coll)+1]] <- unique(c(which(ids), which(ids2)))
  }
}
### ID´s of Polygons
#coll
### Number of Polygons
#lapply(coll, function(x) length(x))

#############################
### plot adjmatrix
# image(mRev, col=c("white", 1:10))

#### plot polygons
# plot (polygons, col='white', border ='lightgray', lwd=1)

#nPoly <- 1 ## number of clusters of districts per plot  
#col <- rainbow(nPoly)
#for (j in 1:nPoly) {
#    adjNew <- adjacency
#    for (i in 1:length(adjacency)) {
#      adjNew[[i]]<- integer(i) #adjNew[[i]][1]
#      w <- which(mRev[i,] == j)
#      if (length(w) > 0) {
#        print(paste("Cluster", j, length(w)))
#        adjNew[[i]]<- as.integer(w)
#     }
#    }
#  plot (adjNew, centroids@coords, col=col[j], lwd=2, add=T) #hier wirde das Randomcluster gezeigt
#mapview(polygons) + mapview(polygons [as.vector (unlist (coll)), ],  col.regions=3)

# }

# List of generated Risk-Vectors from Random Generator 
list_of_collvectors [m] <- coll

#include random Clusters into the list "cluster" and reintroduce "cluster" into the "common trunk of  code"
randomcluster <- as.vector (unlist (coll))

######################################################################
##### exclude "donut-polygons" by filling in the polygon island into the donut  
if(is.element(157, randomcluster)){randomcluster <- c(randomcluster,144)} # Insel (144 einbauen, wenn 157 (Donut) gezogen wird)
if(is.element(402, randomcluster)){randomcluster <- c(randomcluster,401)}
if(is.element(361, randomcluster)){randomcluster <- c(randomcluster,356)}
if(is.element(84, randomcluster)){randomcluster <- c(randomcluster,85)}
if(is.element(63, randomcluster)){randomcluster <- c(randomcluster,64)}
if(is.element(127, randomcluster)){randomcluster <- c(randomcluster,128)}
if(is.element(57, randomcluster)){randomcluster <- c(randomcluster,58)}
if(is.element(59, randomcluster)){randomcluster <- c(randomcluster,60)}
if(is.element(140, randomcluster)){randomcluster <- c(randomcluster,135)}
if(is.element(49, randomcluster)){randomcluster <- c(randomcluster,50)}
if(is.element(48, randomcluster)){randomcluster <- c(randomcluster,47)}
if(is.element(109, randomcluster)){randomcluster <- c(randomcluster,136)}
if(is.element(118, randomcluster)){randomcluster <- c(randomcluster,119)}
if(is.element(131, randomcluster)){randomcluster <- c(randomcluster,130)}
if(is.element(94, randomcluster)){randomcluster <- c(randomcluster,95)}
if(is.element(114, randomcluster)){randomcluster <- c(randomcluster,115)}
if(is.element(121, randomcluster)){randomcluster <- c(randomcluster,122)}
if(is.element(113, randomcluster)){randomcluster <- c(randomcluster,87)}
if(is.element(112, randomcluster)){randomcluster <- c(randomcluster,89)}
if(is.element(30, randomcluster)){randomcluster <- c(randomcluster,2)}
if(is.element(16, randomcluster)){randomcluster <- c(randomcluster,17)}
if(is.element(325, randomcluster)){randomcluster <- c(randomcluster,319)}
if(is.element(327, randomcluster)){randomcluster <- c(randomcluster,326)}
if(is.element(376, randomcluster)){randomcluster <- c(randomcluster,366)}
if(is.element(192, randomcluster)){randomcluster <- c(randomcluster,193)}

randomcluster <- match (clustervector,randomcluster, nomatch =0, incomparables = 0)
randomcluster <- replace(randomcluster, randomcluster>0, 1)
randomcluster <- as.data.frame(randomcluster)

### generate the list of randomclusters (including "filled donuts")
list_of_RandomCluster[m] <-randomcluster 

randomcluster <- data.frame("ID" = 1:nrow(randomcluster), randomcluster)
polygons@data <- polygons@data %>%  left_join(randomcluster,     by ="ID") 

####### Defined Clusters into the mastertable 
polygons$Cluster <- polygons$randomcluster

####### end of "Random Cluster Generator"
} else {

####### Defined Clusters into the mastertable 
polygons$Cluster <- polygons[[Clustertype [cr]]]
} 


#############################################################  
##### COMMON TRUNK (random AND defined Clusters) ############
#############################################################


# calculate Relative Risk for each Polygon
polygons@data$RR <- ifelse (polygons$Cluster== 0, 1,ClusterRR [q])  # hier eigentlich ClusterRR [i]) -..
polygons@data$RR
#Calculate Cases as defined by relative risk
CAS10 =0
RRSim<- 0 # for calcluation of Monte Carlo Error


polygons@data$Cases<- CAS10

###########################################################################
# Calculate Cases and Relative Risk over a time frame of xx aggregated years
##########################################################################
for  (xx in 1:aggregate) {
SimCases <- rpois(n = 1:nrow (polygons@data), lambda = lambda *(polygons@data$Population)*polygons@data$RR) 
CAS10 <- CAS10 + SimCases
RRSim<-RRSim+ SimCases /polygons@data$Population*lambda 
}
#sum(CAS10)
# Cases into the master table 
polygons@data$Cases<- CAS10
polygons@data$Cases

#polygons@data$RRSIM <- RRSim # shows simulated RR per District 
polygons$RRSIM <- polygons$Cases/(polygons$Erwartet)


## Get aggregated counts of population and cases for each county
population <- tapply(polygons@data$Population,polygons@data$ID,sum)
cases <- tapply(polygons@data$Cases,polygons@data$ID,sum)

sum (cases)      
sum (population)

list_of_SimCases [m] <- as.data.frame (CAS10)


if (  Method == "BN" ) {

#####################################
## Besag-Newell Model Calculations ##
##################################### 

# calculate expected cases (SpatialEpi function)
expected.cases <- expected(polygons@data$Population, polygons@data$Cases, n.strata = 1) 

# not controlling for stratas
results <- besag_newell(geo2, population, cases, expected.cases=NULL, k, alpha.level)

#get Cluster IDs from list
cluster <- sapply(results$clusters,"[" , "location.IDs.included")
cluster<- unlist(cluster, recursive = TRUE, use.names = TRUE)
# all polygons only 1 x 
cluster <- unique (cluster)} # end condition Besag Newell 

else if (  Method == "Kulldorff" ) {

###################################
## Kulldorff Model Calculations  ##
###################################   
# calculate expected cases (SpatialEpi function)
expected.cases <- expected(polygons@data$Population, polygons@data$Cases, n.strata = 1) 

## Kulldorff using Poisson likelihoods
poisson <- kulldorff(geo2, cases, population, expected.cases , pop.upper.bound, n.simulations, alpha.level, plot)
cluster <- poisson$most.likely.cluster$location.IDs.included
poisson$secondary.clusters
x <- sapply(poisson$secondary.clusters,"[" , "location.IDs.included")
y<- unlist(x, recursive = TRUE, use.names = TRUE)
# reduce list. remove duplicates
cluster <- unique (c(y, cluster) )

}   # end condition Kulldorff    

else if (  Method == "INLA" ) {

###############################
## INLA Model Calculations   ##
###############################

# Calculate expected cases for INLA Model
polygons@data$Erwartet <- polygons@data$Population*lambda*aggregate 
formula <- Cases ~  f (ID, model="bym", graph=adjacency.adj, scale.model = TRUE, hyper=list (prec.unstruct=list (prior="loggamma", param=c(1,0.001)), prec.spatial=list(prior="loggamma", param=c(1,0.001)))) #+ f(IID,model="iid")
INLACluster<- inla (formula,family ="poisson", data=polygons@data, E=Erwartet, control.compute = list (dic=TRUE))
# Get 95% Credibility-Invervall > 1 
INLACluster <- INLACluster$summary.fitted.values # RR Risk Estimates Posteriors
INLACluster$ID <- 1:nrow(INLACluster) # hier same ID´s as districts
INLACluster <- INLACluster[INLACluster$`0.025quant`>1,] # get only districts with credibility intervals > 97.5 perc)
cluster  <- sapply(INLACluster$ID,"[")
cluster<- unlist(cluster, recursive = TRUE, use.names = TRUE)# remove duplicates

############### for test run purposes to speeds up  #################
#### take out INLA calculation and use dummy cluster (below) as output
#cluster <- c(12,3,33,44,65) # dummy cluster (real simulations use next line)
cluster <- unique (cluster)
}  ##### END INLA CALCULATIONS

######################################################################
### common trunk  ####################################################
### from here same for Kulldorff and BN and INLA #####################
######################################################################
####### P E R F O R M A N C E #### Calculations ######################
######################################################################

# get the "true" Risk Polygons (risk = true clusters, cluster = found clusters )
risk <- unlist(subset(polygons@data, Cluster> 0, select = c(ID))) 
risk <- as.numeric(risk)

# Calculate Performance
risk <- sort (risk)
cluster <- sort (cluster)
cluster <- as.numeric (cluster)
clusnum <- length (cluster)
risknum <- length (risk)

# True positive
truepos <-  intersect (risk, cluster)
trueposnum <- length (truepos)

# False pos
falsepos <- setdiff ( cluster, risk)
falseposnum <- length (falsepos)

#False neg
falseneg <- setdiff (risk,cluster)
falsenegnum <- length (falseneg)

# True negative
negative <- setdiff (clustervector, risk)
trueneg <- setdiff (negative, cluster)
truenegnum <- length (trueneg)

# Minimum Power (1 =at least one of the risk Polygons detected)
MPt  <- if (trueposnum == 0) MPt=0 else MPt =1
MP [m] <- MPt

# Exact power (1= ALL Risk Polygons detected AND NO False positives)
EP [m]<- as.integer (identical (risk, cluster))

# Sensititvity: True Positives / True Positive+ False negative
if ((trueposnum+falsenegnum)>0)
{sensitivity [m]<- trueposnum/(trueposnum+falsenegnum)
}  else {
sensitivity [m]=0}

# Specificity: True negatives/ True negatives + false Positives
specificity [m] <-  truenegnum/(truenegnum+falseposnum)          

# Positive Predictive Value
if ((trueposnum+falseposnum)>0)
{ PPV [m]<- trueposnum/(trueposnum+falseposnum)
} else  {
PPV [m]= 0}

# Negative Predictive value
if ((truenegnum+falsenegnum)> 0)
{NPV [m]<- truenegnum/(truenegnum+falsenegnum)
} else  {
NPV [m]= 0}

# Correct Classification: Percentage of correctly classified polygons (either risk or no risk)
CC [m]<- 100*(trueposnum+truenegnum)/402
# Percent of detected clusters that are risk districts
if ((clusnum)> 0)
{PRVC [m] <- (trueposnum/clusnum)
} else {
PRVC [m] = 0  
}

# False Positive Rate
if ((falseposnum+ truenegnum)> 0)
{FPR [m] <- (falseposnum/(falseposnum+ trueposnum))*100
} else {
FPR [m] = 0  
}

# False Negative Rate
if ((falsenegnum+ trueposnum)> 0)
{FNR [m] <- (falsenegnum/(falsenegnum + truenegnum))*100
} else {
FNR [m] = 0  
}

## Diagnostic Likelihood (pos: Sens / (1-spec); neg: (1-sens/spec))

DLRPOS  [m]<- sensitivity  [m]/(1.01-specificity [m] ) # max DLRPOS = 100 ("1.01")
DLRNEG [m] <- (1- sensitivity  [m])/specificity [m] 

# For the descriptive Statistics 
list_of_riskvectors [[m]] <- risk
list_of_clustervectors [[m]] <- cluster

# for calculation of Monte Carlo Error and Monte Carlo Bias 

# Monte Carlo Error of the Mean is calculated as:
# SD (mean of simulated RR)/sqrt (number of simulations)
RRSimGes [m] <- mean (polygons@data$RRSIM)

# Monte Carlo Bias is calculated as (mean of simulated RR - mean of real RR)
RRTrueGes  [m] <-  mean (polygons@data$RR)
RRBIAS  [m] <- sum(polygons@data$RRSIM -polygons@data$RR)/simulations

#########Performance Table 
Performance [[q]] <- data.frame ( Method,Clustertype [cr], ClusterRR [q], simulations, 
                   mean (sensitivity), sd (sensitivity), mean(sensitivity)-1.96*(sd(sensitivity)/sqrt(simulations)),mean(sensitivity)+1.96*(sd(sensitivity)/sqrt(simulations)),
                   mean(specificity), sd(specificity) , mean(specificity)-1.96*(sd(specificity)/sqrt(simulations)),mean(specificity)+1.96*(sd(specificity)/sqrt(simulations)),
                   mean (PPV), sd(PPV), mean(PPV)-1.96*(sd(PPV)/sqrt(simulations)), mean(PPV)+1.96*(sd(PPV)/sqrt(simulations)),
                   mean (NPV), sd (NPV),mean(NPV)-1.96*(sd(NPV)/sqrt(simulations)),mean(NPV)+1.96*(sd(NPV)/sqrt(simulations)),
                   mean (EP), sd (EP), mean(EP)-1.96*(sd(EP)/sqrt(simulations)), mean(EP)+1.96*(sd(EP)/sqrt(simulations)),
                   mean (MP),sd (MP), mean(MP)-1.96*(sd(MP)/sqrt(simulations)), mean(MP)+1.96*(sd(MP)/sqrt(simulations)),
                   mean (CC), sd (CC),mean(CC)-1.96*(sd(CC)/sqrt(simulations)), mean(CC)+1.96*(sd(CC)/sqrt(simulations)),
                   mean (PRVC), sd (PRVC), mean(PRVC)-1.96*(sd(PRVC)/sqrt(simulations)),mean(PRVC)+1.96*(sd(PRVC)/sqrt(simulations)),
                   mean (DLRPOS), sd (DLRPOS),mean(DLRPOS)-1.96*(sd(DLRPOS)/sqrt(simulations)),mean(DLRPOS)+1.96*(sd(DLRPOS)/sqrt(simulations)),
                   mean (DLRNEG), sd (DLRNEG),mean(DLRNEG)-1.96*(sd(DLRNEG)/sqrt(simulations)),mean(DLRNEG)+1.96*(sd(DLRNEG)/sqrt(simulations)),
                   sd (RRSimGes)/sqrt (simulations),  sd (RRSimGes)/sqrt (simulations)/ sd(RRSimGes)*100,
                   aggregate,
                   lambda,
                   k,
                   alpha.level,
                   pop.upper.bound,
                   Version,
                   Datum, 
                   mean (FPR), sd (FPR),mean(FPR)-1.96*(sd(FPR)/sqrt(simulations)),mean(FPR)+1.96*(sd(FPR)/sqrt(simulations)),
                   mean (FNR), sd (FNR),mean(FNR)-1.96*(sd(FNR)/sqrt(simulations)),mean(FNR)+1.96*(sd(FNR)/sqrt(simulations)),
                   mean ( RRBIAS), sd (RRBIAS),mean(RRBIAS)-1.96*(sd(RRBIAS)/sqrt(simulations)),mean(RRBIAS)+1.96*(sd(RRBIAS)/sqrt(simulations)))
# Reset Cluster/Randomcluster 
polygons$randomcluster <- NULL
polygons$Cluster <- NULL
list_of_SimCasesQ[[q]] <- list_of_SimCases
list_of_RandomClusterQ [[q]] <- list_of_RandomCluster
}
list_of_riskvectorsCR  [[cr]] <- list_of_riskvectors
list_of_collvectorsCR [[cr]] <- list_of_collvectors
list_of_clustervectorsCR [[cr]] <- list_of_clustervectors
PerformanceComplete [[cr]]<- Performance
list_of_RandomClusterCR [[cr]] <- list_of_RandomClusterQ
list_of_SimCasesCR[[cr]] <- list_of_SimCasesQ
} # Loop End for (m ... (m simulations of different Clusters ) 
} #loop end (Random Clusters Generator)


if (  CLT > 1 ) {
# Combined List (only works with at least two clustertypes analyzed
# for two lists
#bind_rows(liste[[2]], liste[[2]])
# for more than two lists 
kombinierteListe <- PerformanceComplete[[1]]
for(i in 2:length(PerformanceComplete)){
kombinierteListe <- bind_rows(kombinierteListe, PerformanceComplete[[i]])
}
} else {
kombinierteListe <- Performance
kombinierteListe <- bind_rows(kombinierteListe)
}
##################        Performance output into Table 
names(kombinierteListe) <-c(
"Methode", 
"Clustertype",
"Relative Risk", 
"Number of Simulations", 
"Mean Sensitivity", "SD Sensitivity", "LCI Sens", "UCI Sens",
"Mean Specificity", "SD Specificity", "LCISpec", "UCISpec",
"Mean Positive Predictive Value", "SD MPPV", "LCI PPV", "UCI PPV",
"Mean Negative Predictive Value", "SD NPV", "LCI NPV", "UCI NPV",
"Mean Exact Power", "SD MEP",  "LCI MEP", "UCI MEP",
"Mean Minimum Power", "SD MMP", "LCI MMP", "UCI MMP",
"Mean Correct Classification", "SD MCC", "LCI MMC", "UCI MMC",
"Mean Correct of found Clusters", "SD PRVC","LCI PRVC", "UCI PRVC",
"Mean Pos Diagn Likelihood", "SD DLR+", "LCI DLR+", "UCI DLR+",
"Mean Neg Diagn Likelihood", "SD DLR-", "LCI DLR-", "UCI DLR-",
"MCERROR", "MCError (%ofSDRR)",
"Aggregierte Jahre",
"Inzidenz",
"k-BN",
"alpha",
"Population OG-CCS",
"Version",
"Datum", 
"Mean False Positive Rate", "SD FPR", "LCI FPR", "UCI FPR",
"Mean False Negative Rate", "SD FNR", "LCI FNR", "UCI FNR",
"Raw Monte Carlo Bias", "SD RMCB", "LCI RMCB", "UCI RMCB")


# export Performance Data to to Open XLXS 
wb <- createWorkbook()
addWorksheet(wb, RANDOMorDEFINED)
writeDataTable( wb,  RANDOMorDEFINED, x = kombinierteListe)
saveWorkbook(wb, paste("Data/",Method,simulations, ALLorWILMS, RANDOMorDEFINED, Datum,".xlsx" ,sep = "") , overwrite = T)
####  save Raw Data
saveRDS (list_of_clustervectorsCR, file =paste("Data/",Method,simulations, ALLorWILMS, RANDOMorDEFINED, Datum, "list_of_clustervectorsCR", sep = ""))
saveRDS (list_of_riskvectorsCR, file =paste("Data/",Method,simulations, ALLorWILMS, RANDOMorDEFINED, Datum, "list_of_riskvectorsCR", sep = ""))
saveRDS (PerformanceComplete, file =paste("Data/",Method,simulations, ALLorWILMS, RANDOMorDEFINED, Datum, "PerformanceComplete", sep = ""))




