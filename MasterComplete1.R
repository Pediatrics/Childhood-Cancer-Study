  rm(list=ls()) # clear environment
  cat("\014")   # clear console
  setwd("~/OneDrive - uni-duisburg-essen.de/Mastercode")
  
  
  ####################### Master Complete Code #########################  
  Version <- "MasterComplete1"
  # Version prepared for "Genericness" for all Methods. Also prepared for Publishability
  # Graphics of drawn clusters etc.
  
  ########################
  # Load libraries
    library('raster')
    library('geosphere')
    library('mapview') # visualization
    library (spdep) # for adjacency matrix
    library(openxlsx)
    library (SpatialEpi)
    library (sp)
    library (data.table) # merge lists
    library(INLA) # for INLA models
    library (maptools)
    library(rgdal) # für readOGR
    require(dataAnalysisMisc)# by Maximilian Knoll (see code for update under local file "code_v3MaxKnollRandomClusters")
    require (tidyverse)
    
  Datum <- Sys.Date()
  set.seed (999999999)
  start_time <- Sys.time()
    
    
  ###########################
  #Presets for all methods
  ###########################
  Method <- "BN" # "Kulldoff" or "INLA" or "BN"
  simulations <- 3
  ALLorWILMS <- "ALL" # set "ALL" for lambda = 14/100000 and "Wilms" for 0.7/1000000
  # incidence of event
  lambda = 14/100000 # incidence of event 14/100000 für all neoplasms, 0.7/100000 for Wilms Tumors
  # aggregation period (years)
  aggregate <- 10  # years aggregated 
  # relative risk in risk clusters
  #ClusterRR <- c(1,1.1,1.2, 1.3,1.4, 1.5, 2, 5,10,100)
  ClusterRR <- c(1,2,3)
  
  # BN + Kulldorff only (alpha level)
  alpha.level <- 0.05 # e.g. 0.05 for BN of Kulldorff (NA for INLA)
  
  # BN only (aggregated cases needed for a local cluster)
  k <- 10# e.g. 6 for Besag Newell (NA for other methods) higher ks mean higher sensitivity and minimum power but less exact power/ correct classificaiton
  
  # Kulldorff only 
  pop.upper.bound <- NA #0.05 # e.g. 0.05 for Kulldorff (NA for others) upper bound on the proportion of total population each zone can include
  n.simulations <- 99 # e.g. 99 for Kulldorff (NA for others) number of Monte Carlo samples used for significance measures
  plot <- F # Plot Histograms of Monte Carlo samples of the log-likelihood of the most likely cluster
  
  
  ############################
  #which kind of risk-clusters is being formed (random size/shape or defined)
  RANDOMorDEFINED <- "defined" 
  Clustertype <-c ("Cluster1", "Cluster2") # for Excel output at least 2 Clustertypes have to be defined 
  #Clustertype <-c ("Cluster1", "Cluster2", "Cluster3","Cluster4","Cluster5", "Cluster6", "Cluster7")  # for defined risk-clusters
  #Clustertype <-c(1,5)
  #Clustertype <-c(1,2,5,10,20,50) # for randomly sized/shaped clusters
  
  clustervector <- c (1:402) # needed several times (number of polygons)
  CRR <- length (ClusterRR) # for Loop over Relative Risiken
  CLT <- length (Clustertype) # for Loop over Random Clusters 
  ##### kann das raus??? -Vector <- paste("RR", ClusterRR, sep ="")
  
  ######################
  #Performance Measures:
  sensitivity = 0 
  specificity =  0
  PPV = 0 # positive predictive Value 
  EP =0 # exact power
  MP = 0 # minimum Power
  NPV = 0 # Negative Predictive VAlue 
  CC =0  # mean correct classification 
  PRVC = 0 # Rate of true Positives of all detected clusters
  RRSimGes = 0  # simulated relative risk (main outcome of simulation) for Monte Carlo Error 
  Performance <- vector(mode = "list",length = CRR)
  PerformanceComplete <- vector (mode = "list",length = CLT)
  # zum testen, ob die Risk und Randomvektoren übereinstimmen (kann hinterher raus)
  list_of_riskvectors   <- list() # from random/defined list
  list_of_riskvectorsCR <- list()
  list_of_collvectors   <- list() # from Random "generator"
  list_of_collvectorsCR <- list()
  list_of_clustervectors <- list() # detected clusters 
  list_of_clustervectorsCR <- list()
      
  #####################################
  #Get SpatialPolygonsDataFrame
  polygons <-shapefile("Data/gadm36_DEU_2.shp") #Polygons of districts. GADM version 3.6. 6th May 2018
  # mapview (polygons)  
  # omit Bodensee ("Large Water Body")
  polygons <- polygons[!is.na(polygons$CC_2), ] 
    
  ######################################
  # get population data Table 12411-0018 from www-genesis.destatis.de - with population etc.  
  # Cluster TAbelle: Stichtag 31.12.2016
  # defined clusters also added in the xlsx file
  ClusterID = read.xlsx("Data/Cluster.xlsx")  
  
  #######################################
  # Merge population and geographic data  (left join from didyverse!! (merge gets the wrong ID order!!!!))  
  polygons@data <- polygons@data %>%  left_join(ClusterID,     by ="CC_2") %>% mutate(ID = 1:402)
  # delete unwanted columns
  polygons@data[c("GID_0","NAME_0","GID_1","NAME_1" ,"NL_NAME_1","ENGTYPE_2" , "HASC_2","VARNAME_2","NL_NAME_2","TYPE_2", "ID.ab.Flensburg", "ID.ab.Alb.Donau", "Case","Mbis3" ,"Mbis6","Mbis10","Mbis15","Fbis3", "Fbis6", "Fbis10", "Fbis15")] <- NULL
  
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
     
  # adj matrix
  adj <- adjacencyweight
    
  # Plot Polygons + Centroids
  geo2 <- centroids@coords[,1:2]
  geo2 <- latlong2grid(geo2)
    
  # Ajaceny from Blangiardo Book page 181:
  adjacencyweight <- as (adjacencyweight, "dgTMatrix")
  nb2INLA ("adjacency.graph", adjacency )
  adjacency.adj <- paste (getwd (), "/adjacency.graph", sep="")
  
  #Plot Adjacency Matrix 
  #adjmatrix <- inla.read.graph(filename="adjacency.graph")
  #image (inla.graph2matrix(adjmatrix), xlab="", ylab="")
   
  # Loop for Cluster-Type
  for (cr in 1: CLT)
     {
  # Loop for RR-Risk Levels
  for(q in 1:CRR)
      {
  # Number of simulations per RR-Level
  set.seed(888888888) # with this seed each Clustertype will be equal in each Risklevel
  for (m in 1:simulations){ 
  
  ## Statement , not random: = defined --> 2. Statement 
  if (RANDOMorDEFINED == "random") {
     
  #### Start of RANDOM Cluster Formation     
      
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
      
  #nPoly <- 1 ## hier anpassen, die anzahl der cluster 
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
  randomcluster <- match (clustervector,randomcluster, nomatch =0, incomparables = 0)
  randomcluster <- replace(randomcluster, randomcluster>0, 1)
  randomcluster <- as.data.frame(randomcluster)
  randomcluster <- data.frame("ID" = 1:nrow(randomcluster), randomcluster)
  polygons@data <- polygons@data %>%  left_join(randomcluster,     by ="ID") 
  polygons$Cluster <- polygons$randomcluster
  
  ####### end of "Random Cluster Generator"
      } else {
        
  ####### Defined Clusters
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
  
  # Calculate Cases and Relative Risk over a time frame of xx aggregated years
  for  (xx in 1:aggregate) {
  SimCases <- rpois(n = 1:nrow (polygons@data), lambda = lambda *(polygons@data$Population)*polygons@data$RR) 
  CAS10 <- CAS10 + SimCases
  RRSim<-RRSim+ SimCases /polygons@data$Population*lambda
    }
    #sum(CAS10)
  # Cases into the master table 
  polygons@data$Cases<- CAS10
  polygons@data$Cases
  
  if (Method=="INLA") {
  
  ##############################
  # INLA MODELL Calculations
  ##############################
  # Calculate expected cases for INLA Model (this means expected for ALL Tumors) - for wilms *0.05
  # hier muss  hin: polygons@data$Erwartet <- polygons@data$Population*lamda*aggregate -> Dann ist Wilms und ALL egal
  polygons@data$Erwartet <- polygons@data$Population*lambda*aggregate #ONLY WILMS (otherwise delete!!)
  
  
  formula <- Cases ~  f (ID, model="bym", graph=adjacency.adj, scale.model = TRUE, hyper=list (prec.unstruct=list (prior="loggamma", param=c(1,0.001)), prec.spatial=list(prior="loggamma", param=c(1,0.001)))) #+ f(IID,model="iid")
  INLACluster<- inla (formula,family ="poisson", data=polygons@data, E=Erwartet, control.compute = list (dic=TRUE))
  # Get 95% Credibility-Invervall > 1 
  INLACluster <- INLACluster$summary.fitted.values # hier neue Tabelle mit Werten --> RR Risk Estimates Posteriors!!
  INLACluster$ID <- 1:nrow(INLACluster) # hier wieder die ID´s der Kreise 
  INLACluster <- INLACluster[INLACluster$`0.025quant`>1,] # hier nur die zeilen und spalten mit signifikanten werten inkl ID)
  cluster  <- sapply(INLACluster$ID,"[")
  cluster<- unlist(cluster, recursive = TRUE, use.names = TRUE)# alle Polygone / Centroide nur 1 x 
  
  
  ############### just for Test Runs Purposes  #############
  #cluster <- c(12,3,33,44,65)
  
  cluster <- unique (cluster)
  } #END INLA Conditon
  else if (Method =="BN") {
    
    ## Get aggregated counts of population and cases for each county
    population <- tapply(polygons@data$Population,polygons@data$ID,sum)
    cases <- tapply(polygons@data$Cases,polygons@data$ID,sum)
    
    
    sum (cases)      
    sum (population)
    
    # calculate expected cases (SpatialEpi function)
    expected.cases2 <- expected(polygons@data$Population, polygons@data$Cases, n.strata = 1) 
    
    # not controlling for stratas
    results <- besag_newell(geo2, population, cases, expected.cases=NULL, k, alpha.level)
    
    #hier hole ich die IDs der CLuster aus der Liste
    cluster <- sapply(results$clusters,"[" , "location.IDs.included")
    
    cluster<- unlist(cluster, recursive = TRUE, use.names = TRUE)
    # alle Polygone / Centroide nur 1 x 
    cluster <- unique (cluster)
  } # END BN Condition  
  else if (Method =="Kulldorff") {
  ## Get aggregated counts of population and cases for each county
  population <- tapply(polygons@data$Population,polygons@data$ID,sum)
  cases <- tapply(polygons@data$Cases,polygons@data$ID,sum)
    
    
  sum (cases)      
  sum (population)
    
  # calculate expected cases (SpatialEpi function)
  expected.cases2 <- expected(polygons@data$Population, polygons@data$Cases, n.strata = 1) 
    
    
  ## Kulldorff using Poisson likelihoods --> Geht nciht 
  # poisson <- kulldorff(geo2, cases2, population2, expected.cases2 , pop.upper.bound, n.simulations, alpha.level, plot)
  # cluster <- poisson$most.likely.cluster$location.IDs.included
    
    
  ## Kulldorff using Binomial likelihoods
  binomial <- kulldorff(geo2, cases, population, NULL, pop.upper.bound, 
                          n.simulations, alpha.level, plot)
  # primäres Cluster
  cluster <- binomial$most.likely.cluster$location.IDs.included
    
  # die secondary clusters rausholen
  binomial$secondary.clusters
    x <- sapply(binomial$secondary.clusters,"[" , "location.IDs.included")
    y<- unlist(x, recursive = TRUE, use.names = TRUE)
    # alle Polygone / Centroide nur 1 x 
    cluster <- unique (c(y, cluster) )
  }
  
  #### here: Common Trunk (not depending on cluster-detection method)
  # get the "true" Risk Polygons
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
  sensitivity [m]<- trueposnum/(trueposnum+falsenegnum)
    
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
  
  # For the descriptive Statistics 
  list_of_riskvectors [[m]] <- risk
  list_of_clustervectors [[m]] <- cluster
  
  # for calculation of Monte Carlo Error 
  RRSimGes [m] <- mean (RRSim)
  
  Performance [[q]] <- data.frame (Clustertype [cr], ClusterRR [q], simulations, 
                                   mean (sensitivity), sd (sensitivity), mean(sensitivity)-(sd(sensitivity)/sqrt(simulations)),mean(sensitivity)+(sd(sensitivity)/sqrt(simulations)),
                                   mean(specificity), sd(specificity) , mean(specificity)-(sd(specificity)/sqrt(simulations)),mean(specificity)+(sd(specificity)/sqrt(simulations)),
                                   mean (PPV), sd(PPV), mean(PPV)-(sd(PPV)/sqrt(simulations)), mean(PPV)+(sd(PPV)/sqrt(simulations)),
                                   mean (NPV), sd (NPV),mean(NPV)-(sd(NPV)/sqrt(simulations)),mean(NPV)+(sd(NPV)/sqrt(simulations)),
                                   mean (EP), sd (EP), mean(EP)-(sd(EP)/sqrt(simulations)), mean(EP)+(sd(EP)/sqrt(simulations)),
                                   mean (MP),sd (MP), mean(MP)-(sd(MP)/sqrt(simulations)), mean(MP)+(sd(MP)/sqrt(simulations)),
                                   mean (CC), sd (CC),mean(CC)-(sd(CC)/sqrt(simulations)), mean(CC)+(sd(CC)/sqrt(simulations)),
                                   mean (PRVC), sd (PRVC), mean(PRVC)-(sd(PRVC)/sqrt(simulations)),mean(PRVC)+(sd(PRVC)/sqrt(simulations)),
                                  sd (RRSimGes)/sqrt (simulations),  sd (RRSimGes)/sqrt (simulations)/ sd(RRSimGes)*100,
                                   aggregate,
                                   lambda,
                                  k,
                                  alpha.level,
                                  pop.upper.bound,
                                   Version,
                                   Datum)
  
  # Reset Cluster/Randomcluster 
  polygons$randomcluster <- NULL
  polygons$Cluster <- NULL
    }
  list_of_riskvectorsCR  [[cr]] <- list_of_riskvectors
  list_of_collvectorsCR [[cr]] <- list_of_collvectors
  list_of_clustervectorsCR [[cr]] <- list_of_clustervectors
  PerformanceComplete [[cr]]<- Performance
  } # Loop Ende for (m ... (m simulationen verscheidener CLuster) 
  } #loop ende Clusterrandom
    
  ##############################
  #view Clusters in Mapview
  ##############################
  #######risk clusters
  #mapview(polygons) + mapview(polygons [risk, ],  col.regions=2)
  #######detected clusters
  #mapview(polygons) + mapview(polygons [cluster, ],  col.regions=3) 
  ###### risk plus detected clusters
  #mapview(polygons) + mapview (polygons[ risk,]) +mapview (polygons[ cluster,], col.regions=3)
  #####centroids of risk clusters
  #mapview(polygons) + mapview(centroids [c(risk), ]) # das hier ist der RandomCluster (gleiches wie Risk)
  # Correctly detected (green), false positive (red) missed (blue) 
  #if (falseposnum > 0 ) { 
  #if (trueposnum > 0 ){ mapview(polygons) + mapview(polygons [c(risk), ],  col.regions=4) +mapview(polygons [truepos, ],  col.regions=3) + mapview(polygons [falsepos, ],  col.regions=2)
  #} else {
  #  mapview(polygons) +  mapview(polygons [c(risk), ],  col.regions=4) +mapview(polygons [falsepos, ],  col.regions=3) 
  #} }else {if (trueposnum > 0 ){ mapview(polygons) + mapview(polygons [c(risk), ],  col.regions=4) +mapview(polygons [truepos, ],  col.regions=3) 
  #} else {
  #  mapview(polygons) +  mapview(polygons [c(risk), ],  col.regions=4)
  #}  }
    
  
  
  # Combined List (only works with at least two clustertypes analyzed
  # for two lists
  #bind_rows(liste[[2]], liste[[2]])
  # for more than two lists 
  kombinierteListe <- PerformanceComplete[[1]]
  for(i in 2:length(PerformanceComplete)){
    kombinierteListe <- bind_rows(kombinierteListe, PerformanceComplete[[i]])
  }
  
  names(kombinierteListe) <-c(
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
    "MCERROR", "MCError (%ofSDRR)",
    "Aggregierte Jahre",
    "Inzidenz",
    "k BN",
    "alpha",
    "Proportion Kulldorff",
    "Version",
    "Datum")
  
  ##########################################
  ##Save and Display the results graphically
  ##########################################
  # For Risk - Clusters (to visualize the randomly drawn risk - polygons. (not relevant for "defined" clusters)
  # best is only use ONE RR Level and ONE Clustertype (that way all the settings work just fine. Otherways the values within the [] brackets have to be changed )
  #construct list of number/percentage of draws of risk clusters (only relevant for "random" Clusters)
  # take all drawn risk polygons from one clustertype (e.g.[1] ) and put into one long vector
  LORV <- unlist(list_of_riskvectorsCR [1], recursive = T, use.names = TRUE) 
  #hist(LORV)
  # LORV + Clustervector - this way all ID´s are represented
  LORV <- c(LORV,clustervector)
  clustertable<-  table (LORV) # Number of hits per ID (+1 because of "Clustervector")
  clustertable<-  as.data.frame(clustertable)
  #subtract 1 from all hits - Frequency of hits
  clustertable$Freq <- clustertable$Freq-1 
  # Introduce ID column for left.join
  clustertable$ID <- seq.int(nrow(clustertable))
  # change name to "RiskFrequency" (number of total draws)
  names(clustertable)[2]<-paste("RiskFrequency") 
  # Percentage of draws
  clustertable$RiskPercentage <- clustertable$RiskFrequency/simulations*100 
  #Merge Distribution of Risk Clusters to Polygons
  polygons@data <- polygons@data %>%  left_join(clustertable, by="ID") 
  
  ################# Display detected Clusters (true positive AND false positive) #############
  # take all detected Cluster polygons from one clustertype (e.g.[1] ) and put into one long vector
  LOCV <- unlist(list_of_clustervectorsCR [1], recursive = T, use.names = TRUE) #liste der Cluster Vectors von Stufe 1 (only one clustertype will be analyzed)
  LOCV <- c(LOCV,clustervector)
  clustertable<-  table (LOCV) # table zählt die Anzahl von elements with the value xxx  in a vector
  clustertable<-  as.data.frame(clustertable)
  clustertable$Freq <- clustertable$Freq-1 
  clustertable$ID <- seq.int(nrow(clustertable))
  names(clustertable)[2]<-paste("ClusterFrequency") # total Draws
  clustertable$ClusterPercentage <- clustertable$ClusterFrequency/simulations*100 # Percentage of draws
  polygons@data <- polygons@data %>%  left_join(clustertable, by="ID") 
  
  ############################################
  # Plot number of Draws in Simulation
  ############################################
  # for the Title: Set  ClusterRR [xx] and "Clustertype [xx] according to wich one is being displayed
  # best is only use ONE RR Level and ONE Clustertype (that way all the settings work just fine. Otherways the values within the [] brackets have to be changed )
  cols= rev(heat.colors(20, 0.6))
  #p1<-  spplot(polygons, 'RiskFrequency', main=paste ("Risk Cluster Frequency (n)","","\n",simulations, "Simulations,", "RR", ClusterRR [1], "\nClustertype", Clustertype[1], "\nVersion", Version), colorkey=T,cuts= 10, col.regions=cols)
  #p1
  #p2 <- spplot(polygons, "RiskPercentage", col.regions=cols, main=paste("Risk Cluster Frequency (%)","","\n",simulations, "Simulations,", "RR", ClusterRR [1], "\nClustertype", Clustertype [1], "\nVersion", Version),cuts= 10)
  #p2   
  #p3<-  spplot(polygons, 'ClusterFrequency', main=paste ("Detected Cluster Frequency (n) in","","\n",simulations, "Simulations,","RR", ClusterRR [1],  "\nClustertype", Clustertype [1], "\nVersion", Version), colorkey=T,cuts= 10, col.regions= rev(heat.colors(20, 0.6)))
  #p3
  #p4 <- spplot(polygons, "ClusterPercentage", col.regions=cols, main=paste ("Detected Cluster Frequency in %","","\n",simulations, "Simulations,", "RR", ClusterRR[1],  "\nClustertype", Clustertype [1], "\nVersion", Version),cuts= 10)
  #p4
  
  # export Performance Data to to Open XLXS 
  wb <- createWorkbook()
  addWorksheet(wb, RANDOMorDEFINED)
  writeDataTable( wb,  RANDOMorDEFINED, x = kombinierteListe)
  saveWorkbook(wb, paste("Data/",Method,simulations, ALLorWILMS, RANDOMorDEFINED, Datum,".xlsx" ,sep = "") , overwrite = T)
  ####  save Raw Data
  saveRDS (list_of_clustervectorsCR, file =paste("Data/",Method,simulations, ALLorWILMS, RANDOMorDEFINED, Datum, "list_of_clustervectorsCR", sep = ""))
  saveRDS (list_of_riskvectorsCR, file =paste("Data/",Method,simulations, ALLorWILMS, RANDOMorDEFINED, Datum, "list_of_riskvectorsCR", sep = ""))
  saveRDS (PerformanceComplete, file =paste("Data/",Method,simulations, ALLorWILMS, RANDOMorDEFINED, Datum, "PerformanceComplete", sep = ""))
  
  end_time <- Sys.time()
  elapsed <- end_time-start_time 
  elapsed
  
  
  # load old list
  #xxx <- list ()
  #xxx <- readRDS (xxx, file ="xxxx")
  
  