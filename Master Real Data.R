  rm(list=ls()) # clear environment
  cat("\014")   # clear console
  setwd("~/OneDrive - uni-duisburg-essen.de/Mastercode")
  
  ####################### Master Complete Code #########################  
  Version <- "Master Real Data"
  # Build from Final BN Generic 2 -->Final Kulldorf SpatialEpi Generic2 --> Final INLA Generic18
  # because Master Generic was producint different results than the single ones
  # Verision MFS1 has been validated from the single ones!!
  # 3 Diagnostic likelihood
  # sensitivity divided by zero geändert
  # 6: Cluster percentage (HITS) Choropleth (graphic presenation with "Chropleth No Donuts KReisebene2")
  #6: can be used for RR Choropleth (graphic presenation with "Chropleth No Donuts KReisebene2")
  
  
  
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
  require(gplots)
  
  Datum <- Sys.Date()
  start_time <- Sys.time()
  set.seed (999999999)
  
  
  ###########################
  #Presets for all methods
  ###########################
  Method <- "Kulldorff" # "BN" , "Kulldorf", "INLA"
  #simulations <-1# 
  k <- 5#  NA or e.g. 10 scheint vernünftig zu sein for Besag Newell 
  alpha.level <- 0.05# NA or e.g. 0.05 For Cluster detection BN MEthod or Kulldorff (NA for INLA)
  pop.upper.bound <- 0.1 # NA or e.g. 0.05 for Kulldorf
  n.simulations <- 99 # NA or 99  for Kulldorf
  plot <- F # Plot Histograms of Monte Carlo Distribution of Lambda (Kulldorff only )
  
  
  #####################################
  #Get SpatialPolygonsDataFrame
  polygons <-shapefile("Data/gadm36_DEU_2.shp") #Polygons of districts. GADM version 3.6. 6th May 2018
  # mapview (polygons)  
  # omit Lake Constance ("large water body" )
  polygons <- polygons[!is.na(polygons$CC_2), ] 
  
  ######################################
  # get population data Table 12411-0018 from www-genesis.destatis.de - with population etc.  
  # defined clusters also added in the xlsx file
  # Cluster TAbelle: Stichtag 31.12.2016
  ClusterID = read.xlsx("Data/Cluster.xlsx")  
  
  #######################################
  # Merge population and geographic data  (left join from tidyverse!! (merge gets the wrong ID order!!!!))  
  polygons@data <- polygons@data %>%  left_join(ClusterID,     by ="CC_2") %>% mutate(ID = 1:402)
  # delete unwanted columns
  polygons@data[c("GID_0","NAME_0","GID_1","NAME_1" ,"NL_NAME_1","ENGTYPE_2" , "HASC_2","VARNAME_2","NL_NAME_2","TYPE_2", "ID.ab.Flensburg", "ID.ab.Alb.Donau", "Case","Mbis3" ,"Mbis6","Mbis10","Mbis15","Fbis3", "Fbis6", "Fbis10", "Fbis15")] <- NULL
  
  # Rename: Population
  polygons@data$Population <- polygons@data$Anzahl.Bevölkerung/10
  # For Total Cases
  #polygons@data$Cases <- polygons@data$Anzahl.Fälle1
  # For Wilms Tumor Cases
  polygons@data$Cases <- polygons@data$Anzahl.Fälle61
  
  
  
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
  
  # Plot Adjacency Matrix 
  # adjmatrix <- inla.read.graph(filename="adjacency.graph")
  # image (inla.graph2matrix(adjmatrix), xlab="", ylab="")
  
  
  ## Get aggregated counts of population and cases for each county
  population <- tapply(polygons@data$Population,polygons@data$ID,sum)
  cases <- tapply(polygons@data$Cases,polygons@data$ID,sum)
  
  sum (cases)      
  sum (population)
  
  if (  Method == "BN" ) {
    
  #####################################
  ## Besag-Newell Model Calculations ##
  ##################################### 
    
  # calculate expected cases (SpatialEpi function)
  expected.cases <- expected(polygons@data$Population, polygons@data$Cases, n.strata = 1) 
  
  # not controlling for stratas
  results <- besag_newell(geo2, population, cases, expected.cases=NULL, k, alpha.level)
  
  #hier hole ich die IDs der CLuster aus der Liste
  cluster <- sapply(results$clusters,"[" , "location.IDs.included")
  cluster<- unlist(cluster, recursive = TRUE, use.names = TRUE)
  # all polygons only 1 x 
  cluster <- unique (cluster)
  }   else if (  Method == "Kulldorff" ) {
  ###################################
  ## Kulldorff Model Calculations  ##
  ###################################   
    # calculate expected cases (SpatialEpi function)
    expected.cases <- expected(polygons@data$Population, polygons@data$Cases, n.strata = 1) 
    
    ## Kulldorff using Poisson likelihoods --> Geht nciht 
    poisson <- kulldorff(geo2, cases, population, expected.cases , pop.upper.bound, n.simulations, alpha.level, plot)
    cluster <- poisson$most.likely.cluster$location.IDs.included
    poisson$secondary.clusters
    x <- sapply(poisson$secondary.clusters,"[" , "location.IDs.included")
    y<- unlist(x, recursive = TRUE, use.names = TRUE)
    # alle Polygone / Centroide nur 1 x 
    cluster <- unique (c(y, cluster) ) 
    
    ## Kulldorff using Binomial likelihoods
    # binomial <- kulldorff(geo2, cases, population, NULL, pop.upper.bound, 
    #                      n.simulations, alpha.level, plot)
    # primäres Cluster
    #cluster <- binomial$most.likely.cluster$location.IDs.included
    # die secondary clusters rausholen
    #binomial$secondary.clusters
    # x <- sapply(binomial$secondary.clusters,"[" , "location.IDs.included")
    # y<- unlist(x, recursive = TRUE, use.names = TRUE)
    # alle Polygone / Centroide nur 1 x 
    # cluster <- unique (c(y, cluster) ) 
    }     else if (  Method == "INLA" ) {
  
  ###############################
  ## INLA Model Calculations   ##
  ###############################
  
  # Calculate expected cases for INLA Model (this means expected for ALL Tumors) - for wilms *0.05
  polygons@data$Erwartet<- expected(polygons@data$Population, polygons@data$Cases, n.strata = 1)  #ONLY WILMS (otherwise delete!!)
  
  formula <- Cases ~  f (ID, model="bym", graph=adjacency.adj, scale.model = TRUE, hyper=list (prec.unstruct=list (prior="loggamma", param=c(1,0.001)), prec.spatial=list(prior="loggamma", param=c(1,0.001)))) #+ f(IID,model="iid")
  INLACluster<- inla (formula,family ="poisson", data=polygons@data, E=Erwartet, control.compute = list (dic=TRUE))
  # Get 95% Credibility-Invervall > 1 
  INLACluster <- INLACluster$summary.fitted.values # hier neue Tabelle mit Werten --> RR Risk Estimates Posteriors!!
  INLACluster$ID <- 1:nrow(INLACluster) # hier wieder die ID´s der Kreise 
  INLACluster <- INLACluster[INLACluster$`0.025quant`>1,] # hier nur die zeilen und spalten mit signifikanten werten inkl ID)
  cluster  <- sapply(INLACluster$ID,"[")
  cluster<- unlist(cluster, recursive = TRUE, use.names = TRUE)# alle Polygone / Centroide nur 1 x 
  
  ############### for test run purposes to speeds up  #################
  #### take out INLA calculation and use dummy cluster (below) as output
  #cluster <- c(12,3,33,44,65)
  cluster <- unique (cluster)}  ##### END INLA CALCULATIONS
  
  
  cluster <- sort (cluster)
  cluster <- as.numeric (cluster)
  clusnum <- length (cluster)
  
  #######detected clusters
   if (clusnum > 0 ) { mapview(polygons) + mapview(polygons [cluster, ],  layer.name="Detected Cluster" ,col.regions=3)
   }else if (clusnum == 0 ){ mapview(polygons)}
