    rm(list=ls()) # clear environment
    cat("\014")   # clear console
    # set user specific wd 
    setwd("~/OneDrive - uni-duisburg-essen.de/Mastercode")
    # files needed (in directory /Mastercode/data) 
    # gadm36_DEU_2.cpg; gadm36_DEU_2.dbf; gadm36_DEU_2.prj; gadm36_DEU_2.shp; gadm36_DEU_2.shx
    # cluster.xlsx 
    ######################################################################
    ####################### Master Complete Code #########################  
    Version <- "Master from Scratch 10"
    ######################################################################
    
    # visualisation of Monte Carlo Bias
    
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
    Method <- "INLA" # "BN" , "Kulldorf", "INLA"
    #simulationsvector <- c(1,3)
    # increase number of simulastions 
    simulationsvector <- c(seq(1, 40, by=1))#, seq(50, 300, by=50)),seq(500, 1000, by=100),2000,2500, 3000, 5002)#
    k <-   NA # or e.g. 50/ALL 5/Wilms  for Besag Newell
    alpha.level <- NA#  0.01/ Besag, 0.05/ Kullforf  (NA for INLA)
    pop.upper.bound <- NA # NA or 0.1 for Kulldorf
    n.simulations <- NA # NA or 99  for Kulldorf
    plot <- F # Plot Histograms of Monte Carlo Distribution of Lambda (Kulldorff only )
    ALLorWILMS <- "ALL"
    lambda = 14/100000 # 14/100000 für all neoplasms, 0.7/100000 for Wilms Tumors  (15.99/0.86 for real data )
    # relative risk in risk clusters
   # ClusterRR <- c(1,1.1,1.2, 1.3,1.4, 1.5, 2, 5,10,100)
     #ClusterRR <- c(1,2,3,4,5,6,7,8,9) # for shorter Test runs
    ClusterRR <- 2 # for loop over number of simulations
    # aggregation period  (years)
    aggregate <- 10
    
    ###### which kind of risk-clusters is being formed (random size/shape or defined)
    RANDOMorDEFINED <- "defined"
     Clustertype <-c ("Cluster3")#, "Cluster2")#, "Cluster6")#, "Cluster2")
     #Clustertype <-c ("Cluster1", "Cluster2", "Cluster3","Cluster4","Cluster5", "Cluster6", "Cluster7")  # for defined risk-clusters
    #Clustertype <-c(1,2)
    #Clustertype <-c(1,2,5,10,20,50) # for randomly sized/shaped clusters
    
    clustervector <- c (1:402) # needed several times (number of polygons)
    CRR <- length (ClusterRR) # for Loop over Relative Risiken
    CLT <- length (simulationsvector) # for Loop over Random Clusters 
    
    ##################################################################
    #Performance Measures:
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
    list_of_clustervectors <- list() # detected clusters 
    list_of_clustervectorsCR <- list()
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
    
    # Loop for Cluster-Type
    for (cr in 1: CLT)
    {
    # Loop for RR-Risk Levels
    for(q in 1:CRR)
    {
    # Number of simulations per RR-Level
      set.seed(111) # set this seed for various "CAS10" if only 1 x drawn
    for (m in 1:simulationsvector[cr]){ 
    simulations <-   simulationsvector[cr]
      ## Statement , not random: = defined --> 2. Statement 
      if (RANDOMorDEFINED == "random") {
        
        #### Start of RANDOM Cluster Formation     
        
        seed <- sample(1:999999999, 1)
        
        # Draw Risk-Cluster of connected Polygons (star=1: all Polygons around the first one (does not work with larger than 8), type=1: Exact numnber of polygons is drawn ("Clustertype")
        mRev <- sampleAdj(adj,  Clustertype, n=1, maxTry=10000, star=0, type=1, seed=seed)
        
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
        randomcluster <- data.frame("ID" = 1:nrow(randomcluster), randomcluster)
        polygons@data <- polygons@data %>%  left_join(randomcluster,     by ="ID") 
      
        ####### Defined Clusters into the mastertable 
       polygons$Cluster <- polygons$randomcluster
        
        ####### end of "Random Cluster Generator"
      } else {
        
        ####### Defined Clusters into the mastertable 
        polygons$Cluster <- polygons[[Clustertype [1]]]
      } 
  
      
      #############################################################  
      ##### COMMON TRUNK (random AND defined Clusters) ############
      #############################################################
      
      
      # calculate Relative Risk for each Polygon
      polygons@data$RR <- ifelse (polygons$Cluster== 0, 1,ClusterRR[q])  # hier eigentlich ClusterRR [i]) -..
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
        
        ## Kulldorff using Binomial likelihoods
        # binomial <- kulldorff(geo2, cases, population, NULL, pop.upper.bound, 
        #                      n.simulations, alpha.level, plot)
        # primary Cluster
        #cluster <- binomial$most.likely.cluster$location.IDs.included
        #  secondary clusters 
        #binomial$secondary.clusters
        # x <- sapply(binomial$secondary.clusters,"[" , "location.IDs.included")
        # y<- unlist(x, recursive = TRUE, use.names = TRUE)
        # remove duplicates
        # cluster <- unique (c(y, cluster) ) 
    
        
        }   # end condition Kulldorff    
    
      else if (  Method == "INLA" ) {
    
    ###############################
    ## INLA Model Calculations   ##
    ###############################
      
      # Calculate expected cases for INLA Model
      polygons@data$Erwartet <- polygons@data$Population*lambda*aggregate 
      #formula <- Cases ~  f (ID, model="bym", graph=adjacency.adj, scale.model = TRUE, hyper=list (prec.unstruct=list (prior="loggamma", param=c(1,0.001)), prec.spatial=list(prior="loggamma", param=c(1,0.001)))) #+ f(IID,model="iid")
      #INLACluster<- inla (formula,family ="poisson", data=polygons@data, E=Erwartet, control.compute = list (dic=TRUE))
      # Get 95% Credibility-Invervall > 1 
      #INLACluster <- INLACluster$summary.fitted.values # table with RR Risk Estimates Posteriors!!
      #INLACluster$ID <- 1:nrow(INLACluster) # hier same ID´s as districts
      #INLACluster <- INLACluster[INLACluster$`0.025quant`>1,] # get only districts with credibility intervals > 97.5 perc)
      #cluster  <- sapply(INLACluster$ID,"[")
      #cluster<- unlist(cluster, recursive = TRUE, use.names = TRUE)# remove duplicates
      
      ############### for test run purposes to speeds up  #################
      #### take out INLA calculation and use dummy cluster (below) as output
      cluster <- c(12,3,33,44,65) # dummy cluster (real simulations use next line)
      #cluster <- unique (cluster)
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
                           mean (sensitivity), sd (sensitivity), mean(sensitivity)-(sd(sensitivity)/sqrt(simulations)),mean(sensitivity)+(sd(sensitivity)/sqrt(simulations)),
                           mean(specificity), sd(specificity) , mean(specificity)-(sd(specificity)/sqrt(simulations)),mean(specificity)+(sd(specificity)/sqrt(simulations)),
                           mean (PPV), sd(PPV), mean(PPV)-(sd(PPV)/sqrt(simulations)), mean(PPV)+(sd(PPV)/sqrt(simulations)),
                           mean (NPV), sd (NPV),mean(NPV)-(sd(NPV)/sqrt(simulations)),mean(NPV)+(sd(NPV)/sqrt(simulations)),
                           mean (EP), sd (EP), mean(EP)-(sd(EP)/sqrt(simulations)), mean(EP)+(sd(EP)/sqrt(simulations)),
                           mean (MP),sd (MP), mean(MP)-(sd(MP)/sqrt(simulations)), mean(MP)+(sd(MP)/sqrt(simulations)),
                           mean (CC), sd (CC),mean(CC)-(sd(CC)/sqrt(simulations)), mean(CC)+(sd(CC)/sqrt(simulations)),
                           mean (PRVC), sd (PRVC), mean(PRVC)-(sd(PRVC)/sqrt(simulations)),mean(PRVC)+(sd(PRVC)/sqrt(simulations)),
                           mean (DLRPOS), sd (DLRPOS),mean(DLRPOS)-(sd(DLRPOS)/sqrt(simulations)),mean(DLRPOS)+(sd(DLRPOS)/sqrt(simulations)),
                           mean (DLRNEG), sd (DLRNEG),mean(DLRNEG)-(sd(DLRNEG)/sqrt(simulations)),mean(DLRNEG)+(sd(DLRNEG)/sqrt(simulations)),
                           sd (RRSimGes)/sqrt (simulations),  sd (RRSimGes)/sqrt (simulations)/ sd(RRSimGes)*100,
                           aggregate,
                           lambda,
                           k,
                           alpha.level,
                           pop.upper.bound,
                           Version,
                           Datum, 
                           mean (FPR), sd (FPR),mean(FPR)-(sd(FPR)/sqrt(simulations)),mean(FPR)+(sd(FPR)/sqrt(simulations)),
                           mean (FNR), sd (FNR),mean(FNR)-(sd(FNR)/sqrt(simulations)),mean(FNR)+(sd(FNR)/sqrt(simulations)),
                           mean ( RRBIAS), sd (RRBIAS),mean(RRBIAS)-(sd(RRBIAS)/sqrt(simulations)),mean(RRBIAS)+(sd(RRBIAS)/sqrt(simulations)))
    # Reset Cluster/Randomcluster 
    polygons$randomcluster <- NULL
    polygons$Cluster <- NULL
    }
    list_of_riskvectorsCR  [[cr]] <- list_of_riskvectors
    list_of_collvectorsCR [[cr]] <- list_of_collvectors
    list_of_clustervectorsCR [[cr]] <- list_of_clustervectors
    PerformanceComplete [[cr]]<- Performance
    } # Loop End for (m ... (m simulations of different Clusters ) 
    } #loop end (Random Clusters Generator)
    
    ########################################
    #view Clusters in R-mapview (web based)
    ########################################
    #######risk clusters
    #mapview(polygons) + mapview(polygons [risk, ], layer.name="Risk Cluster" , col.regions=2)
    #######detected clusters
    #mapview(polygons) + mapview(polygons [cluster, ],  layer.name="Detected Cluster" ,col.regions=3) 
    ###### risk plus detected clusters
    #mapview(polygons) + mapview (polygons[ risk,],layer.name="Risk Cluster")  +mapview (polygons[ cluster,], layer.name="Detected Cluster" , col.regions=3)
    #####centroids of risk clusters
    # mapview(polygons) + mapview(centroids [c(risk), ]) # das hier ist der RandomCluster (gleiches wie Risk)
    
    
    # Correctly detected (green), false positive (red) missed (blue) 
    # if (falseposnum > 0 ) { 
    # if (trueposnum > 0 ){ mapview(polygons) + mapview(polygons [c(risk), ] ,layer.name="Not Detected Risk Cluster (False Negative)" ,  col.regions=4) +mapview(polygons [truepos, ], layer.name="Correctly Detected Cluster (True Positive)" , col.regions=3) + mapview(polygons [falsepos, ], layer.name="Falsely Detectecd Cluster (False Positive)" , col.regions=2)
    # } else {
    #  mapview(polygons) +  mapview(polygons [c(risk), ], layer.name="Not Detected Risk Cluster (False Negative)r" ,  col.regions=4) +mapview(polygons [falsepos, ], layer.name="Falsely Detectecd Cluster (False Positive)" , col.regions=3) 
    #} }else {if (trueposnum > 0 ){ mapview(polygons) + mapview(polygons [c(risk), ], layer.name="Not Detected Risk Cluster (False Negative)" , col.regions=4) +mapview(polygons [truepos, ],  layer.name="Correctly Detected Cluster (True Positive)" ,col.regions=3) 
    #} else {
    #  mapview(polygons) +  mapview(polygons [c(risk), ] ,layer.name="Not Detected Risk Cluster (False Negative)" ,  col.regions=4)
    #  }  }
    
    
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
    
    
    ########################################################
    ##Save and Display the results graphically ############
    ########################################################
    # For Risk - Clusters (to visualize the distribution of randomly drawn risk - polygons. (not relevant for "defined" clusters)
    # best is only use ONE RR Level and ONE Clustertype (that way all the settings work just fine. Otherways the values within the [] brackets have to be changed )
    # construct list of number/percentage of draws of risk clusters (only relevant for "random" Clusters)
    # take all drawn risk polygons from one clustertype (e.g.[1] ) and put into one long vector
    LORV <- unlist(list_of_riskvectorsCR [1], recursive = T, use.names = TRUE) 
    # hist(LORV)
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
    
    #export to Open XLXS 
    # wb <- createWorkbook()
    # addWorksheet(wb,"RandomDrawn")
    # writeDataTable( wb, "RandomDrawn", x = polygons@data)
    # saveWorkbook(wb, "Data/RandomDrawn.xlsx", overwrite = T)
    # hist (polygons@data$RiskPercentage, breaks=100)
    
    
    ############################################
    # Plot number of Draws in Simulation  ######
    ############################################
    # for the Title: Set  ClusterRR [xx] and "Clustertype [xx] according to wich one is being displayed
    # best is only use ONE RR Level and ONE Clustertype (that way all the settings work just fine. Otherways the values within the [] brackets have to be changed )
    # cols= rev(heat.colors(20, 0.6))
    # p1<-  spplot(polygons, 'RiskFrequency', main=paste ("Risk Cluster Frequency (n)","","\n",simulations, "Simulations,", "RR", ClusterRR [1], "\nClustertype", Clustertype[1], "\nVersion", Version), colorkey=T,cuts= 10, col.regions=cols)
    # p1
    # p2 <- spplot(polygons, "RiskPercentage", col.regions=cols, main=paste("Risk Cluster Frequency (%)","","\n",simulations, "Simulations,", "RR", ClusterRR [1], "\nClustertype", Clustertype [1], "\nVersion", Version),cuts= 10)
    # p2   
    # p3<-  spplot(polygons, 'ClusterFrequency', main=paste ("Detected Cluster Frequency (n) in","","\n",simulations, "Simulations,","RR", ClusterRR [1],  "\nClustertype", Clustertype [1], "\nVersion", Version), colorkey=T,cuts= 10, col.regions= rev(heat.colors(20, 0.6)))
    # p3
    # p4 <- spplot(polygons, "ClusterPercentage", col.regions=cols, main=paste ("Detected Cluster Frequency in %","","\n",simulations, "Simulations,", "RR", ClusterRR[1],  "\nClustertype", Clustertype [1], "\nVersion", Version),cuts= 10)
    # p4
    
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
    
    # HITS for  für Choropleth: 1 CLuster Type, 1 RR --> Use Column at the end of saved table for choropleth map (code see next page)
    # (graphic presenation with "Chropleth No Donuts KReisebene2")
    # if not needed: ###### for the next 10 Rows
    # HITSTABLE <-polygons@data$ClusterPercentage
    # hist  (HITSTABLE)
    # HITSTABLE <- as.data.frame(HITSTABLE) %>% mutate(ID = 1:402)
    # ClusterChoro = read.xlsx("Data/ClusterChoro.xlsx")  
    # ClusterChoro <-  ClusterChoro %>%  left_join(HITSTABLE,     by ="ID")
    # export to Open XLXS 
    # wb <- createWorkbook()
    # addWorksheet(wb,"RR")
    # writeDataTable( wb, "RR", x = ClusterChoro)
    # saveWorkbook(wb, "Data/RR.xlsx", overwrite = T)
    
    # Cases and RR for Choropleth (cases are multiplied by "aggregate") so if 1 year needed: Change Seed each time!
    # (graphic presenation with "Chropleth No Donuts Kreisebene2")
    # if not needed: ###### for the next 9 Rows
    # polygons$RRSIM <- polygons$Cases/(polygons$Erwartet*aggregate) # simulated RR!!
    # polygons$CrudeIncidence <- (polygons$Cases/10)/(polygons$Population)*1000000 # simulate crude incidence
    # RRTable <- subset (polygons, select=c("RRSIM", "ID", "CrudeIncidence"))
    # RRTable <- RRTable@data
    # hist  (RRTable$RRSIM)
    # hist  (RRTable$CrudeIncidence)
    # ClusterChoro = read.xlsx("Data/ClusterChoro.xlsx")   # beim ersten mal ClusterChoro !! HAS TO BE SORTED BY ID!!  laden --> dann AUF RR ÄNDERN!!
    # ClusterChoro <-  ClusterChoro %>%  left_join(RRTable, by ="ID")
    # wb <- createWorkbook()
    # addWorksheet(wb,"RR")
    # writeDataTable( wb, "RR", x = ClusterChoro)
    # saveWorkbook(wb, "Data/RR.xlsx", overwrite = T)
    