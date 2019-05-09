      rm(list=ls()) # clear environment
      cat("\014")   # clear console
      setwd("~/OneDrive - uni-duisburg-essen.de/Mastercode")
      
      ####################### Master Complete Code #########################  
      Version <- "Chropleth No Donuts Kreisebene3"
      # 3: Save BMPs right size
      # 4 scale fill gradient rot --> Weisss --> Grün
      
      
      library(raster)
      library(openxlsx)
      library('mapview')
      require (tidyverse)
      library(ggplot2)
      library(rgeos)
      library(maptools)
      library(gpclib) 
      library (scales) # for muted
      library(ggpolypath) # wird glaube ich nciht gebraucht
      
      
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
      # CLuster  Tabele has to be in the order from ID 1 (Alb-Donau-Kreis)
      ClusterID = read.xlsx("Data/Cluster0Choro.xlsx")   %>% mutate(ID = 1:402) # change name of xlsx file as needed
      
    
      #######################################
      # Merge population and geographic data  (left join from didyverse!! (merge gets the wrong ID order!!!!))  
      polygons@data <- polygons@data %>%  left_join(ClusterID,     by ="CC_2") %>% mutate(ID = 1:402)
      # delete unwanted columns
      
      # delete unwanted columns
      polygons@data[c("GID_0","NAME_0","GID_1","NAME_1" ,"NL_NAME_1","ENGTYPE_2" , "HASC_2","VARNAME_2","NL_NAME_2","TYPE_2", "ID.ab.Flensburg", "ID.ab.Alb.Donau", "Case","Mbis3" ,"Mbis6","Mbis10","Mbis15","Fbis3", "Fbis6", "Fbis10", "Fbis15")] <- NULL
      
      
      ## für GGPLOT2 --> Fortify
      fort <- fortify(polygons, region = "CC_2")
      #colnames(fort)[which(names(fort) == "id")] <- "ID"
      colnames(ClusterID)[which(names(ClusterID) == "CC_2")] <- "id"
      
      
      ## merge so that Data are in fort
      fort <- merge(fort,ClusterID, by="id")
      
      # input has to be a fortified data.frame (Function to detect holes...)
      gghole <- function(fort){
        poly <- fort[fort$id %in% fort[fort$hole,]$id,]
        hole <- fort[!fort$id %in% fort[fort$hole,]$id,]
        out <- list(poly,hole)
        test <- list(poly,hole)
        return(out)
      } 
        
    Title <- "Crude Incidence Nephroblastoma \nSimulated 2"
  
      ## coord_map()+    geom_path()+ rausgenommen--> Weiss nciht . 
      ### das hier ist das mit dem Besten Abbild.- Die "Coors Equal machen die Distortion richtig. 1.63 ist ungefähr richitg
      
     IMAGE <-  ggplot(fort, aes(x=long, y=lat, group=group)) +
                ggtitle(label = Title)+ #ggf: , subtitle = "in Percent")+
        geom_polygon(data=gghole(fort)[[1]],aes(fill=CrudeIncidenceNB2),colour="grey40")+
        geom_polygon(data=gghole(fort)[[2]],aes(fill=CrudeIncidenceNB2),colour="grey40")+
         # hiermit geht die Colorbar weg : theme(legend.position = "none")
       # (optionally). Call by name
        #         geom_polygon(data=gghole(map.df)$poly,aes(fill=poverty),colour="grey50")+
        #         geom_polygon(data=gghole(map.df)$hole,aes(fill=poverty),colour="grey50")+
       # For Simulated RR
       #guides(fill=guide_colorbar(title="Relative Risk"),barwidth = 0.1, barheight = 50) +
       # scale_fill_gradientn(colours=rev(heat.colors (10,0.6)), limits = c(0, 6.1),  breaks = c(0,2,4,6))+ # für die RR´s Darstellung 
       # for HITS
       guides(fill=guide_colorbar(title="Crude-\nIncidence \nWT"),barwidth = 0.2, barheight = 50) + # nbin=20
      # scale_fill_gradientn(colours=rev(heat.colors (20,0.8)), limits = c(0, 100),  breaks = c(0,20,40,60,80, 100))+
      # For Right Cluster
        #scale_fill_gradientn(colours=rev(heat.colors (20,0.8)), limits = c(0, 1))+
       # For  Cluster 0
      #scale_fill_gradientn(colours=rev(heat.colors (20,0.8)), limits = c(0, 6),  breaks = c(0,1,2,3,4, 5))+
      # für Scale Fill von Grün über weiss nach rot (z.B. GRafik RR 2 CLuster 1 Aggregated)
       #scale_fill_gradient2( low = "#66FF33", high = "#FF3300", mid = "white", guide="legend", midpoint = 1, limits = c(0, 6))+
       # für Krebsregisterdaten ALL
     #  scale_fill_gradientn(colours=rev(heat.colors (20,1)), limits = c(0, 300),  breaks = c(0,100,150,200,300))+     
      # für Krebsregisterdaten WILMS
      scale_fill_gradientn(colours=rev(heat.colors (20,1)), limits = c(0, 50),  breaks = c(0,10,20,30,40))+
    # scale_fill_gradientn(colours = (palette = "RdPu")), limits = c(0, 300),  breaks = c(0,100,150,200,300))+
    # scale_color_distiller(palette = "YlOrRd")+
      labs(x="",y="")+
        coord_equal(ratio=1.63)+
        theme(axis.line=element_blank(),
              axis.text.x=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              panel.background=element_blank(),
              panel.border=element_blank(),
              panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),
              legend.title=element_text(size=20), # oder legend.title=element_blank(),
              legend.text=element_text(size=20),# oder legend.text=element_blank(),
              legend.key.height = unit (40, "pt"), # color bar von oben nach unten
              legend.key.width = unit (15, "pt"), # dicke der Color bar
              plot.title = element_text(hjust=0.5), # Title in der Mitte
               plot.background=element_blank())+
            # theme(legend.position = "none") # hier ist die Legende und COlorbar weg
            theme(legend.justification=c(0.5,0), legend.position=c(1.1,0.1)) # hiermit kommt die Legende unten rechts hin
    
     # so wird die Grafik ordentlich gespeichert
     png(file =paste("Data/Figures/", Title, Sys.Date(), ".png", sep = ""), width= 7.25, height= 7.25,units= "in",res= 600,pointsize = 30)
     par(mar= c(5, 5, 2, 2),  xaxs= "i",yaxs= "i", cex.axis = 1, cex.lab  = 1)
     plot(IMAGE)
     dev.off()  
     
     
     # um Quantilen für die Color Bar festzulegen  irgendwie so 
    # ClusterID$qnt<- cut(ClusterID$Rohinzidenz1 , breaks=quantile(ClusterID$Rohinzidenz1),
     #                    labels=1:4, include.lowest=TRUE)
     # check ranges
    # tapply(ClusterID$Rohinzidenz1  , ClusterID$qnt, range)
     # und dann würde man die Faktoren ClusterID$qnt benutzen für die Colorbar
     