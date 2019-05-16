  rm(list=ls()) # clear environment
  cat("\014")   # clear console
  setwd("~/OneDrive - uni-duisburg-essen.de/Mastercode")
  
  Version <- "Display results Mastertabelle"
  # Version prepared from Final INLA Generic16 und FINAL Besag Newell
  
  
  # Load libraries
  library(openxlsx)
  library (data.table) # merge lists
  require (tidyverse)
  require(gplots)
  library (gridExtra) # for arrangements of several plots
  library (RColorBrewer)  # Für Color PAletten
  
  Datum <- Sys.Date()
  start_time <- Sys.time()
  
  # Read Mastertabelle
  Master = read.xlsx("Data/Mastertabelle immer nur die Blätter von Einzelner Master einstellen.xlsx", sheet=3)  # 3. Blatt gelesen
  
  # # 
  # # ### einzelne Plots mehrerer Clustertypen pro Methode  ############
  # # methode <- "INLA"
  # # methode2 <- "BN"
  # # methode3 <- "Kulldorff"
  # # inzidenz <- 0.00014 # 0.000007
  # # RanDef <- "Random"
  # # subset <- subset(Master, Inzidenz == inzidenz)#, Methode == "INLA" ) 
  # # subset <- subset(subset, Methode %in% methode)# 
  # # name <- c(
  # #   "Mean.Sensitivity", "Mean.Specificity",
  # #   "Mean.Positive.Predictive.Value", "Mean.Negative.Predictive.Value", 
  # #   "Mean.Exact.Power", "Mean.Minimum.Power", 
  # #   "Mean.Correct.Classification",  
  # #   "Mean.Pos.Diagn.Likelihood", "Mean.Neg.Diagn.Likelihood",
  # #   "Mean.False.Positive.Rate", "Mean.False.Negative.Rate"
  # #  )
  # # name <- c(
  # #   "Mean.Sensitivity"
  # # )
  # # looplength <- length(name)
  # # for (m in 1:looplength){ 
  # # Title <- paste(methode, inzidenz, name[m], RanDef) 
  # # IMAGE <- ggplot(filter(subset,  Clustertype %in% c( "1", "2", "5", "20", "50")),# c ("Cluster1", "Cluster2", "Cluster3","Cluster4","Cluster5", "Cluster6", "Cluster7") ), # c( "1", "2", "5", "20", "50")), #  %>% group_by(Methode) geht nicht  
  # #        aes_string(x="Relative.Risk" ,
  # #            y=name [m] ,
  # #            color="Clustertype"))+
  # #   geom_point()+
  # #   ggtitle (Title) +
  # #   #   xlab("Relative Risk")+
  # #   #   ylab("Mean Exact Power")+
  # #   #geom_smooth(method= "auto", se=F, level = 0.95) +
  # #   geom_line()+
  # #   #geom_errorbar(aes(ymin=LCI.PPV, ymax=UCI.PPV), width=.1) +  
  # #   #scale_x_log10(limits = c(1,100)) # für logarithmisch 1 bis 10
  # #   scale_x_continuous(limits = c(0.8,2)) # für logarithmisch 1 bis 10
  # #   
  # #  plot(IMAGE)
  # #   # # Save the Figure properly
  # #    png(file =paste("Data/Figures/", Title, methode, inzidenz, RanDef, Sys.Date(), ".png", sep = ""), width= 10, height= 7.25,units= "in",res= 600,pointsize = 30)
  # #    par(mar= c(5, 5, 2, 2),  xaxs= "i",yaxs= "i", cex.axis = 1, cex.lab  = 1)
  # #    plot(IMAGE)
  # #    dev.off()  
  # # }
  # # 
  # # 
  # # 
  # # # hier die drei Methoden zusammen  in jeweils einem Plot ganz hübsch
  # # methode1 <- "INLA"
  # # methode2 <- "BN"
  # # methode3 <- "Kulldorff"
  # # inzidenz <- 0.00014 # 0.000007
  # # RanDef <- "Random"
  # # type <- c("Cluster3")
  # # #name <- "Mean.Pos.Diagn.Likelihood"
  # # name <- "Mean.Positive.Predictive.Value"
  # # subset1 <- subset(Master, Inzidenz == inzidenz)#, Methode == "INLA" ) 
  # # subset1 <- subset(subset1, Methode %in% methode1)# 
  # # subset2 <- subset(Master, Inzidenz == inzidenz)#, Methode == "INLA" ) 
  # # subset2 <- subset(subset2, Methode %in% methode2)# 
  # # subset3 <- subset(Master, Inzidenz == inzidenz)#, Methode == "INLA" ) 
  # # subset3 <- subset(subset3, Methode %in% methode3)# 
  # # Title <- paste(inzidenz, name, RanDef) 
  # # IMAGE <- ggplot()+
  # #   geom_point()+
  # #   ggtitle (Title) + theme_bw()+
  # #   #   xlab("Relative Risk")+
  # #   #   ylab("Mean Exact Power")+
  # #   #geom_smooth(method= "auto", se=F, level = 0.95) +
  # #    geom_line(data=filter(subset1,  Clustertype %in% type),# c ("Cluster1", "Cluster2", "Cluster3","Cluster4","Cluster5", "Cluster6", "Cluster7") ), # c( "1", "2", "5", "20", "50")), #  %>% group_by(Methode) geht nicht  
  # #             aes_string(x="Relative.Risk", y=name, color="Clustertype"))+
  # #     #geom_text(data = subset1, aes(label = Methode) ,x = 1, y =8, hjust = 1, vjust = 1, color="red")+
  # #     geom_text(data = subset1, aes(label = Methode), x=Inf, y=Inf,hjust = 1, vjust = 4, color="red")+
  # #   geom_line(data=filter(subset2,  Clustertype %in% type),# c ("Cluster1", "Cluster2", "Cluster3","Cluster4","Cluster5", "Cluster6", "Cluster7") ), # c( "1", "2", "5", "20", "50")), #  %>% group_by(Methode) geht nicht  
  # #             aes_string(x="Relative.Risk" , y=name),color="green")+  # BN
  # #       geom_text(data = subset2, aes(label = Methode), x=Inf, y=Inf,hjust = 1, vjust = 2, color="green")+
  # #   geom_line(data=filter(subset3,  Clustertype %in% type),# c ("Cluster1", "Cluster2", "Cluster3","Cluster4","Cluster5", "Cluster6", "Cluster7") ), # c( "1", "2", "5", "20", "50")), #  %>% group_by(Methode) geht nicht  
  # #             aes_string(x="Relative.Risk" ,y=name),color="blue")+ # Kulldorff
  # #   geom_text(data = subset3, aes(label = Methode) , x=Inf, y=Inf,hjust = 1, vjust = 6, color="blue")+
  # #  # scale_color_discrete( position = "right", name = "Methode", labels = c( methode1 , methode2))+
  # # 
  # # #geom_errorbar(aes(ymin=LCI.PPV, ymax=UCI.PPV), width=.1) +  
  # # scale_x_log10(limits = c(1,10))
  # # plot(IMAGE)
  # # 
# 
  ############################################################
  ##### loop über die drei Methoden zusammen
  ##### Dies hier sind die Abbildungen, die stand 14.05 im Paper sind
  ############################################################

  inzidenz <-   0.000007 # 0.000007
  lamda <- "Nephroblastoma" # wenn inzidenz 0.00014
  RanDef <- "Defined"
  type <- "Cluster7"

  methode1 <- "INLA"
  methode2 <- "BN"
  methode3 <- "Kulldorff"

  subset1 <- subset(Master, Inzidenz == inzidenz)#, Methode == "INLA" )
  subset1 <- subset(subset1, Methode %in% methode1)#
  subset2 <- subset(Master, Inzidenz == inzidenz)#, Methode == "INLA" )
  subset2 <- subset(subset2, Methode %in% methode2)#
  subset3 <- subset(Master, Inzidenz == inzidenz)#, Methode == "INLA" )
  subset3 <- subset(subset3, Methode %in% methode3)#

  subset3$Methode <- "Scan Stat"

  #name <- "Mean.Pos.Diagn.Likelihood"
  # name <- c(
  #   "Mean.Sensitivity", "Mean.Specificity",
  #   "Mean.Positive.Predictive.Value", "Mean.Negative.Predictive.Value",
  #   "Mean.Exact.Power", "Mean.Minimum.Power",
  #   "Mean.Correct.Classification",
  #   "Mean.Pos.Diagn.Likelihood", "Mean.Neg.Diagn.Likelihood",
  #   "Mean.False.Positive.Rate", "Mean.False.Negative.Rate"
  # )
  name <- c("Sensitivity", "Positive.Predictive.Value","Pos.Diagn.Likelihood" ,"Correct.Classification",
            "Specificity", "Negative.Predictive.Value", "Neg.Diagn.Likelihood",
            "Minimum.Power")
   looplength <- length(name)

  plot_list = list()
  for (m in 1:looplength) {

  Title <- paste(RanDef, type, " -", lamda)
  IMAGE <- ggplot()+ theme_light(base_size = 6)+ geom_point()+
    theme(legend.position = "none")+ # oder mit Legende in Plot: theme(legend.position = c(0.9, 0.2), legend.key = element_rect(colour = "transparent", fill = "transparent"))' keine LEgende an der Aussenseite
     xlab("log RR")+ #ggtitle (Title) + # theme(plot.title = element_text(size = 8, face = "plain"))+
    #theme(legend.text=element_text(size=8))+
    theme(panel.background = element_rect(fill = "white"),plot.margin = margin(.3, .3, .3, 0.5, "cm"),
    plot.background = element_rect(fill = "white",colour = "white",size = .1))+
    theme (axis.text=element_text(size=8), axis.title=element_text(size=8,face="plain"))+
    #geom_smooth(method= "auto", se=F, level = 0.95) +
    geom_point(data=filter(subset1,  Clustertype %in% type), shape =1, size =1, # c ("Cluster1", "Cluster2", "Cluster3","Cluster4","Cluster5", "Cluster6", "Cluster7") ), # c( "1", "2", "5", "20", "50")), #  %>% group_by(Methode) geht nicht
               aes_string(x="Relative.Risk", y=name [m] , color="Clustertype"))+
    geom_line(data=filter(subset1,  Clustertype %in% type),
              aes_string(x="Relative.Risk", y=name [m] , color="Clustertype"))+
    geom_text(data = subset1, aes(label = Methode), x=Inf, y=Inf,hjust = 1, vjust = 4, color="red", size=3)+
    geom_line(data=filter(subset2,  Clustertype %in% type),
              aes_string(x="Relative.Risk" , y=name [m] ),color="darkgreen")+  # BN
    geom_point(data=filter(subset2, Clustertype %in% type), shape =0,size =1,
              aes_string(x="Relative.Risk" , y=name [m] ),color="darkgreen")+
    geom_text(data = subset2, aes(label = Methode), x=Inf, y=Inf,hjust = 1, vjust = 2, color="darkgreen", size=3)+
    geom_line(data=filter(subset3,  Clustertype %in% type), # Kulldorff
              aes_string(x="Relative.Risk" ,y=name [m] ),color="blue2")+
    geom_point(data=filter(subset3, Clustertype %in% type), shape =5, size =1,
              aes_string(x="Relative.Risk" ,y=name [m] ),color="blue2")+
    geom_text(data = subset3, aes(label = Methode) , x=Inf, y=Inf,hjust = 1, vjust = 6, color="blue2",  size=3)+
    #geom_errorbar(aes(ymin=LCI.PPV, ymax=UCI.PPV), width=.1) +
    scale_x_log10(limits = c(1,100), breaks=seq(1,5,1))   # for log scale
    #scale_x_continuous(limits = c(1,2))                # for Cont Scale
  plot_list[[m]] = IMAGE
  #png(file =paste("Data/Figures/TEST/", Title, Sys.Date(), ".png", sep = ""), width= 10, height= 7.25,units= "in",res= 300,pointsize = 30)
  #print(plot_list[[m]])
  #dev.off()
  }

  # # mit Library gridExtra kann man mehrere plots arrangieren
  # p1 <- do.call(grid.arrange,plot_list [1:11])  #
  # png(file =paste("Data/Figures/TEST/aaa", type,"-5", RanDef, inzidenz, Sys.Date(), ".png", sep = ""), width= 10, height= 7.25,units= "in",res= 600,pointsize = 10)
  # par(mar= c(5, 5, 2, 2),  xaxs= "i",yaxs= "i", cex.axis = 1, cex.lab  = 1)
  # plot(p1)
  # dev.off()
  #


    # abspeichern als 8 er matrix: from http://r.789695.n4.nabble.com/Changing-layout-in-grid-arrange-td4706973.html
  px <- do.call(grid.arrange  , c(plot_list[as.vector(gdata::interleave(1:4, 5:8))], list(ncol = 2)))
  png(file =paste("Data/Figures/Performance Graphs/10", type , lamda, RanDef, Sys.Date(), ".png", sep = ""), width= 10, height= 7.25,units= "in",res= 600,pointsize = 10)
  par(mar= c(5, 5, 2, 2),  xaxs= "i",yaxs= "i", cex.axis = 1, cex.lab  = 1)
  par(oma=c(3,3,3,3))
  plot(px)
  dev.off()
  


  
  
  ############################################################
  ## loop für alle Random in eine Grid
  ############################################################
# 
#  # hier ist ein Problem, dass die CLustergrößen als Zahlen dann falsch sortiert werden
# # daher umlabeln als Buchstaben --> Dann richtige Reihenfolge 
# 
#  Master = read.xlsx("Data/Mastertabelle immer nur die Blätter von Einzelner Master einstellen.xlsx", sheet=4)  # 4. Blatt gelesen üfr fpr Clustergröße als TEXT
# 
#   inzidenz <-   0.000007 # 0.000007 / 0.00014
#   lamda <- "nephroblastoma" # wenn inzidenz 0.00014
#   RanDef <- "Random -together"
# 
# 
#   methode1 <- "INLA" # Palette "Reds"
#   methode2 <- "BN" # Palette Greens
#   methode3 <- "Kulldorff" # Palette BLues
#   RANDOM <- c("01", "02", "05", "10", "20", "50")
# 
#   type <- c("01", "02", "05", "10", "20", "50")
#   subsetRANDOM <- subset(Master, Inzidenz == inzidenz)
#   subsetRANDOM <- subset(subsetRANDOM, Clustertype %in% RANDOM)#
#   subsetRANDOM$Clustertype <-as.factor(subsetRANDOM$Clustertype) # damit die reihenfolge nicht 1 10 2 20 ... ist
# 
#   subset1 <- subset(subsetRANDOM, Inzidenz == inzidenz)#, Methode == "INLA" )
#   subset1 <- subset(subset1, Methode %in% methode1)#
#   subset2 <- subset(subsetRANDOM, Inzidenz == inzidenz)#, Methode == "BN" )
#   subset2 <- subset(subset2, Methode %in% methode2)#
#   subset3 <- subset(subsetRANDOM, Inzidenz == inzidenz)#, Methode == "Kulldorff" )
#   subset3 <- subset(subset3, Methode %in% methode3)#
# 
#   subset3$Methode <- "Scan Stat"
#   #
#   #palette(brewer.pal(n = 8, name = "Set1")) # colorbrewer palette 2
# 
#   #name <- "Mean.Pos.Diagn.Likelihood"
#   # name <- c(
#   #   "Mean.Sensitivity", "Mean.Specificity",
#   #   "Mean.Positive.Predictive.Value", "Mean.Negative.Predictive.Value",
#   #   "Mean.Exact.Power", "Mean.Minimum.Power",
#   #   "Mean.Correct.Classification",
#   #   "Mean.Pos.Diagn.Likelihood", "Mean.Neg.Diagn.Likelihood",
#   #   "Mean.False.Positive.Rate", "Mean.False.Negative.Rate"
#   # )
#   name <- c("Sensitivity", "Positive.Predictive.Value","Pos.Diagn.Likelihood" ,"Correct.Classification",
#             "Specificity", "Negative.Predictive.Value", "Neg.Diagn.Likelihood",
#             "Minimum.Power")
#   looplength <- length(name)
# 
#   plot_list = list()
#   for (m in 1:looplength) {
# 
#     Title <- paste(RanDef, type, " -", lamda)
#     IMAGE <- ggplot()+ theme_light(base_size = 6)+ geom_point()+
#       theme(legend.position = "right")+ # oder mit Legende in Plot: theme(legend.position = c(0.9, 0.2), legend.key = element_rect(colour = "transparent", fill = "transparent"))' keine LEgende an der Aussenseite
#       xlab("log RR")+ #ggtitle (Title) + # theme(plot.title = element_text(size = 8, face = "plain"))+
#       theme(legend.text=element_text(size=8))+
#       scale_color_brewer(palette = "Reds")+ #Reds/Blues/Greens
#       theme(panel.background = element_rect(fill = "white"),plot.margin = margin(.3, .3, .3, 0.5, "cm"),
#             plot.background = element_rect(fill = "white",colour = "white",size = .1))+
#       theme (axis.text=element_text(size=8), axis.title=element_text(size=8,face="plain"))+
#       #geom_smooth(method= "auto", se=F, level = 0.95) +
#       geom_point(data=filter(subset1,  Clustertype %in% type), shape =1, size =1, # c ("Cluster1", "Cluster2", "Cluster3","Cluster4","Cluster5", "Cluster6", "Cluster7") ), # c( "1", "2", "5", "20", "50")), #  %>% group_by(Methode) geht nicht
#                  aes_string(x="Relative.Risk", y=name [m] , color="Clustertype"))+
#       geom_line(data=filter(subset1,  Clustertype %in% type),
#                 aes_string(x="Relative.Risk", y=name [m] , color="Clustertype"))+
#        geom_text(data = subset1, aes(label = Methode), x=Inf, y=Inf,hjust = 1.1, vjust = 10, color="#990000", size=3)+  # color für INLA #990000 /BN 336633/ Sat Scan 000099
#      # scale_color_brewer(palette = "Reds")+
#      #  geom_line(data=filter(subset2,  Clustertype %in% type),
#      #            aes_string(x="Relative.Risk" , y=name [m] ),color="Clustertype")+  # BN
#       # geom_point(data=filter(subset2, Clustertype %in% type), shape =0,size =1,
#       #           aes_string(x="Relative.Risk" , y=name [m] ),color="darkgreen")+
#       # geom_text(data = subset2, aes(label = Methode), x=Inf, y=Inf,hjust = 1, vjust = 2, color="darkgreen", size=3)+
#      # scale_color_brewer(palette = "Blues")+
#       # geom_line(data=filter(subset3,  Clustertype %in% type), # Kulldorff
#       #         aes_string(x="Relative.Risk" ,y=name [m] ),color="blue2")+
#       # geom_point(data=filter(subset3, Clustertype %in% type), shape =5, size =1,
#       #           aes_string(x="Relative.Risk" ,y=name [m] ),color="blue2")+
#       # geom_text(data = subset3, aes(label = Methode) , x=Inf, y=Inf,hjust = 1, vjust = 6, color="blue2",  size=3)+
#     #geom_errorbar(data=filter(subset1,  Clustertype %in% type), aes(x="Relative.Risk",ymin=LCI.Sens, ymax=UCI.Sens), width=.1) +
#     scale_x_log10(limits = c(1,5), breaks=seq(1,5,1))   # for log scale
#     #scale_x_continuous(limits = c(1,2))                # for Cont Scale
#     plot_list[[m]] = IMAGE
#     # einzelne Files abspeichern
#     #png(file =paste("Data/Figures/TEST/", Title, Sys.Date(), ".png", sep = ""), width= 10, height= 7.25,units= "in",res= 300,pointsize = 30)
#     #print(plot_list[[m]])
#     #dev.off()
#   }
# 
#   # abspeichern als 8 er matrix: from http://r.789695.n4.nabble.com/Changing-layout-in-grid-arrange-td4706973.html
#   px <- do.call(grid.arrange  , c(plot_list[as.vector(gdata::interleave(1:4, 5:8))], list(ncol = 2)))
#   png(file =paste("Data/Figures/Performance Graphs/", type,"-log-", RanDef, "-lambda", inzidenz, Sys.Date(), ".png", sep = ""), width= 10, height= 7.25,units= "in",res= 600,pointsize = 10)
#   par(mar= c(5, 5, 2, 2),  xaxs= "i",yaxs= "i", cex.axis = 1, cex.lab  = 1)
#   par(oma=c(3,3,3,3))
#   plot(px)
#   dev.off()

