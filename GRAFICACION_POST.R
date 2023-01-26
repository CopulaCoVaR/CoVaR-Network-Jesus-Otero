if (0) {
  # Paquetes  ----------------------------------------------------------------
  library(ichimoku) # Para transformar de xts a df
  library(xts)
  library(stargazer)
  library(circlize)
  library(car)
  library(naniar)
  library(readxl)
  library(fpp3)
  library(kldtools)
  library(patchwork)
  library(vars)
  library(forecast)
  library(copula)
  library(ggplot2)
  library(tseries)
  library(rugarch)
  library(moments)
  library(aTSA)
  library(FinTS)
  library(VineCopula)
  library(VC2copula)
  library(sgt)
  library(pracma)
  library(quantmod)
  library(PerformanceAnalytics)
  library(nloptr)
  library(expm)
  library(vars)
  library(lubridate)
  library(plotly)
  library(rgl)
  library(processx)
  library(BigVAR)
  library(readxl)
  library(tidyquant)
  library(reshape2)
  library(igraph)
  library(patchwork)
} #Packages
setwd("C:/Users/maico/OneDrive - Universidad Nacional de Colombia/BanRep/Value at Risk/CoVaR-Network-Jesus-Otero")
wd = getwd()                                # Carpeta de trabajo adaptable al proyecto, se modifica automáticamente en cada computador.
Resultados <<- paste0(wd,'/Resultados')     #<<<<<--- Capeta de resultados qué depende deldirectorio de trabajo. 
n.ahead.connectedness=10                    # Step-ahead del GFEVD para la red. 

# Carga de datos para cada país. 

country=c("USA-Colombia", "USA-Brazil",
          "USA-Guatemala", "USA-Indonesia",
          "USA-Mexico", "USA-Uganda", 
          "USA-Vietnam")

plot.class=c("net","star","sphere", "circle")[1]
if(plot.class=="net")plot.class=NULL

for (kk in country) {
  
if (kk=="USA-Colombia") {
  #BigVAR.Optimo
  load(paste0(wd,"/Resultados/Colombia/R_files/BigVAR-Optimo-USA...Colombia-2023-01-10.R"), verbose=TRUE)
  # Copulas seleccionadas
  load(paste0(wd,"/Resultados/Colombia/R_files/Cop_seleccionadas_USA...Colombia_2023-01-10"), verbose=TRUE)
  #CoVaR Data
  load(paste0(wd,"/Resultados/Colombia/R_files/CoVaR_data_USA...Colombia_2023-01-10"), verbose=TRUE)
  #GIRF
  load(paste0(wd,"/Resultados/Colombia/R_files/GIRF.USA...Colombia"), verbose=TRUE)
  #GFEVD
  load(paste0(wd,"/Resultados/Colombia/R_files/GFEVD.USA...Colombia"), verbose=TRUE)
  #Network
  load(paste0(wd,"/Resultados/Colombia/R_files/Dynamic_Network_USA...Colombia.R"),verbose=TRUE)
  
}
if (kk=="USA-Brazil")   {
  #BigVAR.Optimo
  load(paste0(wd,"/Resultados/Brazil/R_files/BigVAR-Optimo-USA...Brazil-2023-01-11.R"), verbose=TRUE)
  # Copulas seleccionadas
  load(paste0(wd,"/Resultados/Brazil/R_files/Cop_seleccionadas_USA...Brazil_2022-12-23"), verbose=TRUE)
  #CoVaR Data
  load(paste0(wd,"/Resultados/Brazil/R_files/CoVaR_data_USA...Brazil_2023-01-11"), verbose=TRUE)
  #GIRF
  load(paste0(wd,"/Resultados/Brazil/R_files/GIRF.USA...Brazil"), verbose=TRUE)
  #GFEVD
  load(paste0(wd,"/Resultados/Brazil/R_files/GFEVD.USA...Brazil"), verbose=TRUE)
  #Network
  load(paste0(wd,"/Resultados/Brazil/R_files/Dynamic_Network_USA...Brazil.R"),verbose=TRUE)
  
}
if (kk=="USA-Guatemala"){
  #BigVAR.Optimo
  load(paste0(wd,"/Resultados/Guatemala/R_files/BigVAR-Optimo-USA...Guatemala-2023-01-12.R"), verbose=TRUE)
  # Copulas seleccionadas
  load(paste0(wd,"/Resultados/Guatemala/R_files/Cop_seleccionadas_USA...Guatemala_2022-12-24"), verbose=TRUE)
  #CoVaR Data
  load(paste0(wd,"/Resultados/Guatemala/R_files/CoVaR_data_USA...Guatemala_2023-01-12"), verbose=TRUE)
  #GIRF
  load(paste0(wd,"/Resultados/Guatemala/R_files/GIRF.USA...Guatemala"), verbose=TRUE)
  #GFEVD
  load(paste0(wd,"/Resultados/Guatemala/R_files/GFEVD.USA...Guatemala"), verbose=TRUE)
  #Network
  load(paste0(wd,"/Resultados/Guatemala/R_files/Dynamic_Network_USA...Guatemala.R"),verbose=TRUE)
  
}
if (kk=="USA-Indonesia"){
  #BigVAR.Optimo
  load(paste0(wd,"/Resultados/Indonesia/R_files/BigVAR-Optimo-USA...Indonesia-2023-01-12.R"), verbose=TRUE)
  # Copulas seleccionadas
  load(paste0(wd,"/Resultados/Indonesia/R_files/Cop_seleccionadas_USA...Indonesia_2022-12-26"), verbose=TRUE)
  #CoVaR Data
  load(paste0(wd,"/Resultados/Indonesia/R_files/CoVaR_data_USA...Indonesia_2023-01-12"), verbose=TRUE)
  #GIRF
  load(paste0(wd,"/Resultados/Indonesia/R_files/GIRF.USA...Indonesia"), verbose=TRUE)
  #GFEVD
  load(paste0(wd,"/Resultados/Indonesia/R_files/GFEVD.USA...Indonesia"), verbose=TRUE)
  #Network
  load(paste0(wd,"/Resultados/Indonesia/R_files/Dynamic_Network_USA...Indonesia.R"), verbose=TRUE)
  
}
if (kk=="USA-Mexico")   {
  #BigVAR.Optimo
  load(paste0(wd,"/Resultados/Mexico/R_files/BigVAR-Optimo-USA...Mexico-2023-01-13.R"), verbose=TRUE)
  # Copulas seleccionadas
  load(paste0(wd,"/Resultados/Mexico/R_files/Cop_seleccionadas_USA...Mexico_2022-12-28"), verbose=TRUE)
  #CoVaR Data
  load(paste0(wd,"/Resultados/Mexico/R_files/CoVaR_data_USA...Mexico_2023-01-13"), verbose=TRUE)
  #GIRF
  load(paste0(wd,"/Resultados/Mexico/R_files/GIRF.USA...Mexico"), verbose=TRUE)
  #GFEVD
  load(paste0(wd,"/Resultados/Mexico/R_files/GFEVD.USA...Mexico"), verbose=TRUE)
  #Network
  load(paste0(wd,"/Resultados/Mexico/R_files/Dynamic_Network_USA...Mexico.R"),verbose=TRUE)
  
}
if (kk=="USA-Uganda")   {
  #BigVAR.Optimo
  load(paste0(wd,"/Resultados/Uganda/R_files/BigVAR-Optimo-USA...Uganda-2023-01-13.R"), verbose=TRUE)
  # Copulas seleccionadas
  load(paste0(wd,"/Resultados/Uganda/R_files/Cop_seleccionadas_USA...Uganda_2022-12-30"), verbose=TRUE)
  #CoVaR Data
  load(paste0(wd,"/Resultados/Uganda/R_files/CoVaR_data_USA...Uganda_2023-01-13"), verbose=TRUE)
  #GIRF
  load(paste0(wd,"/Resultados/Uganda/R_files/GIRF.USA...Uganda"), verbose=TRUE)
  #GFEVD
  load(paste0(wd,"/Resultados/Uganda/R_files/GFEVD.USA...Uganda"), verbose=TRUE)
  #Network
  load(paste0(wd,"/Resultados/Uganda/R_files/Dynamic_Network_USA...Uganda.R"),verbose=TRUE)
  
}
if (kk=="USA-Vietnam")  {
  #BigVAR.Optimo
  load(paste0(wd,"/Resultados/Vietnam/R_files/BigVAR-Optimo-USA...Vietnam-2023-01-08.R"), verbose=TRUE)
  # Copulas seleccionadas
  load(paste0(wd,"/Resultados/Vietnam/R_files/Cop_seleccionadas_USA...Vietnam_2023-01-07"), verbose=TRUE)
  #CoVaR Data
  load(paste0(wd,"/Resultados/Vietnam/R_files/CoVaR_data_USA...Vietnam_2023-01-08"), verbose=TRUE)
  #GIRF
  load(paste0(wd,"/Resultados/Vietnam/R_files/GIRF.USA...Vietnam"), verbose=TRUE)
  #GFEVD
  load(paste0(wd,"/Resultados/Vietnam/R_files/GFEVD.USA...Vietnam"), verbose=TRUE)
  #Network
  #load(,verbose=TRUE)
  
}


# Network -----------------------------------------------------------------
if (1) {
  Network.plot.data=Connectedness(GFEVD     =GFEVD.res, Title=paste0("Net Pairwise directional connectedness","\n" ,"Stressed serie: ",kk), 
                                  node.size =1.5,   node.label.size=0.7, Q1='95%', Q2='90%', Q3='85%', 
                                  arrow.size=0.6,  layout=plot.class, n.ahead = n.ahead.connectedness, 
                                  edge.width.scale=2, pdf.name.serie=kk) 
  graphics.off()
}

# CoVaR plot --------------------------------------------------------------
if(0)  {
  # Unión de datos en la misma lista.
  CoVaR_DATA=CoVaR_data
  plot.class = c('Up', 'Down', 'Both')[1]
  CoVaR_DATA$CoVaR$horizontal_line = 0
  colnames=colnames(CoVaR_DATA$CoVaRUp)
  #Gráficas
  for (i in colnames){
    if(plot.class=='Up')  range=c(0,max(CoVaR_DATA$CoVaRUp[,i]))
    if(plot.class=='Down')range=c(min(CoVaR_DATA$CoVaR[,i]),0)
    if(plot.class=='Both')range=c(min(CoVaR_DATA$CoVaR[,i]),max(CoVaR_DATA$CoVaRUp[,i]))
    pdf(file = paste0(Resultados,'/Graficas_CoVaR','_',i,'.pdf'), onefile=FALSE)
    print(plotxts(if (plot.class=='Down'|plot.class=='Both')
    { CoVaR_DATA$CoVaR[,i]} else{
      CoVaR_DATA$CoVaRUp[,i] },
    type="l",col="red", grid.col = NA, 
    ylim=range, xlab="Time", ylab="", lwd=1, main=i,format.labels="%Y", major.ticks = 'years', 
    yaxis.left=TRUE, yaxis.right=TRUE, lty='solid', cex=0.5, cex.axis=0.8)) 
    #, cex.main=2.5, cex.sub=1.5, cex.axis=1.5
    if (plot.class=='Down'|plot.class=='Both') print(lines(CoVaR_DATA$VaR[,i],         col="black", lwd=1,lty='dashed'))
    if (plot.class=='Both')                    print(lines(CoVaR_DATA$CoVaRUp[,i],     col="red",   lwd=1,lty='solid'))
    if (plot.class=='Up'|plot.class=='Both')   print(lines(CoVaR_DATA$VaRUp[,i],       col="black", lwd=1,lty='dashed'))
    print(lines(CoVaR_DATA$CoVaR$horizontal_line, col='darkgrey'))
    print(addLegend("topright", lwd=2,legend.names = c('CoVaR', 'VaR'), 
                    lty = c('solid','dashed'), col = c('red',   'black')))
    #print(title(main = Series.corrected[i], cex.main = 1.5))
    dev.off()
  }
}

# Network Dynamic 
if(0)  {
  theme=c("classic", "grey")[2]
  # Preparación
  for(i in 1:ncol(Dynamic_Network$From.Degree.Dynamic)){
    Serie.1 = colnames(Dynamic_Network$From.Degree.Dynamic[,i])
    # Datos
    to        = Dynamic_Network$To.Degree.Dynamic[,i]
    from      = Dynamic_Network$From.Degree.Dynamic[,i]
    net       = Dynamic_Network$Net.Dynamic[,i]
    data.xts  = cbind(to,from,net)
    data.plot = fortify(data.xts)
    colnames(data.plot) = c("Time","To", "From", "Net")
    data.plot[,"Time"]=as.Date(data.plot[,"Time"])
    
    # Graficación
    if(theme=="classic"){
      to.plot   = ggplot(data=data.plot, mapping=aes(x=Time, y=To))    +geom_line(colour="steelblue")+theme_classic() + labs(x = element_blank(), y = "To")  +scale_x_date(expand=c(0,0), limits=c(data.plot[1,"Time"],data.plot[nrow(data.plot),"Time"]))
      from.plot = ggplot(data=data.plot, mapping=aes(x=Time, y=From))  +geom_line(colour="steelblue")+theme_classic() + labs(x = element_blank(), y = "From")+scale_x_date(expand=c(0,0), limits=c(data.plot[1,"Time"],data.plot[nrow(data.plot),"Time"]))
      net.plot  = ggplot(data=data.plot, mapping=aes(x=Time, y=Net))   +geom_line(colour="steelblue")+theme_classic() + labs(x = element_blank(), y = "Net") +scale_x_date(expand=c(0,0), limits=c(data.plot[1,"Time"],data.plot[nrow(data.plot),"Time"]))
      plot      = to.plot + from.plot + net.plot + plot_layout(ncol=1) 

      x11()
      print(plot + plot_annotation(title = paste("Dynamic total directional connectedness of", Serie.1),
                                   subtitle = paste0("Stressed series: ",country)))
      }    
    if(theme=="grey")   {
      to.plot   = ggplot(data=data.plot, mapping=aes(x=Time, y=To))    +geom_line(colour="steelblue")+theme_gray() + labs(x = element_blank(), y = "To")  +scale_x_date(expand=c(0,0), limits=c(data.plot[1,"Time"],data.plot[nrow(data.plot),"Time"])+2)
      from.plot = ggplot(data=data.plot, mapping=aes(x=Time, y=From))  +geom_line(colour="steelblue")+theme_gray() + labs(x = element_blank(), y = "From")+scale_x_date(expand=c(0,0), limits=c(data.plot[1,"Time"],data.plot[nrow(data.plot),"Time"])+2)
      net.plot  = ggplot(data=data.plot, mapping=aes(x=Time, y=Net))   +geom_line(colour="steelblue")+theme_gray() + labs(x = element_blank(), y = "Net") +scale_x_date(expand=c(0,0), limits=c(data.plot[1,"Time"],data.plot[nrow(data.plot),"Time"])+2)
      plot      = to.plot + from.plot + net.plot + plot_layout(ncol=1) 
      
      pdf(file = paste0(Resultados,'/Graficas_CoVaR','_',kk,"_",Serie.1,'.pdf'), onefile=FALSE)
      print(plot + plot_annotation(title = paste("Dynamic total directional connectedness of", Serie.1),
                                   subtitle = paste0("Stressed series: ",country)))
      
      
    }
    graphics.off()
  }
} 

#CoVaR plot
if(0)  {
  for (i in 1:ncol(CoVaR_data$CoVaRUp)) {
    CoVaRUp   = CoVaR_data$CoVaRUp[,i]
    VaRUp     = CoVaR_data$VaRUp[,i]
    data.xts  = cbind(CoVaRUp,VaRUp)
    data.plot = fortify(data.xts)
    colnames(data.plot) = c("Time","CoVaR", "VaR")
    data.plot[,"Time"]=as.Date(data.plot[,"Time"])
    plot=ggplot(title="") + theme_classic()+ geom_line(data = data.plot, aes(x = Time, y = CoVaR), color = "red") +
      geom_line(data = data.plot, aes(x = Time, y = VaR), color = "black") +
      xlab("Time") +
      ylab(element_blank())+
      ggtitle(paste0("CoVaR and VaR for ", colnames(CoVaR_data$CoVaRUp)[i]))+
      scale_x_date(expand=c(0,0), limits=c(data.plot[1,"Time"],data.plot[nrow(data.plot),"Time"]))
    x11()
    print(plot)
  }
}

}
