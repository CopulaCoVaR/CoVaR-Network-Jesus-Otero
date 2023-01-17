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
} #Packages
setwd("C:/Users/maico/OneDrive - Universidad Nacional de Colombia/BanRep/Value at Risk/Coffee")
wd = getwd()                                # Carpeta de trabajo adaptable al proyecto, se modifica automáticamente en cada computador.
# Carga de datos para cada país. 
variety="USA...Colombia"

if (country=="Colombia") {
  #BigVAR.Optimo
  load("/Resultados/Colombia/R_files/BigVAR-Optimo-USA...Colombia-2023-01-10.R", verbose=TRUE)
  # Copulas seleccionadas
  load("/Resultados/Colombia/R_files/Cop_seleccionadas_USA...Colombia_2023-01-10", verbose=TRUE)
  #CoVaR Data
  load("/Resultados/Colombia/R_files/CoVaR_data_USA...Colombia_2023-01-10")
  #GIRF
  load("/Resultados/Colombia/R_files/GIRF.USA...Colombia", verbose=TRUE)
  #GFEVD
  load("/Resultados/Colombia/R_files/GFEVD.USA...Colombia", verbose=TRUE)
  #Network
  
}
if (country=="Brazil")   {
  #BigVAR.Optimo
  load("/Resultados/Brazil/R_files/BigVAR-Optimo-USA...Brazil-2023-01-11.R", verbose=TRUE)
  # Copulas seleccionadas
  load("/Resultados/Brazil/R_files/Cop_seleccionadas_USA...Brazil_2022-12-23", verbose=TRUE)
  #CoVaR Data
  load("/Resultados/Brazil/R_files/CoVaR_data_USA...Brazil_2023-01-11", verbose=TRUE)
  #GIRF
  load("/Resultados/Brazil/R_files/GIRF.USA...Brazil", verbose=TRUE)
  #GFEVD
  load("/Resultados/Brazil/R_files/GFEVD.USA...Brazil", verbose=TRUE)
  #Network
  load("/Resultados/Brazil/R_files/Dynamic_Network_USA...Brazil.R",)
  
}
if (country=="Guatemala"){
  #BigVAR.Optimo
  load("/Resultados/Guatemala/R_files/BigVAR-Optimo-USA...Guatemala-2023-01-12.R", verbose=TRUE)
  # Copulas seleccionadas
  load("/Resultados/Guatemala/R_files/Cop_seleccionadas_USA...Guatemala_2022-12-24", verbose=TRUE)
  #CoVaR Data
  load("/Resultados/Guatemala/R_files/CoVaR_data_USA...Guatemala_2023-01-12", verbose=TRUE)
  #GIRF
  load("/Resultados/Guatemala/R_files/GIRF.USA...Guatemala", verbose=TRUE)
  #GFEVD
  load("/Resultados/Guatemala/R_files/GFEVD.USA...Guatemala", verbose=TRUE)
  #Network
  load("/Resultados/Guatemala/R_files/Dynamic_Network_USA...Guatemala.R",verbose=TRUE)
  
}
if (country=="Indonesia"){
  #BigVAR.Optimo
  load("/Resultados/Indonesia/R_files/BigVAR-Optimo-USA...Indonesia-2023-01-12.R", verbose=TRUE)
  # Copulas seleccionadas
  load("/Resultados/Indonesia/R_files/Cop_seleccionadas_USA...Indonesia_2022-12-26", verbose=TRUE)
  #CoVaR Data
  load("/Resultados/Indonesia/R_files/CoVaR_data_USA...Indonesia_2023-01-12", verbose=TRUE)
  #GIRF
  load("/Resultados/Indonesia/R_files/GIRF.USA...Indonesia", verbose=TRUE)
  #GFEVD
  load("/Resultados/Indonesia/R_files/GFEVD.USA...Indonesia", verbose=TRUE)
  #Network
  load("/Resultados/Indonesia/R_files/Dynamic_Network_USA...Indonesia.R", verbose=TRUE)
  
}
if (country=="Mexico")   {
  #BigVAR.Optimo
  load("/Resultados/Mexico/R_files/BigVAR-Optimo-USA...Mexico-2023-01-13.R", verbose=TRUE)
  # Copulas seleccionadas
  load("/Resultados/Mexico/R_files/Cop_seleccionadas_USA...Mexico_2022-12-28", verbose=TRUE)
  #CoVaR Data
  load("/Resultados/Mexico/R_files/CoVaR_data_USA...Mexico_2023-01-13", verbose=TRUE)
  #GIRF
  load("/Resultados/Mexico/R_files/GIRF.USA...Mexico", verbose=TRUE)
  #GFEVD
  load("/Resultados/Mexico/R_files/GFEVD.USA...Mexico", verbose=TRUE)
  #Network
  load("/Resultados/Mexico/R_files/Dynamic_Network_USA...Mexico.R",verbose=TRUE)
  
}
if (country=="Uganda")   {
  #BigVAR.Optimo
  load("/Resultados/Uganda/R_files/BigVAR-Optimo-USA...Uganda-2023-01-13.R", verbose=TRUE)
  # Copulas seleccionadas
  load("/Resultados/Uganda/R_files/Cop_seleccionadas_USA...Uganda_2022-12-30", verbose=TRUE)
  #CoVaR Data
  load("/Resultados/Uganda/R_files/CoVaR_data_USA...Uganda_2023-01-13", verbose=TRUE)
  #GIRF
  load("/Resultados/Uganda/R_files/GIRF.USA...Uganda", verbose=TRUE)
  #GFEVD
  load("/Resultados/Uganda/R_files/GFEVD.USA...Uganda", verbose=TRUE)
  #Network
  load("/Resultados/Uganda/R_files/Dynamic_Network_USA...Uganda.R",verbose=TRUE)
  
}
if (country=="Vietnam")  {
  #BigVAR.Optimo
  load("/Resultados/Vietnam/R_files/BigVAR-Optimo-USA...Vietnam-2023-01-08.R", verbose=TRUE)
  # Copulas seleccionadas
  load("/Resultados/Vietnam/R_files/Cop_seleccionadas_USA...Vietnam_2023-01-07", verbose=TRUE)
  #CoVaR Data
  load("/Resultados/Vietnam/R_files/CoVaR_data_USA...Vietnam_2023-01-08", verbose=TRUE)
  #GIRF
  load("/Resultados/Vietnam/R_files/GIRF.USA...Vietnam", verbose=TRUE)
  #GFEVD
  load("/Resultados/Vietnam/R_files/GFEVD.USA...Vietnam", verbose=TRUE)
  #Network
  #load(,verbose=TRUE)
  
}

if (0) {# Preparación
  Serie.1 = country
  Serie.2 = colnames(Dynamic_Network$From.Degree.Dynamic[,i])
  theme   = c("classic", "minimal", "void", "grey", "bw")[1]
  for(i in 1:ncol(Dynamic_Network$From.Degree.Dynamic)){
  # Datos
  to        = Dynamic_Network$To.Degree.Dynamic[,i]
  from      = Dynamic_Network$From.Degree.Dynamic[,i]
  net       = Dynamic_Network$Net.Dynamic[,i]
  data.xts  = cbind(to,from,net)
  data.plot = fortify.zoo(data.xts)
  colnames(data.plot) = c("Time", "To", "From", "Net")
  
  # Graficación
  if(theme=="classic"){
    to.plot   = ggplot(data=data.plot, mapping=aes(x=Time, y=To))  +geom_line(colour="steelblue")+theme_classic() + labs(x = element_blank(), y = "To")+scale_x_date(date_labels = "%b")
    from.plot = ggplot(data=data.plot, mapping=aes(x=Time, y=From))+geom_line(colour="steelblue")+theme_classic() + labs(x = element_blank(), y = "From")+scale_x_date(date_labels = "%b")
    net.plot  = ggplot(data=data.plot, mapping=aes(x=Time, y=Net)) +geom_line(colour="steelblue")+theme_classic() + labs(x = element_blank(), y = "Net")+scale_x_date(date_labels = "%b")
    x11()
    print(  to.plot   + scale_x_continuous(expand = c(0, 0))
          + from.plot + scale_x_continuous(expand = c(0, 0))
          + net.plot  + scale_x_continuous(expand = c(0, 0))
          + plot_layout(ncol=1) 
          + plot_annotation(title = paste("Dynamic total directional connectedness of", variety),
                            subtitle = paste0("Stressed series: ",colnames(Dynamic_Network$From.Degree.Dynamic[,i]))))
  }

  }
  if(theme=="minimal"){
    to.plot   = ggplot(data=data.plot, mapping=aes(x=Time, y=To))  +geom_line(colour="steelblue")+theme_minimal()
    from.plot = ggplot(data=data.plot, mapping=aes(x=Time, y=From))+geom_line(colour="steelblue")+theme_minimal()
    net.plot  = ggplot(data=data.plot, mapping=aes(x=Time, y=Net)) +geom_line(colour="steelblue")+theme_minimal()
    x11()
    to.plot + from.plot + net.plot + plot_layout(ncol=1) + plot_annotation(title = paste("Dynamic total directional connectedness of", Serie.1),
                                                                           subtitle = paste0("Stressed series: ",Serie.2))
  }
  if(theme=="void")   {
    to.plot   = ggplot(data=data.plot, mapping=aes(x=Time, y=To))  +geom_line(colour="steelblue")+theme_void()
    from.plot = ggplot(data=data.plot, mapping=aes(x=Time, y=From))+geom_line(colour="steelblue")+theme_void()
    net.plot  = ggplot(data=data.plot, mapping=aes(x=Time, y=Net)) +geom_line(colour="steelblue")+theme_void()
    x11()
    to.plot + from.plot + net.plot + plot_layout(ncol=1) + plot_annotation(title = paste("Dynamic total directional connectedness of", Serie.1),
                                                                           subtitle = paste0("Stressed series: ",Serie.2))
  }
  if(theme=="grey")   {
    to.plot   = ggplot(data=data.plot, mapping=aes(x=Time, y=To))  +geom_line(colour="steelblue")+theme_grey()
    from.plot = ggplot(data=data.plot, mapping=aes(x=Time, y=From))+geom_line(colour="steelblue")+theme_grey()
    net.plot  = ggplot(data=data.plot, mapping=aes(x=Time, y=Net)) +geom_line(colour="steelblue")+theme_grey()
    x11()
    to.plot + from.plot + net.plot + plot_layout(ncol=1) + plot_annotation(title = paste("Dynamic total directional connectedness of", Serie.1),
                                                                           subtitle = paste0("Stressed series: ",Serie.2))
  }
  if(theme=="bw")     {
    to.plot   = ggplot(data=data.plot, mapping=aes(x=Time, y=To))  +geom_line(colour="steelblue")+theme_bw()
    from.plot = ggplot(data=data.plot, mapping=aes(x=Time, y=From))+geom_line(colour="steelblue")+theme_bw()
    net.plot  = ggplot(data=data.plot, mapping=aes(x=Time, y=Net)) +geom_line(colour="steelblue")+theme_bw()
    x11()
    to.plot + from.plot + net.plot + plot_layout(ncol=1) + plot_annotation(title    = paste("Dynamic total directional connectedness of", Serie.1),
                                                                           subtitle = paste0("Stressed series: ",Serie.2))
  }
  
  
} #Network Dynamic
