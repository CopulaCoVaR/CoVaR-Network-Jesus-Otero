# Preparación -------------------------------------------------------------
setwd("C:/Users/maico/OneDrive - Universidad Nacional de Colombia/BanRep/Value at Risk/CoVaR-Network-Jesus-Otero")
wd = getwd()                                # Carpeta de trabajo adaptable al proyecto, se modifica automáticamente en cada computador.
# ----------------------
Resultados <<- paste0(wd,'/Resultados')     #<<<<<--- Capeta de resultados qué depende deldirectorio de trabajo. 
# ----------------------
data.file    = 'ENSO_DATA.r'                #<<<--- String con el nombre del archivo de datos en la carpeta de trabajo.
# ----------------------
Series.1     = c("USA...Colombia",        "USA...Brazil",          "USA...Guatemala",       "USA...Indonesia",      
                 "USA...Mexico",          "USA...Uganda",            "USA...Vietnam",       "Germany...Colombia",
                 "Germany...Brazil",      "Germany...El.Salvador", "Germany...Guatemala",   "France...Colombia",
                 "France...Brazil",       "France...Cote.dIvoire", "France...Indonesia",    "France...Uganda", 
                 "France...Vietnam")
                 #"USA...Colombia") #<<<--- Columnas de series de análisis
# ----------------------
Serie.2      = c("NINO3"         , "NINO3.4"        , "NINO4"          , "USA...Colombia", 
                 "USA...Brazil"  , "USA...Guatemala", "USA...Indonesia", "USA...Mexico"  , 
                 "USA...Uganda", "USA...Vietnam")[6]         #<<<--- Columna de serie de referencia.
# ----------------------
alpha.in  =c(0.01, 0.05, 0.95, 0.99)[2]      # Alpha para el CoVaR
beta.in   =c(0.01, 0.05, 0.95, 0.99)[2]     # Beta para el CoVaR
# ----------------------
CoVaR.type   = c('Equal','Less')[2]         #<<<--- Seleccion del tipo de metodologia para calcular el CoVaR <Equal> para Liu(2022) y <Less> para RU(2016)
# ----------------------
estimate.copula = c(TRUE, FALSE)[2]
if (estimate.copula==FALSE){
  if (Serie.2=='NINO3')                         copula.file= 'Cop_seleccionadas_NINO3_2022-12-10'
  if (Serie.2=='NINO3.4' & CoVaR.type=='Less')  copula.file= 'Cop_seleccionadas_NINO3.4_2022-12-11'
  if (Serie.2=='NINO3.4' & CoVaR.type=='Equal') copula.file= 'Cop_seleccionadas_NINO3.4_2022-12-15'
  if (Serie.2=='NINO4')                         copula.file= 'Cop_seleccionadas_NINO4_2022-12-12'
  if (Serie.2=='USA...Colombia')                copula.file= paste0(wd,'/Resultados/Colombia/R_files/Cop_seleccionadas_USA...Colombia_2023-01-10')
  if (Serie.2=='USA...Brazil')                  copula.file= paste0(wd,'/Resultados/Brazil/R_files/Cop_seleccionadas_USA...Brazil_2022-12-23')
  if (Serie.2=='USA...Guatemala')               copula.file= paste0(wd,'/Resultados/Guatemala/R_files/Cop_seleccionadas_USA...Guatemala_2022-12-24')
  if (Serie.2=="USA...Indonesia")               copula.file= paste0(wd,'/Resultados/Indonesia/R_files/Cop_seleccionadas_USA...Indonesia_2022-12-26')
  if (Serie.2=="USA...Uganda")                  copula.file= paste0(wd,'/Resultados/Uganda/R_files/Cop_seleccionadas_USA...Uganda_2022-12-30')
  if (Serie.2=="USA...Mexico")                  copula.file= paste0(wd,"/Resultados/Mexico/R_files/Cop_seleccionadas_USA...Mexico_2022-12-28")
} # Copulas SSTA
# ----------------------
Forecast     = c('in sample', 'rolling')[2] #<<<--- Determina el tipo de pronóstico a llevar a cabo.
if(Forecast=='rolling') refit=10            #<<<--- Sí el pronóstico es rolling, se define cada cuanto reestimar el modelo.       
# ----------------------
log.Serie.1  = c(TRUE, FALSE)[1]            #<<<--- <T> se aplican logaritmos a las <Serie.1>, <F> No se aplican.
log.Serie.2  = c(TRUE, FALSE)[1]            #<<<--- <T> se aplican logaritmos a la <Serie.2>, <F> No se aplica.
dif.Serie.1  = c(TRUE, FALSE)[1]            #<<<--- <T> diferencia las <Serie.1>, <F> deja los datos en nivel. 
dif.Serie.2  = c(TRUE, FALSE)[1]            #<<<--- <T> diferencia la <Serie.2>, <F> deja los datos en nivel. 
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
# Funciones ---------------------------------------------------------------
source('RebUgo_Functions.R')
source('Copula_seleccion.R')
source('Copula_CoVaR.R')
source('CoVaR_LF.R')
source('Rolling.R')
source('garch_roll.R')
source('Optimal_model.R')
source('GIRF_BigVAR.R')
source('GFEVD_BigVAR.R')
source('Connectedness.R')
source('DynCopulaCoVaR.R')
source('DynCopulaCoVaRUpper.R')
# Datos -------------------------------------------------------------------
#--Base de datos de RETORNOS para el periodo de crisis. 
#---- Supuesto_1: la primera col es la FECHA de la base de datos
#---- Supuesto_2: Las col.2 hasta la penultima col. son las series que se modelan w.r.t la serie de la ultima columna (Ej. Ind Acc de varios paises)
#---- Supuesto_3: La ultima col. es la serie que se mantiene en todas copulas (Ej: WTI)
DATOS=read_xlsx(data.file)
DATOS_xts    = xts(DATOS[,-1], order.by = as.POSIXct(DATOS[[1]])) # (objeto XTS) Se resta la primera col. (FECHA) 
if (Serie.2 %in% Series.1)N.Series1=ncol(DATOS_xts[,Series.1[-which(Series.1==Serie.2)]])else N.Series1=ncol(DATOS_xts[,Series.1])                                      # Se resta la primera col. (FECHA) y la ultima columna (Ej: WTI)
Name.Serie2=Serie.2                                                             # Nombre de la ultima col. es la serie que se mantiene en todas copulas (Ej: WTI)
if (Serie.2 %in% Series.1) Name.Series1=Series.1[-which(Series.1==Serie.2)]else Name.Series1 = Series.1   #                                        # Se quitan la col de  fechas  y se definen las series.1 (i.e. country' Stock returns)
NAs=sum(is.na(DATOS_xts))                                                       # Numero datos faltantes
# Interpolacion lineal para completar los datos faltantes
if (NAs!=0){
  cat('\nNo. de datos faltantes: ',NAs,'\n') # Numero datos faltantes
  DATOS_WNA = na.fill(DATOS_xts, 'extend')
}else DATOS_WNA=DATOS_xts
#Se transforman las series si se requiere
#Logaritmos
if(log.Serie.1==TRUE) DATOS_WNA[,Name.Series1]=log(DATOS_WNA[,Name.Series1])  # Logaritmo de las Serie.1
if(log.Serie.2==TRUE) DATOS_WNA[,Name.Serie2]=log(DATOS_WNA[,Name.Serie2])  # Logaritmo de la Serie.2                                  # Logaritmos a todas las series.
#Diferencias
if(dif.Serie.1==TRUE) DATOS_WNA[,Name.Series1]=diff(DATOS_WNA[,Name.Series1]) #Diferencia de las  Serie.1
if(dif.Serie.2==TRUE) DATOS_WNA[,Name.Serie2]=diff(DATOS_WNA[,Name.Serie2]) #Diferencia de la  Serie.2
if(dif.Serie.1==TRUE|dif.Serie.2==TRUE) DATOS_WNA =DATOS_WNA[-1,]                     #Eliminamos la primera fila de datos (perdidos en la diferencia)
DATOS_WNA = na.fill(DATOS_WNA, 'extend')
# ---- Se elige la muestra a trabajar en función del argumento inicial Sample ---- #
Sample=DATOS_WNA
# Estadísticas descriptivas -----------------------------------------------
if(0){
  data.stats = Sample
  Series     = colnames(data.stats)
  N          = length(Series)
  stats      = matrix(0,12,N,dimnames=list(c('Mean','Max','Min','SD','Skew','Kurt','J-B', 'p-value' ,'ARCH','p-value' ,paste0('Corr.',Name.Serie2),'No.NA'),Series))
  ARMA.Order = ARMA.ORDER.DF(Datos=data.stats)
  for (i in 1:N){
    arima       = try(arima(data.stats[,Series[i]], method = 'ML', order=c(ARMA.Order[Series[i],],0)))
    ARCH        = try(arch.test(arima))
    stats[1,i]  = try(mean(data.stats[,Series[i]]))
    stats[2,i]  = try(max(data.stats[,Series[i]]))
    stats[3,i]  = try(min(data.stats[,Series[i]]))
    stats[4,i]  = try(sd(data.stats[,Series[i]]))
    stats[5,i]  = try(skewness(data.stats[,Series[i]]))
    stats[6,i]  = try(kurtosis(data.stats[,Series[i]]))
    stats[7,i]  = try(jarque.bera.test(data.stats[,Series[i]])$statistic)
    stats[8,i]  = try(jarque.bera.test(data.stats[,Series[i]])$p.value)
    stats[9,i]  = try(ARCH[5,'LM'])
    stats[10,i] = try(ARCH[5,5])
    stats[11,i] = try(cor(data.stats[,Series[i]],data.stats[,Name.Serie2], method = 'kendall'))
    stats[12,i] = try(sum(is.na(data.stats[,Series[i]])))
  }
  print(t(stats), digits=3)
  write.csv(stats, file=paste0('Descriptive_',Serie.2,'.csv')) # Ajusta el objeto guardado para qué tenga el nombre de la variable de referencia.
}
# Selección de Copula----------------
if(estimate.copula==TRUE){
  Data.copula = Sample
  time.ini    = Sys.time()
  Serie2      = Name.Serie2 #Ej: WTI
  Res.Tot     = array(NA,dim=c(N.Series1,2), 
                      dimnames=list(Name.Series1, c('Min.AIC', 'Min.BIC')))
  ii.n      = 0 
  for (ii in Name.Series1){
    print(ii)
    ii.n            = ii.n + 1
    res             = Copula_seleccion(Datos=Data.copula, arma.order=NULL, GARCH.model="gjrGARCH", serie.1=ii, serie.2=Serie2, CoVaR.type=CoVaR.type)
    Res.Tot[ii.n, ] = res$Best.Copula
    print(res)
  }
  
  #-- Time of execution --#
  time.diff = (Sys.time() - time.ini) 
  time.diff.over.60 = (Sys.time() - time.ini)/60 
  
  #-- Printout of final results --#
  print(Res.Tot)
  print(time.diff)
  print(time.diff.over.60)
  Copulas.seleccionadas = Res.Tot; Copulas.date=Sys.Date()
  save(Copulas.seleccionadas, file=paste0('Cop_seleccionadas_',Serie.2,'_',today()))
} 
# Calculo del CoVaR -----------------------------------
#---- Orden ARMA automático
ARMA.Order = ARMA.ORDER.DF(Datos=Sample[,c(Series.1, Serie.2)]) 
#---- Carga de las mejores Copulas seleccionadas según fecha.
if(estimate.copula==FALSE) load(copula.file)
copulas.Sel=Copulas.seleccionadas[,'Min.AIC']
#----Estimacion del VaR, CoVaR, DeltaCoVaR y CoVaRmedian
CoVaR_data=CoVaR_DF(Data=Sample,Serie.1=Name.Series1[-8], Serie.2=Name.Serie2, 
                         copulas=copulas.Sel, alpha=alpha.in, beta=beta.in, plot=TRUE, 
                         COND=CoVaR.type, forecast.type=Forecast, refit.every=refit, 
                         ARMA.Order=ARMA.Order, external.regressors=NULL, window.size=200)
save(CoVaR_data, file=paste0('CoVaR_data_',Name.Serie2,'_',today()))
# ----
# #############################################################################
#  Para información más detallada ver 'http://www.wbnicholson.com/BigVAR.html'  
# #############################################################################  
# Objetivos: 
# (1) Estimar un modelo VAR penalizado con un numero grande de variables (ej. 20).
#     En particular cuando <struct='BasicEN'> en la función <constructModel> del paquete BigVAR
#     Se estima un VAR-ElasticNetwork(Lambda, alpha). alpha=0 (Ridge) y alpha=1 (Lasso)
#    (1-a) Especificacion del modelo: <constructModel()> 
#    (1-b) Estimacion del modelo:     <cv.BigVAR()> 
# (2) Estimar las funciones de impulso-respuesta y GFEVD asociados al VAR de (1).
#
# ¿Como se estima Lambda en (1)? -----------------------------------------------------
# La función <cv.BigVAR> estima Lambda mediante rolling windows (de tamaño <window.size>) minimizando 
# el MSFE un paso adelante durante el período de entrenamiento(Por defecto: T/3 hasta 2T/3, donde T es el tamaño total de la muestra).
#
# Nota: También se puede usar el MSFE <h> pasos adelante.
#
# ¿Como se determina alpha en (1)?
# Se especifica como argumento de la función <constructModel> así: <model.controls = list(alpha = alpha)> 
# Sí es un escalar se toma dicho valor. 
# Sí es un vector, la funcion selecciona de ese vector el <alpha> optimo que minimice el MSFE junto a Lambda. 
# Nota: El valor predeterminado es 1/(k+1).x|
# ----------------------------------------------------------------------------
# 
# (1.a) Los argumentos de entrada DE <constructModel> son:
#   
#  <Y>: serie de tiempo multivariada
#  <p>: orden de rezago máximo
#  <struct>:     Estructura de la penalización. Las opciones son
#    Basic:     Penalización por Lazo
#    BasicEN:   Lasso con Red Elástica (Lasso o Ridge)
#    lag:       penalización por retraso
#    OwnOther:  Propia/Otra Penalización
#    SparseLag: penalización por retraso disperso
#    SparseOwnOther: Escasa propia/otra penalización por retraso
#    SCAD:      Desviación absoluta recortada suavemente
#    MCP:       Penalización cóncava minimax
#    EFX:       Endógeno-Primero VARX
#    HLAGC:     HLAG por componentes
#    HLAGOO:    Propia/Otra HLAG
#    HLAGELEM:  HLAG elemental
#    Tapered:   Lasso con penalización por retraso
#    BGR:       VAR bayesiano como se detalla en Bańbura, Giannone y Reichlin (2010).
#  Los primeros 7 se pueden aplicar a los modelos VAR y VARX, EFX solo se puede aplicar a los modelos VARX, los 5 restantes solo se aplican a los modelos VAR.
# 
#  <gran>: dos opciones para la cuadrícula de parámetros de penalización λ.
#   - La primera opción controla la profundidad de la cuadrícula lambda (una buena opción predeterminada es 50). 
#   - La segunda opción controla el número de valores de cuadrícula (un buen valor predeterminado es 10).
#      Si la cuadrícula no es lo suficientemente profunda, los resultados de los pronósticos pueden ser subóptimos, pero si es demasiado profundo, la rutina puede tardar una cantidad considerable de tiempo en ejecutarse. 
#      El índice del parámetro de penalización óptimo es monitoreado por <cv.BigVAR()>. Si está en el borde de la cuadrícula, se recomienda volver a ejecutar la función con un parámetro de granularidad mayor. 
#      Si establece la <ownlambdas=TRUE>, <gran> se usa para proporcionar una cuadrícula definida por el usuario.
# 
#   <h>: Horizonte de pronóstico en el que minimizar MSFE (por defecto 1).
#   <verbose>: Lógico, si es VERDADERO, mostrará una barra de progreso para los procedimientos de validación y evaluación.
#   <window.size>: Cuanco es > 0: Tamaño de la ventana para el rolling cross validation. 
#                  Cuando es 0 :  Se utiliza una ventana recursiva(expansiva) para el cross validation. (Este es el valor por defecto).
#                  
#   <cv>: Tipo de validación que se utiliza para seleccionar el parámetro de penalización 
#         Las opciones son:
#         1. "rolling" (predeterminado) 
#         2. "LOO", un pseudo procedimiento cv de "Leave One Out Validation" que respeta la dependencia en el tiempo.
# 
#   <ownlambdas>: <FALSE> si se utilizan los dos parámetros de <gran> discutidos anteriormente. 
#                  <TRUE> si el usuario desea sumnistrar un conjunto de valores de <Lambda> para minimizar el MSFE, en cuyo caso los indica en <gran>
#                 
# 
#   <separate_lambdas>: <TRUE>  para usar parámetros de penalización separados para cada serie. Esta opción solo es válida para las estructuras Basic,BasicEN,HLAG,HLAGOO,HLAGELEM,SCAD,MCP.
#                       <FALSE> Usa el mismo parámetro de penalización para todas las series (opción por defecto)
#
#   <model.controls>: Se especifican los parámetros en la lista <model.controls>. Estos parámetros incluyen:
#     - intercept:  Tenga en cuenta que el intercepto se ajusta por separado y no está sujeto a penalización. ver. "Nicholson, W. B., Matteson, D. S., & Bien, J. (2014). Structured Regularization for Large Vector Autoregressions. 14853, 1–40." Apendix: 9.1
#                  <TRUE>  se incluye en el modelo (por defecto)
#                  <FALSE> no se incluye en el modelo
#     - alpha: ver (¿Como se determina alpha en (1)?)
#     - linear: <TRUE>  construye una cuadrícula de Lambda lineal(por defecto).
#               <FALSE> construye una cuadrícula logarítmica-lineal.
#
#-------------------------------------#
#----
#--- Estimacion del BigVAR para la muestra completa ---# 
BigVAR.Model            = Optimal.model(Data=CoVaR_data$CoVaRUp , alpha=seq(0,1,0.05), auto.lambdas=TRUE, auto.length=30, grid=c(10000,10)) # Estimación del modelo óptimo bajo la opción
save(BigVAR.Model, file = paste0("BigVAR-Optimo-",Serie.2,'-',today(),".R"))
n.ahead.connectedness=10
#BigVAR models
if (Serie.2=="USA...Colombia") load("BigVAR-Optimo-USA...Colombia-2023-01-10.R")
if (Serie.2=="USA...Vietnam") load("BigVAR-Optimo-USA...Vietnam-2023-01-08.R")
# Impulso respuesta y GFEVD ---------------------------------------------------
GIRF.res        = GIRF_BigVAR(results=BigVAR.Model$Model, data=CoVaR_data$CoVaRUp, n.ahead=n.ahead.connectedness,
                              plot.out='GIRF', ortho=FALSE, std_shock=TRUE, magnitude=1, 
                              vars.to.shock=colnames(CoVaR_data$CoVaRUp), vars.to.resp=colnames(CoVaR_data$CoVaRUp),
                              x11=FALSE, pdf=FALSE, grid.dims=c(1,1)) #para el calc. adecuado de GIRF, <std_shock> debe ser TRUE.

save(GIRF.res, file = paste0('GIRF.',Serie.2))   #Se guarda el objeto

GFEVD.res       = GFEVD(Girf=GIRF.res, N.ahead=n.ahead.connectedness, plot.out='GFEVD', vars.to.shock=colnames(CoVaR_data$CoVaRUp), 
                        vars.to.resp=colnames(CoVaR_data$CoVaRUp))

save(GFEVD.res, file = paste0('GFEVD.',Serie.2)) #Se guarda el objeto

Static_Network  = Connectedness(GFEVD=GFEVD.res, Title='Net Pairwise directional connectedness', node.size=1, 
                                Q1='95%', Q2='90%', Q3='85%', arrow.size=0.6, layout="circle", n.ahead = n.ahead.connectedness, 
                                node.label.size = 1, edge.width.scale=8) 

Dynamic_Network = Rolling_GFEVD(Data=CoVaR_data$CoVaRUp, structure='BasicEN', window.size=100,
                                n.ahead=n.ahead.connectedness, pdf=TRUE, x11=TRUE, alpha=BigVAR.Model$Model@alpha, plot=TRUE,
                                BigVAR.window=24, Num.alphas.rolling=10)
save(Dynamic_Network, file=paste0('Dynamic_Network_',Serie.2,".R"))

# Post-Estimación ---------------------------------------------------------
# Prueba KS
if(0){
  test     = c('ksboot', 'created.function')[1]
  n.rep    = 1000       
  x        = sample$CoVaR
  y        = sample$VaR
  alt      = c("greater", "two.sided", "less")[1]
  P.values = matrix(NA, nrow=ncol(x), ncol=6, dimnames=list(colnames(x), c('VaR average', 'CoVaR average','SD-VaR', 'SD-CoVaR', 'Statistic','P-value')))
  for (i in 1:ncol(x)) {
    if (test=='created.function') {
      Bootstrap.KS               = try(KS.bootstrap(x=as.matrix(x[,i]), y=as.matrix(y[,i]), 
                                                    n.boot=n.rep, density.plot=F,  alternative=alt))
      P.values[i,'P-value']       = Bootstrap.KS$p.value
      P.values[i,'Statistic']     = Bootstrap.KS$KS.1
    }else{
      Bootstrap.KS                = ksboot(x=as.matrix(x[,i]), y=as.matrix(y[,i]), alternative=alt)
      P.values[i,'P-value']       = Bootstrap.KS$ksboot.pvalue 
      KS.1                        = ks.test(x=as.matrix(x[,i]),y=as.matrix(y[,i]), alternative=alt)
      P.values[i,'Statistic']     = KS.1$statistic
    }
    P.values[i,'CoVaR average'] = mean(x[,i])
    P.values[i,'VaR average']   = mean(y[,i])
    P.values[i,'SD-VaR']        = sd(y[,i])
    P.values[i,'SD-CoVaR']      = sd(x[,i])
  }
  print(P.values, digits=3)
  write.csv(P.values, file=paste('KS_Test_',Serie.2,'.csv'))
} 
# Graficación ECDF
if(0){
  sample=CoVaR_Data
  class    = c('pdf','x11')[1]
  x        = sample$CoVaRUp
  y        = sample$VaRUp
  if (class=='x11'){
    x11()
    par(mfrow=c(4,3))
  }
  for (i in 1:ncol(x)) {
    ecdx=ecdf(as.matrix(x)[,i])
    ecdy=ecdf(as.matrix(y)[,i])
    if (class=='pdf') pdf(file = paste0(Resultados,'/CDF_',colnames(x)[i],'.pdf'), onefile=FALSE)
    print(plot(ecdx, main=paste0(colnames(x)[i],'_',samp), col='red', lwd=1))
    print(lines(ecdy, lwd=1))
    if(class=='pdf')dev.off()
  }
}
# Graficación CoVaR
if(0){
  # Unión de datos en la misma lista.
  CoVaR_DATA=CoVaR_Data
  plot.class = c('Up', 'Down', 'Both')[1]
  titles=c('USA - Colombia',	'USA - Brazil',	'USA - Guatemala',	'USA - Indonesia',
           'USA - Mexico', 'USA - Uganda', 'USA - Vietnam',	'Germany - Colombia',	'Germany - Brazil',
           'Germany - El Salvador',	'Germany - Guatemala',	'France - Colombia',	
           'France - Brazil', 'France - Cote dIvoire', 'France - Indonesia', 'France - Uganda',	'France - Vietnam')
  names(titles)=Series.1
  CoVaR_DATA$CoVaR$horizontal_line = 0
  #Gráficas
  for (i in Series.1){
    if(plot.class=='Up')  range=c(0,max(CoVaR_DATA$CoVaRUp[,i]))
    if(plot.class=='Down')range=c(min(CoVaR_DATA$CoVaR[,i]),0)
    if(plot.class=='Both')range=c(min(CoVaR_DATA$CoVaR[,i]),max(CoVaR_DATA$CoVaRUp[,i]))
    pdf(file = paste0(Resultados,'/Graficas_CoVaR','_',i,'.pdf'), onefile=FALSE)
    print(plot(if (plot.class=='Down'|plot.class=='Both'){
      CoVaR_DATA$CoVaR[,i]
    }else{
      CoVaR_DATA$CoVaRUp[,i]
    } ,type="l",col="red", grid.col = NA, 
    ylim=range, xlab="Time", ylab="", lwd=1,format.labels="%Y", major.ticks = 'years', 
    yaxis.left=TRUE, yaxis.right=TRUE, lty='solid'))
    if (plot.class=='Down'|plot.class=='Both') print(lines(CoVaR_DATA$VaR[,i],         col="black", lwd=1,lty='dashed'))
    if (plot.class=='Both')                    print(lines(CoVaR_DATA$CoVaRUp[,i],     col="red",   lwd=1,lty='solid'))
    if (plot.class=='Up'|plot.class=='Both')   print(lines(CoVaR_DATA$VaRUp[,i],       col="black", lwd=1,lty='dashed'))
    print(lines(CoVaR_DATA$CoVaR$horizontal_line, col='darkgrey'))
    print(addLegend("topright", lwd=2,legend.names = c('CoVaR', 'VaR'), 
                    lty = c('solid','dashed'), col = c('red',   'black')))
    print(title(main = titles[i], cex.main = 1.5))
    dev.off()
  }
}
# Garch individual
if(0){
  sample=c('Full_Sample', 'Pre_Crisis', 'Crisis', 'Post_Crisis')[4] # Selección de la muestra.
  if (1) {
    Sample=list(Full_Sample=DATOS_WNA, Pre_Crisis=Pre_Crisis, Crisis=Crisis, Post_Crisis=Post_Crisis)
    if (sample=='Full_Sample') Data.GARCH=Sample$Full_Sample
    if (sample=='Pre_Crisis') Data.GARCH=Sample$Pre_Crisis
    if (sample=='Crisis') Data.GARCH=Sample$Crisis
    if (sample=='Post_Crisis') Data.GARCH=Sample$Post_Crisis
  } # Se crea el objeto Data.GARCH que contiene los datos para la muestra elegida.
  Series.names=colnames(Data.GARCH)
  ARMA.Order   = ARMA.ORDER.DF(Datos=Data.GARCH) #Se obtiene el orden ARMA de la serie.
  models.table = matrix(NA, 24, length(Series.names), 
                        dimnames = list(c('mu','','ar1','','ar2','',
                                          'ar3','','ar4','','ar5','','omega','','alpha1','','beta1', '','gamma1','','skew','', 'shape',''), Series.names))
  
  for (s in Series.names) {
    model                   = ugarchspec(variance.model=list(model="gjrGARCH", garchOrder=c(1,1)), 
                                         mean.model=list(armaOrder=ARMA.Order[s,]), distribution.model="sstd")
    model.fit               = ugarchfit(spec=model, data=Data.GARCH[,s], solver='hybrid')
    models.table['mu',s]    = model.fit@fit$matcoef['mu',1]  
    models.table[2,s ]      = model.fit@fit$matcoef['mu',2]
    models.table['ar1',s]   = try(model.fit@fit$matcoef['ar1',1]) 
    models.table[4,s]       = try(model.fit@fit$matcoef['ar1',2])
    models.table['ar2',s]   = try(model.fit@fit$matcoef['ar2',1])  
    models.table[6,s]       = try(model.fit@fit$matcoef['ar2',2])
    models.table['ar3',s]   = try(model.fit@fit$matcoef['ar3',1])  
    models.table[8,s]       = try(model.fit@fit$matcoef['ar3',2])
    models.table['ar4',s]   = try(model.fit@fit$matcoef['ar4',1])  
    models.table[10,s]      = try(model.fit@fit$matcoef['ar4',2])
    models.table['ar5',s]   = try(model.fit@fit$matcoef['ar5',1])  
    models.table[12,s]      = try(model.fit@fit$matcoef['ar5',2])
    models.table['omega',s] = model.fit@fit$matcoef['omega',1]  
    models.table[14,s]      = model.fit@fit$matcoef['omega',2]
    models.table['alpha1',s]= model.fit@fit$matcoef['alpha1',1]  
    models.table[16,s]      = model.fit@fit$matcoef['alpha1',2]
    models.table['beta1',s] = model.fit@fit$matcoef['beta1',1]  
    models.table[18,s]      = model.fit@fit$matcoef['beta1',2]
    models.table['gamma1',s]= model.fit@fit$matcoef['gamma1',1]  
    models.table[20,s]      = model.fit@fit$matcoef['gamma1',2]
    models.table['skew',s]  = model.fit@fit$matcoef['skew',1]  
    models.table[22,s]      = model.fit@fit$matcoef['skew',2]
    models.table['shape',s] = model.fit@fit$matcoef['shape',1]  
    models.table[24,s]      = model.fit@fit$matcoef['shape',2]
    print(models.table, digits=3)
    write.csv(models.table, file=paste0(sample,'_GARCH.csv'))
  }
  
  # Comparación con modelo de la función
  Prueba.garch=CoVaR.Copula(Serie.1=Series.names[1], Serie.2=Series.names[12], Datos=Data.GARCH, ALPHA=0.05,
                            BETA=0.05, type=copulas.Sel[1], COND='Less', 
                            forecast.type='in sample', ARMA.Order=ARMA.Order)
  Prueba.garch$model.garch.fit@fit$matcoef
}
# Copulas result
if(0){
  Copulas    = matrix(NA,12,11, dimnames=list(rep(c('Type', 'par1', 'par2', 'AIC'),3),Serie.1))
  samp = c('Full_Sample', 'Pre_Crisis', 'Crisis', 'Post_Crisis')[4]
  Sample  = list(Full_Sample=DATOS_WNA, Pre_Crisis=Pre_Crisis, Crisis=Crisis, Post_Crisis=Post_Crisis)[samp]
  Sample  = Sample[[1]]
  ARMA.Order = ARMA.ORDER.DF(Datos=Sample) 
  Serie.2=c('vix', 'embi')[1]
  if (Serie.2=='vix') {
    if (samp=='Pre_Crisis')  copula.file = 'Cop_seleccionadas_vix_Pre_Crisis_2022-10-23'
    if (samp=='Crisis')      copula.file = 'Cop_seleccionadas_vix_Crisis_2022-10-23'
    if (samp=='Post_Crisis') copula.file = 'Cop_seleccionadas_vix_Post_Crisis_2022-10-23'
  }
  if (Serie.2=="EMBI_Global") {
    if (samp=='Pre_Crisis')  copula.file = 'CoVaR_data_EMBI_Global_Pre_Crisis_2022-10-25'
    if (samp=='Crisis')      copula.file = 'CoVaR_data_EMBI_Global_Crisis_2022-10-26'
    if (samp=='Post_Crisis') copula.file = 'CoVaR_data_EMBI_Global_Post_Crisis_2022-10-26'
  }
  load(copula.file)
  copulas.Sel=Copulas.seleccionadas[,'Min.AIC']
  model.Serie.2     = ugarchspec(variance.model = list(model="sGARCH", garchOrder = c(1,1)), 
                                 mean.model = list(ARMA.Order[Serie.2,]), 
                                 distribution.model = "sstd")
  model.Serie.2.fit = ugarchfit(spec = model.Serie.2, data = Sample[,Serie.2])
  sd_errors2 = residuals(model.Serie.2.fit)/sigma(model.Serie.2.fit)
  u2         = rugarch::pdist(distribution='sstd', q=sd_errors2, skew=coef(model.Serie.2.fit)['skew'], shape=coef(model.Serie.2.fit)['shape'])
  for (i in Serie.1) {
    # Models Serie.1
    model.Serie.1     = ugarchspec(variance.model=list(model="gjrGARCH", garchOrder=c(1,1)), 
                                   mean.model=list(armaOrder=ARMA.Order[i,]), distribution.model="sstd")
    model.Serie.1.fit = ugarchfit(spec=model.Serie.1, data=Sample[,i], solver='hybrid')
    sd_errors1 = residuals(model.Serie.1.fit)/sigma(model.Serie.1.fit)
    u1         = rugarch::pdist(distribution='sstd', q=sd_errors1, skew=coef(model.Serie.1.fit)['skew'], shape=coef(model.Serie.1.fit)['shape'])
    u.s        = cbind(u1, u2)
    if (1) {
      cat('Copula fitting started...\n')
      type=copulas.Sel[i]
      if (type=='Gaussian' | type=='Gaussian.Up'){
        fit.Copula = BiCopEst(u1,u2, family=1, method='mle')
        par.Cop.1  = fit.Copula$par
        par.Cop.2  = NULL
        print(fit.Copula)
      }
      if (type=='Gumbel' | type=='Gumbel.Up'){
        fit.Copula = BiCopEst(u1,u2, family=4, method='mle')
        par.Cop.1  = fit.Copula$par
        par.Cop.2  = NULL
        print(fit.Copula)
      }
      if (type=='Joe' | type=='Joe.Up'){
        fit.Copula = BiCopEst(u1,u2, family=6, method='mle')
        par.Cop.1  = fit.Copula$par
        par.Cop.2  = NULL
        print(fit.Copula)
      }
      if (type=='Frank' | type=='Frank.Up'){
        fit.Copula = BiCopEst(u1,u2, family=5, method='mle')
        par.Cop.1  = fit.Copula$par
        par.Cop.2  = NULL
        print(fit.Copula)
      }
      if (type=='Rot.Gumbel' | type=='Rot.Gumbel.Up'){
        fit.Copula = BiCopEst(u1,u2, family=14, method='mle') 
        par.Cop.1  = fit.Copula$par
        par.Cop.2  = NULL
        print(fit.Copula)
      }
      if (type=='Rot.Clayton' | type=='Rot.Clayton.Up'){
        fit.Copula = BiCopEst(u1,u2, family=13, method='mle')
        par.Cop.1  = fit.Copula$par
        par.Cop.2  = NULL
        print(fit.Copula)
      }
      if (type=='AMH' | type=='AMH.Up'){
        cop_model  = amhCopula(dim=2)
        fit.Copula = fitCopula(cop_model, u.s , method='ml')
        par.Cop.1  = fit.Copula@estimate
        par.Cop.2  = NULL
        print(fit.Copula)
      }
      if (type=='Plackett' | type=='Plackett.Up'){
        cop_model  = plackettCopula()
        fit.Copula = fitCopula(cop_model, u.s , method='ml')
        par.Cop.1  = fit.Copula@estimate
        par.Cop.2  = NULL
        print(fit.Copula)
      }
      if (type=='BB7' | type=='BB7.Up'){
        fit.Copula = BiCopEst(u1,u2, family=9, method='mle')
        par.Cop.1  = fit.Copula$par
        par.Cop.2  = fit.Copula$par2 
        print(fit.Copula)
      }
      if (type=='Rot.BB7' | type=='Rot.BB7.Up'){
        fit.Copula = BiCopEst(u1,u2, family=19, method='mle') 
        par.Cop.1  = fit.Copula$par
        par.Cop.2  = fit.Copula$par2
        print(fit.Copula)
      }
      if (type=='Clayton' | type=='Clayton.Up'){
        fit.Copula = BiCopEst(u1,u2, family=3, method='mle')
        par.Cop.1  = fit.Copula$par
        par.Cop.2  = NULL 
        print(fit.Copula)
      }
      if (type=='Student' | type=='Student.Up'){
        fit.Copula = BiCopEst(u1,u2, family=2, method='mle')
        par.Cop.1  = fit.Copula$par
        par.Cop.2  = fit.Copula$par2
        print(fit.Copula)
      }
      if (type=='Dyn.Student' | type=='Dyn.Student.Up'){
        fit.Copula = dynamicT.LF(data=u.s, plot=FALSE, printout=FALSE)
        par.Cop.1  = fit.Copula$tvtpdep; par.Cop.1[1:10] = mean(fit.Copula$tvtpdep[-c(1:10)])
        par.Cop.2  = fit.Copula$result['nu','coef']
        print(fit.Copula$result)
      }
      if (type=='Dyn.Gaussian' | type=='Dyn.Gaussian.Up'){
        fit.Copula = dynamicnormal.LF(data=u.s,plot=FALSE, printout=FALSE) 
        par.Cop.1  = fit.Copula$tvtpdep; par.Cop.1[1:10] = mean(fit.Copula$tvtpdep[-c(1:10)])
        par.Cop.2  = NULL  
        print(fit.Copula$result)
      }  
      if (type=='Dyn.Gumbel' | type=='Dyn.Gumbel.Up'){
        fit.Copula = dynamicGum.LF(data=u.s,plot=FALSE, rotated=FALSE, printout=FALSE) 
        par.Cop.1  = fit.Copula$tvtpdep; par.Cop.1[1:10] = mean(fit.Copula$tvtpdep[-c(1:10)])
        par.Cop.2  = NULL  
        print(fit.Copula$result)
      } 
      if (type=='Dyn.Rot.Gumbel' | type=='Dyn.Rot.Gumbel.Up'){
        fit.Copula = dynamicGum.LF(data=u.s,plot=FALSE, rotated=TRUE, printout=FALSE) 
        par.Cop.1  = fit.Copula$tvtpdep; par.Cop.1[1:10] = mean(fit.Copula$tvtpdep[-c(1:10)])
        par.Cop.2  = NULL  
        print(fit.Copula$result)
      } 
      if (type=='Dyn.BB7' | type=='Dyn.BB7.Up'){
        fit.Copula = dynamicBB7.LF(data=u.s,plot=FALSE, printout=FALSE) 
        par.Cop.1  = fit.Copula$par1.t; par.Cop.1[1:10] = mean(fit.Copula$par1.t[-c(1:10)])
        par.Cop.2  = fit.Copula$par2.t; par.Cop.2[1:10] = mean(fit.Copula$par2.t[-c(1:10)])  
        print(fit.Copula$result)
      }
    }
    if (samp=='Pre_Crisis') {
      Copulas[1,i] = copulas.Sel[i]
      Copulas[2,i] = mean(par.Cop.1)
      Copulas[3,i] = mean(par.Cop.2)
      Copulas[4,i] = try(fit.Copula$AIC)
      
    }
    if (samp=='Crisis') {
      Copulas[5,i] = copulas.Sel[i]
      Copulas[6,i] = mean(par.Cop.1)
      Copulas[7,i] = mean(par.Cop.2)
      Copulas[8,i] = try(fit.Copula$AIC)
    }
    if (samp=='Post_Crisis') {
      Copulas[9,i] = copulas.Sel[i]
      Copulas[10,i] = mean(par.Cop.1)
      Copulas[11,i] = mean(par.Cop.2)
      Copulas[12,i] = try(fit.Copula$AIC)
    }
    
  }
}