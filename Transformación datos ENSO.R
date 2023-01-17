ENSO_COFFEE=readxl::read_xlsx("C:\\Users\\maico\\OneDrive - Universidad Nacional de Colombia\\BanRep\\Value at Risk\\ENSO\\ENSO.xlsx")
ENSO_COFFEE= xts(sapply(ENSO_COFFEE, as.numeric), order.by = as.POSIXct(ENSO_COFFEE[[1]])) # (objeto XTS) Se resta la primera col. (FECHA) 
for(i in which(ENSO_COFFEE==-9.99)){
  ENSO_COFFEE[[i]]=NA
} # Reemplazo de -9.99 to NA
#ENSO_CORRECTED=na.fill(ENSO_COFFEE, 'extend')
# Regiones
reg3  =c("SST95W", "SST110W", "SST125W", "SST140W")
reg3.4=c("SST125W", "SST140W", "SST155W", "SST170W")
reg4  =c("SST155W", "SST170W", "SST180W")
# Columnas nuevas
NINO3  =rowMeans(ENSO_COFFEE[,reg3],   na.rm=TRUE);cat(sum(is.na(NINO3)), '\n')
NINO3.4=rowMeans(ENSO_COFFEE[,reg3.4], na.rm=TRUE);cat(sum(is.na(NINO3.4)), '\n')
NINO4  =rowMeans(ENSO_COFFEE[,reg4],   na.rm=TRUE);cat(sum(is.na(NINO4)), '\n')
#Unimos el dataset original con las regiones del ENSO
ENSO_COFFEE=cbind(ENSO_COFFEE, NINO3, NINO3.4, NINO4)
ENSO_COFFEE=ENSO_COFFEE[,9:27]
# Verificamos con nombres de columnas
colnames(ENSO_COFFEE)
#Guardamos
save(ENSO_COFFEE, file='ENSO_DATA.r')
# Prueba=rowMeans(ENSO_COFFEE[1:10,c("SST125W", "SST140W", "SST155W", "SST170W")], na.rm=TRUE)
# Prueba_individual=rowMeans(ENSO_COFFEE[1,c("SST125W", "SST140W", "SST155W", "SST170W")], na.rm=TRUE)
# Gráficas ----------------------------------------------------------------
x11()
par(mfrow=c(5,6))
for (i in 1:ncol(DATOS_WNA)) {
  print(plot(DATOS_WNA[,i], main=colnames(DATOS_WNA)[i]))
}

