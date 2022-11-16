# ///////////////////////////////////////////////////////////////////////////////////////////
# Proyecto - Teoria de Copulas (Rendimientos)
# ///////////////////////////////////////////////////////////////////////////////////////////

library(quantmod)

# Cargamos la serie de datos
cartera=c("WALMEX.MX","KOFUBL.MX")
getSymbols(cartera,src = "yahoo",from="2020-01-01")


# Guardamos en unas variables los precios de cierre
p_WALMEX<-WALMEX.MX$WALMEX.MX.Close
p_FEMSA<-KOFUBL.MX$KOFUBL.MX.Close

# Creamos una tabla donde juntamos ambos precios de cierre
tabla_precios<-cbind(p_WALMEX,p_FEMSA)
colnames(tabla_precios)<-c("WALMEX","FEMSA")


# Definimos la tabla como un dataframe
tabla_precios<-as.data.frame(tabla_precios)


# Calculamos los rendimientos  
tabla_rendimientos<-data.frame()
for(i in 1:2){
  
  for (j in 1:length(tabla_precios$WALMEX)){
    tabla_rendimientos[j,i]<-tabla_precios[j+1,i]/tabla_precios[j,i]-1
  }  
} 


# Cambiamos el nombre de las columnas al dataframe   
colnames(tabla_rendimientos)<-c("WALMEX","FEMSA")


# Eliminamos la ultima observacion pues es un dato NA  
tabla_rendimientos<-tabla_rendimientos[-c(nrow(tabla_rendimientos)),]


#######################
# Ultimo precio - Portafolio  
ultimo_precio<-tabla_precios[c(nrow(tabla_precios)),]
ultimo_precio<-as.data.frame(ultimo_precio)

#######################
# Revaluacion - Portafolio

tabla_revaluacion<-data.frame()
for(i in 1:2){
  
  for( j in 1:nrow(tabla_rendimientos)){
    tabla_revaluacion[j,i]<-ultimo_precio[,i]*(1+tabla_rendimientos[j,i])
  } 
  
}

colnames(tabla_revaluacion)<-c("WALMEX","FEMSA")

#######################
# P&L individual
# Creamos la funcion P&L de cada emisora y las juntamos en un dataframe llamado PL_Portafolio


PL_Portafolio<-data.frame()  


for (i in 1:2){
  
  for (j in 1:nrow(tabla_revaluacion)){
    PL_Portafolio[j,i]<-ultimo_precio[i]-tabla_revaluacion[j,i]
    
  }
  
}  

hist(PL_Portafolio$WALMEX)


# https://cran.r-project.org/web/views/Distributions.html#Continuous
# Para ver las distribuciones continuas en R

library(rriskDistributions)  
library(fitdistrplus)
library(MASS) 
library(actuar) 
library(univariateML)


############################################################################################
# Buscamos que distribucion de probabilidad se ajusta a los rendimientos de TSLA
  hist(tabla_rendimientos$WALMEX)
  
  fit.cont(tabla_rendimientos$WALMEX)  
      # Logistica

# Hallamos los parametros de la distribucion logistica  
  m1<-model_select(tabla_rendimientos$WALMEX,models="logis",criterion="aic")
  coef(m1) 

#####
# Una mejor distribucion que se ajusta: Laplace
#  mod_TSLA<-model_select(tabla_rendimientos$TSLA,criterion="aic")
#  mod_TSLA

library(VGAM)

# Calculamos los valores de u1 con la distribucion logistica
  u1<-plogis(tabla_rendimientos$WALMEX,location=coef(m1)[1],scale=coef(m1)[2])

############################################################################################
# Buscamos que distribucion de probabilidad se ajusta a los rendimientos de AAPL
  hist(tabla_rendimientos$FEMSA)
  fit.cont(tabla_rendimientos$FEMSA)  
  # Logistica
  

# Hallamos los parametros de la distribucion logistica  
  m2<-model_select(tabla_rendimientos$FEMSA,models="logis",criterion="aic")
  coef(m2) 
  
#####
# Una mejor distribucion que se ajusta: t-student
  mod_AAPL<-model_select(tabla_rendimientos$APPL,criterion="aic")
  mod_AAPL
  
library(VGAM)
  
# Calculamos los valores de u2 con la distribucion logistica
  u2<-plogis(tabla_rendimientos$FEMSA,location=coef(m2)[1],scale=coef(m2)[2])
  

##############################################
# Grafico de los puntos u1,u2
  plot(u1,u2)  
  

##############################################
# Determinamos la mejor copula
  library(VineCopula)
  
  BiCopSelect(u1,u2,familyset=NA,selectioncrit = "AIC")
  # Survival Gumbel
  # par=1.15, tau=0.13
  
  
############################### Para checar dependencia
  library(lcopula)
  
  cor.test(x=tabla_rendimientos$WALMEX,
           y=tabla_rendimientos$FEMSA,method = "spearman")
  
  valores<-cbind(tabla_rendimientos$WALMEX,tabla_rendimientos$FEMSA)
  K.plot(valores)
  
############################### Construccion de la copula
  library(VC2copula)
  
  copula<-surGumbelCopula(param = 1.15)
  

############################### Construccion de la funcion de distribucion conjunta
  DistConj<-mvdc(copula,margins=c("logis","logis"),
                 paramMargins=list(
                   list(location=coef(m1)[1],scale=coef(m1)[2]),
                   list(location=coef(m2)[1],scale=coef(m2)[2])
                 )
  )
  
############################### Graficos de mvdc
  library(scatterplot3d)
  
# Simulamos 1000 valores de X e Y
  z <- rMvdc(n=1000,DistConj)
  
# Creamos el grafico de la pdf
  pdf<-dMvdc(z,DistConj) 
  scatterplot3d(z[,1],z[,2],pdf,highlight.3d=T)
  
# Creamos el grafico de la cdf
  cdf<-pMvdc(z,DistConj) 
  scatterplot3d(z[,1],z[,2],cdf,highlight.3d=T)
  
  
# gofCopula para pruebas de bondad de ajuste
  
  valores<-cbind(tabla_rendimientos$TSLA,tabla_rendimientos$AAPL)
  
  
  gofCopula(copula,valores) # p-value de 0.6239

  
# Prueba con ua copula random
  copulaprueba<-claytonCopula(param=1.5,dim=2)
  gofCopula(copulaprueba,valores) # p-value de 0.001499
  
  
###########################################################################################  
# Simulacion de rendimientosy obtención del VaR
  
VaR95<-data.frame()
tVaR95<-data.frame()  
library(cvar)  


  for(i in 1:5){ # Este "for" corresponde al numero de simulaciones, tarda mucho tiempo, se puede correr una sola simulacion para ver los resultados rapidos
    
    rendimientos_sim<-data.frame()
    
   
    for(k in 1:2){# Crear bien el for
      for(j in 1:nrow(tabla_rendimientos)){
        rendimientos_sim[j,k]<-rMvdc(n=nrow(tabla_rendimientos),DistConj)[j,k]
      }
    } 
    
    
    
    #######################
    # Revaluacion
    # Una vez obtenido los rendimientos simulados, procedemos a calcular la revaluacion para cada emisora
    
    tabla_revaluacionSIM<-data.frame()
    for(k in 1:2){
      
      for( j in 1:nrow(rendimientos_sim)){
        tabla_revaluacionSIM[j,k]<-ultimo_precio[,k]*(1+rendimientos_sim[j,k])
      }
      
    }
    
    #######################
    # P&L indivual
    # Construimos la P&L de cada emisora
    PL_EmisorasSIM<-data.frame()
    
    for (k in 1:2){
      
      for(j in 1:nrow(tabla_revaluacionSIM)){
        
        PL_EmisorasSIM[j,k]<-ultimo_precio[k]-tabla_revaluacionSIM[j,k]
      }
    }
    
    
    #######################
    # Para cada emisora, calculamos el VaR y los guardamos en un dataframe, de modo que
    # tendremos 10 valores de VaR al 95% de confianza y se guardan todos en una fila
    # del dataframe VARSM95 (para los otros dos es analogo)
    
    
#####################################################################################  
# Todo lo que está abajo se hzi para calcular el VaR y ES de forma manual
# Calculamos el tVaR
    

    
# Creamos el vector de probabilidades
#    prob<-rep(1/nrow(PL_EmisorasSIM),times=nrow(PL_EmisorasSIM))
  
    # Unimos ambas tablas
#    Tabla_Alisado<-cbind(PL_EmisorasSIM,prob)
#    colnames(Tabla_Alisado)<-c(cartera,"probabilidades")
#    head(Tabla_Alisado)

    # Creamos una tabla para cada emisora donde se ordenen los valores de su P&L
    # Las siguientes lineas solo calculan el VaR por Alisado exponencial de cada emisora
    
# ---------------------------------------------------------------------------------
    # TSLA
#    Tabla_Alisado1<-Tabla_Alisado[,c(1,3)]
#    Tabla_Alisado1 <- Tabla_Alisado1[with(Tabla_Alisado1, order(-Tabla_Alisado1$TSLA)), ] 
    
    # Creamos la columna de Fx
#    Alisado1<-Tabla_Alisado1$probabilidades
    
#    Fx<-vector()
    
#    for(i in 1:length(Alisado1)){
#      Fx[i]<-sum(Alisado1[i:length(Alisado1)])
#    }
    
#    head(Fx,30)
    
    # Unimos la columna de Fx con la Tabla_Alisado
#    Tabla_Alisado1<-cbind(Tabla_Alisado1,Fx)
#    head(Tabla_Alisado1,5)
    
    #Guardamos las observaciones mayores que estén por arriba del 95%
#    tabla_emisora1_95<-Tabla_Alisado1[which(Tabla_Alisado1$Fx>=0.95),] # Guarda todos los valores mayores a 0.95
    
    # VaR al 95% (1 dia)
#    VaRAE_95EMISORA1<-tabla_emisora1_95[length(tabla_emisora1_95$Fx),1] # Se toma el primer valor que sea mayor a 0.95
#    VaRAE_95EMISORA1
    
    # tVaR AL 95% (1 dia)
#    tVaR_95<-mean(tabla_emisora1_95$TSLA)
    
    
#    # Resultados - VaR 1 dia de TESLA
#    VaRAE_emisora1<-cbind(VaRAE_95EMISORA1,tVaR_95) 
#    colnames(VaRAE_emisora1) <-c("VaR al 95%","tVaR AL 95%")  
#   VaRAE_emisora1
#    
# ---------------------------------------------------------------------------------  
    

    for(j in 1:2){
      VaR95[i,j]<-quantile(PL_EmisorasSIM[,j],probs=0.95)
      tVaR95[i,j]<-ES(dist=PL_EmisorasSIM[,j],p_loss = 0.05)
    }
    

        
    
}
colnames(VaR95)<-c("TSLA","AAPL")
colnames(tVaR95)<-c("TSLA","AAPL")  
  

# VaR y tVaR
colMeans(VaR95)
colMeans(tVaR95)
  
#table<-rbind("VaR al 95%", "tVaR al 95%")  
#table<-cbind(colMeans(VaR95),colMeans(tVaR_95))  




  
  
