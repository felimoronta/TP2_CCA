
################################################################################################################
#
#
# Materia: Computación Científica Actuarial
# Trabajo Práctico N°2
#
#Integrantes:
#           *Bitler Tomas
#           *García Scarafia Carolina
#           *Kerszerblat Magali
#           *Moronta Felipe
#
################################################################################################################

#Hay otro Script con el Aplicativo Shiny ( independiente de este script)


# Librerias utilizadas ----------------------------------------------------

  #Generamos una lista con los paquetes utilizados y los carga en la sesión.
  #Si previamente no estan descargados, los descarga:
  
    paquetes <- list(
      "Scrapping"=list("rvest","xml2"),
      "Grafico" = list("plotly","ggplot2","gplots"),
      "Financiero" = list("quantmod","FinTS","PerformanceAnalytics","PortfolioAnalytics","fPortfolio","fAssets"),
      "Modelado" = list("stats", "tseries", "fBasics"),
      "Analisis" = list("forecast", "rlang","ROI","ROI.plugin.glpk","pracma","ROI.plugin.quadprog")
    )
    
    lapply(as.list(c(paquetes, recursive = T, use.names = F)),
           function(x) {
             if (x %in% rownames(installed.packages()) == FALSE) {
               install.packages(x, verbose = F)
             }
             library(x, character.only = T, verbose = F)
           })
    rm(list = c("paquetes"))
    

  #Observación: Las conclusiones expuestas son con información al día 30/07/2020
  
  
#Cartera:Descarga de Datos  ---------------------------------------------------------------- 


  # Obteniendo nuestra cartera de activos ---------------------------------------------------
  
  # En base al aplicativo Shiny seleciconamos estos 5 activos que van a ser los que 
  # formarán nuestra cartera:


    activos=c("AAPL","XOM","HD","JPM","JNJ")
    getSymbols(activos,from="2010-01-01")
    precios<- cbind(Ad(AAPL),Ad(XOM),Ad(HD),Ad(JPM),Ad(JNJ))
    names(precios)<-c("AAPL","XOM","HD","JPM","JNJ") 

      # Activo APPLE ------------------------------------------------------------
    
      #Utilizaremos el registro diario de los precios de cierre
      AAPLcierre=AAPL[,4]
        x11(title ="Evolucion de los Precios de Apple")
      par(mfrow=c(1,1))
      plot(AAPLcierre)
      
      #Vemos la autocorrelacion y la autocovarianza
      x11(title="Autocorrelacion y Autocovarianza de Apple")
      par(mfrow=c(1,2))
      acf(AAPLcierre, main= "ACF")
      pacf(AAPLcierre, main= "PACF")
      
      #En el modelo ACF se observa una tendencia lineal propia de una serie no estacionaria.
      
          
      #Creamos un modelo ARIMA
      auto.arima(AAPLcierre, seasonal = FALSE)  #usamos forecast
      
      #el arima optimo a utilizar en base a estimadores de información bayesianos (AIC,AICc, BIC)
      #es ARIMA(5,2,0) 
    
    
      x11(title="Residuos del modelo ARIMA")
      par(mfrow=c(2,1))
      arima_1=auto.arima(AAPLcierre, seasonal = FALSE)
      tsdisplay(residuals(arima_1), lag.max = 40, main = "residuos del modelo ARIMA")

      
      ArchTest(AAPLcierre, lags = 10, demean = TRUE)                                #usamos FinTS4
      
      #vemos que el pvalor es muy cercano a cero, por ende podemos decir que la serie puede
      #ser representada por un modelo ARCH
      
      #Generamos un modelo ARCH generalizado (GARCH):
      
      APPLE<-garch(AAPLcierre, order = c(0,1))
      summary(APPLE)
      
      
      # Indice S&P 500 ----------------------------------------------------------
      
      
      getSymbols('^GSPC', from="2010-01-01")
      GSPCcierre=GSPC[,6]
      x11(title="Evolución de precios del indice S&P 500")
      plot(GSPCcierre, col="darkgreen", xlab="Fecha", ylab="AdjClose"); title(main="Evolucion S&P 5OO")
      
          
        
    
# Cartera:Gráficas y métricas ------------------------------------------------------------
    
    #Graficamos los distintos activos que componen la cartera
        
    precios.ts <- as.timeSeries(precios)  # usamos TimeSeries
    x11(title = "Activos que componen la cartera")
    plot(precios.ts,main="Activos que componen la cartera")

    
    # Retornos de los activos----------------------------------------------
    
    x11(title="Retornos diarios de la cartera")
    par(mfcol = c(3, 2)) 
    returnPlot(precios.ts,type = "l",labels = c("A","B"))     #Retornos de cada activo que compone la cartera
    
    #matriz de retornos
    
    retornos <- na.omit(Return.calculate(precios, method = "log"))                # usamos PerformanceAnalytics
    retornos <- retornos[-1,] #saco fechas
    
    #calculo las estadisticas basicas
    
    basicStats(retornos)                                                          #usamos fBasics
    medias=colMeans(retornos*100)
    covarianza<-cov(retornos)
    desvios=apply(retornos,2, sd)
    
    #graficamos el riesgo en relacion al retorno de cada accion
    relacion=data.frame(cbind(desvios,medias))
    ACCIONES=colnames(retornos)
    
    x11(title="Grafico de dispersion Riesgo/Retorno") 
    ggplot(relacion, aes(x= desvios, y = medias , color = ACCIONES)) +           #usamos ggplot2
    geom_point(size = 5) +
    theme_bw() + ggtitle("Riesgo Retorno") +
    xlab("Volatilidad") + ylab("Retornos Esperados")
    
    # vemos que la que mas retorno otorga es APPLE, pero al mismo tiempo 
    # es uno de los retornos mas volátiles.HD o JNJ, en cambio, se pueden decir 
    # que poseen un mayor Sharpe Ratio, lo veremos ahora.
    
            
        #Obtención de la tasa libre de riesgo mediante Web Scrapping --------------
        
        url = "https://datosmacro.expansion.com/bono/usa"
        pagina = read_html(url)
        
        pagina %>%
          html_nodes(".numero:nth-child(1)") %>%
          html_text()  -> tlr
        tlr = as.numeric(gsub(",", ".", tlr))
        tlr_diaria=nthroot((1+tlr),365)-1  #Usamos equivalencia de tasas para hallar
                                          #la tasa de interés diaria.
        
        #Suponemos que no existe arbitraje en la curva de tasas
        
        
        #Calculamos para cada activo el Sharpe Ratio Diario y vemos si 
        #las conclusiones obtenidas del grafico son correctas.

      # Cálculo del Sharpe Ratio y Betas de cada activo -----------------------------------------------

      SharpeRatio_diario=(relacion$medias-tlr_diaria)/relacion$desvios
      names(SharpeRatio_diario)=NOMBRES
      
      SR_Acciones=sort(SharpeRatio_diario,decreasing = T)
      SR_Acciones
      
      #Según esta medida, HD se posiciona mejor que AAPL,
      #siendo verdaderas nuestras sospechas.
      
      #Ahora calculamos los Betas que poseen los distintos activos
        
      getSymbols(NOMBRES,from="2011-01-01", periodicity = "monthly") #traemos los valores mensuales ya que el S&P se descarga mensual
      preciosmensuales<- cbind(Ad(AAPL),Ad(XOM),Ad(HD),Ad(JPM),Ad(JNJ))
      names(preciosmensuales)<-c("AAPL","XOM","HD","JPM","JNJ") 
      
      retornosmensuales <- na.omit(Return.calculate(preciosmensuales, method = "log"))
      retornosmensuales <- retornosmensuales[-1,] #saco fechas
      retornosmercado <-
      Return.calculate(GSPCcierre, method = "log") %>% 
      na.omit()
      
      betas=CAPM.beta(retornosmensuales, retornosmercado)
      #Vemos que Home Depot es el activo que mayor elasticidad tiene antes cambios del mercado (negativamente)
      #mientras que Johnson & Johnson es el activo mas inelástico respecto al mercado.
        
# Cartera:Matriz de correlaciones -----------------------------------------
        
    x11(title = "Matriz de correlacion entre activos")
    generate_heat_map <- function(correlationMatrix, title)
    {
          
      heatmap.2(x = correlationMatrix,         # usamos gplots
        cellnote = correlationMatrix,   
        main = title,           
        symm = TRUE,            
        dendrogram="none",      
        Rowv = FALSE,           
        trace="none",           
        density.info="none",        
        notecol="black")          
          }
        
    corr1 <- round(cor(retornos), 2)
    generate_heat_map(corr1,"Heatmap: Correlaciones")
          

# Cartera: Creando un portafolio eficiente usando ROI --------------------------------


    # portafolio con ventas en descubierto ------------------------------------

      
        
        portafolio <- portfolio.spec(colnames(retornos))                   # usamos PortfolioAnalytics
        
      #Restricción de inversión completa de modo que los pesos sumen 1
        portafolio <- add.constraint(portfolio = portafolio,
                                     type = "full_investment")
        
        #Objetivo para minimizar la desviación estándar de la cartera
        portafolio <- add.objective(portfolio = portafolio, type = "risk",
                                    name = "StdDev")
        
        #Optimización de la cartera utilizando optimize_method = ROI".
        portafolio <- optimize.portfolio(retornos, portfolio =
                                           portafolio,
                                         optimize_method = "ROI")
        portafolio
        
        

    # portafolio sin ventas en descubierto método 1 ------------------------------------


        portafolio_info <- portfolio.spec(colnames(retornos))
        
        #Restriccion de inversion completa de modo que los pesos sumen 1
        portafolio_info <- add.constraint(portfolio = portafolio_info,
                                          type = "full_investment")
        
        #Restriccion larga de modo que el peso de un activo este entre 0 y 1
        portafolio_info <- add.constraint(portfolio = portafolio_info, 
                                          type = "long_only")
        
        #Objetivo para minimizar la desviacion estandar de la cartera
        portafolio_info <- add.objective(portfolio = portafolio_info, type = "risk",
                                         name = "StdDev")
        
        #Optimizacion de la cartera utilizando optimize_method = ROI".
        portafolio_minvar <- optimize.portfolio(retornos, portfolio =              #usamos las 3 librerias ROI*
                                               portafolio_info,
                                             optimize_method = "ROI")
        portafolio_minvar
        
          

    # portafolio sin ventas en descubierto método 2 ---------------------------

            
        R=as.timeSeries(retornos)
        PSVC = portfolioSpec(                                                  #usamos fPortfolio y fAssets
                model =
                list(type = "MV", 
                     optimize = "minRisk",
                     estimator = "covEstimator", 
                     tailRisk = list(), 
                     params = list(alpha = 0.01, a = 1)), 
              
                portfolio = 
                list(weights = NULL, 
                     targetReturn = NULL, 
                     targetRisk = NULL, 
                     riskFreeRate = NULL, 
                     nFrontierPoints = 100, 
                     status = NA),
              
                optim = 
                list(solver = "solveRquadprog", 
                     objective = NULL,
                     options = list(meq = 2), 
                     control = list(), 
                     trace = FALSE),
                messages = list())
            
        Const.PSVC = "LongOnly"

      ######## portafolios frontera
      portafoliosfrontera = portfolioFrontier(R, PSVC, Const.PSVC) 
      s.psvc = portafoliosfrontera@portfolio@portfolio$targetRisk[,2] 
      u.psvc = portafoliosfrontera@portfolio@portfolio$targetReturn[,1] 
      
      ######### portafolios minima varianza
      portafoliomv = minvariancePortfolio(R,PSVC, Const.PSVC)
      ( mediadelportafolio = portafoliomv@portfolio@portfolio$targetReturn[1] )
      ( sigmadelportafolio = portafoliomv@portfolio@portfolio$targetRisk[2] )
      ( cantidades = portafoliomv@portfolio@portfolio$weights)

# Cartera: Creando un portafolio eficiente usando Retorno objetivo --------


    setTargetReturn(PSVC) = 0.0008
    portafolioeficiente = efficientPortfolio(R,PSVC, Const.PSVC)
    ( media1 = portafolioeficiente@portfolio@portfolio$targetReturn[1] )
    ( sigma1 = portafolioeficiente@portfolio@portfolio$targetRisk[2] )
    ( cantidades1 = portafolioeficiente@portfolio@portfolio$weights)


#Cartera:Gráfica de ponderación de activos en función del retorno objetivo -------
    
    
    
    x11(title="Ponderación de activos en función del retorno objetivo")
    par(mfrow=c(1,1))
    weightsPlot(portafoliosfrontera, title = FALSE)
    title(xlab = 'retorno esperado', ylab = 'proporciones')
    mtext("PROPORCIONES",side=3,line=2) 
    mtext('riesgo esperado', side = 3, adj = 1, line = 0)
    
    

# Cartera:gráfica de frontera eficiente -----------------------------------
  

    espcartera<-portfolioSpec()
    x11(title="Gráfica de Frontera Eficiente")
    setRiskFreeRate(espcartera)<- tlr_diaria      #Retorno diario del activo Libre de Riesgo Diaria
    setNFrontierPoints(espcartera) <- 20
    frontierPlot(portafoliosfrontera, title =  FALSE)
    title('FRONTERA EFICIENTE', xlab = "riesgo esperado", ylab = "retorno esperado")
    grid()
    minvariancePoints(portafoliosfrontera, col="blue", pch=19, cex=2)
    monteCarloPoints(portafoliosfrontera, mCsteps=500, col="green", cex=0.001) 
