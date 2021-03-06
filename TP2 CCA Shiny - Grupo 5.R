
################################################################################################################
#
#
# Materia: Computaci�n Cient�fica Actuarial
# Trabajo Pr�ctico N�2
#
#Integrantes:
#           *Bitler Tomas
#           *Garc�a Scarafia Carolina
#           *Kerszerblat Magali
#           *Moronta Felipe
#
################################################################################################################



################################################################################################################
#####################################APLICATIVO INTERACTIVO EN SHINY############################################
################################################################################################################

# Librerias utilizadas ----------------------------------------------------

#Generamos una lista con los paquetes utilizados y los carga en la sesi�n.
#Si previamente no estan descargados, los descarga:


paquetes <- list(
  "Shiny" = list("shiny","shinyWidgets","shinythemes"),
  "Financiero" = list("quantmod","fPortfolio","fAssets"),
  "Modelado" = list("fBasics")
      )
lapply(as.list(c(paquetes, recursive = T, use.names = F)),
       function(x) {
         if (x %in% rownames(installed.packages()) == FALSE) {
           install.packages(x, verbose = F)
         }
         library(x, character.only = T, verbose = F)
       })
rm(list = c("paquetes"))


#seleccionamos todo( Ctrl+A) y ejecutamos (Ctrl+R).
#De esta forma activamos las librerias para la sesi�n
#(en caso de no estar instaladas, se instalan),
#definimos la funci�n server y la funci�n ui
#y ejecutamos la aplicaci�n utilizando ShinyApp()



# SERVER ------------------------------------------------------------------

# Definimos las acciones que realiza nuestro Tablero Shiny por dentro
#definiendo el sever

s<-function(input, output) {
  # Generando la informacion que debemos utilizar y que reaccione ante cambios 
  # en el input ingresado:
  
  dataInput <- reactive({
    if (input$get == 0)
      return(NULL)
    
    return(isolate({
      getSymbols(input$symb, auto.assign = FALSE)
    }))
  })
  
  datesInput <- reactive({
    if (input$get == 0)
      return(NULL)
    
    return(isolate({
      paste0(input$dates[1], "::",  input$dates[2])
    }))
  })
  
  returns <- reactive({ 
    if (input$get == 0)
      return(NULL)
    
    dailyReturn(dataInput())
  })
  
  xs <- reactive({ 
    if (input$get == 0)
      return(NULL)
    
    span <- range(returns())
    seq(span[1], span[2], by = diff(span) / 100)
  })
  
  
# Asignamos cada cuadro control a su correspondiente pesta�a:
  output$newBox <- renderUI({
    switch(input$tab,
           "Gr�fico" = chartControls,
           "Modelo" = modelControls
    )
  })
  
  # Indicamos el tipo de gr�fico que deseamos en la pesta�a GRAFICO:
  
  chartControls <- div(
    wellPanel(
      selectInput("chart_type",
        label = "Tipo de Gr�fico",
        choices = c("Gr�fico de velas" = "candlesticks", 
          "Matchstick" = "matchsticks",
          "Gr�fico de barras" = "bars",
          "Gr�fico de lineas" = "line"),
        selected = "Gr�fico de velas"
      ),
      checkboxInput(inputId = "log_y", label = "Escala logar�tmica", 
                    value = FALSE)
    ),
    
    #Las diferentes medidas t�cnicas que se desean agregar al gr�fico
    
    wellPanel(
      p(strong("An�lisis T�cnico")),
      checkboxInput("ta_vol", label = "Volumen", value = FALSE),
      checkboxInput("ta_sma", label = "Media M�vil Simple (SMA)", 
                    value = FALSE),
      checkboxInput("ta_ema", label = "Media M�vil Exponencial (EMA)", 
                    value = FALSE),
      checkboxInput("ta_wma", label = "Media M�vil Ponderada (WMA)", 
                    value = FALSE),
      checkboxInput("ta_macd", label = "Media M�vil de Convergencia/Divergencia (MACD)", 
                    value = FALSE),
      checkboxInput("ta_rsi", label = "Indice de Fuerza Relativa (RSI)", 
                    value = FALSE),
      checkboxInput("ta_bb", label = "Bandas de Bollinger", 
                    value = FALSE),
      checkboxInput("ta_momentum", label = "Momentum", 
                    value = FALSE),
      actionButton("chart_act", "Agregar m�tricas t�cnicas"),
      br()
      )
  )
  
  MTInput <- reactive({
    if (input$chart_act == 0)
      return("NULL")
    
    tas <- isolate({c(input$ta_vol, input$ta_sma, input$ta_ema, 
      input$ta_wma,input$ta_macd,input$ta_rsi,input$ta_bb, input$ta_momentum)})
    funcs <- c(addVo(), addSMA(), addEMA(), addWMA(),addMACD()
               ,addRSI(),addBBands(), addMomentum())
    
    if (any(tas)) funcs[tas]
    else "NULL"
  })
  #Funci�n que grafica en funci�n de lo ingresado/seleccionado:
  output$chart <- renderPlot({
    chartSeries(dataInput(),
                name = input$symb,
                type = input$chart_type,
                subset = datesInput(),
                log.scale = input$log_y,
                theme = "white",
                grid=TRUE,
                TA = MTInput())
  })

  #Funci�n que grafica los retornos diarios de la pesta�a RETORNO:
  output$daily <- renderPlot({
            returnPlot(as.timeSeries(dataInput()),col = "black"
                      )
  })


  #Controles correspondientes a la pesta�a MODELO:
  
  modelControls <- div(
      br(),
      
      sliderInput("n", "Cantidad de intervalos contemplados en el histograma",
        min = 1, max = 250, value = 30
      ),
      
      br(),
    
    wellPanel(
      selectInput("dist", "Distribuci�n a utilizar",
                choices = c("Normal", "Exponencial Doble", "t"),
                selected = "Normal"
      ),
    sliderInput("mu", "Media",
        min = -1, max = 1, value = 0, step = 0.01
      ), 
      sliderInput("sigma", "Desv�o est�ndar",
        min = 0, max = 0.1, value = 0.05, step = 0.001
      ),
      conditionalPanel(condition = "input.dist == 't'",
        sliderInput("df", "Grados de Libertad",
          min = 2, max = 1000, value = 10
                      )
      )
      
    )
  )
    
  output$cs<-renderText({paste0(input$symb," - ","Retornos diarios:desde ", 
        input$dates[1], " hasta ", input$dates[2], sep = "")})
  
  ys <- reactive({ 
    if (input$get == 0)
      return(NULL)
    
    switch(input$dist,
           "Doble exponencial" = dlaplace(xs(), 
                                location = input$mu, 
                                scale = input$sigma
                                ),
           "Normal" = dnorm(xs(), 
                             mean = input$mu, 
                             sd = input$sigma
                             ),
           "t" = dt((xs() - input$mu) / input$sigma,
                    df = input$df) * sqrt(2 * length(returns()))
    )
  })
    
  ks <- reactive({
    switch(input$dist,
           "Doble exponencial" = ks.test(returns(), "plaplace", 
                                          input$mu, input$sigma),
           "Normal" = ks.test(returns(), "pnorm", 
                              input$mu, input$sigma),

           "t" = ks.test((returns() - input$mu) / input$sigma, "pt", 
                         input$df)
    )
  })

  #funci�n que grafica el histograma presentado en la pesta�a MODELO:
  
  output$hist <- renderPlot({
    hist(returns(), xlab = "returns", freq = FALSE,
      main = paste(input$symb," - ", "Retornos diarios:desde ", 
        input$dates[1], " hasta ", input$dates[2], sep = ""),
      breaks = input$n)
   lines(xs(), ys(), col = "red")
  })
  
  
  output$ks <- renderText({
    paste0("Estad�stico de Kolmogorov-Smirnov : ", round(ks()$statistic,10))
  })
  output$ksp <- renderText({
    paste0("P-valor del Modelo: ", round(ks()$p.value,10))
  })
 
}


# UI ----------------------------------------------------------------------

# Indicaremos lla interfaz gr�fica general del aplicativo como es el t�tulo principal,
# el tema del aplicativo, el tablero control com�n a todas las pesta�as y asignaremos
# a cada una de ellas sus outputs correspondientes creados en el servidor:

ui <-bootstrapPage( shinyUI(fluidPage(
  theme=shinytheme("journal"),
  themeSelector(),
  pageWithSidebar(
    
    headerPanel("Gr�fico Hist�rico de precios"),
    
    sidebarPanel(
      
      helpText("Indica la acci�n a graficar.
        La informaci�n surge de Yahoo finance."),
      
      selectInput(inputId = "symb", label="Selecciona el activo que desea analizar:",
                  choices = list(
                    "Activos Argentinos"=list("MELI","TEN.MI", "GLOB", "YPF","GGAL","PAM","TEO"),
                    "Activos Internacionales"=list("AAPL","MSFT","AMZN","GOOG","FB","WMT","JPM",
                                                   "TSLA","VZ","BAC","PFE","NFLX","DIS","INTC",
                                                   "CSCO","IBM","C","BA","GE","F"),
                    "Indices"=list("^GSPC","^DJI","^IXIC","^MERV","^MXX","^BVSP")), 
                  selected = "", width = "100%"),
      
      dateRangeInput("dates", 
                     "Precios diarios: desde ",
                     start = "2020-01-01", end = "2020-07-28",separator = " hasta "),
      actionButton("get", "Actualizar datos"),
      
      br(),
      
      br(),
      
      uiOutput("newBox")
      
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Gr�fico", plotOutput("chart")), 
        tabPanel("Retorno",div(h3(textOutput("cs"))),
                 plotOutput("daily")),
        tabPanel("Modelo", div(h3(textOutput("ks"))), 
                 div(h3(textOutput("ksp"))), 
                 plotOutput("hist")),
        id = "tab"
      )
    )
  )
)))


# Teniendo creadas y cargadas las funciones s(Servidor) y ui ( Interfaz de usuario).
# ejecutamos el aplicativo:

# C�digo de ejecuci�n -----------------------------------------------------

shinyApp(ui = ui,server = s)


