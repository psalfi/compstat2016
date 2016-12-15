## app.R ##
library(dplyr)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(magrittr)
library(MASS)
library(ADGofTest)
library(DT)
library(Rcpp)
wdir<-"C:/Users/psalfi/Documents/EstadisticaComputacional/Tareas"
setwd(wdir)

sourceCpp("FR.cpp")


ui <- dashboardPage(skin = "green",
  dashboardHeader(title = "Paulina Salgado Estadistica Computacional 2016", titleWidth = 550),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Tarea 0 - Numeros Aleatorios", tabName = "tarea0", icon = icon("university")),
      menuItem("Tarea 1 - Funcion Inversa", tabName = "tarea1", icon = icon("university")),
      menuItem("Tarea 2 - Integracion Numerica MC", tabName = "tarea2", icon = icon("university")),
      menuItem("Tarea 3 - Regresion", tabName = "tarea3", icon = icon("calendar"))
     
     
    )
  ),
  
  dashboardBody(
    tabItems(
      # Contenido de los tabs
      tabItem(tabName = "tarea0",
              fluidRow(
                box(
                  title = "Inputs", status="primary", solidHeader = TRUE,
                  sliderInput("slider1", "Cantidad a generar:", 1, 1000, 500),
                  numericInput("nbin0",label = "Numero de cajones en el histograma", min = NA, max = NA, value = 20),
                  height = 300
                ),
                box(title = "Histograma", status="success",solidHeader = TRUE,
                plotOutput("plot1_1",height = 240),
                height = 300
                ),
                box(title = "Dispersion de los puntos", status="success",solidHeader = TRUE,
                plotOutput("plot1_2", height = 240),
                height = 300
                    )
              )
      ),
      tabItem(tabName = "tarea1",
              
              fluidRow(
                box(
                  title = "Objetivo", status="warning", solidHeader = TRUE,
                  "Reforzar el metodo de la funcion inversa como una tecnica 
                   para simular variables aleatorias independientes.", br(),br(), 
                   "Para ello se simulara una variable exponencial ",
                  height = 300
                ),
        
                box(
                  title = "Inputs", status="primary", solidHeader = TRUE,
                  sliderInput("slider2", "Cantidad a generar:", 1, 1000, 500),
                  numericInput("lambda",label = "Valor lambda", min = NA, max = NA, value = 0.5),
                  numericInput("nbin1",label = "Numero de cajones en el histograma", min = NA, max = NA, value = 20),
                  height = 300
                ),
                box(title = "Histograma", status="success",solidHeader = TRUE,
                    plotOutput("plot2_1",height = 240),
                    height = 300
                ),
                box(title = "Funcion de distribucion", status="success",solidHeader = TRUE,
                    plotOutput("plot2_2", height = 240),
                    height = 300
                ),
                box(title = "Prueba de bondad de ajuste", status="success",solidHeader = TRUE,
                    plotOutput("plot2_3", height = 240),
                    height = 300
                ),
                valueBoxOutput("progressBox")
                )
              ),
      tabItem(tabName = "tarea2",
              fluidRow(
                
                box(
                  title = "Objetivo", status="warning", solidHeader = TRUE,
                  "Practicar el entendimiento de Monte Carlo como tecnica de integracion 
                  numerica y usar el enfoque de valor esperado para crear intervalos de precision.", br(),br(), 
                  "Output",br(),
                  "-El resultado de calcular int_a^b f(x) dx",br(),
                  "-Un grafico que muestre los resultados de la simulacion usando N =10, 100, 1000, 10000, 100000 puntos.
                    En el eje y incluir el resultado de la estimacion con N puntos y en el eje x poner log_10(N) (logaritmo base 10).",br(),
                  "-El grafico debe incluir intervalos de confianza al nivel c %",br(),
                  
                  "Intput",br(),
                  "-nsim: numero de simulaciones",br(),
                  "-lambda: parametro de la densidad exponencial que se quiere simular",
                  
                  height = 300
                ), 
              
                 box(
                  title = "Inputs",status="primary", solidHeader = TRUE,
                  textInput("textInput1", value = "function (x) 2*sqrt(4-x^2)", width=NULL, label = "Ingresa una funcion"),
                  numericInput("minimo",label = "Limite inferior", min = -100, max = 100, value = 0),
                  numericInput("maximo",label = "Limite superior", min = -90, max = 110, value = 1),
                  numericInput("alpha",label = "Confianza", min = NA, max = NA, value = 0.05)
                ),
                
               
                
                box(title = "Grafica de la funcion a integrar en el intervalo (a,b)", status="success", solidHeader = TRUE,
                    plotOutput("plot3_1", height = 250)
                    ),
                box(title = "Grafica de simulacion de integracion", status="success", solidHeader = TRUE,
                  plotOutput("plot3_2", height = 250)
                  )    
                )
              ),
      tabItem(tabName = "tarea3",
              fluidRow(
                box(
                  title = "Objetivo", status="warning", solidHeader = TRUE,
                  "Reforzar el metodo de la funcion inversa como una tecnica 
                   para simular variables aleatorias independientes.", br(),br(), 
                  "Para ello se simulara una variable exponencial ",
                  height = 300
                ),
                box(
                  title = "Datos",status="primary", solidHeader = TRUE,
                  DT::dataTableOutput('tbl')
                ),
                box(
                  title = "Inputs",status="primary", solidHeader = TRUE,
                  selectInput("variable1", "Variable dependiente:",
                              c("muertes" = "y",
                                "horas" = "x",
                                "Expuestos" = "n")),
                  
                  selectInput("variable2", "Variable explicativa:",
                              c("muertes" = "y",
                                "horas" = "x",
                                "Expuestos" = "n")),
                 
                  tableOutput("data1")
                ),
                
                box(title = "Grafica de los datos a ajustar", status="success", solidHeader = TRUE,
                    plotOutput("plot4_1", height = 250)
                )
                
                )
              ),
      tabItem(tabName = "tarea5"),
      tabItem(tabName = "tarea6")
    )
  )
)


server <- function(input, output) {
  
  
  datos<-reactive({
      input$slider1
    })  
  
  output$plot1_1 <- renderPlot({
      data <- GCL(datos())
      ggplot()+aes(data)+ geom_histogram(bins=input$nbin0 ,col="green",fill="green",alpha=.2)
    
    })

  output$plot1_2 <- renderPlot({
    data <- GCL(datos())
    x <- data[-length(data)]
    y <- data[-1]
    ggplot()+aes(x,y)+geom_point(col="green",fill="green",alpha=.3)
  })
  
  U2<-reactive({
     runif(input$slider2)
  })
 
  output$plot2_1 <- renderPlot({
   
    X <- Finv(U2(), input$lambda)
    ggplot()+aes(X)+ geom_histogram(aes(y =..density..),bins=input$nbin1,col="green",fill="green",alpha=.2)+geom_density(col=2)
    })
 
  output$plot2_2 <- renderPlot({
   
    X <- Finv(U2(), input$lambda)
    y <- pexp(X, rate = input$lambda)
    Fn<-ecdf(X)
    ggplot()+aes(X,y)+geom_line(col="red")
           })
    
  output$plot2_3 <- renderPlot({
    
    X <- Finv(U2(), input$lambda)
    y <- quantile(X,c(0.25,0.75))
    Xe<-qexp(c(0.25,0.75))
    slope<-diff(y)/diff(Xe)
    int <- y[1]-slope*Xe[1]
    ggplot()+aes(sample=X)+stat_qq(distribution=qexp)+geom_abline(intercept = int,slope=slope,col="red")
  })
  
  output$progressBox <- renderValueBox({
    X <- Finv(U2(), input$lambda)
    ks<- as.numeric(ks.test(X, "pexp", rate =input$lambda))
    ks<-round(ks,3)
      valueBox(
      paste0("p-value=",ks[2]),"Prueba de ajuste Kolmogorov-Smirnov",icon = icon("list"),
      color = "purple"
    )
  })

  output$plot3_1 <- renderPlot({
    X <- seq(input$minimo, input$maximo, length.out = 500)
    
    FUN1 <- reactive({
      texto <- paste("aux <-", input$textInput1)
      eval(parse(text = texto))
      aux
    })
    
    Y_x <- sapply(X, FUN1())
    
    ggplot()+aes(X,Y_x)+geom_line(col="red")
    
  })

  output$plot3_2 <- renderPlot({  
    
    datos<-Integra(input$textInput1,input$minimo,input$maximo)
    
    quant <- qnorm(input$alpha/2, lower.tail=FALSE) 
   
    ggplot(datos,aes(x=log(N)))+
    geom_ribbon(aes(ymin=Int+lim.inff*quant,ymax=Int+lim.supp*quant),fill="grey",alpha=0.4)+
    geom_line(aes(y=Int),color="blue")# +
    })

  
  
  datos2<-read.csv("BASE.csv",header = TRUE)
  output$tbl = DT::renderDataTable(datos2, options = list(lengthChange = FALSE))
  
  output$data1 <- renderTable({
    datos2[, c(input$variable1,input$variable2), drop = FALSE]
  }, rownames = TRUE)
  
  output$plot4_1 <- renderPlot({
   
    x<-datos2[c(input$variable1), drop = FALSE]
    y<-datos2[c(input$variable2), drop = FALSE]
       ggplot()+aes(x,y)+geom_line(col="red")
    
  })
  
  
  }



##############Funciones auxiliares

####Tarea1
#####
####Tarea1
#Funcion GCl para Generar numeros pesudoaleatorios

GCL <-function(nsim, semilla=160314, incremento=1, multiplicador=2456651, M=2^32){
  seq <- semilla
  for(i in 1:nsim){
    new <- (seq[i]*multiplicador +  incremento)%%M
    seq <- c(seq, new)
  }
  return(seq/M)
}
# Fin - Tarea1

#####
####Tarea 2
##Metodo de la funcion inversa

Finv <- function(u, lambda = 0.2) {return(-log(1-u)/lambda)}

####FIN TAREA 2

#####
###Tarea 3
##Integracion numerica usando Monte Carlo

Integra<-function(fun1,minimo,maximo)
{

  fun <- reactive({
    texto <- paste("aux <-", fun1)
    eval(parse(text = texto))
    aux
  })

S2=0
Int=0
lim.inff=0
lim.supp=0
N <- c(10,100,1000,10000,100000)

for(i in 1:5){
  
  U<-runif(N[i],minimo, maximo)
  Y_u <- sapply(U, fun())
  Int[i] = (maximo-minimo)*mean(Y_u)
  S2= var(Y_u)
  
  lim.inff[i] <- (sqrt(S2/N[i])) 
  lim.supp[i] <- (-sqrt(S2/N[i]))  
}

Valor <- integrate(fun(),minimo,maximo)

 return(data.frame(N,Int,lim.inff,lim.supp))

 
}  

int_tra<-function(){

}




shinyApp(ui,server)



