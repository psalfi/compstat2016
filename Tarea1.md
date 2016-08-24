#Maestría en Ciencias de Datos
* Tarea 1 Simulador de datos exponenciales
* Profesor: Mauricio Benjamin García Tec
* Alumna: Paulina Lisett Salgado Figueroa
* Clave: 160314

##Código desarrollado en R
````R
library(shiny)
library(plotly)

Finv<-function(u,lambda){return(-log(1-u)/lambda)}

ui<-fluidPage(
  numericInput("nsim", "Numero de Simulaciones:", 100, 1),
  numericInput("semilla", "Semilla:", 160124, 1),
  numericInput("lambda", "lambda", .2, 1),
  plotlyOutput("hist"),
  verbatimTextOutput("Prueba")
)

server<-function(input,output){
  
  output$hist<-renderPlotly({
    U<-runif(input$nsim)
    X<-Finv(U,input$lambda)
    X2<-rexp(input$nsim,input$lambda)
    plot_ly(x=X,type="histogram",opacity=.3,group="Simulacion")%>%
    add_trace(x=X2,type="histogram",opacity=.3,group="Exponencial")
  })
  output$Prueba<-renderPrint({
  print("Prueba de bondad de ajuste Kolmogorov-Smirnov")
  p<-(ks.test(X,"pexp",lambda))
  p
  })
}

shinyApp(ui=ui,server=server)



