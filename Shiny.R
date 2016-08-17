library(shiny)
GCL<-function(nsim,semilla=21849,incremento=1,multiplicador=2456651,M=2^32){
  seq<-semilla
  for(i in 1:nsim){
    new<-(seq[i]*multiplicador+incremento)%%M
    seq<-c(seq,new)
  }
  return(seq/M)
}
ui<-fluidPage(
  sliderInput(inputId="num",
              label="Elige el numero de simulaciones",
              value = 25, min=1, max=1000),
      plotOutput("hist"),
      plotOutput("grafica")
  )

server<-function(input,output){
   
  output$hist<-renderPlot({
  X<-GCL(input$num)
  hist(X)
  })
  output$grafica<-renderPlot({
    X<-GCL(input$num)
    ant<-X[-length(X)]
    sig<-X[-1]
    plot(ant,sig,pch=16)
  })
}

shinyApp(ui=ui,server=server)
