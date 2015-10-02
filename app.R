myasian<-function(r,sigma,s0,radio,k,t){
  #r<-0.04
  #sigma<-0.27
  #s0<-236500
  #radio<-1
  #k<-236500
  #t<-1
  
  delta.t<-1/250
  T<-round(t/delta.t)
  m<-1000
  c.t<-0
  p.t<-0
  sum.S<-0
  final<-0

  if(radio==1){            # ct 계산 
    for(i in 1:m){
      sum.S<-0
      for(j in 1:T){
        Z.j<-rnorm(1)
        if(j==1){
          S.j<-s0*exp((r-sigma^2/2)*delta.t+sigma*sqrt(delta.t)*Z.j)
        }else{
          S.j<-S.j*exp((r-sigma^2/2)*delta.t+sigma*sqrt(delta.t)*Z.j)
        }
        sum.S<-sum.S+S.j
      }
      s.bar<-sum.S/T
      if(s.bar-k>0){
        c.t<-c.t+exp(-r*t)*(s.bar-k)}
    }
    final<-c.t/m
  }else{
    for(i in 1:m){
      sum.S<-0
      for(j in 1:T){
        Z.j<-rnorm(1)
        if(j==1){
          S.j<-s0*exp((r-sigma^2/2)*delta.t+sigma*sqrt(delta.t)*Z.j)
        }else{
          S.j<-S.j*exp((r-sigma^2/2)*delta.t+sigma*sqrt(delta.t)*Z.j)
        }
        sum.S<-sum.S+S.j
      }
      s.bar<-sum.S/T
      if(s.bar-k>0){
        c.t<-c.t+exp(-r*t)*(s.bar-k)}else{
          p.t<-p.t+exp(-r*t)*(k-s.bar)}
    }
    final<-p.t/m
  }
  
  return(as.numeric(final))}
myasian(0.04,0.27,236500,1,236500,1)


install.packages("shiny")
library(shiny)

ui<-
  shinyUI(pageWithSidebar(
    headerPanel("Asian option Application"),
    
     sidebarPanel(
       radioButtons("radio",label=h3("Option"),choices=list("Asian Call"=1,"Asian Put"=2),selected=1),
       sliderInput("r","Interest",min=0,max=1,step=0.01,value=0.5),
      numericInput("sigma","Volatility",0.27),
      numericInput("s0","Stock price",235600),
      
      numericInput("k","Option Price",235600),
      numericInput("t","due(year)",1),
      
      submitButton("View")
      
    ),
    mainPanel(
      h3(textOutput("caption")),
      verbatimTextOutput("price") )
  ))
  server<-
  shinyServer(function(input,output){
    output$caption<-renderText({
      "Asian Call & Asian Put"
    })
    
    output$price<-renderText({
      myasian(input$r,input$sigma,input$s0,input$radio,input$k,input$t)
      
    })
  })
shinyApp(ui,server)
