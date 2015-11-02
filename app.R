
mypremium<-function(kilometres,zone,bonus,make,limit,deductable){

  alpha1<-ifelse(kilometres=="less than 1000",1,0)
  alpha2<-ifelse(kilometres=="from 1000 to 15000",1,0)
  alpha3<-ifelse(kilometres=="15000 to 20000",1,0)
  alpha4<-ifelse(kilometres=="20000 to 25000",1,0)
  alpha5<-ifelse(kilometres=="more than 25000",1,0)
  
  beta1<-ifelse(zone=="Stockholm, Goteborg, Malmo with surroundings",1,0)
  beta2<-ifelse(zone=="Other large cities with surroundings",1,0)
  beta3<-ifelse(zone=="Smaller cities with surroundings in southern Sweden",1,0)
  beta4<-ifelse(zone=="Rural areas in southern Sweden",1,0)
  beta5<-ifelse(zone=="Smaller cities with surroundings in northern Sweden",1,0)
  beta6<-ifelse(zone=="Rural areas in northern Sweden",1,0)
  beta4<-ifelse(zone=="Gotland",1,0)
  
  r1<-ifelse(bonus==1,1,0)
  r2<-ifelse(bonus==2,1,0)
  r3<-ifelse(bonus==3,1,0)
  r4<-ifelse(bonus==4,1,0)
  r5<-ifelse(bonus==5,1,0)
  r6<-ifelse(bonus==6,1,0)
  r7<-ifelse(bonus==7,1,0)
  
  s1<-ifelse(make==1,1,0)
  s2<-ifelse(make==2,1,0)
  s3<-ifelse(make==3,1,0)
  s4<-ifelse(make==4,1,0)
  s5<-ifelse(make==5,1,0)
  s6<-ifelse(make==6,1,0)
  s7<-ifelse(make==7,1,0)
  s1<-ifelse(make==8,1,0)
  s7<-ifelse(make==9,1,0)

testdata1<-c(alpha1,alpha2,alpha3,alpha4,beta1,beta2,beta3,beta4,beta5,r1,r2,r3,r4,r5,r6,s1,s2,s3,s4,s5,s6,alpha1*beta4,alpha4*beta4,r1*s1,r1*s5,r2*s1,r2*s5,r3*s1,alpha1*r1,alpha1*r3,alpha2*r1,alpha2*r2,alpha3*r1,alpha3*r6)


alpha1_1<-ifelse(kilometres==1,1,0)
  alpha2_1<-ifelse(kilometres==2,1,0)
  alpha3_1<-ifelse(kilometres==3,1,0)
  alpha4_1<-ifelse(kilometres==4,1,0)
  alpha5_1<-ifelse(kilometres==5,1,0)
  
  beta1_1<-ifelse(zone==1,1,0)
  beta2_1<-ifelse(zone==2,1,0)
  beta3_1<-ifelse(zone==3,1,0)
  beta4_1<-ifelse(zone==4,1,0)
  beta5_1<-ifelse(zone==5,1,0)
  beta6_1<-ifelse(zone==6,1,0)
  beta7_1<-ifelse(zone==7,1,0)
  
  r1_1<-ifelse(bonus==1,1,0)
  r2_1<-ifelse(bonus==2,1,0)
  r3_1<-ifelse(bonus==3,1,0)
  r4_1<-ifelse(bonus==4,1,0)
  r5_1<-ifelse(bonus==5,1,0)
  r6_1<-ifelse(bonus==6,1,0)
  r7_1<-ifelse(bonus==7,1,0)
  
  s1_1<-ifelse(make==1,1,0)
  s2_1<-ifelse(make==2,1,0)
  s3_1<-ifelse(make==3,1,0)
  s4_1<-ifelse(make==4,1,0)
  s5_1<-ifelse(make==5,1,0)
  s6_1<-ifelse(make==6,1,0)
  s7_1<-ifelse(make==7,1,0)
  s8_1<-ifelse(make==8,1,0)
  s9_1<-ifelse(make==9,1,0)

 testdata2<-c(alpha2_1,alpha3_1,alpha4_1,alpha5_1,beta2_1,beta3_1,beta4_1,beta5_1,beta6_1,beta7_1,r2_1,r3_1,r4_1,r5_1,r6_1,r7_1,s2_1,s3_1,s4_1,s5_1,s6_1,s7_1,s8_1,s9_1)
 coef.data1<-c(-3.121578,0.5704,-0.4144,-0.2873,-0.2068,0.3189,0.0798,-0.0679,-0.2557,-0.2054,1.0697,0.7859,0.6552,0.5067,0.4067,0.3086,0.007,0.1522,-0.1606,-0.5656,0.1640,-0.2619,-0.0684,0.0932,0.2752 ,0.2436,0.2501,0.3146,0.1379,0.2143,-0.1055,0.3098,0.0905,0.1530,0.10233)

 coef.data2<-c(8.476,3.080,-8.337,7.659,-1.237,2.287,4.785,1.287,5.170,1.465,2.278,6.917,4.979,5.119,-7.706,-1.821,-5.207,-3.523,8.435,-1.643,-8.718,-3.932,-1.194,2.135,-5.490 )
  n.claim<-exp(sum(coef.data1[2:length(coef.data1)]*testdata1)+coef.data1[1])
  size.claim<-exp(sum(coef.data2[2:length(coef.data2)]*testdata2)+coef.data2[1])
  #limit<-0
#deductable<-1000
  if (size.claim<limit){
    size.claim1<-0}else if(size.claim>=limit & size.claim<deductable){
      size.claim1<-size.claim-limit
    }else if(size.claim>=deductable){
      size.claim1<-deductable-limit
    }
  premium<-n.claim*size.claim1
  return(premium)
}


library(shiny)

ui<-
 shinyUI(pageWithSidebar(
  headerPanel("자동차 보험료 계산"),
  sidebarPanel(  
   selectInput(inputId="kilometres",
               label="주행거리(kilometer)",
               choices=c("less than 1000","from 1000 to 15000","15000 to 20000","20000 to 25000","more than 25000"),
               selected="less than 1000"
               ),
   selectInput(inputId="zone",
               label="운전지역(zone)",
               choices=c("Stockholm, Goteborg, Malmo with surroundings",
                         "Other large cities with surroundings",
                         "Smaller cities with surroundings in southern Sweden",
                         "Rural areas in southern Sweden",
                         "Smaller cities with surroundings in northern Sweden",
                         "Rural areas in northern Sweden",
                         "Gotland"),
               selected="Stockholm, Goteborg, Malmo with surroundings"),
   selectInput(inputId="bonus",
               label="무사고 보너스",
               choices=seq(1,7),
               selected=1),
   selectInput(inputId="make",
               label="자동차 종류",
               choices=seq(1,9),
               selected=1),
   numericInput("limit",
               label="보상한도(limit)",value=0),
   numericInput("deductable",
               label="자기공제액(deductable)",value=10000),
   submitButton("View")
  ),
 mainPanel(
  h3(textOutput("caption")),
  verbatimTextOutput("premium"))
 ))
 server<-
 shinyServer(function(input,output){
   output$caption<-renderText({
     "자동차 보험료"
   })
   output$premium<-renderText({
    mypremium(input$kilometres,input$zone,input$bonus,input$make,input$limit,input$deductable)
  })
})
shinyApp(ui,server)
