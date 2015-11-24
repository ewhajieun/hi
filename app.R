glm.ftn<-function(sol.data,s2,s4,s9,s11,s12,d1,l1,l3,x1,g1,g2,p2,p4,p7,a1,a2,a3,x40,x43,x44,x41,x42,x49,x71,x74,x48,x51,x52){
  
  #sol.data<-sol.data2
  glm.fit<-glm(delta~s2+s4+s9+sqrt(s11)+sqrt(s12)+d1+l1+l3+x1+g1+g2+sqrt(p2)+p4+p7+log(a1)+sqrt(a2)+a3+log(x40)+x43+log(x44)+
                 as.factor(x41)+as.factor(x42)+x49+x71+x74+x48+x51+x52,family=binomial(probit),data=sol.data)
  new.data<-data.frame(s2=s2,s4=s4,s9=s9,s11=s11,s12=s12,d1=d1,l1=l1,l3=l3,
                       x1=x1,g1=g1,g2=g2,p2=p2,p4=p4,p7=p7,a1=a1,a2=a2,a3=a3,x40=x40,x43=x43,x44=x44,x41=as.factor(x41),
                       x42=as.factor(x42),x49=x49,x71=x71,x74=x74,x48=x48,x51=x51,x52=x52)
  glm.pred<-1/(1+exp(-predict(glm.fit,newdata=new.data)))
  return(glm.pred)}

gam.ftn<-function(sol.data2,a5,s4,s6,s8,s9,s10,s11,d1,l3,x1,g1,p2,p3,p4,p7,p9,a1,a2,a3,a7,x40,x43,x70,x71,x74,x78){
  library(nlme)
  library(mgcv)
  
  gam.ftn<-gam(delta~a5+s4+s6+s(s8)+s9+s10+s11+d1+l3+s(x1)+g1+s(p2)+p3+p4+p7+p9+a1+a2+s(a3)+a7+x40+x43+s(x70)+s(x71)+s(x74)+x78, family=binomial("logit"),data=sol.data2)
  new.data<-data.frame(a5=a5,s4=s4,s6=s6,s8=s8,s9=s9,s10=s10,s11=s11,d1=d1,l3=l3,x1=x1,g1=g1,p2=p2,p3=p3,p4=p4,p7=p7,p9=p9,a1=a1,a2=a2,a3=a3,a7=a7,x40=x40,x43=x43,x70=x70,x71=x71,x74=x74,x78=x78)
  gam.pred<-predict(gam.ftn,newdata=new.data,type = "response")
  return(gam.pred)}


p.fun<-function(model,newdata){
  
  surv1<-c()
  surv2<-c()
  p<-c()
  newdata$y.i<-round(newdata$y.i)
  surv.data<-survfit(model)$surv
  time.data<-survfit(model)$time
  fit.time<-seq(time.data[length(time.data)])
  length(time.data)
  surv.t<-c()
  
  for(i in 1:length(surv.data)){
    if(i==1){
      surv.t<-c(surv.t,rep(surv.data[i],time.data[i]))}else if(i==length(surv.data)){
        surv.t<-c(surv.t,rep(surv.data[i],(length(fit.time)-length(surv.t))))}else{
          
          surv.t<-c(surv.t,rep(surv.data[i],(time.data[i]-time.data[i-1])))}
  }
  
  surv.data<-data.frame(time=fit.time,surv.t=surv.t)
  mu.hat<-exp(predict(model,newdata=newdata))
  
  for(i in 1:nrow(newdata)){
    if(newdata$y.i[i]<=365){
      surv1[i]<-1
      surv2[i]<-surv.data$surv.t[surv.data$time==(newdata$y.i[i])]}else{
        if(newdata$y.i[i]>length(surv.data$time)){
          surv1[i]<-surv.data$surv.t[nrow(surv.data)]
          surv2[i]<-surv.data$surv.t[nrow(surv.data)]}else{
            surv1[i]<-surv.data$surv.t[surv.data$time==(newdata$y.i[i]-365)]
            surv2[i]<-surv.data$surv.t[surv.data$time==(newdata$y.i[i])]}}
    p[i]<-1-(surv2[i]/surv1[i])^mu.hat[i]
  }
  return(p)
}

cox.ftn<-function(sol.data2,s10,a7,l3,s3,x78,a4,d1,p5,a2,s6,a3,s11,a1,p6,x42,p2,x72,x43,l1,x40){
  
  sol.data2$y<-sol.data2$y+sol.data2$x40
  library("survival")
  
  my.surv<-Surv(sol.data2$y,sol.data2$delta)
  
  cox.fit<-coxph(formula = my.surv ~ s10 + a7 + sqrt(l3) + s3 + sqrt(x78) + a4 + 
                   d1 + p5 + a2 + s6 + a3 + sqrt(s11) + a1 + p6 + as.factor(x42) + p2 + x72 + 
                   x43 + l1+sqrt(l3):s6 +s3:l1 , data = sol.data2, method = "breslow")
  new.data<-data.frame(s10=s10,a7=a7,l3=l3,s3=s3,x78=x78,a4=a4,
                       d1=d1,p5=p5,a2=a2,s6=s6,a3=a3,s11=s11,a1=a1,
                       p6=p6,x42=as.factor(x42),p2=p2,x72=x72,x43=x43,l1=l1,s3=s3)
  new.data$y.i<-x40
  cox.pred<-p.fun(cox.fit,newdata=new.data)
  return(cox.pred)
}

myp.value<-function(x1,g1,g2,p1,p2,p3,p4,p5,p6,p7,p8,p9,s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,
                    d1,d2,d3,d4,l1,l2,l3,a1,a2,a3,a4,a5,a6,a7,a8,x40,x41,x42,x43,x44){
  
  setwd("D:\\")
  sol.data2<-read.csv("train2.csv",header=T)
  
  sol.data2$x45<-exp(sol.data2$x43) #매출액
  sol.data2$x46<-exp(sol.data2$x44) #총자산
  sol.data2$x47<-(sol.data2$x45/sol.data2$a8)*100 #총자본
  sol.data2$x48<-(sol.data2$s10*sol.data2$x47)/100 #차입금**
  sol.data2$x49<-(sol.data2$s9*sol.data2$x46)/100 #자기자본**
  sol.data2$x50<-(sol.data2$s8*sol.data2$x46)/100 #유보액
  sol.data2$x51<-(sol.data2$x45/sol.data2$a4)*100 #매출채권*
  sol.data2$x52<-(sol.data2$x45/sol.data2$a3)*100 #매입채무***
  sol.data2$x53<-(sol.data2$x45/sol.data2$a7)*100 #재고자산
  sol.data2$x54<-(sol.data2$x45/sol.data2$a6)*100 #자본금(초기자본)
  sol.data2$x55<-(sol.data2$s11*sol.data2$x48)/100 #고정자산
  sol.data2$x56<-(sol.data2$x45/sol.data2$a1)*100 #경영자본
  sol.data2$x57<-(sol.data2$s6*sol.data2$x49)/100 #유동부채**
  sol.data2$x58<-(sol.data2$s7*sol.data2$x57)/100 #유동자산**
  sol.data2$x59<-(sol.data2$s4*sol.data2$x46)/100 #총부채 
  sol.data2$x59<-(sol.data2$s5*sol.data2$x46)/100 #순부채
  sol.data2$x60<-(sol.data2$s1*sol.data2$x49)/100 #고정부채
  sol.data2$x61<-(sol.data2$p7*sol.data2$x47)/100 #순이익
  sol.data2$x62<-(sol.data2$p6*sol.data2$x47)/100 #경상이익
  sol.data2$x63<-(sol.data2$p4*sol.data2$x49)/100 #당기순이익
  sol.data2$x64<-(sol.data2$p2*sol.data2$x59)/100 #금융비용 -
  sol.data2$x65<-(sol.data2$x64/sol.data2$p3)*100 #총비용 -
  sol.data2$x66<-(sol.data2$d3*sol.data2$x48)/100 #총CF
  sol.data2$x67<-(sol.data2$d4*sol.data2$x48)/100 #CF
  sol.data2$x68<-(sol.data2$l3*sol.data2$x57)/100 #현금예금
  sol.data2$x69<-((sol.data2$x68+sol.data2$x51)/sol.data2$x58)*100 #당좌비율###################-
  sol.data2$x70<-(sol.data2$p8*sol.data2$x47)/100 #영업이익
  sol.data2$x71<-(sol.data2$x70/sol.data2$x45)*100 #매출액영업이익률
  sol.data2$x72<-(sol.data2$x62/sol.data2$x45)*100 #매출액경상이익률
  sol.data2$x73<-(sol.data2$x45/(sol.data2$x58-sol.data2$x57))*100 #운전자산회전율-
  sol.data2$x74<-((sol.data2$x58-sol.data2$x57)/sol.data2$x46)*100 #순운전자본비율
  sol.data2$x75<-(sol.data2$x55/(sol.data2$x49+sol.data2$x60))*100 #고정장기적합률-
  sol.data2$x76<-sol.data2$a2*sol.data2$x71 #고정자산영업이익률(효율성)
  sol.data2$x77<-sol.data2$a2*sol.data2$x72 #고정자산경상이익률(효율성)
  sol.data2$x78<-(sol.data2$x45/sol.data2$x46)*100 #총자산회전율
  sol.data2$x79<-(sol.data2$x45/sol.data2$x64)*100 #금융비율부담률
  sol.data2$y.i<-(sol.data2$y+sol.data2$x40)           #일단위로 time환산
  
  sol.data2$x41<-as.factor(sol.data2$x41)
  sol.data2$x42<-as.factor(sol.data2$x42)
  x41<-as.factor(x41)
  x42<-as.factor(x42)
  
  x45<-exp(x43) #매출액
  x46<-exp(x44) #총자산
  x47<-(x45/a8)*100 #총자본
  x48<-(s10*x47)/100 #차입금**
  x49<-(s9*x46)/100 #자기자본**
  x50<-(s8*x46)/100 #유보액
  x51<-(x45/a4)*100 #매출채권*
  x52<-(x45/a3)*100 #매입채무***
  x53<-(x45/a7)*100 #재고자산
  x54<-(x45/a6)*100 #자본금(초기자본)
  x55<-(s11*x48)/100 #고정자산
  x56<-(x45/a1)*100 #경영자본
  x57<-(s6*x49)/100 #유동부채**
  x58<-(s7*x57)/100 #유동자산**
  x59<-(s4*x46)/100 #총부채 
  x59<-(s5*x46)/100 #순부채
  x60<-(s1*x49)/100 #고정부채
  x61<-(p7*x47)/100 #순이익
  x62<-(p6*x47)/100 #경상이익
  x63<-(p4*x49)/100 #당기순이익
  x64<-(p2*x59)/100 #금융비용 -
  x65<-(x64/p3)*100 #총비용 -
  x66<-(d3*x48)/100 #총CF
  x67<-(d4*x48)/100 #CF
  x68<-(l3*x57)/100 #현금예금
  x69<-((x68+x51)/x58)*100 #당좌비율###################-
  
  x70<-(p8*x47)/100 #영업이익
  x71<-(x70/x45)*100 #매출액영업이익률
  x72<-(x62/x45)*100 #매출액경상이익률
  x73<-(x45/(x58-x57))*100 #운전자산회전율-
  x74<-((x58-x57)/x46)*100 #순운전자본비율
  x75<-(x55/(x49+x60))*100 #고정장기적합률-
  x76<-a2*x71 #고정자산영업이익률(효율성)
  x77<-a2*x72 #고정자산경상이익률(효율성)
  x78<-(x45/x46)*100 #총자산회전율
  x79<-(x45/x64)*100 #금융비율부담률
  p.glm<-glm.ftn(sol.data2,s2,s4,s9,s11,s12,d1,l1,l3,x1,g1,g2,p2,p4,p7,a1,a2,a3,x40,x43,x44,x41,x42,x49,x71,x74,x48,x51,x52)
  p.cox<-cox.ftn(sol.data2,s10,a7,l3,s3,x78,a4,d1,p5,a2,s6,a3,s11,a1,p6,x42,p2,x72,x43,l1,x40)
  p.gam<-gam.ftn(sol.data2,a5,s4,s6,s8,s9,s10,s11,d1,l3,x1,g1,p2,p3,p4,p7,p9,a1,a2,a3,a7,x40,x43,x70,x71,x74,x78)
  
  print(list(p.glm=p.glm,p.gam=p.gam,p.cox=p.cox))
}
library(shiny)

lift.plot.ftn<-function(test.data,p){
  lift.plot.value<-c()
  new.data<-cbind(test.data,p)
  for(i in 1:10){
    lift.data<-new.data[order(new.data$p,decreasing=T)[round(length(p)*0.1*(i-1)):round(length(p)*0.1*i)],]
    lift.plot.value[i]<-mean(lift.data$delta)}
  return(lift.plot.value)}

mytable<-function(x1,g1,g2,p1,p2,p3,p4,p5,p6,p7,p8,p9,s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,d1,d2,d3,d4,l1,l2,l3,a1,a2,a3,a4,a5,a6,a7,a8,x40,x41,x42,x43,x44){
  p.value<-myp.value(x1,g1,g2,p1,p2,p3,p4,p5,p6,p7,p8,p9,s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,
                     d1,d2,d3,d4,l1,l2,l3,a1,a2,a3,a4,a5,a6,a7,a8,x40,x41,x42,x43,x44)
  
  library(data.table)
  dat <- rbind(data.table(Model="GLM",value=p.value[1]),
               data.table(Model="GAM",value=p.value[2]),
               data.table(Model="COX-PH",value=p.value[3])
  )
  return(dat)
  
}

library(shiny)

ui<-
shinyUI(pageWithSidebar(
    headerPanel("기업 부도 예측"),
    sidebarPanel(  
      
      numericInput(inputId="x1",
                   label="총자본투자효율",
                   value=5),
      numericInput(inputId="g1",
                   label="매출채권증가율",
                   value=1.95),
      numericInput(inputId="g2",
                   label="재고자산증가율",
                   value=104.82),
      numericInput(inputId="p1",
                   label="경영자본순이익율",
                   value=1.68),
      numericInput(inputId="p2",
                   label="금융비용/총부채비율",
                   value=8.52),
      numericInput(inputId="p3",
                   label="금융비용/총비용비율",
                   value=6.55),
      numericInput(inputId="p4",
                   label="자기자본순이익율",
                   value=3.97),
      numericInput(inputId="p5",
                   label="자본금순이익율",
                   value=3.95),
      numericInput(inputId="p6",
                   label="총자본경상이익율",
                   value=1.72),
      numericInput(inputId="p7",
                   label="총자본순이익률",
                   value=3.092),
      numericInput(inputId="p8",
                   label="총자본영업이익률",
                   value=2.92),
      numericInput(inputId="p9",
                   label="총자산사업이익률",
                   value=2.92),
      numericInput(inputId="s1",
                   label="고정부채비율",
                   value=74.52),
      numericInput(inputId="s2",
                   label="고정비율",
                   value=147.34),
      numericInput(inputId="s3",
                   label="부채비율",
                   value=83.49),
      numericInput(inputId="s4",
                   label="부채총계/자산총계비율",
                   value=45.50),
      numericInput(inputId="s5",
                   label="순부채/총자산비율",
                   value=45.50),
      numericInput(inputId="s6",
                   label="유동부채비율",
                   value=8.97),
      numericInput(inputId="s7",
                   label="유동비율",
                   value=402),
      numericInput(inputId="s8",
                   label="유보액/총자산비율",
                   value=15.92),
      numericInput(inputId="s9",
                   label="자기자본비율",
                   value=54.49),
      numericInput(inputId="s10",
                   label="차입금의존도",
                   value=40.61),
      numericInput(inputId="s11",
                   label="고정자산/차입금비율",
                   value=197.72),
      numericInput(inputId="s12",
                   label="차입금/자기자본비율",
                   value=74.52),
      numericInput(inputId="d1",
                   label="고정재무비보상배율",
                   value=1.41),
      numericInput(inputId="d2",
                   label="총차입금/(총차입금+자기자본)비율",
                   value=42.70),
      numericInput(inputId="d3",
                   label="총CF/차입금비율",
                   value=6.75),
      numericInput(inputId="d4",
                   label="CF/차입금비율",
                   value=-7.37),
      numericInput(inputId="l1",
                   label="순운전자본/총자본비율",
                   value=14.81),
      numericInput(inputId="l2",
                   label="유동부채구성비율",
                   value=4.89),
      numericInput(inputId="l3",
                   label="현금비율",
                   value=3.08),
      numericInput(inputId="a1",
                   label="경영자본회전율",
                   value=0.60),
      numericInput(inputId="a2",
                   label="고정자산회전율",
                   value=0.73),
      numericInput(inputId="a3",
                   label="매입채무회전율",
                   value=9067.15),
      numericInput(inputId="a4",
                   label="매출채권회전율",
                   value=3.76),
      numericInput(inputId="a5",
                   label="자기자본회전율",
                   value=1.10),
      numericInput(inputId="a6",
                   label="자본금회전율",
                   value=1.53),
      numericInput(inputId="a7",
                   label="재고자산회전율",
                   value=25.57),
      numericInput(inputId="a8",
                   label="총자본회전율",
                   value=0.58),
      numericInput(inputId="x40",
                   label="기업나이(2006년 1월 1일- 창업일)",
                   value=2830),
      selectInput(inputId="x41",
                  label="업종",
                  choices=list("경공업"=1,"중공업"=2,"건설업"=3,"도소매"=4,"서비스"=5),
                  selected=2),
      selectInput(inputId="x42",
                  label="규모",
                  choices=list("외감"=1,"비외감1"=2,"비외감2"=3,"소호"=4,"개인"=5),
                  selected=3),
      numericInput(inputId="x43",
                   label="로그매출액",
                   value=14),
      numericInput(inputId="x44",
                   label="로그자산",
                   value=14)
    ),
    mainPanel(
      h3(textOutput("caption")),
      tableOutput('table'))
  ))
  
 server<-
 shinyServer(function(input,output){
    output$caption<-renderText({
      "기업부도확률"
    })
    output$table <- renderTable({mytable(input$x1,input$g1,input$g2,input$p1,input$p2,input$p3,input$p4,input$p5,input$p6,
                                         input$p7,input$p8,input$p9,input$s1,input$s2,input$s3,input$s4,input$s5,input$s6,
                                         input$s7,input$s8,input$s9,input$s10,input$s11,input$s12,
                                         input$d1,input$d2,input$d3,input$d4,input$l1,input$l2,input$l3,input$a1,input$a2,
                                         input$a3,input$a4,input$a5,input$a6,input$a7,input$a8,input$x40,input$x41,input$x42,input$x43,input$x44)})
    
  })

shinyApp(ui,server)
