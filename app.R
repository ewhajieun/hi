setwd("C:\\Users\\PC\\Desktop")
final.data<-read.csv("final.csv",header=T)

#############################################################################
#sbp<-106
#dbp<-68
#age<-60
#scl<-239
#bmi<-22.9
#sex<-1
#month<-1

alt.ftn<-function(final.data,sbp,dbp,age,scl,bmi,sex,month){
final.data$sex<-as.factor(final.data$sex)
 my.surv<-Surv(final.data$followup,final.data$chdfate)
 alt.fit<-survreg(my.surv~sbp+dbp+scl+age+sqrt(bmi)+month+sex+sbp*scl+sbp*age+dbp*scl+dbp*age+age*sqrt(bmi)+age*sex,data=final.data,dist="weibull")
  new.data<-c(1,sbp,dbp,scl,age,sqrt(bmi),month,sex,sbp*scl,sbp*age,dbp*scl,dbp*age,age*sqrt(bmi),age*as.numeric(sex))
 z<-(log(3650)-sum(alt.fit$coef*new.data))/alt.fit$scale
 alt.p<-1-exp(-exp(z))
  
  return(alt.p)
}


alt.ftn2<-function(final.data,sbp,dbp,age,scl,bmi,sex,month){
final.data$sex<-as.factor(final.data$sex)
  my.surv2<-Surv(final.data$followup+age*365.25,final.data$chdfate)
  alt.fit<-survreg(my.surv2 ~ sbp + scl + age + bmi + sex + scl:age + scl:bmi + scl:sex + age:bmi, data = final.data, dist = "lognormal")
  new.data<-c(1,sbp,scl,age,bmi,sex,scl*age,scl*bmi,scl*as.numeric(sex),age*bmi)
  z4<-(log(3650+age*365.25)-sum(alt.fit$coef*new.data))/alt.fit$scale
  z5<-(log(age*365.25)-sum(alt.fit$coef*new.data))/alt.fit$scale
  G4<-(1-exp(-exp(z4))-(1-exp(-exp(z5))))/(exp(-exp(z4)))
  alt.p<-(pnorm(z4)-pnorm(z5))/(1-pnorm(z5))


  return(alt.p)
}

surv.prob<-function(model,time){
  #model<-cox.fit
  #time<-3650
  data.surv<-data.frame(time=survfit(model)$time,surv=survfit(model)$surv)
  for(i in 1:nrow(data.surv)){
  if(data.surv$time[i]<=time & time<data.surv$time[i+1]){
    a<-data.surv$surv[i]
  }}
  return(a)
}

cox.ftn<-function(final.data,sbp,dbp,age,scl,bmi,sex,month){
final.data$sex<-as.factor(final.data$sex)
  sq.dbp<-final.data$dbp^2
  log.bmi<-log(final.data$bmi)
  cox.fit<-coxph(Surv(followup,chdfate==0)~sbp+dbp+sq.dbp+scl+age+log.bmi+month+
                                             sex+sbp:scl+sbp:month+dbp:age+sq.dbp:scl+sbp:age+
                                             sq.dbp:age+age:log.bmi+age:sex
                                            ,data=final.data,method = "breslow")

  
  new.data<-data.frame(sbp=(sbp-mean(final.data$sbp)),dbp=(dbp-mean(final.data$dbp)),
                       scl=(scl-mean(final.data$scl)),age=(age-mean(final.data$age)),bmi=(bmi-mean(final.data$bmi)),month=month,sex=as.factor(sex),
                       sq.dbp=(dbp^2-mean(sq.dbp)),log.bmi=(log(bmi)-mean(log.bmi)))

 
  new.data2<-c(new.data$sbp,new.data$dbp,new.data$sq.dbp,new.data$scl,new.data$age,new.data$log.bmi,new.data$month,
                                             new.data$sex,new.data$sbp*new.data$scl,new.data$sbp*new.data$month,new.data$dbp*new.data$age,new.data$sq.dbp*new.data$scl,
                                             new.data$sbp*new.data$age,
                                             new.data$sq.dbp*new.data$age,new.data$age*new.data$log.bmi,new.data$age*as.numeric(new.data$sex))

  cox.p<-1-surv.prob(cox.fit,3650)^exp(sum(cox.fit$coef*new.data2))
  return(cox.p)
  }

cox.ftn2<-function(final.data,sbp,dbp,age,scl,bmi,sex,month){
final.data$sex<-as.factor(final.data$sex)
  sq.sbp<-final.data$sbp^2
  sq.dbp<-final.data$dbp^2
  sq.scl<-final.data$scl^2
  log.bmi<-log(final.data$bmi)
  cox.fit<-coxph(Surv(followup+age*365.25,chdfate==0)~sbp+sq.sbp+dbp+sq.dbp+scl+sq.scl+log.bmi+sex+
                                                         sbp:log.bmi+sq.sbp:sq.scl+sq.sbp:log.bmi+dbp:sq.scl+dbp:log.bmi+
                                                         sq.dbp:sq.scl+sq.dbp:log.bmi+scl:sq.scl+scl:sex+sq.scl:sex
  									,data=final.data,method = "breslow")
  
  new.data<-data.frame(sbp=(sbp-mean(final.data$sbp)),dbp=(dbp-mean(final.data$dbp)),
                       scl=(scl-mean(final.data$scl)),age=as.numeric(age-mean(final.data$age)),bmi=(bmi-mean(final.data$bmi)),month=month,sex=as.factor(sex),
                       sq.dbp=(dbp^2-mean(sq.dbp)),log.bmi=(log(bmi)-mean(log.bmi)),sq.sbp=sbp^2-mean(sq.sbp),sq.scl=scl^2-mean(sq.scl))

 new.data2<-c(new.data$sbp,new.data$sq.sbp,new.data$dbp,
              new.data$sq.dbp,new.data$scl,new.data$sq.scl,
             new.data$log.bmi,new.data$sex,new.data$sbp*new.data$log.bmi,
             new.data$sq.sbp*new.data$sq.scl,new.data$sq.sbp*new.data$log.bmi,new.data$dbp*new.data$sq.scl,
            new.data$dbp*new.data$log.bmi,new.data$sq.dbp*new.data$sq.scl,new.data$sq.dbp*new.data$log.bmi,
            new.data$scl*new.data$sq.scl,new.data$scl*as.numeric(new.data$sex),new.data$sq.scl*as.numeric(new.data$sex))

  cox.p<-1-(surv.prob(cox.fit,(3650+new.data$age*365.25))/surv.prob(cox.fit,(new.data$age*365.25)))^exp(sum(as.numeric(cox.fit$coef)*new.data2))
  return(cox.p)
  }

myp.value<-function(model,sbp,dbp,age,scl,height,weight,sex,month){
  bmi<-weight/(height^2)
  month<-as.numeric(month)
  age<-as.numeric(age)
  sex<-as.factor(sex)
  
  library(survival)
  setwd("C:\\Users\\PC\\Desktop")
  final.data<-read.csv("final.csv",header=T)
#model<-2

 model<-as.numeric(model)
  if(model==1){

  alt.p<-alt.ftn(final.data,sbp,dbp,age,scl,bmi,sex,month)
  cox.p<-cox.ftn(final.data,sbp,dbp,age,scl,bmi,sex,month)}else{
  alt.p<-alt.ftn2(final.data,sbp,dbp,age,scl,bmi,sex,month)
  cox.p<-cox.ftn2(final.data,sbp,dbp,age,scl,bmi,sex,month)}

  print(list(alt.p=alt.p,cox.p=cox.p))
}
#height<-170
#weight<-56
mytable<-function(model,sbp,dbp,age,scl,height,weight,sex,month){
  library(data.table)
  p.value<-myp.value(model,sbp,dbp,age,scl,height,weight,sex,month)
  dat <- rbind(data.table(Model="ALT",value=p.value$alt.p),
               data.table(Model="COX-PH",value=p.value$cox.p))
  return(dat)
}
library(shiny)




ui<-
shinyUI(pageWithSidebar(
    headerPanel("심장병 발생 확률"),
    sidebarPanel(  

      radioButtons(inputId="model",
                   label="Model",
                   choices=list("followup"=1,"followup+age"=2),
                   selected=2),

      numericInput(inputId="sbp",
                   label="SBP(systolic blood pressure)",
                   value=106),
      numericInput(inputId="dbp",
                   label="DBP(diastolic blood pressure)",
                   value=68),
      
      numericInput(inputId="age",
                   label="age",
                   value=60),
      sliderInput(inputId="height",
                   label="height",
                   min=100,
                   max=200,
                   value=170),
      sliderInput(inputId="weight",
                   label="weight",
                   min=0,
                   max=150,
                   value=55),
      selectInput(inputId="month",
                  label="month",
                  choices=list("1"=1,"2"=2,"3"=3,"4"=4,"5"=5,"6"=6,"7"=7,"8"=8,"9"=9,"10"=10,"11"=11,"12"=12),
                  selected=1),
      radioButtons(inputId="sex",
                   label="sex",
                   choices=list("male"=1,"female"=2),
                   selected=1)
      ),
    mainPanel(
      h3(textOutput("caption")),
      tableOutput("table"))
  ))

  
 server<-
 shinyServer(function(input,output){
    
    output$caption<-renderText({
      "10년내 심장병발생 확률"
    })
    output$table <- renderTable({mytable(input$model,input$sbp,input$dbp,input$age,input$scl,input$height,input$weight,
                                         input$sex,input$month)})
    
  })

shinyApp(ui,server)
