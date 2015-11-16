
due.glm.ftn<-function(during.exp,goods.m1,y.i,current.age,
                      exp.prop,how.long,p.left.date){ # 만기 
  beta<-c(14.33,-0.04391,0.0361,0.1295,-786.3,1.023,-0.01011,-15.9,-9.033,-1.035,-0.01164,
          -0.0007603,9.792,-1.036,-0.001723,9.438)
  goods.m2<-ifelse(goods.m1==2,1,0)
  goods.m4<-ifelse(goods.m1==4,1,0)
  goods.m5<-ifelse(goods.m1==5,1,0)
  x.data<-c(1,during.exp,goods.m2,goods.m4,goods.m5,y.i,current.age,
                exp.prop,how.long,p.left.date,goods.m2*y.i,goods.m4*y.i,
                goods.m5*y.i,y.i*exp.prop,y.i*how.long,exp.prop*how.long)
  #length(beta)
  #length(x.data)
  p.value<-pnorm(sum(beta*x.data),mean=0,sd=1)
  return(p.value)
}

no.due.glm.ftn<-function(live,y.i,current.age,exp.prop,how.long){ # 무만기 
  live1<-ifelse(live==1,1,0)
  beta1<-c(7.15582,0.51521,0.13055,-0.02371,-7.13301,-1.51052)
  x.data1<-c(1,live1,y.i,current.age,exp.prop,how.long)
  #length(beta1)
  #length(x.data1)
  p.value1<-pnorm(sum(beta1*x.data1),mean=0,sd=1)
  return(p.value1)
}

not.due.gam<-function(y.i,current.age,exp.prop,how.long.gam,premium.once){ #만기 
  library(nlme)
  library(mgcv)
  setwd("D://")
  
  notmu<-read.table("notmu1.csv",header=T,sep="")
  colnames(notmu)<-c("Target_Y","age","how.exp","during.exp","how.pay","premium","live","con.date","due.date",
                     "final.count","goods.m","goods.s","y.i","current.age","exp.prop","how.long","p.left.date","premium.once")

  final.model2.2<-gam(Target_Y ~ y.i + current.age + s(exp.prop) + how.long +
                        s(premium.once),family=binomial(probit),data=notmu)
  notmumu<-data.frame(y.i=y.i,current.age=current.age,exp.prop=exp.prop,how.long=how.long.gam,premium.once=premium.once)
  p<-predict(final.model2.2,newdata=notmumu,type = "response")
  return(p)
}


due.gam<-function(during.exp,live1,goods.m1,y.i,current.age,exp.prop,how.long.gam,p.left.date){ # 무만기 
  library(nlme)
  library(mgcv)
  setwd("D://")
  mu<-read.table("mu1.csv",header=T,sep="")
  colnames(mu)<-c("Target_Y","age","how.exp","during.exp","how.pay","premium","live","con.date","due.date",
                  "final.count","goods.m","goods.s","y.i","current.age","exp.prop","how.long","p.left.date","premium.once")
  final.model11<-gam(Target_Y ~ s(during.exp) + live + goods.m + s(y.i) + s(current.age) + s(exp.prop) + 
                       how.long + p.left.date,family=binomial(logit),data=mu)
  mumu<-data.frame(during.exp=during.exp,live=live1,goods.m=goods.m1,y.i=y.i,current.age=current.age,
                   exp.prop=exp.prop,how.long=how.long.gam,p.left.date=p.left.date)
  mumup<-predict(final.model11,mumu,type = "response")
  return(mumup)}

cox.ftn<-function(y.i,age,how.long,exp.prop,p.left.date){
  change.exp<-(exp.prop)^2
  change.date<-sqrt(p.left.date)
  beta2<-c(-0.0239438957,-0.43118438776,-9.9925334,9.5183863,0.1514444,
           -11.9250271638)
  x.data2<-c(age,how.long,change.exp,change.date,how.long*change.exp,
            change.exp*change.date)
  #length(beta2)
  #length(x.data2)
  mu.hat<-exp(sum(beta2*x.data2))
  surv.p<-c(0.999999932560542,0.999999805076187,0.999999585095816,0.99999907810301,
            0.999996347568668,0.999991939908845,0.999966791890089,
            0.999956604819783,0.999933248247925,0.999906473101917,
            0.999886111234325,0.999856773671286,0.999815442740395,
            0.999776353481332,0.999726046372571,0.999704140634257,
            0.999671279759543,0.99934316824031,0.998686979440193,
            0.998150898712199,0.99738491097043,0.996534814181666,
            0.995364833023809,0.994081425831923,0.993345859026553,
            0.990802838609986,0.989796916665572,0.988461403261724,
            0.987239260939469,0.978987423555786,0.952076136307492,
            0.937470286631671,0.921328624540981,0.903085670232579,
            0.872893335133969,0.850379491867935,0.827532497644607,
            0.827532497644607,0.827532497644607,0.752219867849147,
            0.752219867849147,0.752219867849147,0.752219867849147,
            0.752219867849147,0.752219867849147,0.752219867849147,
            0.752219867849147,0.107198466018926,0.0207983871066236,
            0.000368370153037703,0.000368370153037703,
            0.00000753958747645294,0.0000000000378079374762143)
  time<-seq(1,length(surv.p))
  surv.table<-data.frame(time=time,surv.p=surv.p)
  if(y.i<=length(surv.p)){
    surv.value1<-surv.table$surv.p[time==y.i]
    surv.value2<-surv.table$surv.p[time==(y.i-3)]
  }else if(length(surv.p)<=y.i & y.i<(length(surv.p)+3)){
    surv.value1<-0
    surv.value2<-1
  }else{
    surv.value1<-1
    surv.value2<-1
  }
  p.value2<-1-(surv.value1/surv.value2)^mu.hat
  return(p.value2)
}
#p.method<-1
#age<-30
#how.exp<-"월납"
#during.exp<-10
#live<-"Yes"
#con.date<-"2001-01-01"
#final.count<-10
#goods.m<-"A"
#due<-TRUE
#premium<-100000
myp.value(p.method,age,how.exp,during.exp,live,con.date,
          final.count,goods.m,due)
myp.value<-function(p.method,age,how.exp,during.exp,live,con.date,final.count,goods.m,due,premium){
  if(how.exp=="월납"){how.exp1<-1
  }else if(how.exp=="3월납"){how.exp1<-2
  }else if(how.exp=="6월납"){how.exp1<-3
  }else if(how.exp=="년납"){how.exp1<-4}
  if(live=="Yes"){
    live1<-1
  }else{live1<-0}
  if(goods.m=="A"){
    goods.m1<-1
  }else if(goods.m=="B"){
    goods.m1<-2
  }else if(goods.m=="C"){
    goods.m1<-3
  }else if(goods.m=="D"){
    goods.m1<-4
  }else if(goods.m=="E"){
    goods.m1<-5
  }else if(goods.m=="F"){
    goods.m1<-6
  }
  premium.once<-premium*ifelse(how.exp1==1,1,0)+premium*ifelse(how.exp1==2,1,0)/3+
                premium*ifelse(how.exp1==3,1,0)/6+premium*ifelse(how.exp1==4,1,0)/12
  y.i<-ifelse(how.exp1==1,1,0)*final.count+ifelse(how.exp1==2,1,0)*final.count*3+
                 ifelse(how.exp1==3,1,0)*final.count*6+ifelse(how.exp1==4,1,0)*final.count*12
    current.age<-(2001-as.numeric(substr(con.date,1,4)))+age
   how.long.gam<-(2001-as.numeric(substr(con.date,1,4))+1)
   how.long.cox<-(2001-as.numeric(substr(con.date,1,4))+1)*12
   p.left.date<-y.i/(during.exp*12)
   
   today<-as.Date("2001-07-01", format="%Y-%m-%d")	
   
     aa<-as.numeric((today-as.Date(con.date, format="%Y-%m-%d"))/365*12)		
     x9.g<- ifelse(how.exp1==1,ceiling(aa),0)+ifelse(how.exp1==2,ceiling(aa/3),0)+
            ifelse(how.exp1==3,ceiling(aa/6),0)+ifelse(how.exp1==4,ceiling(aa/12),0)	
     x9.t<- ifelse(how.exp1==1,ceiling(during.exp*12),0)+ifelse(how.exp1==2,ceiling(during.exp*4),0)+
             ifelse(how.exp1==3,ceiling(during.exp*2),0)+ifelse(how.exp1==4,during.exp,0)	
     exp.prop<-ifelse((aa/12)>during.exp,(final.count/x9.t),(final.count/x9.g))		
     
     if(p.method==1){
       if(due==TRUE){
         pp.value<-due.glm.ftn(during.exp,goods.m1,y.i,current.age,exp.prop,how.long.gam,p.left.date)
       }else{
       pp.value<-no.due.glm.ftn(live1,y.i,current.age,exp.prop,how.long.gam)}
     }else if(p.method==2){
       if(due==TRUE){
         pp.value<-not.due.gam(y.i,current.age,exp.prop,how.long.gam,premium.once)
       }else{
         
        pp.value<-due.gam(during.exp,live1,goods.m1,y.i,current.age,exp.prop,how.long.gam,p.left.date)
       }
        
     }else if(p.method==3){
        pp.value<-cox.ftn(y.i,age,how.long.cox,exp.prop,p.left.date)
     }
     return(pp.value)
}


library(shiny)

ui<-
shinyUI(pageWithSidebar(
    headerPanel("보험 해지 확률"),
    sidebarPanel(  
      
      radioButtons(inputId="p.method",
                  label="방법",
                  choices=list("GLM"=1,"GAM"=2,"Cox PH"=3),
                  selected=1),
      numericInput(inputId="age",
                   label="가입연령",
                   value=44),
      selectInput(inputId="how.exp",
                  label="납입방법",
                  choices=list("월납","3월납","6월납","년납"),
                  selected=1),
      numericInput(inputId="during.exp",
                   label="납입기간/년",
                   value=20),
      checkboxGroupInput(inputId="live",
                    label="부활여부",
                    choices=list("Yes"=1,"No"=2),
                    selected=2),
      dateInput(inputId="con.date",
                label="계약일자",
                value="1990-02-08"),
      numericInput(inputId="final.count",
                   label="최종납입횟수",
                   value=137),
      numericInput(inputId="premium",
                   label="보험료",
                   value=30000),
      selectInput(inputId="goods.m",
                  label="상품중분류",
                  choices=list("A","B","C","D","E","F"),
                  selected=1),
      checkboxInput(inputId="due",
                    label="만기",
                    value=TRUE),
      actionButton("View",label="확인")
      
      ),
    mainPanel(
      h3(textOutput("caption")),
      verbatimTextOutput("p.value"))
  ))
  
 server<-
 shinyServer(function(input,output){
    
    output$caption<-renderText({
      "생명보험 해지확률"
    })
    output$p.value<-renderText({
      myp.value(input$p.method,input$age,input$how.exp,input$during.exp,input$live,input$con.date,
                input$final.count,input$goods.m,input$due,input$premium)
    })
  })

shinyApp(ui,server)
