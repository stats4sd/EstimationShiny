#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(plotly)
library(ggplot2)
library(scales)
library(doBy)
library(DT)

set.seed(1)
populationdata<-data.frame(ID=1:1000,Training=sample(c("Yes","No"),size = 1000,replace=TRUE,prob = c(0.95,0.18)),Gender=sample(c("Male","Female"),size = 1000,replace=TRUE,prob = c(0.64,0.88)),
                           Age=round(rnorm(1000,45,10)))

populationdata$Income<-ifelse(populationdata$Gender=="Male",50+10*round(rexp(407,1/10000)/10,0),50+10*round(rexp(593,1/9000)/10,0))
pdat<-populationdata
pdat$Gender<-100*as.numeric(pdat$Gender=="Female")   
pdat$Training<-100*as.numeric(pdat$Training=="Yes")  
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title

    sidebarLayout(
 
 
  sidebarPanel(style = "position:fixed;width:inherit;",
    
    selectInput(inputId = "Variable",
                "Select A Variable",
                c("Gender of Farmer" = "Gender",
                  "Age of Farmer"= "Age" ,
                  "Sales Income from Oranges"= "Income",
                  "Training Attendance"="Training")),
    sliderInput(inputId = "Add10",
                label="Sample Size",min = 0,max=1000,value = 1000,step=10),
      actionButton(inputId = "refresh",
                "New Survey"),
    actionButton(inputId = "reset",
                 "Reset"),width = 2),
mainPanel(
  titlePanel("Estimation"),
  tabsetPanel(
    tabPanel("Data",DT::dataTableOutput("data1")),
      tabPanel("Summary",tableOutput("stats1"),plotlyOutput("plot1"),plotlyOutput("plot1a")),
       tabPanel("Repeatability",actionButton(inputId = "nrep",
                                          label="Add 100 New Surveys"),plotlyOutput("plot3"),
                checkboxInput("meanline","Add Mean Line",value=FALSE), checkboxInput("ciline","Add 95% CI Lines",value=FALSE), 
                checkboxInput("fix","Fix Axis",value=FALSE)),
    tabPanel("Precision",tableOutput("stats2"),plotlyOutput("plot5"),plotlyOutput("plot4")),
   tabPanel("Disaggregation", selectInput(inputId = "group",
                                          "Select A Disaggregation Variable",
                                          c("Gender of Farmer" = "Gender","Training Attendance"="Training")),tableOutput("stats3"),plotlyOutput("plot_d"),
            checkboxInput("fix1","Fix Axis",value=TRUE))
    
  
    )
  ,width = 8)
)
)

  yaxis<-list(axis.automargin=TRUE)
  

# Define server logic required to draw a histogram
server <- function(input, output) {

  counter <- reactiveValues(n = 0)
  sampledata<-eventReactive(input$refresh|counter$n==0,{
    if(input$Add10=="1000"|counter$n==0){
      data<-populationdata
    }
    else{
    data<-populationdata[sample(1:1000,as.numeric(as.character(input$Add10))),]
    data<-data[order(data$ID),]
    }
  data})


  observeEvent(input$refresh, {counter$n <- counter$n + 1})
  observeEvent(input$reset, {  set.seed(1)
    counter$n <- 0})
  observeEvent(input$nrep, {counter$n <- counter$n + 100})
  
 output$data1<-DT::renderDataTable({
    sampledata()
    
  }, options = list(
    pageLength = 25)  )
  
  output$plot1<-renderPlotly({
    
    if(input$Variable=="Gender"){
      p1<-ggplot(data=sampledata(),aes(y = (..count..)/sum(..count..),x=Gender,fill=Gender))+geom_bar(position="dodge",col="black")+
        xlab("Gender of Farmer")+ylab("Number of Farmers")+ggtitle("Farmers by Gender",subtitle = paste("N=",nrow(sampledata())))+
        scale_y_continuous(labels=percent,limits=c(0,1))
    }
    if(input$Variable=="Training"){
      p1<-  ggplot(data=sampledata(),aes(y = (..count..)/sum(..count..),x=Training,fill=Training))+geom_bar(position="dodge",col="black")+
        xlab("Training Attended in Past 12 Months")+ylab("Number of Farmers")+ggtitle("Farmers by Training Attendance",subtitle = paste("N=",nrow(sampledata())))+
        scale_y_continuous(labels=percent,limits=c(0,1))
    }   
    if(input$Variable=="Income"){
      p1<-   ggplot(data=sampledata(),aes(x =Income))+geom_histogram(fill="red",col="black",alpha=0.6)+
        xlab("Sales Income From Oranges")+ggtitle("Sales Income From Oranges",subtitle = paste("N=",nrow(sampledata())))+xlim(0,100000)
    }   
    if(input$Variable=="Age"){
      p1<-ggplot(data=sampledata(),aes(x =Age))+geom_histogram(fill="red",col="black",alpha=0.6)+
        xlab("Age of Farmer")+ggtitle("Age of Farmer",subtitle = paste("N=",nrow(sampledata())))+xlim(0,100)
    }  
   p2<- p1+theme(axis.title = element_text(size=14),title = element_text(size=18,face = "bold"),
             legend.text = element_text(size=14),legend.title = element_text(size=16,face="bold"),axis.text =element_text(size=14) )
   yaxis1<-list(axis.automargin=TRUE,tickprefix="       ")
   new_plot<-layout(ggplotly(p2),yaxis=yaxis1)
   new_plot
  })  
  output$plot1a<-renderPlotly({
    
    if(input$Variable=="Income"|input$Variable=="Age"){
      if(input$Variable=="Income"){
        p1<-   ggplot(data=sampledata(),aes(y =Income,x="   "))+geom_boxplot(position="dodge",col="black")+
          xlab(" ")+ylab("Sales Income From Oranges")+ggtitle("Sales Income From Oranges",subtitle = paste("N=",nrow(sampledata())))+ylim(0,100000)+coord_flip()
      }   
      if(input$Variable=="Age"){ 
        p1<-ggplot(data=sampledata(),aes(y =Age,x="   "))+geom_boxplot(position="dodge",col="black")+
          xlab(" ")+ylab("Age of Farmer")+ggtitle("Age of Farmer",subtitle = paste("N=",nrow(sampledata())))+ylim(0,100)+coord_flip()
      }  
    p2<-p1+theme(axis.title = element_text(size=14),title = element_text(size=18,face = "bold"),
             legend.text = element_text(size=14),legend.title = element_text(size=16,face="bold"),axis.text =element_text(size=14) )
    yaxis1<-list(axis.automargin=TRUE,tickprefix="       ")
    new_plot<-layout(ggplotly(p2),yaxis=yaxis1)
    new_plot
    }
    if(input$Variable=="Gender"){
      p1<-ggplot(data=sampledata(),aes(y = (..count..)/sum(..count..),x=Gender,fill=Gender))+geom_bar(position="dodge",col="black")+
        xlab("Gender of Farmer")+ylab("Number of Farmers")+ggtitle("Farmers by Gender",subtitle = paste("N=",nrow(sampledata())))+
        scale_y_continuous(labels=percent,limits=c(0,1))
    }
    if(input$Variable=="Training"){
      p1<-  ggplot(data=sampledata(),aes(y = (..count..)/sum(..count..),x=Training,fill=Training))+geom_bar(position="dodge",col="black")+
        xlab("Training Attended in Past 12 Months")+ylab("Number of Farmers")+ggtitle("Farmers by Training Attendance",subtitle = paste("N=",nrow(sampledata())))+
        scale_y_continuous(labels=percent,limits=c(0,1))
    }  
    p2<-p1+theme(axis.title = element_text(size=14),title = element_text(size=18,face = "bold"),
                 legend.text = element_text(size=14),legend.title = element_text(size=16,face="bold"),axis.text =element_text(size=14) )
    yaxis1<-list(axis.automargin=TRUE,tickprefix="       ")
    new_plot<-layout(ggplotly(p2),yaxis=yaxis1)
    new_plot
  })  
  
  

  
  output$stats1<-renderTable({
    if(input$Variable=="Gender"){
      df1<- data.frame(data.frame(table(sampledata()$Gender)), Percent=data.frame(prop.table(table(sampledata()$Gender)))[,2])
      colnames(df1)<-c("Gender","Frequency","Percentage")
      df1$Percentage<-paste(round(100*df1$Percentage),"%",sep="")
      
    }
    if(input$Variable=="Training"){
      df1<- data.frame(data.frame(table(sampledata()$Training)), Percent=data.frame(prop.table(table(sampledata()$Training)))[,2])
      colnames(df1)<-c("Training","Frequency","Percentage")
      df1$Percentage<-paste(round(100*df1$Percentage),"%",sep="")
      
    } 
    if(input$Variable=="Income"){
      df1<- data.frame(Statistic=c("Mean","Standard Deviation","Median","Minimum","Maximum","5th Percentile","25th Percentile","75th Percentile","95th Percentile"),
                       Estimate=round(c(mean(sampledata()$Income),sd(sampledata()$Income),
                       median(sampledata()$Income),
                       min(sampledata()$Income),
                       max(sampledata()$Income),
                       quantile(sampledata()$Income,0.05),
                       quantile(sampledata()$Income,0.25),
                       quantile(sampledata()$Income,0.75),
                       quantile(sampledata()$Income,0.95))))
      
    } 
    if(input$Variable=="Age"){
      df1<-data.frame(Statistic=c("Mean","Standard Deviation","Median","Minimum","Maximum","5th Percentile","25th Percentile","75th Percentile","95th Percentile"),
                      Estimate=round(c(mean(sampledata()$Age),sd(sampledata()$Age),
                                       median(sampledata()$Age),
                                       min(sampledata()$Age),
                                       max(sampledata()$Age),
                                       quantile(sampledata()$Age,0.05),
                                       quantile(sampledata()$Age,0.25),
                                       quantile(sampledata()$Age,0.75),
                                       quantile(sampledata()$Age,0.95)),1))
      
    } 
    df1
  })  

  output$stats2<-renderTable({
    tmp<-sampledata()
    tmp$Gender<-100*as.numeric(tmp$Gender=="Female")   
    tmp$Training<-100*as.numeric(tmp$Training=="Yes") 
    
    out1<-data.frame(Variable=c("Age","Gender","Income","Training"),Type=c("Mean","% Female","Mean","% Received Training"),Sample=input$Add10,
                     Estimate=c(mean(tmp$Age),mean(tmp$Gender),mean(tmp$Income),mean(tmp$Training)),
                     SD=c(sd(tmp$Age),
                          100*sqrt(mean(tmp$Gender/100)*(1-mean(tmp$Gender/100))),
                          sd(tmp$Income),
                          100*sqrt(mean(tmp$Training/100)*(1-mean(tmp$Training/100)))))
    
    out1$SE=sqrt((1000-as.numeric(as.character(input$Add10)))/999)*out1$SD/sqrt(as.numeric(as.character(input$Add10)))
   out1$LCI=out1$Estimate-qt(0.975,as.numeric(as.character(input$Add10))-1)*out1$SE
  out1$UCI=out1$Estimate+qt(0.975,as.numeric(as.character(input$Add10))-1)*out1$SE
  out1$UCI[out1$UCI>100&input$Variable=="Gender"|input$Variable=="Training"]<-100
  out1$LCI[out1$LCI<0&input$Variable=="Gender"|input$Variable=="Training"]<-0
  colnames(out1)[5:8]<-c("Standard Deviation","Standard Error","95% Confidence Interval (Lower)","95% Confidence Interval (Upper)")
    out1[out1$Variable==input$Variable,]
  })
  
  
  
  output$stats3<-renderTable({
    if(input$group=="Gender"){
    tmp<-sampledata()
    tmp$Gender<-100*as.numeric(tmp$Gender=="Female")   
    tmp$Training<-100*as.numeric(tmp$Training=="Yes") 
  
    Men=subset(tmp,Gender==0)
    Women=subset(tmp,Gender==100)
    out1<-data.frame(Variable=c("Age","Income","Training"),Type=c("Mean","Mean","% Received Training"),Group="Male Farmers",Sample=nrow(Men),
                     Estimate=c(mean(Men$Age),mean(Men$Income),mean(Men$Training)),
                     SD=c(sd(Men$Age),
                          sd(Men$Income),
                          100*sqrt(mean(Men$Training/100)*(1-mean(Men$Training/100)))))
    out2<-data.frame(Variable=c("Age","Income","Training"),Type=c("Mean","Mean","% Received Training"),Group="Female Farmers",Sample=nrow(Women),
                     Estimate=c(mean(Women$Age),mean(Women$Income),mean(Women$Training)),
                     SD=c(sd(Women$Age),
                          sd(Women$Income),
                          100*sqrt(mean(Women$Training/100)*(1-mean(Women$Training/100)))))
   
    out1$SE=sqrt((407-out1$Sample)/406)*out1$SD/sqrt(out1$Sample)
    out2$SE=sqrt((593-out2$Sample)/592)*out2$SD/sqrt(out2$Sample)
  
    out3<-rbind(out1,out2)
    out3<- out3[out3$Variable==input$Variable,]
    out3$LCI=out3$Estimate-qt(0.975,as.numeric(as.character(input$Add10))-1)*out3$SE
    out3$UCI=out3$Estimate+qt(0.975,as.numeric(as.character(input$Add10))-1)*out3$SE
    out3$UCI[out3$UCI>100&input$Variable=="Gender"|input$Variable=="Training"]<-100
    out3$LCI[out3$LCI<0&input$Variable=="Gender"|input$Variable=="Training"]<-0
    colnames(out3)[6:9]<-c("Standard Deviation","Standard Error","95% Confidence Interval (Lower)","95% Confidence Interval (Upper)")

    }
    if(input$group=="Training"){
      tmp<-sampledata()
      tmp$Gender<-100*as.numeric(tmp$Gender=="Female")   
      tmp$Training<-100*as.numeric(tmp$Training=="Yes") 
      
      Men=subset(tmp,Training==0)
      Women=subset(tmp,Training==100)
      out1<-data.frame(Variable=c("Age","Income","Gender"),Type=c("Mean","Mean","% Female Farmers"),Group="No Training",Sample=nrow(Men),
                       Estimate=c(mean(Men$Age),mean(Men$Income),mean(Men$Gender)),
                       SD=c(sd(Men$Age),
                            sd(Men$Income),
                            100*sqrt(mean(Men$Gender/100)*(1-mean(Men$Gender/100)))))
      out2<-data.frame(Variable=c("Age","Income","Gender"),Type=c("Mean","Mean","% Female Farmers"),Group="Training",Sample=nrow(Women),
                       Estimate=c(mean(Women$Age),mean(Women$Income),mean(Women$Gender)),
                       SD=c(sd(Women$Age),
                            sd(Women$Income),
                            100*sqrt(mean(Women$Gender/100)*(1-mean(Women$Gender/100)))))
      
      out1$SE=sqrt((161-out1$Sample)/160)*out1$SD/sqrt(out1$Sample)
      out2$SE=sqrt((839-out2$Sample)/838)*out2$SD/sqrt(out2$Sample)
      
      out3<-rbind(out1,out2)
      out3<- out3[out3$Variable==input$Variable,]
      out3$LCI=out3$Estimate-qt(0.975,as.numeric(as.character(input$Add10))-1)*out3$SE
      out3$UCI=out3$Estimate+qt(0.975,as.numeric(as.character(input$Add10))-1)*out3$SE
      out3$UCI[out3$UCI>100&input$Variable=="Gender"|input$Variable=="Training"]<-100
      out3$LCI[out3$LCI<0&input$Variable=="Gender"|input$Variable=="Training"]<-0
      colnames(out3)[6:9]<-c("Standard Deviation","Standard Error","95% Confidence Interval (Lower)","95% Confidence Interval (Upper)")
      
    }
    out3
  })
  
  output$plot_d<-renderPlotly({
    if(input$group=="Gender"){
    tmp<-sampledata()
    tmp$Gender<-100*as.numeric(tmp$Gender=="Female")   
    tmp$Training<-100*as.numeric(tmp$Training=="Yes") 
    
    Men=subset(tmp,Gender==0)
    Women=subset(tmp,Gender==100)
    out1<-data.frame(Variable=c("Age","Income","Training"),Type=c("Mean","Mean","% Received Training"),Group="Male Farmers",Sample=nrow(Men),
                     Estimate=c(mean(Men$Age),mean(Men$Income),mean(Men$Training)),
                     SD=c(sd(Men$Age),
                          sd(Men$Income),
                          100*sqrt(mean(Men$Training/100)*(1-mean(Men$Training/100)))))
    out2<-data.frame(Variable=c("Age","Income","Training"),Type=c("Mean","Mean","% Received Training"),Group="Female Farmers",Sample=nrow(Women),
                     Estimate=c(mean(Women$Age),mean(Women$Income),mean(Women$Training)),
                     SD=c(sd(Women$Age),
                          sd(Women$Income),
                          100*sqrt(mean(Women$Training/100)*(1-mean(Women$Training/100)))))
    
    out1$SE=sqrt((407-out1$Sample)/406)*out1$SD/sqrt(out1$Sample)
    out2$SE=sqrt((593-out2$Sample)/592)*out2$SD/sqrt(out2$Sample)
    
    out3<-rbind(out1,out2)
    out3<- out3[out3$Variable==input$Variable,]
    out3$LCI=out3$Estimate-qt(0.975,as.numeric(as.character(input$Add10))-1)*out3$SE
    out3$UCI=out3$Estimate+qt(0.975,as.numeric(as.character(input$Add10))-1)*out3$SE
    out3$UCI[out3$UCI>100&input$Variable=="Gender"|input$Variable=="Training"]<-100
    out3$LCI[out3$LCI<0&input$Variable=="Gender"|input$Variable=="Training"]<-0
    
   p1<-ggplot(data=out3,aes(y=Estimate,x=Group,ymax=UCI,ymin=LCI))+geom_errorbar()+geom_point(aes(col=Group),size=5)+
     xlab("Gender of Farmer")+ylab(paste("Estimate of",input$Variable))+
     theme(axis.title = element_text(size=14),title = element_text(size=18,face = "bold"),
           legend.text = element_text(size=14),legend.title = element_text(size=16,face="bold"),axis.text =element_text(size=14) )
    }
    if(input$group=="Training"){
      tmp<-sampledata()
      tmp$Gender<-100*as.numeric(tmp$Gender=="Female")   
      tmp$Training<-100*as.numeric(tmp$Training=="Yes") 
      
      Men=subset(tmp,Training==0)
      Women=subset(tmp,Training==100)
      out1<-data.frame(Variable=c("Age","Income","Gender"),Type=c("Mean","Mean","% Female Farmers"),Group="No Training",Sample=nrow(Men),
                       Estimate=c(mean(Men$Age),mean(Men$Income),mean(Men$Gender)),
                       SD=c(sd(Men$Age),
                            sd(Men$Income),
                            100*sqrt(mean(Men$Gender/100)*(1-mean(Men$Gender/100)))))
      out2<-data.frame(Variable=c("Age","Income","Gender"),Type=c("Mean","Mean","% Female Farmers"),Group="Training",Sample=nrow(Women),
                       Estimate=c(mean(Women$Age),mean(Women$Income),mean(Women$Gender)),
                       SD=c(sd(Women$Age),
                            sd(Women$Income),
                            100*sqrt(mean(Women$Gender/100)*(1-mean(Women$Gender/100)))))
      
      out1$SE=sqrt((161-out1$Sample)/160)*out1$SD/sqrt(out1$Sample)
      out2$SE=sqrt((839-out2$Sample)/838)*out2$SD/sqrt(out2$Sample)
      
      out3<-rbind(out1,out2)
      out3<- out3[out3$Variable==input$Variable,]
      out3$LCI=out3$Estimate-qt(0.975,as.numeric(as.character(input$Add10))-1)*out3$SE
      out3$UCI=out3$Estimate+qt(0.975,as.numeric(as.character(input$Add10))-1)*out3$SE
      out3$UCI[out3$UCI>100&input$Variable=="Gender"|input$Variable=="Training"]<-100
      out3$LCI[out3$LCI<0&input$Variable=="Gender"|input$Variable=="Training"]<-0
      
      p1<-ggplot(data=out3,aes(y=Estimate,x=Group,ymax=UCI,ymin=LCI))+geom_errorbar()+geom_point(aes(col=Group),size=5)+
        xlab("Training Received?")+ylab(paste("Estimate of",input$Variable))+
        theme(axis.title = element_text(size=14),title = element_text(size=18,face = "bold"),
              legend.text = element_text(size=14),legend.title = element_text(size=16,face="bold"),axis.text =element_text(size=14) )
    }
   
    if(input$fix1==TRUE){
      p1<-p1+ylim(min(pdat[,input$Variable]),max(pdat[,input$Variable]))
    }
    
     yaxis1<-list(axis.automargin=TRUE,tickprefix="       ")
    new_plot<-layout(ggplotly(p1),yaxis=yaxis1)
    new_plot
  })
  
  
  output$plot3<-renderPlotly({
     
 
  reps<-data.frame(Survey=1:counter$n,Estimate=NA,sd=NA)
  set.seed(1)
for(i in 1:counter$n){
  tmp<-pdat[sample(1:1000,as.numeric(as.character(input$Add10))),input$Variable]
  reps$Estimate[i]<-mean(tmp)
  reps$sd[i]<-ifelse(input$Variable=="Income"|input$Variable=="Age",sd(tmp),
                                               100*sqrt(mean(tmp/100)*(1-mean(tmp/100))))
}
  reps$upper<-reps$Estimate+qt(0.975,as.numeric(as.character(input$Add10))-1)*sqrt((1000-as.numeric(as.character(input$Add10)))/999)*reps$sd/sqrt(as.numeric(as.character(input$Add10)))
  reps$lower<-reps$Estimate-qt(0.975,as.numeric(as.character(input$Add10))-1)*sqrt((1000-as.numeric(as.character(input$Add10)))/999)*reps$sd/sqrt(as.numeric(as.character(input$Add10)))
p1<-ggplot(data=reps,aes(y=Estimate,x=Survey))
  if(input$meanline==TRUE){
  p1<-p1+geom_hline(aes(yintercept=mean(Estimate)),col="blue")
  }
if(input$ciline==TRUE){
  
  
  p1<-p1+
    geom_errorbar(aes(ymax=upper,
                      ymin=lower),col="red",linetype=2)
}
if(input$fix==TRUE){
  p1<-p1+ylim(min(pdat[,input$Variable]),max(pdat[,input$Variable]))
}

    p2<-p1+geom_point(size=2,col="forestgreen")+
      theme(axis.title = element_text(size=14),title = element_text(size=18,face = "bold"),
            legend.text = element_text(size=14),legend.title = element_text(size=16,face="bold"),axis.text =element_text(size=14) )+
      ylab(input$Variable)+xlab("Survey Number")+ggtitle(paste("Estimates of",input$Variable,"\nfrom",counter$n,"surveys each of sample size",input$Add10))
    yaxis1<-list(axis.automargin=TRUE,tickprefix="       ")
    new_plot<-layout(ggplotly(p2),yaxis=yaxis1)
    new_plot
  
  })
  
  
  output$plot5<-renderPlotly({
    pdat<-populationdata
    pdat$Gender<-100*as.numeric(pdat$Gender=="Female")   
    pdat$Training<-100*as.numeric(pdat$Training=="Yes")   
    
    
    
    ns<-data.frame(N=10:1000,Training=0.95/(0.95+0.18),Gender=0.88/(0.88+0.64),Income=10050,Age=45)
    ns$Training_SE<-sqrt((ns$Training*(1-ns$Training))/ns$N)*sqrt((1000-ns$N)/999)
    ns$Gender_SE<-sqrt((ns$Gender*(1-ns$Gender))/ns$N)*sqrt((1000-ns$N)/999)
    ns$Age_SE<-10/sqrt(ns$N)*sqrt((1000-ns$N)/999)
    ns$Income_SE<-10000/sqrt(ns$N)*sqrt((1000-ns$N)/999)
    
    if(input$Variable=="Gender"){
      ns$Upper<-ns$Gender+qt(0.975,ns$N-1)*ns$Gender_SE
        ns$Lower<-ns$Gender-qt(0.975,ns$N-1)*ns$Gender_SE
      p1<-ggplot(data=ns,aes(y=Gender,x=N))+
        geom_line()+geom_line(aes(y=Upper),col="red")+
        geom_line(aes(y=Lower),col="red")+
        xlab("Sample Size")+ylab("Confidence Interval")+ggtitle("% Female Farmers + Confidence Interval")
    }
    if(input$Variable=="Training"){
      ns$Upper<-ns$Training+qt(0.975,ns$N-1)*ns$Training_SE
      ns$Lower<-ns$Training-qt(0.975,ns$N-1)*ns$Training_SE
      p1<-   ggplot(data=ns,aes(y=Training,x=N))+geom_line()+
        geom_line(aes(y=Upper),col="red")+
        geom_line(aes(y=Lower),col="red")+
        xlab("Sample Size")+ylab("Confidence Interval")+ggtitle("% Trained Farmers + Confidence Interval")
    }
    if(input$Variable=="Age"){
      ns$Upper<-ns$Age+qt(0.975,ns$N-1)*ns$Age_SE
      ns$Lower<-ns$Age-qt(0.975,ns$N-1)*ns$Age_SE
      p1<-     ggplot(data=ns,aes(y=Age,x=N))+geom_line()+     
        geom_line(aes(y=Upper),col="red")+
        geom_line(aes(y=Lower),col="red")+
        xlab("Sample Size")+ylab("Confidence Interval")+ggtitle("Mean Age + Confidence Interval")
    }
    if(input$Variable=="Income"){
      ns$Upper<-ns$Income+qt(0.975,ns$N-1)*ns$Income_SE
      ns$Lower<-ns$Income-qt(0.975,ns$N-1)*ns$Income_SE
      p1<-      ggplot(data=ns,aes(y=Income,x=N))+geom_line()+
        geom_line(aes(y=Lower),col="red")+
        geom_line(aes(y=Lower),col="red")+
        xlab("Sample Size")+ylab("Confidence Interval")+ggtitle("Mean Income + Confidence Interval")
    }
    p2<-p1  +
      theme(axis.title = element_text(size=14),title = element_text(size=18,face = "bold"),
            legend.text = element_text(size=14),legend.title = element_text(size=16,face="bold"),axis.text =element_text(size=14) )
    yaxis1<-list(axis.automargin=TRUE,tickprefix="       ")
    new_plot<-layout(ggplotly(p2),yaxis=yaxis1)
    new_plot
  })
  
  output$plot4<-renderPlotly({
    pdat<-populationdata
    pdat$Gender<-100*as.numeric(pdat$Gender=="Female")   
    pdat$Training<-100*as.numeric(pdat$Training=="Yes")   
    

    
    ns<-data.frame(N=10:1000,Training=0.95/(0.95+0.18),Gender=0.88/(0.88+0.64),Income=10050,Age=45)
    ns$Training_SE<-sqrt((ns$Training*(1-ns$Training))/ns$N)*sqrt((1000-ns$N)/999)
    ns$Gender_SE<-sqrt((ns$Gender*(1-ns$Gender))/ns$N)*sqrt((1000-ns$N)/999)
    ns$Age_SE<-10/sqrt(ns$N)*sqrt((1000-ns$N)/999)
    ns$Income_SE<-10000/sqrt(ns$N)*sqrt((1000-ns$N)/999)
      
    if(input$Variable=="Gender"){
p1<-ggplot(data=ns,aes(y=Gender_SE,x=N))+geom_line()+xlab("Sample Size")+ylab("Standard Error")+ggtitle("Standard Error of % Female Farmers")
    }
    if(input$Variable=="Training"){
      p1<-   ggplot(data=ns,aes(y=Training_SE,x=N))+geom_line()+xlab("Sample Size")+ylab("Standard Error")+ggtitle("Standard Error of % Trained Farmers")
    }
    if(input$Variable=="Age"){
      p1<-     ggplot(data=ns,aes(y=Age_SE,x=N))+geom_line()+xlab("Sample Size")+ylab("Standard Error")+ggtitle("Standard Error of Mean Age")
    }
    if(input$Variable=="Income"){
      p1<-      ggplot(data=ns,aes(y=Income_SE,x=N))+geom_line()+xlab("Sample Size")+ylab("Standard Error")+ggtitle("Standard Error of Mean Income")
    }
p2<-  p1  +
    theme(axis.title = element_text(size=14),title = element_text(size=18,face = "bold"),
          legend.text = element_text(size=14),legend.title = element_text(size=16,face="bold"),axis.text =element_text(size=14) )
yaxis1<-list(axis.automargin=TRUE,tickprefix="       ")
new_plot<-layout(ggplotly(p2),yaxis=yaxis1)
new_plot
  })
  
  output$plot_c<-renderPlotly({
    
    if(input$Variable=="Gender"){
      p1<-ggplot(data=sampledata(),aes(y = (..count..)/sum(..count..),x=Gender,fill=Gender))+geom_bar(position="dodge",col="black")+
        xlab("Gender of Farmer")+ylab("Number of Farmers")+ggtitle("Farmers by Gender",subtitle = paste("N=",nrow(sampledata())))+
        scale_y_continuous(labels=percent,limits=c(0,1))
    }
    if(input$Variable=="Training"){
      p1<-  ggplot(data=sampledata(),aes(y = (..count..)/sum(..count..),x=Training,fill=Training))+geom_bar(position="dodge",col="black")+
        xlab("Training Attended in Past 12 Months")+ylab("Number of Farmers")+ggtitle("Farmers by Training Attendance",subtitle = paste("N=",nrow(sampledata())))+
        scale_y_continuous(labels=percent,limits=c(0,1))
    }   
    if(input$Variable=="Income"){
      p1<-   ggplot(data=sampledata(),aes(x =Income))+geom_histogram(fill="red",col="black",alpha=0.6)+
        xlab("Sales Income From Oranges")+ggtitle("Income From Oranges",subtitle = paste("N=",nrow(sampledata())))+xlim(0,100000)
    }   
    if(input$Variable=="Age"){
      p1<-ggplot(data=sampledata(),aes(x =Age))+geom_histogram(fill="red",col="black",alpha=0.6)+
        xlab("Age of Farmer")+ggtitle("Age of Farmer",subtitle = paste("N=",nrow(sampledata())))+xlim(0,100)
    }  
    p2<-p1+theme(axis.title = element_text(size=14),title = element_text(size=18,face = "bold"),
             legend.text = element_text(size=14),legend.title = element_text(size=16,face="bold"),axis.text =element_text(size=14) )
    yaxis1<-list(axis.automargin=TRUE,tickprefix="       ")
    new_plot<-layout(ggplotly(p2),yaxis=yaxis1)
    new_plot
  })  
  
  output$plotx<-renderPlotly({
    
    if(input$Variable1=="Gender"){
      plt1<-data.frame(percents=c(mean(sampledata()[,"Training"]=="Yes"),mean(sampledata()[,"Training"]=="No")),Training=c("Yes","No"))
      plt1$upper< plt1$percents+qt(0.975,as.numeric(as.character(input$Add10))-1)*sqrt((1000-nrow(sampledata()))/999)*(plt1$percents*(1-plt1$percents))/sqrt(nrow(sampledata()))
      plt1$lower<-plt1$percents-qt(0.975,as.numeric(as.character(input$Add10))-1)*sqrt((1000-nrow(sampledata()))/999)*(plt1$percents*(1-plt1$percents))/sqrt(nrow(sampledata()))
      plt1$lower[plt1$lower<0]<-0
      p1<-ggplot(data=plt1,aes(y =percents,x=Training))+
        geom_point()+
        geom_errorbar(aes(ymax=upper,
                          ymin=lower))+
        xlab("Gender of Farmer")+ylab("Number of Farmers")+ggtitle(paste("Farmers by GenderN=",nrow(sampledata())))+
        scale_y_continuous(labels=percent)+ylim(0,1)
    }
    if(input$Variable1=="Training"){
      plt1<-data.frame(percents=c(mean(sampledata()[,"Training"]=="Yes"),mean(sampledata()[,"Training"]=="No")),Training=c("Yes","No"))
      plt1$upper< plt1$percents+qt(0.975,as.numeric(as.character(input$Add10))-1)*sqrt((1000-nrow(sampledata()))/999)*(plt1$percents*(1-plt1$percents))/sqrt(nrow(sampledata()))
      plt1$lower<-plt1$percents-qt(0.975,as.numeric(as.character(input$Add10))-1)*sqrt((1000-nrow(sampledata()))/999)*(plt1$percents*(1-plt1$percents))/sqrt(nrow(sampledata()))
      plt1$upper[plt1$upper>1]<-1
      plt1$lower[plt1$lower<0]<-0
      p1<-ggplot(data=plt1,aes(y =percents,x=Training))+
        geom_point()+
        geom_errorbar(aes(ymax=upper,
                          ymin=lower))+
        xlab("Training Attended in Past 12 Months")+ylab("Number of Farmers")+ggtitle(paste("Farmers by Training AttendanceN=",nrow(sampledata())))+
        scale_y_continuous(labels=percent)+ylim(0,1)
    }   
    if(input$Variable1=="Income"){
      
      plt1<-data.frame(mean=mean(sampledata()[,"Income"]),sd=sd(sampledata()[,"Income"]))
      plt1$upper< plt1$mean+qt(0.975,as.numeric(as.character(input$Add10))-1)*sqrt((1000-nrow(sampledata()))/999)* plt1$sd/sqrt(nrow(sampledata()))
      plt1$lower<-plt1$mean-qt(0.975,as.numeric(as.character(input$Add10))-1)*sqrt((1000-nrow(sampledata()))/999)* plt1$sd/sqrt(nrow(sampledata()))
      p1<-ggplot(data=plt1,aes(y =mean,x=1))+
        geom_point()+
        geom_errorbar(aes(ymax=upper,
                          ymin=lower))+
        xlab(" ")+ylab("Sales Income From Oranges")+ggtitle(paste("Sales Income From OrangesN=",nrow(sampledata())))+ylim(0,25000)
    }   
    if(input$Variable1=="Age"){
      plt1<-data.frame(mean=mean(sampledata()[,"Age"]),sd=sd(sampledata()[,"Age"]))
      plt1$upper< plt1$mean+qt(0.975,as.numeric(as.character(input$Add10))-1)*sqrt((1000-nrow(sampledata()))/999)* plt1$sd/sqrt(nrow(sampledata()))
      plt1$lower<-plt1$mean-qt(0.975,as.numeric(as.character(input$Add10))-1)*sqrt((1000-nrow(sampledata()))/999)* plt1$sd/sqrt(nrow(sampledata()))
      
      
      p1<-ggplot(data=plt1,aes(y =mean,x=1))+
        geom_point()+
        geom_errorbar(aes(ymax=upper,
                          ymin=lower))+
        xlab(" ")+ylab("Age of Farmer")+ggtitle(paste("Age of FarmerN=",nrow(sampledata())))+ylim(0,25000)
    }  
   p2<- p1+
      theme(axis.title = element_text(size=14),title = element_text(size=18,face = "bold"),
            legend.text = element_text(size=14),legend.title = element_text(size=16,face="bold"),axis.text =element_text(size=14) )
   yaxis1<-list(axis.automargin=TRUE,tickprefix="       ")
   new_plot<-layout(ggplotly(p2),yaxis=yaxis1)
   new_plots
  }) 
}



# Run the application 
shinyApp(ui = ui, server = server)


