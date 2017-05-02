library(shinydashboard)
library(shiny)
library(flux)
library(DT)
library(rhandsontable)
ui <- dashboardPage(dashboardHeader(title="ROC curve"),
                    dashboardSidebar(
                      sidebarMenu(
                        sliderInput("N",
                                    "Number samples:",
                                    min = 10,
                                    max = 200,
                                    value = 30,step=1),
                        sliderInput("P",
                                    "Percentage 1:",
                                    min = 0.1,
                                    max = 0.9,
                                    value = 0.5,step=0.01),
                        sliderInput("C",
                                    "Predictable:",
                                    min = 0,
                                    max = 1,
                                    value = 0,step=0.01),
                        sliderInput("TH",
                                    "Threshold:",
                                    min = 0,
                                    max = 1,
                                    value = 0.5,step=0.001),
                        menuItem("ROC/PR curves", tabName = "Nonlinear", icon = icon("th"))
                      )
                    ),
                    dashboardBody(
                      tabItems(tabItem(tabName = "Nonlinear",
                                       fluidRow(box(width=3,rHandsontableOutput("hot")),
                                                box(width=4,plotOutput("ROC")),
                                                box(width=4,plotOutput("PR")),
                                                box(width=2,tabPanel('Display length',DT::dataTableOutput('ex1'))))
                      )
                      ) )
)# ui



server<-function(input, output,session) {
  values = reactiveValues()
  
  N<-10
  P<-0.5
  C<-0
  data = reactive({
    #  browser()
    if (!is.null(input$hot)) {
      DF = hot_to_r(input$hot)
      if (N!= input$N || P!= input$P || C!= input$C){
        DF = data.frame(array(0,c(input$N,4)))
        colnames(DF)<-c("Prob(Y=1)","Target Y","Prediction","TP/FP/TN/FN")
        DF[,1]=sort(runif(input$N))
        post=0.5+(DF[,1]-0.5)*input$C+rnorm(input$N,sd=0.1)
        DF[,2]= as.numeric(post>quantile(post,1-input$P) )  ##sample(c(0,1),input$N,rep=TRUE,prob=c(1-input$P,input$P))
        DF[,3]<-as.numeric(DF[,1]>input$TH)
        N<<-input$N
        P<<-input$P
        C<<-input$C
      }
      
      DF[,3]<-as.numeric(DF[,1]>input$TH)
      scores=DF[,1]
      classes=DF[,2]
      thr=input$TH
      DF[which(scores<=thr & classes==1),4]<-"FN" 
      DF[which(scores >thr & classes==0),4]<-"FP"
      DF[which(scores <=thr & classes==0),4]<-"TN"
      DF[which(scores>thr & classes==1),4]<-"TP"
    } else {
      
      if (is.null(values[["DF"]]) ){
        
        DF = data.frame(array(0,c(input$N,4)))
        colnames(DF)<-c("Prob(Y=1)","Target Y","Prediction","TP/FP/TN/FN")
        DF[,1]=sort(runif(input$N))
        DF[,2]= as.numeric((0.5+(DF[,1]-0.5)*input$C+rnorm(input$N,sd=0.5))>input$P)   ##sample(c(0,1),input$N,rep=TRUE,prob=c(1-input$P,input$P))
        DF[,3]<-as.numeric(DF[,1]>input$TH)
        N<<-input$N
        P<<-input$P
        C<<-input$C
        
      }else
        DF = values[["DF"]]
    }
    
    
    values[["DF"]] = DF
    
    DF
  })
  
  output$ex1 <- DT::renderDataTable({
    Conf<-data.frame(array(0,c(2,2)))
    colnames(Conf)<-c("N","P")
    rownames(Conf)<-c("Nh","Ph")
    DF = data()
    scores<-DF[,1]
    classes=DF[,2]
    thr<-input$TH
    Conf[1,1]<-paste("TN=",length(which(scores <=thr & classes==0))) ##"TN"
    Conf[1,2]<-paste("FN=",length(which(scores<=thr & classes==1))) ## FN
    Conf[2,1]<-paste("FP=",length(which(scores >thr & classes==0))) ## "FP"
    Conf[2,2]<-paste("TP=",length(which(scores>thr & classes==1))) ## TP
    
    DT::datatable(Conf, options = list(dom = 't'))
  })
  
  
  output$hot <- renderRHandsontable({
    
    DF = data()
    
    if (!is.null(DF) ){
      
      rhandsontable(DF, stretchH = "all")
    }
  })
  
  output$ROC <-renderPlot( {
    
    DF = data()
    scores<-DF[,1]
    classes=DF[,2]
    thr<-input$TH
    FN0<-length(which(scores<=thr & classes==1) )
    FP0<-length(which(scores >thr & classes==0))
    TN0<-length(which(scores <=thr & classes==0))
    TP0<-length(which(scores>thr & classes==1))
    FPR0<-FP0/(FP0+TN0)
    SE0<-TP0/(TP0+FN0)
    
    Pos<-length(which(classes==1))
    Neg<-length(which(classes!=1))
    
    FPR<-NULL
    SE<-NULL
    for (thr in seq(0,1,by=0.01)){
      
      FN<-length(which(scores<=thr & classes==1) )
      FP<-length(which(scores >thr & classes==0))
      TN<-length(which(scores <=thr & classes==0))
      TP<-length(which(scores>thr & classes==1))
      FPR<-c(FPR,FP/(FP+TN))
      SE<-c(SE,TP/(TP+FN))
    }
    plot(FPR,SE,xlim=c(0,1),ylim=c(0,1),type="l",main=paste("P=",Pos,"N=",Neg, "AUC=",round(auc(FPR,SE),2)))
    abline(a=0,b=1,lty=3)
    
    points(FPR0,SE0,lwd=4,pch=3,col="red")
  })
  
  output$PR <-renderPlot( {
    
    DF = data()
    scores<-DF[,1]
    classes=DF[,2]
    thr<-input$TH
    FN0<-length(which(scores<=thr & classes==1) )
    FP0<-length(which(scores >thr & classes==0))
    TN0<-length(which(scores <=thr & classes==0))
    TP0<-length(which(scores>thr & classes==1))
    PR0<-TP0/(FP0+TP0)
    SE0<-TP0/(TP0+FN0)
    MISCL<- (FP0+FN0)/input$N
    BER<- 0.5*(FP0/(TN0+FP0)+FN0/(FN0+TP0))
    PR<-NULL
    mPR<-NULL
    SE<-NULL
    for (thr in seq(0,1,by=0.01)){
      
      FN<-length(which(scores<=thr & classes==1) )
      FP<-length(which(scores >thr & classes==0))
      TN<-length(which(scores <=thr & classes==0))
      TP<-length(which(scores>thr & classes==1))
      if (TP>0){
        if (length(PR)>=1){
          mPR<-c(mPR,max(TP/(FP+TP),max(PR)))
          PR<-c(PR,TP/(FP+TP))
        }else {
          PR<-TP/(FP+TP)
          mPR<-TP/(FP+TP)
        }
        SE<-c(SE,TP/(TP+FN))
      } else{
        mPR<-c(mPR,max(PR))
        PR<-c(PR,0)
        SE<-c(SE,0)
      }
    }
    
    
    
    USE=unique(SE)
    UPR<-numeric(length(USE))
    for (i in 1:length(USE)){
      UPR[i]<-max(mPR[which(SE==USE[i])])
    }
    plot(SE,PR,xlim=c(0,1),ylim=c(0,1),type="p",main=paste( "MISCL=",round(MISCL,2),"BER=",round(BER,2), "AUPR=[",round(auc(SE,PR),2),round(auc(USE,UPR),2),"]") )
    lines(USE,UPR)
    abline(h=length(which(classes>0))/length(classes),lty=3)
    is=which.min(abs(USE-SE0))
    
    points(USE[is],UPR[is],lwd=4,pch=3,col="red")
    points(SE0,PR0,lwd=4,pch=3,col="red")
  })
  
}
shinyApp(ui, server)