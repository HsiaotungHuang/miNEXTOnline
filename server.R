#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
#install.packages("devtools")
library(devtools)
#install_github("chaolab2019/miNEXT2")
library(miNEXT2)
library(gridExtra)
source("./source/utility_miNEXT.r")

spider.abun = read.table("data/spider.txt")
bird.inc = read.table("data/bird.txt")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  
  observeEvent(input$source == "upload", {
    removeTab(inputId = "tabs", target = "Species composition information")
  })
  
  
  
  data <- reactive({
    print(input$datatype)
    print(input$source)
    print(input$nput$inc.demo)
    if (input$source == "demo"  & input$datatype=="abundance" & input$abun.demo=="spider.abun") {  
      df <- spider.abun
      
    }
    if (input$source == "demo"   & input$datatype=="incidence_freq" & input$inc.demo=="bird.inc") {  
      df <- bird.inc
      print(df)
    
    }
   
    if (input$source == "upload") {
      if(is.null(input$files$datapath)!=T){
        df = read.table(input$files$datapath, header=T)
        ##avoid error for species name as column name1 not in rowname
        if(!is.numeric(df[2,1])){
          rownames(df) <- df[,1]
          df[,1] <- NULL
        }
      
        
      } else{
        df <- NULL
      }
    } 
    return(df)
  })
  
  # output$fileUploaded <- reactive({
  #   df<-NULL
  #   df<-data()
  #  # print(df)
  #   return(!is.null(data()))
  # })
  # outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  
  
  # checkdata<- reactive({
  #   print("checkdata")
  #   ana.df <-NULL
  #   ana.df = Fun_data2list(data(),"2")
  #   if (input$datatype=="incidence_freq"){
  #     maxindex<-sapply(ana.df, function(x) which.max(x))
  #     print(maxindex)
  #     print(max(maxindex))
  #     if (max(maxindex) !=1) {
  #       showModal(dataModal())
  #       validate(validate_incidence(ana.df))
  #     }else {ana.df}
  #     
  #   }else{
  #     ana.df
  #   }
  #   return(ana.df)
  #   
  # })
  # 
  
  output$Data.raw <- renderTable({
    if(input$goButton==0) return(NULL)
   # else if (input$checkFile==0) return(NULL)
    isolate({
      df <- data()
      return(df)
    })
    
  },digits = 0, align = "c",rownames = T)
  
  
  
  
  ######hsiaotung for incidence data format check
  validate_incidence <- function(ds) {
    maxindex<-sapply(ds, function(x) which.max(x))
    #  print(maxindex)
    #  print(max(maxindex))
    
    if (max(maxindex) !=1) {
      # HTML('<script type="text/javascript">alert("CSV, please!");</script>')
      paste(ds,"\n For incidence data, the first entry should be the # sampling units.\n Species incidence frequence should be less than the number of sampling units.\n   ")
    } else {
      NULL
    }
  }
  #######hsiaotung for incidence data format check
  dataModal <- function(failed = FALSE) {
    modalDialog(size="l",
                title = HTML('<span style="color:red; font-size: 20px; font-weight:bold; font-family:sans-serif ">Error Message: Your incidence data format is not correct!<span>'),
                
                div("Species incidence frequencies should be less than or equal to the # of sampling units.", 
                    style="font-size:20px;font-weight:bold;font-family:sans-serif; color:darkred"),
                
                br(),
                div("Below is the example of incidence data:", 
                    style="font-size:20px;font-weight:bold;font-family:sans-serif; color:black"),
                br(),
                tags$img(src=base64enc::dataURI(file = "./Images/incidenceErrorMsg.png", mime = "image/png")),
                footer = tagList(
                  modalButton("Cancel")
                  #  actionButton("ok", "OK")
                )
    )
  }
  
  
  anadata<-reactive({
    if (input$source == "upload") {
      ana.df = Fun_data2list(data(),"2")
      if (input$datatype=="incidence_freq"){
        #validate(validate_incidence(dataset))
        
        maxindex<-sapply(ana.df, function(x) which.max(x))
          print(maxindex)
          print(max(maxindex))
        if (max(maxindex) !=1) {
          showModal(dataModal())
          validate(validate_incidence(ana.df))
        }else {ana.df}
        
      }else{
        ana.df
      }
      
    } 
    else{
      ana.df<-Fun_data2list(data(),"2")
    }
    return(ana.df)
    
  })
  
  
 
  
  output$Data.summary <- renderTable({
    if(input$goButton==0) return(NULL)
    isolate({
      if(class(anadata())=="list"){
        sdata <- Fun_createSummary(anadata(),input$datatype)
        print(sdata)
        return(sdata)
      }else{
        return(NULL)
      }
    })
    
  },digits = 0, align = "c",rownames = T)
  
  
  output$Data.mix.real <- renderTable({
    if(input$goButton==0) return(NULL)
    isolate({
      if (input$source=='upload'){
        if(input$datatype == "incidence_freq"){
          sdata <- Incidence(data(), knots = 20, size = NULL,allpts = FALSE,nboots = 0)
          print(sdata$q012)
          return(sdata$q012)
        }else if(input$datatype == "abundance"){
          sdata <- Abundance(data(), knots = 20, size = NULL,nboots = 0)
          print(sdata$q012)
          return(sdata$q012)
          
        }
        
      }
     else{
        return(NULL)
      }
    })
    
  },digits = 0, align = "c",rownames = T)
  
  
 
  
  
  output$image1 <- renderImage({
    if(input$goButton==0) return(NULL)
    isolate({
      if (input$source=='demo' & input$datatype == "abundance") {
        pngfile="www/spider_d_small.png"
        print(paste("file exists:",file.exists(pngfile),pngfile))
      }
      else if(input$source=='demo' & input$datatype == "incidence_freq"){
        pngfile="www/birds_d_small.png"
        print(paste("file exists:",file.exists(pngfile),pngfile))
      
      }
      list(
        src = pngfile,width="600px",height="1650px")
    })
    
  }, deleteFile = FALSE)
  
  output$image2 <- renderImage({
    if(input$goButton==0) return(NULL)
    isolate({
      if (input$source=='demo' & input$datatype == "abundance") {
        pngfile="www/spider_p_small.png"
        print(paste("file exists:",file.exists(pngfile),pngfile))
      }
      else if(input$source=='demo' & input$datatype == "incidence_freq"){
        pngfile="www/birds_p_small.png"
        print(paste("file exists:",file.exists(pngfile),pngfile))
        
      }
      list(
        src = pngfile,width="600px",height="600px")
    })
    
  }, deleteFile = FALSE)
  
  draw1 <- reactive({
    if(input$goButton==0) return(NULL)
    isolate({
      print(input$datatype)
      if(input$datatype == "incidence_freq") {
        sdata <- Incidence(data(), knots = 20, size = NULL,allpts = FALSE,nboots = 0)
        result0<-sdata$q0
        p1<-draw.f_Div(data1=data(),output=sdata$q0,q=0,type="incidence")
        p2<-draw.f_Div(data1=data(),output=sdata$q1,q=1,type="incidence")
        p3<-draw.f_Div(data1=data(),output=sdata$q2,q=2,type="incidence")
        p_ana<-plot_comb1(data1 = data(),output_d = sdata$q0 ,output_p = sdata$q0_ana,type="incidence")
        
        ptlist <- list(p1,p2,p3,p_ana)
        p<-grid.arrange(grobs=ptlist,nrow=4)
        
        
        
        return(p)
      }else if(input$datatype == "abundance"){
        sdata <- Abundance(data(), knots = 20, size = NULL,nboots = 0)
        result0<-sdata$q0
        p1<-draw.f_Div(data1=data(),output=sdata$q0,q=0,type="abundance")
        p2<-draw.f_Div(data1=data(),output=sdata$q1,q=1,type="abundance")
        p3<-draw.f_Div(data1=data(),output=sdata$q2,q=2,type="abundance")
        p_ana<-plot_comb1(data1 = data(),output_d = sdata$q0 ,output_p = sdata$q0_ana,type="abundance")
        
        ptlist <- list(p1,p2,p3,p_ana)
        lay <- rbind(c(1,4),
                     c(2,4),
                     c(3,NA))
        p<-grid.arrange(grobs=ptlist,layout_matrix =lay)
        return(p)
        
      }
      else return(NULL)
    })
  })
  
  output$draw1 <- renderPlot({
    if(input$goButton==0) return(NULL)
    isolate({
      if (input$source=='upload'){
        draw1()
      }
      else return(NULL)
      
    })
  },height = 850, width = 1000)
  
  draw2 <- reactive({
    if(input$goButton==0) return(NULL)
    isolate({
      print(input$datatype)
      if(input$datatype == "incidence_freq") {
        sdata <- Incidence(data(), knots = 20, size = NULL,allpts = FALSE,nboots = 0)
        result0<-sdata$q0
        p1<-draw.f_Div(data1=data(),output=sdata$q0,q=0,type="incidence")
        p2<-draw.f_Div(data1=data(),output=sdata$q1,q=1,type="incidence")
        p3<-draw.f_Div(data1=data(),output=sdata$q2,q=2,type="incidence")
        ptlist <- list(p1,p2,p3)
        p<-grid.arrange(grobs=ptlist,nrow=3)
        return(p)
      }else if(input$datatype == "abundance"){
        sdata <- Abundance(data(), knots = 20, size = NULL,nboots = 0)
        result0<-sdata$q0
        p1<-draw.f_Div(data1=data(),output=sdata$q0,q=0,type="abundance")
        p2<-draw.f_Div(data1=data(),output=sdata$q1,q=1,type="abundance")
        p3<-draw.f_Div(data1=data(),output=sdata$q2,q=2,type="abundance")
        ptlist <- list(p1,p2,p3)
        p<-grid.arrange(grobs=ptlist,nrow=3)
        return(p)
        
      }
      else return(NULL)
    })
  })
  
  
})
