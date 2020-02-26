#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
source("./source/utility_miNEXT.r")

spider.abun = read.table("data/spider.txt")
bird.inc = read.table("data/bird.txt")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
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
      df = read.table(input$files$datapath, header=T)
    } 
    return(df)
  })
  
  
  anadata<-reactive({
    if (input$source == "upload") {
      ana.df = Fun_data2list(data(),"2")
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
  
 
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
  })
  
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
  
})
