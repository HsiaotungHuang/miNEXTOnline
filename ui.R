#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  #titlePanel("Old Faithful Geyser Data"),
  titlePanel(title=div(img(src="minext.jpg",height = 80, width = 80),"miNEXT")),
  column(3,
         wellPanel(
         #  radioButtons("source","Choose one:",choices=c("Demo data"="demo","Upload data(txt)"="upload")),
           radioButtons("source","Choose one:",choices=c("Demo data"="demo")),
           
           selectInput("datatype","Select data type:", choices = c("abundance data"="abundance","incidence data"="incidence_freq")),
           conditionalPanel("input.source=='demo' & input.datatype=='abundance'",
                            radioButtons("abun.demo","Choose demo data:",choices=c("Spider"="spider.abun"))
                            
           ),
           conditionalPanel("input.source=='demo' & input.datatype=='incidence_freq'",
                            radioButtons("inc.demo","Choose demo data:",choices=c("Bird"="bird.inc"))
           ),
           conditionalPanel("input.source=='upload'",
                            fileInput("files","Choose file (txt)", multiple = FALSE)
                            
           )
         ),
         actionButton("goButton",span("Analyse",style="font-size: 20px"),icon("triangle-right",lib = "glyphicon"))
         
         
  ),
  column(9,
         tabsetPanel(
           tabPanel("Data Summary",icon=icon("list-alt"),
                    h3("Basic information"),
                    tableOutput("Data.summary"),
                    br()
                  #  h3("Data Viewer"),
                  #  tableOutput("Dataviewer"),
                  #  div(
                  #    div(style="display:inline-block;",tags$li("Download all data:  ")),
                  #    div(style="display:inline-block;",downloadButton("download.DataViewer"))
                  #    
                  #  )
           ),
           tabPanel("Mixture Rarefaction and Extrapolation",icon=icon("picture-o"),
                    imageOutput("image1")
           ),
           tabPanel("Species composition information",icon=icon("picture-o"),
                    imageOutput("image2"))
            
          
         )
  )
  
  
  
  
))
