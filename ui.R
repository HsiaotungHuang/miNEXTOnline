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
           radioButtons("source","Choose one:",choices=c("Demo data"="demo","Upload data"="upload")),
           
           selectInput("datatype","Select data type:", choices = c("abundance data"="abundance","incidence data"="incidence_freq")),
           conditionalPanel("input.source=='demo' & input.datatype=='abundance'",
                            radioButtons("abun.demo","Choose demo data:",choices=c("Spider"="spider.abun"))
                            
           ),
           conditionalPanel("input.source=='demo' & input.datatype=='incidence_freq'",
                            radioButtons("inc.demo","Choose demo data:",choices=c("Bird"="bird.inc"))
           ),
         
           conditionalPanel("input.source=='upload'",
                            fileInput("files","Choose File (txt)", multiple = TRUE,accept = c('text/tab-separated-values') )
                            # conditionalPanel(
                            #    condition = "output.fileUploaded == true ",
                            #    actionButton("checkFile", "check Fileformat"))
                            # 
                            # 
                            # 
           ),
           
           conditionalPanel("input.source=='upload'",
                            radioButtons("cutpoint","Choose cutpoint:",choices=c("Specify endpoint and # of knots"="knots","Specify sample sizes"="sample")),
                            conditionalPanel("input.cutpoint=='knots'",
                                             uiOutput("choose_size"),
                                             numericInput("knots", "Number of knots",  min = 1,  max = 2000, value = 40)
                            ),
                            conditionalPanel("input.cutpoint=='sample'",
                                             h4("Sample size"),
                                             tags$textarea(id="size", rows = 3, cols=28,"")
                            )
           ),
           conditionalPanel("input.source=='upload'",
                            numericInput("nboot", "Number of bootstraps(Time consuming, enter '0' to skip bootstrap)",  min = 1,  max = 200, value = 0)
                            
                            
           )
 
         ),
         actionButton("goButton",span("Analyse",style="font-size: 20px"),icon("triangle-right",lib = "glyphicon"))
         
         
  ),
  column(9,
         tabsetPanel(id = "tabs",
           tabPanel("Original Data",icon=icon("list-alt"),
                    h3("Data"),
                    tableOutput("Data.raw"),
                    br()
           ),
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
           tabPanel("Demo plot1",icon=icon("picture-o"),
                    plotOutput("image1")
           ),
           tabPanel("Demp plot2",icon=icon("picture-o"),
                    imageOutput("image2")
           ),
           tabPanel(" RealTime Mixture Rarefaction and Extrapolation",icon=icon("picture-o"),
                #    h4("(1) Sample-size-based rarefaction and extrapolation sampling curve"),
                 #   htmlOutput("Profile.plot.mle.TT00"),
                    plotOutput("draw1")
           ),
           tabPanel("RealTime Mixture data ",icon=icon("list-alt"),
                 #   h3("Basic information"),
                    tableOutput("Data.mix.real"),
                    br()
           )
           
           
           
          
           
           # conditionalPanel("input.source=='upload'",
           #                  tabPanel("Mixture Rarefaction and Extrapolation",icon=icon("picture-o"),
           #                           imageOutput("image1")
           #                  )     
           #   
           # )
         )
  )
  
  
  
  
))
