library(shiny)
library(shinythemes)

# Define UI for application that draws EWS for disease data
shinyUI(fluidPage( theme = shinytheme("cerulean"),


  
  
  # Application title. Appears in the title bar and at the top of page
  titlePanel("Early-warning signals for disease emergence"),
  
  sidebarLayout(
    
    # Sidebar containing input selection options
    sidebarPanel(
      
      #Include MathJax (optional)
    # tags$head( tags$script(src="http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_HTML-full", type = 'text/javascript'),
    #           tags$script(src = 'https://c328740.ssl.cf1.rackcdn.com/mathjax/2.0-latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML', type = 'text/javascript'),
    #             tags$script( "MathJax.Hub.Config({tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}});", type='text/x-mathjax-config')
    #
    #             ),
      
      
      radioButtons("selectInterval", 
        label = "Choose length of time for emergence in years",
        choices = c("16", "100","1000"),
        selected = "16"),
      
      
      selectInput("selectEWS", 
        label = "Choose early-warning signal to display",
        choices = c("Mean", "Variance","Coefficient of variation", "Index of dispersion"),
        selected = "Variance"),

      sliderInput("bins",
                  "Choose the window size used for time averaging",
                  min = 2,
                  max = 300,
                  value = 4),
      
      fileInput("datafile",
                label = "Upload datafile",
                accept = "text/csv"),
      
      htmlOutput("selectedImage")
      
      ),
    
    #Main panel, on rhs
    mainPanel(
      # Show a plot of the computed EWS:
      plotOutput("distPlot"),
      #Some text under the plot:
      textOutput("selectedInterval"),
      p("This plot was made on ", em(Sys.Date()))
      #,
      #img(src="norad.jpg", alt="Mountain View", style="width:304px;height:228px;")

    )
  )
))
