library(shiny)
#library(shinythemes)

# Define UI for application that draws EWS for disease data
shinyUI(fluidPage( #theme = shinytheme("cerulean"),


  
  
  # Application title. Appears in the title bar and at the top of page
  titlePanel("Early-warning signals for polio"),
  
  sidebarLayout(
    
    # Sidebar containing input selection options
    sidebarPanel(

      
      
      selectInput("selectState", 
        label = "Choose state",
        choices = strsplit("Alabama,Arizona,Arkansas,California,Colorado,Connecticut,Delaware,Florida,Georgia,Idaho,Illinois,Indiana,Iowa,Kansas,Kentucky,Louisiana,Maine,Maryland,Massachusetts,Michigan,Minnesota,Mississippi,Missouri,Montana,Nebraska,Nevada,New Hampshire,New Jersey,New Mexico,New York,North Carolina,North Dakota,Ohio,Oklahoma,Oregon,Pennsylvania,Rhode Island,South Carolina,South Dakota,Tennessee,Texas,Utah,Vermont,Virginia,Washington,West Virginia,Wisconsin,Wyoming", ",")[[1]],
        selected = "Alabama"),
      
      
       radioButtons("selectData", 
        label = "Choose data source",
        choices = c("CDC", "Tycho"),
        selected = "CDC"),

      sliderInput("bins",
                  "Choose the window size used for time averaging (in years)",
                  min = 1,
                  max = 20,
                  value = 2),
      
      sliderInput("lag", "Choose lag for autocorrelation function:*",
                  min = 1, max = 52, value = 1),
      
      sliderInput("range", "Output range:",
                min = 1900, max = 2012, value = c(1938,2012)),
      p("*Lag is in months for CDC data and weeks for Tycho data")
      #htmlOutput("selectedImage")
      
      ),
    
    #Main panel, on rhs
    mainPanel(
      # Show a plot of the computed EWS:
      plotOutput("distPlot",  height = "800", width = "100%")#,
      #Some text under the plot:
      #textOutput("selectedInterval"),
      #p("This plot was made on ", em(Sys.Date()))
      #,
      #img(src="norad.jpg", alt="Mountain View", style="width:304px;height:228px;")

    )
  )
)) 



     
      #Include MathJax (optional)
    # tags$head( tags$script(src="http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_HTML-full", type = 'text/javascript'),
    #           tags$script(src = 'https://c328740.ssl.cf1.rackcdn.com/mathjax/2.0-latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML', type = 'text/javascript'),
    #             tags$script( "MathJax.Hub.Config({tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}});", type='text/x-mathjax-config')
    #
    #             ),
