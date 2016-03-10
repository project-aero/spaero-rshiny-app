library(shiny)
library(shinydashboard)
#library(shinythemes)

# Define UI for application that draws EWS for disease data
dashboardPage( #theme = shinytheme("cerulean"),


  
  
  # Application title. Appears in the title bar and at the top of page
  dashboardHeader(title ="Early-warning signals"),

    # Sidebar containing input selection options
  dashboardSidebar(
    hr(),
    sidebarMenu(id="tabs"
              ,menuItem("Plot early-warning signal", tabName="plot", icon=icon("line-chart"), selected=TRUE)
 #             ,menuItem("Plot derivative", tabName = "plot_derivative", icon=icon("line-chart"))
              ,menuItem("Plot entropy", tabName = "plot_entropy", icon=icon("line-chart"))
              
              #menuItem("Codes",  icon = icon("file-text-o"),
              #         menuSubItem("ui.R", tabName = "ui", icon = icon("angle-right")),
              #         menuSubItem("server.R", tabName = "server", icon = icon("angle-right"))
              #),
             # menuItem("ReadMe", tabName = "readme", icon=icon("mortar-board")),
              ,menuItem("About", tabName = "about", icon = icon("question"))
  ),
  hr(),
  
      selectInput("selectPathogen", 
        label = "Choose pathogen",
        choices = c("Hepatitis A", "Influenza", "Measles", "Mumps", "Pertussis", "Polio", "Rubella", "Smallpox"),
        selected = "Polio"),
  
      selectInput("selectState", 
        label = "Choose state",
        choices = strsplit("Alabama,Arizona,Arkansas,California,Colorado,Connecticut,Delaware,Florida,Georgia,Idaho,Illinois,Indiana,Iowa,Kansas,Kentucky,Louisiana,Maine,Maryland,Massachusetts,Michigan,Minnesota,Mississippi,Missouri,Montana,Nebraska,Nevada,New Hampshire,New Jersey,New Mexico,New York,North Carolina,North Dakota,Ohio,Oklahoma,Oregon,Pennsylvania,Rhode Island,South Carolina,South Dakota,Tennessee,Texas,Utah,Vermont,Virginia,Washington,West Virginia,Wisconsin,Wyoming", ",")[[1]],
        selected = "Alabama"),
      
      
       radioButtons("selectData", 
        label = "Choose data source",
        choices = c("Tycho" = "Tycho", "CDC (only for polio and pertussis)" = "CDC"),
        selected = "Tycho"),

      sliderInput("bins",
                  "Choose the window size used for time averaging (in years)",
                  min = 1,
                  max = 20,
                  value = 2),
      
      sliderInput("lag", "Choose lag for autocorrelation function:*",
                  min = 1, max = 52, value = 1),
      
      sliderInput("range", "Output range:",
                min = 1900, max = 2012, value = c(1938,2012)),
      p("*Lag is in months for CDC data and weeks for Tycho data"),
      hr()
      #htmlOutput("selectedImage")
      ),
    
    #Main panel, on rhs
    dashboardBody(
      # Show a plot of the computed EWS:
      tabItems(
	      tabItem(tabName = "plot",
		      fluidRow(
			      box(  width = NULL, plotOutput("distPlot",  height = "800", width = "100%"), collapsible = TRUE,
				  title = "Plot", status = "primary", solidHeader = TRUE)

		      )#,
	      ),
	      tabItem(tabName = "plot_derivative",
		      fluidRow(
			      box(  width = NULL, plotOutput("derivativePlot",  height = "800", width = "100%"), collapsible = TRUE,
				  title = "Plot", status = "primary", solidHeader = TRUE)
		      )#,
	      ),
				tabItem(tabName = "plot_entropy",
				          fluidRow(
				            box(  width = NULL, plotOutput("entropyPlot",  height = "800", width = "100%"), collapsible = TRUE,
				                  title = "Plot", status = "primary", solidHeader = TRUE)
				         #   ,box(tableOutput("probability"))
				          )
	      ),      
	      tabItem(tabName = "about",
            	includeHTML("_include/about.html")
	      )     	      
	      

	    )
	#,textOutput("selectedInterval")	     
	      #Some text under the plot:
	      #p("This plot was made on ", em(Sys.Date()))
	      #,
	      #img(src="norad.jpg", alt="Mountain View", style="width:304px;height:228px;")
    )
  )








     
      #Include MathJax (optional)
    # tags$head( tags$script(src="http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_HTML-full", type = 'text/javascript'),
    #           tags$script(src = 'https://c328740.ssl.cf1.rackcdn.com/mathjax/2.0-latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML', type = 'text/javascript'),
    #             tags$script( "MathJax.Hub.Config({tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}});", type='text/x-mathjax-config')
    #
    #             ),
