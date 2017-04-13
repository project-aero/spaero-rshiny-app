library(shiny)
library(shinydashboard)
#library(shinythemes)

hashProxy <- function(inputoutputID) {
  div(id=inputoutputID,class=inputoutputID,tag("div",""));
}

state_names <- gsub(" ", "_", strsplit("Alabama,Arizona,Arkansas,California,Colorado,Connecticut,Delaware,Florida,Georgia,Idaho,Illinois,Indiana,Iowa,Kansas,Kentucky,Louisiana,Maine,Maryland,Massachusetts,Michigan,Minnesota,Mississippi,Missouri,Montana,Nebraska,Nevada,New Hampshire,New Jersey,New Mexico,New York,North Carolina,North Dakota,Ohio,Oklahoma,Oregon,Pennsylvania,Rhode Island,South Carolina,South Dakota,Tennessee,Texas,Utah,Vermont,Virginia,Washington,West Virginia,Wisconsin,Wyoming", ",")[[1]])
names(state_names) <-  gsub("_", " ", state_names)

pathogen_names <-  c("Hepatitis_A", "Influenza", "Measles", "Mumps", "Pertussis", "Polio", "Rubella", "Smallpox")
names(pathogen_names) <-  gsub("_", " ", pathogen_names)


# Define UI for application that draws EWS for disease data
dashboardPage(

  # Application title. Appears in the title bar and at the top of page
  dashboardHeader(title ="Early-warning signals"),

    # Sidebar containing input selection options 
  dashboardSidebar(
    hr(),
    sidebarMenu(id="tabs"
              ,menuItem("Plot early-warning signal", tabName="plot", icon=icon("line-chart"), selected=TRUE)
              ,menuItem("About", tabName = "about", icon = icon("question"))
  ),
  hr(),
  
      selectInput("selectPathogen", 
        label = "Choose pathogen",
        choices = pathogen_names,
        selected = "Polio"),
  
      selectInput("selectState", 
        label = "Choose state",
        choices = state_names,
        selected = "Alabama"),
      

      sliderInput("bins",
                  "Choose the window size used for time averaging (in years)",
                  min = 1,
                  max = 20,
                  value = 2),
      
      sliderInput("lag", "Choose lag for autocorrelation function:*",
                  min = 1, max = 52, value = 1),
      
      sliderInput("range", "Output range:",
                min = 1900, max = 2012, value = c(1938,2012)),
      p("*Lag is in weeks for Tycho data"),
      hr()

      ),
    
    #Main panel, on rhs
    dashboardBody(
      includeHTML("www/js/URL.js"),
      hashProxy("hash"),

      # Show a plot of the computed EWS:
      tabItems(
	      tabItem(tabName = "plot",
		      fluidRow(
			      box(  width = NULL, plotOutput("distPlot",  height = "800", width = "100%"), collapsible = TRUE,
				  title = "Plot", status = "primary", solidHeader = TRUE)

		      )#,
	      ),
	      tabItem(tabName = "about",
            	includeHTML("_include/about.html")
	      )     	      
	      

	    )
    )
  )

     
      #Include MathJax (optional)
    # tags$head( tags$script(src="http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_HTML-full", type = 'text/javascript'),
    #           tags$script(src = 'https://c328740.ssl.cf1.rackcdn.com/mathjax/2.0-latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML', type = 'text/javascript'),
    #             tags$script( "MathJax.Hub.Config({tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}});", type='text/x-mathjax-config')
    #
    #             ),
