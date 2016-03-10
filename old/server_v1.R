library(shiny)
#library(ggplot2)
#library(grid)

source("helper.R")

#------ INITIAL OPERATIONS: ------#

#Read in time series data:

#Compute theoretical values of EWS from stationary theory:
R0_theory <- (2.0/1000.0)*0:1000
ews_theory <-ewsStationary(R0_theory)

#Equations for stationary theoretical EWS:
ewsEquations <- c("\\(\\mu \\)", "\\(\\sigma^2 = \\frac{\\zeta}{(1-R_0)^2}\\)","\\(\\sigma/\\mu = \\sqrt(\\zeta)\\)","\\(\\sigma^2/\\mu = 1/(1-R_0)\\)")
names(ewsEquations) <- c("Mean", "Variance", "Coefficient of variation", "Index of dispersion")


#------ SERVER LOGIC ------#

         
# Define server logic required to draw figure etc.
shinyServer(function(input, output) {

  #Read in time series data:
  timeseries <- reactive({read.table(paste("~/Dropbox/ecology/emerging_app/data/test_timeseries_",input$selectInterval,"years.csv", sep = "", collapse = ""), sep='\t')
    })
 
  #Number of infected individuals:
  x <- reactive({ timeseries()[, 3] 
    })
  
  #Value of R0:
  R0 <- reactive({timeseries()[,4]
    })

  #Compute theoretical values of EWS from stationary theory:
 # ews_theory <- reactive({ ewsStationary( R0() )
  #  })
  
  #Compute theoretical values of EWS from finite speed theory:
  ews_finite_theory <- reactive({ewsFinite(R0())    
  })
  
  #Calculate the EWS from the timeseries data. All EWS recalculated whenever size of window is changed (input$bins).
  ews <- reactive({ ewsAverage(R0(),x(),input$bins)
  })
	   
  #Switch which selects which EWS is displayed 
   selectedEWS <- reactive({switch(input$selectEWS, 
        "Mean" = list( ews()$mu, ews_theory$Mean, ews_finite_theory()$Mean ),
        "Variance" = list(ews()$var, ews_theory$Variance,ews_finite_theory()$Variance),
        "Coefficient of variation" = list(ews()$cov, ews_theory$`Coefficient of variation`, ews_finite_theory()$`Coefficient of variation`),
        "Index of dispersion" = list(ews()$iod, ews_theory$`Index of dispersion`, ews_finite_theory()$`Index of dispersion`)
        )
	})

 #Plot figure
  output$distPlot <- renderPlot({

  	plot( R0(), selectedEWS()[[1]] , log= "y", xlab = "R0", ylab = input$selectEWS, type='l', lwd=2)
  	title( paste(input$selectEWS, "averaged over a window of period", input$bins, "weeks."))
  	lines( R0_theory, selectedEWS()[[2]], col = "#2a76dd", lwd=2)
  	lines(R0(),selectedEWS()[[3]], col = "red", lwd=2)
  	legend("bottomright", c("Moving average", "Stationary BDI process", "BDI process"), lty=c(1,1,1), lwd=c(2,2,2), col=c("black","#2a76dd","red"))
  })
  
  #Output selected period:
  output$selectedInterval <- renderText({paste("Data stored in \"~/ecology/emerging_app/data/test_timeseries_",input$selectInterval,"years.csv\"", sep = "") })

  
  #Output selected image
  output$selectedImage <- renderText({ "<img src=\"norad.jpg\" alt=\"Mountain View\" style=\"width:304px;height:228px;\">" })
})


