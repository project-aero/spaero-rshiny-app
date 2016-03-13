library(shiny)
#library(ggplot2)
#library(grid)

source("helper.R")

#working_dir <- "~/Dropbox/ecology/emerging_app"
#working_dir <- "/srv/shiny-server/emerging_app"
working_dir <- "."

url_fields_to_sync <- c("selectPathogen","selectState","selectData", "lag","bins");

#------ INITIAL OPERATIONS: ------#



#Read in time series data:

#Compute theoretical values of EWS from stationary theory:
R0_theory <- (2.0/1000.0)*0:1000
ews_theory <-ewsStationary(R0_theory)

#Equations for stationary theoretical EWS:
ewsEquations <- c("\\(\\mu \\)", "\\(\\sigma^2 = \\frac{\\zeta}{(1-R_0)^2}\\)","\\(\\sigma/\\mu = \\sqrt(\\zeta)\\)","\\(\\sigma^2/\\mu = 1/(1-R_0)\\)")
names(ewsEquations) <- c("Mean", "Variance", "Coefficient of variation", "Index of dispersion")

#colours used in figures:
col <- c("#2a76dd", "#1B9E77", "#D95F02", "#7570B3")
names(col) <- names(ewsEquations)


#disease <- "Pertussis"
#working_dir <- "/srv/shiny-server/pertussis"
#cdc_data <- read.csv(paste(working_dir,"/data/pertussis.51.12.csv", sep=""), sep=",", comment.char = "#", stringsAsFactors=FALSE)
#tycho_data <- read.csv(paste(working_dir,"/data/PERTUSSIS_Cases_1938-2012_20160108115812.csv",sep=""), sep=",", comment.char = "#", stringsAsFactors=FALSE)

disease <- "Polio"
#working_dir <- "/srv/shiny-server/polio"
cdc_data <- read.csv(paste(working_dir,"/data/POLIO_cdc.csv", sep = ""), sep=",", comment.char = "#", stringsAsFactors=FALSE)
tycho_data <- read.csv("./data/POLIO_tycho.csv", sep=",", comment.char = "#",stringsAsFactors=FALSE)


#tycho_data <- read.csv("./data/INFLUENZA_Cases_1919-1951_20160126120221.csv", sep=",", comment.char = "#", stringsAsFactors=FALSE)




names(cdc_data) <- toupper(names(cdc_data))


tycho_data[tycho_data=="-"]<-0
cdc_data[cdc_data=="-"]<-0

cdc_data$Time <- cdc_data$YEAR + cdc_data$MONTH/12.0
tycho_data$Time <- tycho_data$YEAR + tycho_data$WEEK/52.0

tychoImport <- function(pathogen){
	td <- read.csv(paste("./data/",toupper(gsub( " ", "_", pathogen)) ,"_tycho.csv", sep = "" ), sep=",", comment.char = "#",stringsAsFactors=FALSE)
	td[td=="-"]<-0
	td$Time <- td$YEAR + td$WEEK/52.0
	return(td)
}

cdcImport <- function(pathogen){
  td <- read.csv(paste("./data/",toupper(gsub( " ", "_", pathogen)) ,"_cdc.csv", sep = "" ), sep=",", comment.char = "#",stringsAsFactors=FALSE)
  td[td=="-"]<-0
  td$Time <- td$YEAR + td$MONTH/12.0
  return(td)
}

#------ SERVER LOGIC ------#

         
# Define server logic required to draw figure etc.
shinyServer(function(input, output) {


#Import datafile:
#tycho_data <- reactive({read.csv(paste("./data/",toupper(gsub( " ", "_", input$selectPathogen)) ,"_tycho.csv" ), sep=",", comment.char = "#",stringsAsFactors=FALSE)})


#gsub( "_", " ", pathogen)
tycho_data <- reactive({tychoImport(input$selectPathogen)})
cdc_data <- reactive({cdcImport(input$selectPathogen)})



  #Number of infected individuals:
  x <- reactive({switch(input$selectData, "CDC" = cdc_data()[toupper(gsub(" ", ".", input$selectState))][[1]], "Tycho" = tycho_data()[toupper(gsub("_", ".", input$selectState))][[1]])
    })
 t <- reactive({switch(input$selectData, "CDC" = cdc_data()$Time, "Tycho" = tycho_data()$Time)
    })

  #Compute theoretical values of EWS from stationary theory:
 # ews_theory <- reactive({ ewsStationary( R0() )
  #  })
  
  #Compute theoretical values of EWS from finite speed theory:
  ews_finite_theory <- reactive({ewsFinite(cdc_data()$Time)    
  })
  
  #Calculate the EWS from the timeseries data. All EWS recalculated whenever size of window is changed (input$bins).
  ews <- reactive({ ewsAverage(t(),as.numeric(x()),  switch(input$selectData, "CDC" = 12, "Tycho" = 52)*input$bins,input$lag)
  })
  
  #Calculate probability distribution from timeseries:
  probability <- reactive({ ewsProbTimeseries(as.numeric(t()),as.numeric(x()), switch(input$selectData, "CDC" = 12, "Tycho" = 52)*input$bins)
  })
  #calculate entropy of timeseries
  entropy <- reactive({ ewsEntropy(probability())
  })
  #Calculate Gaussian entropy of timeseries
  gaussian_entropy <- reactive({log(2*pi*exp(1)*ews()$`Variance`)/2})
  
	   
  
  
  
  #Switch to select displayed EWS (unused now!): 
   selectedEWS <- reactive({switch(input$selectEWS, 
        "Mean" = list( ews()$Mean, ews_theory$Mean, ews_finite_theory()$Mean ),
        "Variance" = list(ews()$Variance, ews_theory$Variance,ews_finite_theory()$Variance),
        "Coefficient of variation" = list(ews()$`Coefficient of variation`, ews_theory$`Coefficient of variation`, ews_finite_theory()$`Coefficient of variation`),
        "Index of dispersion" = list(ews()$`Index of dispersion`, ews_theory$`Index of dispersion`, ews_finite_theory()$`Index of dispersion`)
        )
	})

 #Plot figure:
  output$distPlot <- renderPlot({
  	par(mfrow=c(4,1), ps = 24, cex = 1, cex.main = 1.5)
  	par(mar=c(1,5,3,1))
  	layout(matrix(c(1,2,3,4,5), 5,1, byrow=TRUE), heights=c(2,1,1,1))
 	plot(t(),  x(), type='l', xlab="Year", ylab="Cases", col='grey', lty=1, lwd=1.5, xaxt='n', xlim=c(input$range[[1]],input$range[[2]]), ylim=ewsYlim(t(), x(),input$range[[1]], input$range[[2]])  )
  	 axis(side=1,labels=F) 
  	lines(t(), ews()$'Mean', type='l',  col=col['Mean'], lty=1, lwd=2.5)
    	title( paste(gsub("_", " ", input$selectPathogen),"in", gsub("_", " ",input$selectState), "(window ", input$bins, "years)"))
	plot(t(), ews()$`Variance`, type='l',  ylab="Variance", col=col[2], lty=1, lwd=2.5,xaxt='n', xlim=c(input$range[[1]],input$range[[2]]),ylim=ewsYlim(t(), ews()$`Variance`,input$range[[1]], input$range[[2]])   )
  	axis(side=1,labels=F) 
  	plot(t(), ews()$`Index of dispersion`, type='l',  ylab="IoD", col=col[3], lty=1, lwd=2.5,xaxt='n', xlim=c(input$range[[1]],input$range[[2]]), ylim=ewsYlim(t(), ews()$`Index of dispersion`,input$range[[1]], input$range[[2]]))
  	axis(side=1,labels=F) 
  	plot(t(), ews()$`Autocorrelation`, type='l',  ylab=paste("AC(",input$lag,")",sep=""), xlab="Year", col=col[4], lty=1, lwd=2.5, xlim=c(input$range[[1]],input$range[[2]]), ylim=ewsYlim(t(), ews()$`Autocorrelation`,input$range[[1]], input$range[[2]]))
  	mtext(text='Year',side=1,line=4)
  })
  
  
  #Plot derivative:
  output$derivativePlot <- renderPlot({
  	par(mfrow=c(4,1), ps = 24, cex = 1, cex.main = 1.5)
  	par(mar=c(1,5,3,1))
  	layout(matrix(c(1,2,3,4,5), 5,1, byrow=TRUE), heights=c(2,1,1,1))
  	plot(t(),  x(), type='l', xlab="Year", ylab="Cases", col='grey', lty=1, lwd=1.5, xaxt='n', xlim=c(input$range[[1]],input$range[[2]]), ylim=ewsYlim(t(), ewsDerivative(t(),ews()$Mean),input$range[[1]], input$range[[2]]))
  	 axis(side=1,labels=F) 
  	lines(t(), ewsDerivative(t(),ews()$Mean), type='l',  col=col[1], lty=1, lwd=2.5)
  	title( paste(input$selectPathogen,"in",input$selectState, "(window ", input$bins, "years)"))
  	plot(t(), ewsDerivative(t(),ews()$`Variance`), type='l',  ylab="Var", col=col[2], lty=1, lwd=2.5,xaxt='n', xlim=c(input$range[[1]],input$range[[2]]), ylim=ewsYlim(t(), ewsDerivative(t(),ews()$`Variance`),input$range[[1]], input$range[[2]]))
  	axis(side=1,labels=F) 
  	plot(t(), ewsDerivative(t(),ews()$`Index of dispersion`), type='l',  ylab="IoD", col=col[3], lty=1, lwd=2.5,xaxt='n', xlim=c(input$range[[1]],input$range[[2]]), ylim=ewsYlim(t(), ewsDerivative(t(),ews()$`Index of dispersion`),input$range[[1]], input$range[[2]]))
  	axis(side=1,labels=F) 
  	plot(t(), ewsDerivative(t(),ews()$`Autocorrelation`), type='l',  ylab=paste("AC(",input$lag,")",sep=""), xlab="Year", col=col[4], lty=1, lwd=2.5, xlim=c(input$range[[1]],input$range[[2]]), ylim=ewsYlim(t(), ewsDerivative(t(),ews()$`Autocorrelation`),input$range[[1]], input$range[[2]]))
  	mtext(text='Year',side=1,line=4)
  }) 
  

  #Plot entropy:
  output$entropyPlot <- renderPlot({
    par(mfrow=c(4,1), ps = 24, cex = 1, cex.main = 1.5)
    par(mar=c(1,5,3,1))
    layout(matrix(c(1,2,3,4,5), 5,1, byrow=TRUE), heights=c(2,1,1,1))
    plot(t(),  x(), type='l', xlab="Year", ylab="Cases", col='grey', lty=1, lwd=1.5, xaxt='n', xlim=c(input$range[[1]],input$range[[2]]), ylim=ewsYlim(t(), x(),input$range[[1]], input$range[[2]])  )
    axis(side=1,labels=F) 
    lines(t(), ews()$'Mean', type='l',  col=col['Mean'], lty=1, lwd=2.5)
    title( paste(gsub("_", " ", input$selectPathogen),"in", gsub("_", " ",input$selectState), "(window ", input$bins, "years)"))
    plot(t(), entropy(), type='l',  ylab="Entropy", col=col[2], lty=1, lwd=2.5,xaxt='n', xlim=c(input$range[[1]],input$range[[2]]),ylim=ewsYlim(t(),2*entropy(),input$range[[1]], input$range[[2]])   )
    axis(side=1,labels=F)
    lines(t(),gaussian_entropy(), col=col[3], lty=1, lwd=2.5)
    plot(t(),entropy()-gaussian_entropy(), type='l',  ylab=paste("Ent-logVar",sep=""), xlab="Year", col=col[4], lty=1, lwd=2.5, xlim=c(input$range[[1]],input$range[[2]]), ylim=ewsYlim(t(),entropy() - gaussian_entropy(),input$range[[1]], input$range[[2]]) )
    mtext(text='Year',side=1,line=4)
  }) 
  
  
  output$selectedInterval <- renderText({ewsYlim(t(),entropy() - gaussian_entropy(),input$range[[1]], input$range[[2]]) })
  output$probability <- renderDataTable({probability})

  #Output selected image
  #output$selectedImage <- renderText({ "<img src=\"norad.jpg\" alt=\"Mountain View\" style=\"width:304px;height:228px;\">" })

## URL hash:
  firstTime <- TRUE
  
  output$hash <- reactiveText(function() {
    
    newHash = paste(collapse=",",
                    Map(function(field) {
                          paste(sep="=",
                                field,
                                input[[field]])
                        },
                        url_fields_to_sync))
    
    # the VERY FIRST time we pass the input hash up.
    return(
      if (!firstTime) {
        newHash
      } else {
        if (is.null(input$hash)) {
          NULL
        } else {
          firstTime<<-F;
          isolate(input$hash)
        }
      }
    )
  })


})


