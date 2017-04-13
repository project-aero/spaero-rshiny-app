library(shiny)
#library(ggplot2)
#library(grid)

source("helper.R")


working_dir <- "."

url_fields_to_sync <- c("selectPathogen","selectState", "lag","bins");

#------ INITIAL OPERATIONS: ------#

#colours used in figures:
col <- c("#2a76dd", "#1B9E77", "#D95F02", "#7570B3")
names(col) <-c("Mean", "Variance", "Coefficient of variation", "Index of dispersion")

names(col) <- c("Mean", "Variance", "Coefficient of variation", "Index of dispersion")
disease <- "Polio"
tycho_data <- read.csv("./data/POLIO_tycho.csv", sep=",", comment.char = "#",stringsAsFactors=FALSE)
tycho_data[tycho_data=="-"]<-0
tycho_data$Time <- tycho_data$YEAR + tycho_data$WEEK/52.0

tychoImport <- function(pathogen){
	td <- read.csv(paste("./data/",toupper(gsub( " ", "_", pathogen)) ,"_tycho.csv", sep = "" ), sep=",", comment.char = "#",stringsAsFactors=FALSE)
	td[td=="-"]<-0
	td$Time <- td$YEAR + td$WEEK/52.0
	return(td)
}



#------ SERVER LOGIC ------#

         
# Define server logic required to draw figure etc.
shinyServer(function(input, output) {

  #Import tycho data
  tycho_data <- reactive({tychoImport(input$selectPathogen)})

  #Number of infected individuals:
  x <- reactive({tycho_data()[toupper(gsub("_", ".", input$selectState))][[1]]})
  t <- reactive({tycho_data()$Time})

  #Compute theoretical values of EWS from finite speed theory:
  ews_finite_theory <- reactive({ewsFinite(cdc_data()$Time)})
  
  #Calculate the EWS from the timeseries data. All EWS recalculated whenever size of window is changed (input$bins).
  ews <- reactive({ ewsAverage(t(),as.numeric(x()),  52*input$bins,input$lag)})
  
  #Calculate probability distribution from timeseries:
  probability <- reactive({ewsProbTimeseries(as.numeric(t()),as.numeric(x()), 52*input$bins)})

 #Plot figure:
  output$distPlot <- renderPlot({
  	par(mfrow=c(4,1), ps = 24, cex = 1, cex.main = 1.5)
  	par(mar=c(1,5,3,1))
  	layout(matrix(c(1,2,3,4,5), 5,1, byrow=TRUE), heights=c(2,1,1,1))
 	plot(t(),  x(), type='l', xlab="Year", ylab="Cases", col='grey', lty=1, lwd=1.5, xaxt='n', xlim=c(input$range[[1]],input$range[[2]]), ylim=ewsYlim(t(), x(),input$range[[1]], input$range[[2]])  )
  	 axis(side=1,labels=F) 
  	lines(t(), ews()$'Mean', type='l',  col=col['Mean'], lty=1, lwd=2.5)
    	title( paste(gsub("_", " ", input$selectPathogen),"in", gsub("_", " ",input$selectState), "(window", input$bins, "years)"))
	plot(t(), ews()$`Variance`, type='l',  ylab="Variance", col=col[2], lty=1, lwd=2.5,xaxt='n', xlim=c(input$range[[1]],input$range[[2]]),ylim=ewsYlim(t(), ews()$`Variance`,input$range[[1]], input$range[[2]])   )
  	axis(side=1,labels=F) 
  	plot(t(), ews()$`Index of dispersion`, type='l',  ylab="IoD", col=col[3], lty=1, lwd=2.5,xaxt='n', xlim=c(input$range[[1]],input$range[[2]]), ylim=ewsYlim(t(), ews()$`Index of dispersion`,input$range[[1]], input$range[[2]]))
  	axis(side=1,labels=F) 
  	plot(t(), ews()$`Autocorrelation`, type='l',  ylab=paste("AC(",input$lag,")",sep=""), xlab="Year", col=col[4], lty=1, lwd=2.5, xlim=c(input$range[[1]],input$range[[2]]), ylim=ewsYlim(t(), ews()$`Autocorrelation`,input$range[[1]], input$range[[2]]))
  	mtext(text='Year',side=1,line=4)
  })
  
  
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


