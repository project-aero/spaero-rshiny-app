#!/usr/bin/env Rscript

#install.packages("shiny")
#install.packages("shinythemes")
#install.packages("shinydashboard")

library(shiny)



#Path to directory containing Rshiny app folder
setwd("~/Dropbox/ecology")
#Rshiny app name (= folder name)
runApp("emerging_app", launch.browser =TRUE)
