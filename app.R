
# include the shiny package
library(shiny)
library(htmltools)
library(bslib)
library(quarto)
library(shinyAce)  # for displaying the Quarto script
library(shinyalert)  # for popup messages
library(DT)  # to show data tables that are editable, etc.
library(colourpicker)




shinyApp(ui = ui, server = server) 


