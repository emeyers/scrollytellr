

# include the shiny package
# seems like I need to load the libraries here otherwise I'm getting an error
# if they are only loaded in app.R
library(shiny)
library(htmltools)
library(bslib)
library(quarto)
library(shinyAce)  # for displaying the Quarto script
library(shinyalert)  # for popup messages
library(shinyjs) # for resetting inputs to their default values
library(DT)  # to show data tables that are editable, etc.
library(colourpicker)



# source the UI components
source("UIs/add_stickies_panel.R")
source("UIs/add_narrations_panel.R")
source("UIs/set_appearance_panel.R")
source("UIs/display_quarto_panel.R")
source("UIs/display_html_panel.R")



ui <- page_fillable( #page_fluid(  # page_navbar(   # fluidPage( 
  
  shinyjs::useShinyjs(),
  
  navset_tab(id = "MainMenu",
             
             add_stickies_panel,
             
             add_narrations_panel,
             
             set_appearance_panel, 
             
             display_quarto_panel,
             
             display_html_panel
             
             # Downloads menue will be added dynamically when narration_df is not an empty data frame
             
  )
  
  
) # closing parenthesis for the UI  





