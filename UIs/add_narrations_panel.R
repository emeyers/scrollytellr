

add_narrations_panel <- nav_panel(title = "Add Narration", {
  
  list(
    
    layout_columns(
      
      card(
        
        card_header("Add narration"),
        
        p("\n"),
        
        
        # will need to dynamically generate the list of stickies based on what has been added
        selectInput(inputId = "NarrationSticky", label = "Select Sticky", 
                    choices = c("No stickies created yet")),
        
        
        textAreaInput(
          inputId = "NarrationText",
          label = "Narration Text",
          height = "250px",
          width = "100%",
          placeholder = "Add your narration text here"),
        
        # Additional sticky selection options
        #textInput(inputId = "NarrationStickyOptions", label = "Sticky Options"),
        
        
        # Add the sticky to the list/data frame of stickies
        actionButton(inputId = "AddNarration", label = "Add Narration") 
        
      ),
      
      card(
        
        card_header("Narrations created"),
        
        p("\n"),
        
        # print a table of stickies that exist
        #tableOutput('ShowNarrations')
        DTOutput('ShowNarrationsDT'),
        actionButton("deleteNarrationRows", "Delete highlighted narrations")
        
      ),
      
      
      card(
        accordion(open = FALSE,
          
          accordion_panel("Narration Options", open = FALSE,
                          
                          layout_columns(
                            
                            card("Options",
                                 
                                 # Need to make this conditional depending on the type of sticky
                                 
                                 uiOutput("SelectLinesToHighlight"),
                                 
                                 shinyWidgets::sliderTextInput("NarrationOptionsScale", "Scale",
                                                               choices=c(0, .25, .5, .75, 1, 1.5, 2, 2.5, seq(3, 10)),
                                                               selected=1, grid = T),
                                 
                                 sliderInput(inputId = "NarrationOptionsPanHorizontal", 
                                             label = "Pan horizontal", min = -100, max = 100, value = 0),
                                 
                                 sliderInput(inputId = "NarrationOptionsPanVertical", 
                                             label = "Pan vertical", min = -100, max = 100, value = 0)
                                 
                                 
                                 
                            ),
                            
                            card("Preview",
                                 
                                 # Add a generate preview button here...
                                 htmlOutput(outputId = "PreviewOutput")
                                 
                            )
                            
                          ) # end layout_columns
                          
                          
                          
                          
          )
        )
      ),
      col_widths = c(6, 6, 12)
      
    )
    
 
  )})   # end narration panel

