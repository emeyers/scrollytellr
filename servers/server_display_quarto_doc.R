


# Display Quarto text in a Shiny Ace editor  (use either this, or QuartoText but not both)
display_quarto_doc <- function(input, output, session) {
 
  
  output$DisplayQuartoDoc <- renderUI({ 
    
    stickies_df_reactive()
    narration_df_reactive()
    
    # if no narrations have been created yet, can't render the Quarto document
    if (nrow(narration_df) == 0) {
      validate(
        need((nrow(narration_df) != 0), 
             message = "\n\nYou need to first add narrations before you can generate a Quarto document")
      )
    }
    
    
    create_header_list(input)
    
    #quarto_text <- generate_Closeread_Quarto_doc(narration_df, stickies_df, header_list)
    quarto_text <- generate_Closeread_Quarto_doc(narration_df_reactive(), 
                                                 stickies_df, 
                                                 header_list)
    
    header_list_reactive()
    
    ace_editor <- shinyAce::aceEditor("DisplayQuartoDoc",
                                      quarto_text,
                                      mode = "markdown",
                                      readOnly = TRUE)
    
    ace_editor
    
  })
  
  
}

