

# When HTML panel is entered run this code to display compiled HTML closeread document
display_html_doc <- function(input, output, session) { 
  
  
  output$HTMLOutput <- renderUI({
    #output$ClosereadOutput <- renderUI({
    
    # make sure the html document is regenerated every time the narration_df changes
    #narration_df_reactive() 
    
    # if no narrations have been created yet, can't render the HTML document
    if (nrow(narration_df) == 0) {
      validate(
        need((nrow(narration_df) != 0), 
             message = "\n\nYou need to first add narrations before you can generate an HTML document")
      )
    }
    
    
    create_header_list(input)
    header_list_reactive()
    
    quarto_text <- generate_Closeread_Quarto_doc(narration_df_reactive(), 
                                                 stickies_df, 
                                                 header_list)
    
    html_output <- getClosereadPage(quarto_text, "closeread_doc", 800, 800)
    
    html_output    
    
  })
  
  
  
}




