

save_state_button_pressed <- function(input, output, session) {
  
  
    output$downloadAppState <- downloadHandler(
      
      
      filename = "app_state.zip",   # can give any name here
      

      content = function(file) {
        
        # Save the state of the app
        save(stickies_df, narration_df, header_list, file = "app_state.rda")
        
        # zip the images/ directory and the app_state.rda file to a file called app_state.zip
        zip("app_state.zip", c("images", "app_state.rda"))
        
        file.copy("app_state.zip", file)
        
      }
    
      
    )  # end the downloadHandler

}







load_state_button_pressed <- function(input, output, session) {
  
  observeEvent(input$UploadSavedAppState, { 
    
    print("load state pressed")
    
    # Load the state of the app
    
    print(input$UploadSavedAppState)
  
    
    unzip(input$UploadSavedAppState$datapath)
    
    load("app_state.rda")
    
    print(stickies_df)
    
    
    narration_df <<- narration_df
    stickies_df <<- stickies_df
    header_list <<- header_list
    
    narration_df_reactive <<- reactiveVal(narration_df)
    stickies_df_reactive <<- reactiveVal(stickies_df)
    header_list_reactive <<- reactiveVal(header_list)
    
    # need to update the stickies DT
    show_stickies_dt(input, output, session)
    show_narration_dt(input, output, session)
    
    # Set the narrations stickies options to be the stickies that have been loaded
    updateSelectInput(session, "NarrationSticky",
                      label = "Select Sticky",
                      choices = stickies_df$name,
                      selected = stickies_df$name[1])
    
    
    
    
    # Restore the appearance settings
    # textInput("DocumentTitle", label = "Title"),       # value = "", width = NULL, placeholder = NULL)
    # 
    # colourInput("header_narrative_text_color", "Narrative Text Color", "white"),
    # 
    # selectInput("DocumentLayout", "Layout", choices = c("overlay-left", "overlay-center", "overlay-right",
    #                                                     "sidebar-left", "sidebar-right")),
    # colourInput("header_narrative_background_color", "Narrative Background Color", "darkgreen"),
    # colourInput("header_section_background_color", "Section Background Color", "gray"),
    # selectInput("header_narrative_font", "Narrative font", choices = font_names, selectize=FALSE, selected = "Helvetica"),
    # selectInput("header_poem_font", "Poem font", choices = font_names, selectize=FALSE, selected = "Helvetica"),
    # numericInput("header_narrative_font_size", "Narrative font size", 12)
    # 
    
    updateTextInput(session, "DocumentTitle", value = header_list$title)
    updateColourInput(session, "header_narrative_text_color", value = header_list$narrative_text_color_overlay)
    updateSelectInput(session, "DocumentLayout", selected = header_list$layout)
    updateColourInput(session, "header_narrative_background_color", value = header_list$narrative_background_color_overlay)
    updateColourInput(session, "header_section_background_color", value = header_list$section_background_color)
    updateSelectInput(session, "header_narrative_font", selected = header_list$narrative_font_family)
    updateSelectInput(session, "header_poem_font", selected = header_list$poem_font_family)
    updateNumericInput(session, "header_narrative_font_size", value = as.numeric(gsub("pt", "", header_list$narrative_font_size)))
    
    
    
    # Update the Quarto and HTML documents
    display_quarto_doc(input, output, session)
    display_html_doc(input, output, session)   # sometimes seems to work without this...
    
    
    # Make the download menu visible again
    add_downloads_menu(input, output, session)
    
    })

}







reset_state_button_pressed <- function(input, output, session) {
  
  observeEvent(input$AppStateReset, {
    
    
    print("hi")
    
    
    # Global variables used by the server
    stickies_df <<- data.frame()
    narration_df <<- data.frame()
    header_list <<- list()
    
    narration_df_reactive <<- reactiveVal(narration_df)
    stickies_df_reactive <<- reactiveVal(stickies_df)
    header_list_reactive <<- reactiveVal(header_list)
    
    # Reset the stickies and narration data tables
    show_stickies_dt(input, output, session)
    show_narration_dt(input, output, session)
    
    # Reset the Quarto and HTML documents
    display_quarto_doc(input, output, session)
    display_html_doc(input, output, session)   # is working without this
    
    
    # Need to reset the header options?
    
    
  })

}











