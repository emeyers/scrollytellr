
save_load_panel <- nav_panel(title = "Save/Load", {
  
  list(
    
    
    
    #fluidRow(
    layout_columns(  
      
      
      card(
        
        card_header("Save app state"),
        
        p("Save the current state of the app. This will save the stickies, narration, header options, and uploaded images
          as a .zip file that can be used to restore the state of the app."),
        
        
        # Download the stickies_df, narration_df, header_list, and images/ directory
        downloadButton("downloadAppState", label = "Download app state") 
        
      ),
      
      
      
      
      card(
        
        card_header("Load app state"),
        
        p("Load a previously saved state of the app that was saved as a .zip file. This will restore 
        the stickies, narration, header options, and uploaded images to the state they were in when the app was saved."),
        
        fileInput("UploadSavedAppState", "Upload a saved app state", accept = c(".zip"))
        
        
      ),
      
      
      
      
      card(
        
        card_header("Reset"),
        
        p("Warning: if you reset the state you will lose all your work so if you want to 
          save your progress please be sure to save the state of the app first."),
        
        
        # Add the sticky to the list/data frame of stickies
        actionButton(inputId = "AppStateReset", label = "Reset app state") 
        
      )               
                                  
      
      
      ) # end layout_rows
                                  
                                  
    
    
  )})   # end narration panel














