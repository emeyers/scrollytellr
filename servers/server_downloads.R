



# Add the Downloads menu to the UI once narrations have been added 

add_downloads_menu <- function(input, output, session) {
  
  observeEvent(narration_df_reactive(), {
    
    nav_remove("MainMenu", target = "Downloads")
    
    if (nrow(narration_df) > 0) {
      
      nav_insert(
        "MainMenu", target = "HTML output",
        nav_menu("Downloads", 
                 nav_spacer(),
                 nav_item(downloadButton("downloadQuarto", "Download Quarto Document")),
                 nav_spacer(),
                 nav_item(downloadButton("downloadHTML", "Download HTML Page"))
                 
                 # should also add an item to save the stickies_df, narration_df, header_list
                 # so that they can be reloaded into the app...
                 
        ) # end nav_panel
        
      ) # end insert Downloads tab
      
    } 
    
  })
  
  
}  # end add_downloads_menu






# Code to download the Quarto document
download_quarto <- function(input, output, session) {
  
  output$downloadQuarto <- downloadHandler(
    filename = "closeread_doc.qmd",   # can give any name here
    content = function(file) {
      file.copy("closeread_doc.qmd", file)
    }
  )
  
}





# Code to download the HTML document
download_html <- function(input, output, session) {
  
  output$downloadHTML <- downloadHandler(
    
    filename = function() {
      "closeread_doc.html"   # can give any name for this file
    },  
    
    
    content = function(file) {
      
      # create the close read html document
      quarto_text <- generate_Closeread_Quarto_doc(narration_df_reactive(), 
                                                   stickies_df, 
                                                   header_list)
      
      html_output <- getClosereadPage(quarto_text, "closeread_doc", 800, 800)
      
      file.copy("closeread_doc.html", file)
    }
  )
  
}




