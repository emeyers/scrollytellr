



# Show the table that has the narrations that have been added
# This table can be edited by clicking on it, etc.

show_narration_dt  <- function(input, output, session) {
  
  output$ShowNarrationsDT <- renderDT({
    
    # need to change narrationNewOrder, ShowNarrationsDT, and narration_df
    # to work with other data frames...
    
    #input$AddNarration  # update every time the AddNarration is pressed
    
    js <- generate_js_reorder_rows_code(narration_df, "narrationNewOrder")
    
    the_data_table <- datatable(
      #narration_df,
      narration_df_reactive(),
      rownames = FALSE,
      extensions = "RowReorder",
      callback = JS(js),
      options = list(rowReorder = TRUE, dom = 't', ordering=F),
      editable = TRUE)  # can edit the values in the table
    
    the_data_table
    
  }, server = TRUE)  # end renderDT
  
  
}




# Function called when the AddNarration button is pressed

add_narrations_button_pressed <- function(input, output, session) {
  
  
  observeEvent(input$AddNarration, {
    
    # if no stickies have been created yet, do not allow one to add narrations
    if (nrow(stickies_df) == 0) {
      
      shinyalert("No stickies exist yet", 
                 "You need to first create a sticky before you can add narrations.", 
                 type = "info")
      
      validate(
        need((nrow(stickies_df) != 0), 
             message = "You need to first create a sticky before you can add narrations.")
      )
      
    }
    
    curr_narration_df <- data.frame(sticky = input$NarrationSticky,
                                    text = input$NarrationText,
                                    options = get_narration_options(input))
    
    
    # clear the text once the narration has been added
    updateTextInput(session, "NarrationText", value = "") 
    
    # reset all the narration/sticky options
    shinyjs::reset("NarrationOptionsHighlightLines")
    shinyjs::reset("NarrationOptionsPanHorizontal")
    shinyjs::reset("NarrationOptionsPanVertical")
    
    # shinyjs::reset() doesn't work with shinyWidgets
    shinyWidgets::updateSliderTextInput(session, "NarrationOptionsScale", selected = 1) 

    narration_df <<- rbind(narration_df, curr_narration_df)
    
    # update the visual display of the table  
    proxy <- dataTableProxy("ShowNarrationsDT")  # additional code to get the row order to rearrange in the narrations DT
    
    replaceData(proxy, narration_df, resetPaging = FALSE, rownames = FALSE)
    narration_df_reactive(narration_df)
    
  })
  
  
}
 

# Function for reordering the narration table rows 

reorder_narration_dt_rows <- function(input, output, session) {

  observeEvent(input$narrationNewOrder, {
    narration_df_original <- narration_df_reactive()
    narration_df_reordered <- narration_df_original[input$narrationNewOrder + 1, ]
    proxy <- dataTableProxy("ShowNarrationsDT")  # additional code to get the row order to rearrange in the narrations DT
    replaceData(proxy, narration_df_reordered, resetPaging = FALSE, rownames = FALSE)
    narration_df_reactive(narration_df_reordered)
    narration_df <<- narration_df_reordered
  })
  
}


# Function for deleting rows from the narration table (narration data frame)
delete_narration_dt_rows <- function(input, output, session) {
  
  observeEvent(input$deleteNarrationRows,{
    
    if (!is.null(input$ShowNarrationsDT_rows_selected)) {
      
      narration_df <<- narration_df[-as.numeric(input$ShowNarrationsDT_rows_selected),]
      
      # update the visual display of the table  
      proxy <- dataTableProxy("ShowNarrationsDT")  # additional code to get the row order to rearrange in the narrations DT
      replaceData(proxy, narration_df, resetPaging = FALSE, rownames = FALSE)
      narration_df_reactive(narration_df)
    }
    
  })
  
}


# Function for when a cell in the narration data table/frame is edited
edit_narration_dt_rows <- function(input, output, session) {
  
  observeEvent(input$ShowNarrationsDT_cell_edit, {
    
    row  <- input$ShowNarrationsDT_cell_edit$row
    clmn <- input$ShowNarrationsDT_cell_edit$col + 1  # not sure why I need to add 1 here, but ok
    narration_df[row, clmn] <<- input$ShowNarrationsDT_cell_edit$value
    
    narration_df_reactive(narration_df)
    
  })
  
}




preview_narration_image <- function(input, output, session) {

  # When HTML tab is entered run this code
  output$PreviewOutput <- renderUI({
    
    # # if no narrations have been created yet, can't render the HTML document
    # if (nrow(narration_df) == 0) {
    #   validate(
    #     need((nrow(narration_df) != 0), 
    #          message = "\n\nYou need to first add narrations before you can generate an HTML document")
    #   )
    # }
    
    
    preview_header_list <- create_header_list(input)   #list(layout = "sidebar-left", narrative_background_color_overlay = "gray")
    
    
    preview_narration_df <- data.frame(sticky = input$NarrationSticky,
                                       text = input$NarrationText,
                                       options = get_narration_options(input))
    
    preview_sticky_df <- subset(stickies_df, name == preview_narration_df$sticky)
    
    quarto_preview <- generate_Closeread_Quarto_doc(preview_narration_df, preview_sticky_df, preview_header_list)
    
    html_output <- getClosereadPage(quarto_preview, "preview", 500, 500)
    
    html_output    
    
  })
  
  
  
}
  
 


select_lines_to_highlight <- function(input, output, session) {
  

  output$SelectLinesToHighlight <- renderUI({
    
    narration_df_reactive()

    selected_sticky <- subset(stickies_df_reactive(), name == input$NarrationSticky) 
    
    if (selected_sticky$type  == "Text") {
      
      sticky_text_lines <- unlist(strsplit(subset(stickies_df_reactive(), name == selected_sticky$name)$text, split = "\n"))
      
      # If every line of the text is unique, select lines to highlight based on the full text of the line
      # Otherwise select the line based on the line number
      if (length(unique(sticky_text_lines)) == length(sticky_text_lines)) {
        hight_line_choices <- sticky_text_lines
      } else {
        hight_line_choices <- seq(1, length(sticky_text_lines))
      }

      
      selectizeInput("NarrationOptionsHighlightLines", "Highlight lines" , 
                     choices = hight_line_choices, multiple = TRUE)

    }

    
    # do I need to add an else for not text choices?
    
    
  })
  
  
}



