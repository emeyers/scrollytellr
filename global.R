
# Global variables used by the server
stickies_df <- data.frame()
narration_df <- data.frame()
header_list <- list()

narration_df_reactive <- reactiveVal(narration_df)
stickies_df_reactive <- reactiveVal(stickies_df)
header_list_reactive <- reactiveVal(header_list)



# could potentially put some of these functions elsewhere (in the server file?)


# Gets all the header options created by the appearance panel. Used for generating
# Quarto and HTML documents
create_header_list <- function(input) {
  
  header_list$title <<- input$DocumentTitle
  header_list$layout <<-input$DocumentLayout
  header_list$narrative_text_color_overlay <<- input$header_narrative_text_color
  header_list$narrative_text_color_sidebar <<- input$header_narrative_text_color
  header_list$narrative_background_color_overlay <<- input$header_narrative_background_color
  header_list$narrative_background_color_sidebar <<- input$header_narrative_background_color
  header_list$section_background_color <<- input$header_section_background_color
  header_list$narrative_font_family <<- input$header_narrative_font
  header_list$poem_font_family <<- input$header_poem_font
  header_list$narrative_font_size <<- paste0(input$header_narrative_font_size, 'pt')
  
  #header_list$poem_font_size <<- paste0(input$header_poem_font_size, 'pt')  # not a Closeread option :(
  
  header_list_reactive <- reactiveVal(header_list)
  header_list_reactive <- header_list_reactive(header_list)
  
  header_list
}




# Helper function to get a string of the narration options
get_narration_options <- function(input) {
  

  options_string <- ""
  
  
  # Panning
  if ((input$NarrationOptionsPanHorizontal != 0) || (input$NarrationOptionsPanVertical != 0)) {
    options_string <- paste0("pan-to=", input$NarrationOptionsPanHorizontal, "%,",
                             input$NarrationOptionsPanVertical, "%")
  }

                           
  # Scaling (zooming) on the image
  if (input$NarrationOptionsScale != 1) {
    options_string <- paste0(options_string, " scale-by=", input$NarrationOptionsScale)
  }
  
  
  # Highlighting line numbers 
  if (!(is.null(input$NarrationOptionsHighlightLines))) {
    
    selected_sticky <- subset(stickies_df_reactive(), name == input$NarrationSticky) 
    
    # will be a problem if two lines are the same :(
    all_lines <- unlist(strsplit(selected_sticky$text, split = "\n"))
    all_highlights <- input$NarrationOptionsHighlightLines

    # if multiple lines of the sticky are the same, need to highlight based on line number
    if (length(unique(all_lines)) != length(all_lines)) {
      all_lines <- as.character(seq(1, length(all_lines)))
    }
    
    highlight_line_numbers <- c()
    for (iLine in seq_along(all_lines)) {
      if (all_lines[iLine] %in% all_highlights) {
        highlight_line_numbers <- c(highlight_line_numbers, iLine)
      }
    }
    
    highlight_options <- paste0("highlight=", "'", paste0(highlight_line_numbers, collapse = ","), "'")
    options_string <- paste(highlight_options, options_string)
  
  }
  
  
  options_string
  
}




# Write the quarto document as a file. Then renders it to create an HTML document.
# Finally returns the html document in an iframe for display. 
getClosereadPage <- function(quarto_text, save_page_name = "closeread_doc", width, height) {
  
  writeLines(quarto_text, paste0(save_page_name, ".qmd"))
  
  # Adding a one second delay to make sure the file is down being written to before rendering
  # If this is not included, sometimes the .html rendering file is missing content
  # This is a pretty ugly solution, would be great if there is a better one
  Sys.sleep(.25)  #  Sys.sleep(1) 
  
  quarto_render(paste0(save_page_name, ".qmd"))
  
  # could perhaps alternatively use addResourcePath() to include _extensions directory
  temp_quarto_dir <- paste(sample(letters, 20, replace = TRUE), collapse = "")
  addResourcePath(temp_quarto_dir, getwd())
  
  closeread_html <- tags$iframe(
    src=paste0(temp_quarto_dir, "/", save_page_name, ".html"),
    width=width, height=height)
  
  closeread_html
  
}






# A function to generate code for reordering rows in the narration DT table
# Currently only used by the narration table but could be useful if I want to 
# add the ability to reorder rows in the stickies table
generate_js_reorder_rows_code <- function(the_df, new_order_variable_name) {
  
  input_val_string <- paste0("  Shiny.setInputValue('", new_order_variable_name,  "', order);")
  
  js <- c(
    "table.on('row-reorder', function(e, details, edit) {",
    sprintf("  var order = [%s];", toString(0:(nrow(the_df)-1))),
    "  for(entry of details) {",
    "    order[entry.newPosition] = entry.oldPosition;",
    "  }", 
    input_val_string,
    "});"
  )
  
  js
  
}








