


# function to convert the navigation_df, stickies_df, and header_list into a Closeread Quarto document
# that can be rendered into a .html scrollytelling document.

generate_Closeread_Quarto_doc <- function(narration_df, stickies_df, header_list) {
  
  quarto_text <- paste0(generate_header(header_list),
                        '\n\n:::{.cr-section}\n\n',
                        generate_narrations(narration_df),
                        generate_stickies(stickies_df),
                        '\n\n:::')
  
  
  # write text to a .qmd file
  
  quarto_text
  
}



generate_narrations <- function(narration_df) {
  
  all_narration_text <- ""
  
  for (i in 1:nrow(narration_df)) {
    
    curr_narration_text <- generate_one_narration(narration_df$text[i], 
                                               narration_df$sticky[i], 
                                               narration_df$options[i])
    
    
    all_narration_text <- paste0(all_narration_text, "\n", curr_narration_text)
    
  }
  
  
  all_narration_text
  
}



generate_one_narration <- function(text, sticky_name, options = "") {
  
  #narration_item_text <- paste0("\n", text, 
  #                             ' @cr-', sticky_name)
  
  narration_item_text <- paste0("\n", text, 
                                ' [@cr-', sticky_name, ']')
  
  
  if (options != "") {
    narration_item_text <- paste0( narration_item_text, '{', options, '}')
  }
                               
 
  narration_item_text <- paste(narration_item_text, '\n')
  
  narration_item_text 
  
}





# generate all the stickies
generate_stickies <- function(stickies_df) {
  
  all_sticky_text <- ""
  
  for (i in 1:nrow(stickies_df)) {
    
    print(stickies_df$name[i])
    print(stickies_df$type[i])
    print(stickies_df$text[i])
    
    curr_sticky_text <- generate_one_sticky(stickies_df$name[i], 
                                            stickies_df$type[i], 
                                            stickies_df$text[i])
    
    
    all_sticky_text <- paste0(all_sticky_text, "\n\n", curr_sticky_text)
    
  }
  
  
  all_sticky_text
  
}





# helper function to generate one sticky
generate_one_sticky <- function(sticky_name, sticky_type, sticky_text) {
  
  if (sticky_type == "Image") {
    
    sticky_content <- paste0("![](", sticky_text , ")")
    
  
  } else if (sticky_type == "Text") {

    sticky_content <- paste0("| ", gsub("\n", "\n| ", sticky_text))
    
  } else if ( (sticky_type == "RCode") | (sticky_type == "R Code")) {
    
    sticky_content <- paste0("```{r}\n",
                             sticky_text,
                             "\n```")
  } else {
    
    stop(paste(sticky_type, 'is an invalid type. The sticky_type must be set to "Image", "Text", or "RCode" '))
  }
  

  sticky_item_text <- paste0(':::{#cr-', sticky_name, '}\n',
                             sticky_content,
                             '\n:::'
                             )


  # return the stick item text                               
  sticky_item_text
  
}






# generate the header text
generate_header <- function(header_list) {
  
#   header <- '
# ---
# title: Experiment with Closeread
# format:
#   closeread-html:
#     embed-resources: true
#     cr-section:
#       layout: "overlay-left"
#     cr-style:
#       narrative-background-color-overlay: darkslategrey
#       narrative-text-color-overlay: "#e2e2e2"
# ---\n'
  
  if (is.null(header_list$title)) {
    header_list$title <- "Default title"
  }
  
  if (is.null(header_list$layout)) {
    header_list$layout <- "overlay-left"
  }
  
  if (is.null(header_list$narrative_background_color_overlay)) {
    header_list$narrative_background_color_overlay <- "darkslategrey"
  }
  
  if (is.null(header_list$narrative_text_color_overlay)) {
    header_list$header_list$narrative_text_color_overlay <- "white"
  }
  
  
  

  header <- paste0('
---
title: ', header_list$title, '\n',
'format:
  closeread-html:
    embed-resources: true
    cr-section:
      layout: "', header_list$layout, '"', '\n',
    'cr-style:
      narrative-background-color-overlay: ', header_list$narrative_background_color_overlay, '\n',
      'narrative-text-color-overlay: ', header_list$header_list$narrative_text_color_overlay, '\n',
'---\n')
  
  
  # return the header text
  header
  
}







