


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

    if (is.na(sticky_text) || sticky_text == "") {
      sticky_content <- ""
    } else {
      sticky_content <- paste0("| ", gsub("\n", "\n| ", sticky_text))
    }
    
    
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
  
  if (is.null(header_list$title) || header_list$title == "") {
    title_line <- "\n"
  } else {
    title_line <- paste0('\ntitle: ', header_list$title, '\n')
  }
  
  # if (is.null(header_list$layout)) {
  #   header_list$layout <- "overlay-left"
  # }
  

  
header <- paste0('---', title_line,
'format:
  closeread-html:
    embed-resources: true
    cr-section:
      layout: "', header_list$layout, '"', '\n',
'    cr-style:\n',
get_all_header_style_strings(header_list),
'---\n')
  
  
  # return the header text
  header
  
}




get_single_header_style_string <- function(header_string_type, header_list) {
  
  property_val <- eval(parse(text=paste0("header_list$", gsub("-", "_", header_string_type))))
  
  if (is.null(property_val)) {
    return("")
  }
    
  # will not put quotes around color names like "white" but will put quotes 
  # around RGB values like "#e2e2e2"
  if (!(property_val %in% colors())) {
    property_in_quotes <- TRUE
  } else {
    property_in_quotes <- FALSE
  }
  
  if (property_in_quotes) {
    header_string <-   paste0('       ',  header_string_type, ': ',  "'", property_val, "'", "\n")
  } else {
    header_string <-   paste0('       ', header_string_type, ': ',  property_val, "\n")
  }
     
  
  header_string
}



get_all_header_style_strings <- function(header_list) {
  
  # could generate this list automatically if I use better header_list naming conventions
  # e.g., use header$style_narrative_background_color_overlay and then strip off style part of name
  header_string_names <- c("narrative-background-color-overlay", 
                           "narrative-background-color-sidebar", 
                           "narrative-text-color-overlay", 
                           "narrative-text-color-sidebar",
                           "section-background-color",
                           "narrative-font-family", 
                           "poem-font-family", 
                           "narrative-font-size",
                           "narrative-sidebar-width")
  
  all_header_style_strings <- ""
  
  for (curr_header_type in header_string_names) {
    all_header_style_strings <- paste0(all_header_style_strings, 
          get_single_header_style_string(curr_header_type, header_list))
  }
  
  all_header_style_strings
}






# test code...


# narration_df <- data.frame(text = c("Check out our graphic",
#                                     "We can keep referring to it as several triggers scroll by",
#                                     "Here's an image"),
#                            sticky = c("mygraphic", "mygraphic", "mypic"),
#                            options = "")
# 
# 
# 
# stickies_df <- data.frame(name = c("mygraphic", "mypic"),
#                           type = c("RCode", "Image"),
#                           text = c("hist(rexp(150))
#                          hist(rexp(100))",
#                                    "ants.png"))
# 
# 
# header_list <- list()
# header_list$title <- "The title"
# header_list$layout <- "overlay-left"
# header_list$narrative_background_color_overlay <- "darkslategrey"
# header_list$narrative_text_color_overlay <- "white"
# 
# 
# generated_text <- generate_Closeread_Quarto_doc(narration_df, stickies_df, header_list)
# generated_text




