
library(systemfonts)
font_info_df <- system_fonts()
font_names <- sort(font_info_df$name)


set_appearance_panel <- nav_panel(title = "Set Appearance", {
  
  header_UI_elements <- list(
    
    textInput("DocumentTitle", label = "Title"),       # value = "", width = NULL, placeholder = NULL)
    
    colourInput("header_narrative_text_color", "Narrative Text Color", "white"),
    
    selectInput("DocumentLayout", "Layout", choices = c("overlay-left", "overlay-center", "overlay-right",
                                                        "sidebar-left", "sidebar-right")),
    colourInput("header_narrative_background_color", "Narrative Background Color", "darkgreen"),
    colourInput("header_section_background_color", "Section Background Color", "gray"),
    selectInput("header_narrative_font", "Narrative font", choices = font_names, selectize=FALSE, selected = "Helvetica"),
    selectInput("header_poem_font", "Poem font", choices = font_names, selectize=FALSE, selected = "Helvetica"),
    numericInput("header_narrative_font_size", "Narrative font size", 12)
    
  )  # end list
  
  
  fluidRow(
    lapply(
      X = split(header_UI_elements, f = rep(c(1, 2), length.out = length(header_UI_elements))),
      FUN = column, width = 6
    )
  )
  
  
  
}) # end appearance panel

