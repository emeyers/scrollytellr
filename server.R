

# Code for converting the stickies_df, narration_df, and header_list into a Quarto Closeread document
source("generate_Closeread_Quarto_doc.R")


# The sourced server files for the server logic 
source("servers/server_stickies.R")
source("servers/server_narrations.R")
source("servers/server_display_quarto_doc.R")
source("servers/server_display_html_doc.R")
source("servers/server_downloads.R")



server <- function(input, output, session) {
  
  # sourced stickies functions
  add_sticky_button_pressed(input, output, session)
  show_stickies_dt(input, output, session) 
  
  
  # sourced narration functions
  show_narration_dt(input, output, session) 
  add_narrations_button_pressed(input, output, session)
  reorder_narration_dt_rows(input, output, session)
  delete_narration_dt_rows(input, output, session)
  edit_narration_dt_rows(input, output, session)
  select_lines_to_highlight(input, output, session)  
  preview_narration_image(input, output, session)
  
  
  # sourced from server_render_quarto
  display_quarto_doc(input, output, session)
  
  
  # sourced from server_render_html
  display_html_doc(input, output, session)
  
  
  # sourced from server_download_documents
  add_downloads_menu(input, output, session)  # adds a new menu to download quarto/html file when the at least one narration exists
  download_quarto(input, output, session)
  download_html(input, output, session)
  
  

  
}   # closing brace for the server function 





