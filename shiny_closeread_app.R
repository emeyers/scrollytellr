

# include the shiny package
library(shiny)
library(htmltools)
library(bslib)
library(quarto)
library(shinyAce)  # for displaying the Quarto script
library(shinyalert)  # for popup messages
library(DT)  # to show data tables that are editable, etc.
library(colourpicker)

library(systemfonts)
font_info_df <- system_fonts()
font_names <- sort(font_info_df$name)


# code for converting the three data frames below into a Quarto Closeread doc
source("generate_Closeread_Quarto_doc.R")

# Global variables
stickies_df <- data.frame()
narration_df <- data.frame()
header_list <- list()


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


# function to generate code for reordering rows in the narration DT table
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





# 1. The function to create the user interface
ui <- page_fillable( #page_fluid(  # page_navbar(   # fluidPage( 
  
  
  navset_tab(id = "MainMenu",
    
    
    
    ########## Add stickies ##########
    
    nav_panel(title = "Add Sticky", {
      
      list(
        
        layout_columns(
          
          card(
            
            card_header("Add new Sticky"),
            
            p("\n"),
            
            
            # textbox for the name of the sticky
            textInput(inputId = "StickyName", label = "Sticky Name"),    #value, width, placeholder),
            
            
            # dropdown menu for the stick type
            selectInput(inputId = "StickyType", label = "Type", 
                        choices = c("Image", "Text", "R Code")),  # selected, multiple, selectize, width, size) 
            
            
            
            ### condition the type of sticky input area depending on whether it is an image or text/code
            
            
            # sticky that is a textbox or r code 
            conditionalPanel( 
              condition = "input.StickyType == 'Text' || input.StickyType == 'R Code'",
              textAreaInput(
                inputId = "StickyContent",
                label = "Sticky Conent",
                height = "250px",
                placeholder = "Add your sticky content here")
            ),
            
            
            # sticky that is an image, give a file upload
            conditionalPanel( 
              condition = "input.StickyType == 'Image'",
              fileInput("ImageUpload", "Upload an image", accept = "image/*")
            ),
            
            
            # Add the sticky to the list/data frame of stickies
            actionButton(inputId = "AddSticky", label = "Add Sticky") 
            
            
          ),
          
          card(
            
            card_header("Stickes that exist"),
          
            # print a table of stickies that exist
            #tableOutput('ShowStickiesBasic')
            DTOutput('ShowStickiesDT')
          
          )  # end the card
          
        )
      
      
      )}),  # end stickies panel
    
    
    
    
    
    ########## Add narration elements ##########
    
    nav_panel(title = "Add Narration", {
      
      list(
  
        layout_columns(
          
          card(
            
            card_header("Add narration"),
            
            p("\n"),
            
            
            # will need to dynamically generate the list of stickies based on what has been added
            selectInput(inputId = "NarrationSticky", label = "Select Sticky", 
                        choices = c("No stickies created yet")),
            
            
            textAreaInput(
              inputId = "NarrationText",
              label = "Narration Text",
              height = "250px",
              placeholder = "Add your narration text here"),
            
            # Additional sticky selection options
            #textInput(inputId = "NarrationStickyOptions", label = "Sticky Options"),
            
            
            # Add the sticky to the list/data frame of stickies
            actionButton(inputId = "AddNarration", label = "Add Narration") 
            
          ),
          
          card(
            
            card_header("Narrations created"),
            
            p("\n"),
            
            # print a table of stickies that exist
            #tableOutput('ShowNarrations')
            DTOutput('ShowNarrationsDT'),
            actionButton("deleteNarrationRows", "Delete highlighted narrations")
            
            ),
          
          
          card(
            accordion(
              accordion_panel("Narration Options",
                              
                              layout_columns(
                                
                                card("Options",
                                     
                                     # Need to make this conditional depending on the type of sticky
                                     
                                     # For images...
                                     

                                     shinyWidgets::sliderTextInput("NarrationOptionsScale", "Scale",
                                                                   choices=c(0, .25, .5, .75, 1, 1.5, 2, 2.5, seq(3, 10)),
                                                                   selected=1, grid = T),
                                     
                                     sliderInput(inputId = "NarrationOptionsPanHorizontal", 
                                                 label = "Pan horizontal", min = -100, max = 100, value = 0),
                                     
                                     sliderInput(inputId = "NarrationOptionsPanVertical", 
                                                 label = "Pan vertical", min = -100, max = 100, value = 0)
                                     
                                ),
                                
                                card("Preview",
                                     
                                     # Add a generate preview button here...
                                     htmlOutput(outputId = "PreviewOutput")
                                     
                                )
                                
                              ) # end layout_columns
                              
                              

                              
              )
            )
          ),
          col_widths = c(6, 6, 12)
          
          
        )
        
        

      )}),   # end narration panel
    
    
    
    nav_panel(title = "Set Appearance", {
      
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
      
      
      
    }), # end appearance panel
    
    
    nav_panel(title = "Quarto Documenent", {
      
      uiOutput("DisplayQuartoDoc")
      
      # textOutput(outputId = "QuartoText")
      
    }), # end quarto document panel
    
    
    
    nav_panel(title = "HTML output", {
      
      list(
        
        htmlOutput(outputId = "ClosereadOutput")
        
    )}), # end HTML output panel
    
    
    
    # Downloads tab will be added dynamically when narration_df is not an empty data frame
    
    
  )


  
) # closing parenthesis for the UI  






##############################   Server   ##################




# 2. The function to create the server
server <- function(input, output, session) {
  
  
  header_list_reactive <- reactiveVal(header_list)
  

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
  
  
  
  # if AddSticky button is pressed
  observeEvent(input$AddSticky, {
  
    
    # Check that the sticky name doesn't already exist, and is not empty
    
    if (input$StickyName %in% stickies_df$name) {
      shinyalert("A sticky with that name already exists", 
                 "Please pick a unique sticky name", 
                 type = "info")
    }
    validate(
      need(!(input$StickyName %in% stickies_df$name), 
           message = "A sticky with that name already exists. Please pick a unique sticky name.")
    )

    
    if (input$StickyName == "") {
      shinyalert("No sticky name given", 
                 "You must supply a name for the sticky", 
                 type = "info")
    }
    validate(
      need(input$StickyName != "", 
           message = "No sticky name supplied.")
    )
    
    

    # need to do things conditionally depending on whether text or an image...
  
    if (input$StickyType == "Image") {
      
      #input$ImageUpload$name
      
      if(is.null(input$ImageUpload$name)) {  
        # alternatively could use:  if(is.null(input$ImageUpload$ImageUpload$datapath))   
        
        shinyalert("Please upload an image", 
                   "An image must be uploaded prior to adding the sticky", 
                   type = "info")
        
        validate(
          need(!(is.null(input$ImageUpload$name)), 
               message = "An image must be uploaded prior to adding the sticky.")
        )
        
        
      }
      
      curr_text <- input$ImageUpload$datapath
      
  
    } else {  # not an image (i.e., R code or text)
    
      curr_text <- input$StickyContent
      updateTextInput(session, "StickyContent", value = "")   # Clear the text once the stikcy has been added
    
    }
    
    
    curr_sticky_df <- data.frame(name = input$StickyName, 
                                 type = input$StickyType,
                                 text = curr_text)
    

    # Add to global variable stickies_df
    stickies_df <<- rbind(stickies_df, curr_sticky_df)
      
    # update the narration tab so this sticky is avaliable to select
    updateSelectInput(session, "NarrationSticky",
                      label = "Select Sticky",
                      choices = stickies_df$name,
                      selected = stickies_df$name[1]
    )
    
    
    #print(stickies_df)
    
  })
  
  

  # display of the stickies table - just using a simple table
  #output$ShowStickiesBasic <- renderTable({
  #  input$AddSticky # update everytime a sticky is added
  #  stickies_df  # return the stickies_df which should be displayed
  #})
  
  
  # display of the stickies table - using a DataTable which is editable, etc.
  output$ShowStickiesDT <- renderDT({
    
    input$AddSticky # update everytime a sticky is added
    
    datatable(stickies_df, escape = FALSE, selection = "none",
              options = list(dom = 't'))
    
  })
  
  
  
  
  # display of the narration table 
  #output$ShowNarrationsBasic <- renderTable({
  #  input$AddNarration # update every time a sticky is added
  #  narration_df  # return the stickies_df which should be displayed
  #})
  
  
  # display of the stickies table - using a DataTable which is editable, etc.
  # first simple version of this
  #output$ShowNarrationsDT <- renderDT({
  #  input$AddNarration # update every time a sticky is added
  #  datatable(narration_df, escape = FALSE, selection = "none",
  #            options = list(dom = 't'))
  #})
  
  
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
  
  
  
  # additional code to get the row order to rearrange
  proxy <- dataTableProxy("ShowNarrationsDT")
  narration_df_reactive <- reactiveVal(narration_df)
  
  # if AddSticky button is pressed
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
                                    options = get_narration_options())
                                    #options = input$NarrationStickyOptions)
    
    # clear the text once the narration has been added
    updateTextInput(session, "NarrationText", value = "") 
    
    narration_df <<- rbind(narration_df, curr_narration_df)
    
    # update the visual display of the table    
    replaceData(proxy, narration_df, resetPaging = FALSE, rownames = FALSE)
    narration_df_reactive(narration_df)
    
  })
  
  
  
  # for reordering the narration table 
  observeEvent(input$narrationNewOrder, {
    narration_df_original <- narration_df_reactive()
    narration_df_reordered <- narration_df_original[input$narrationNewOrder + 1, ]
    replaceData(proxy, narration_df_reordered, resetPaging = FALSE, rownames = FALSE)
    narration_df_reactive(narration_df_reordered)
    narration_df <<- narration_df_reordered
  })
  
  
  # for deleting rows from the narration table (narration data frame)
  observeEvent(input$deleteNarrationRows,{
    
    if (!is.null(input$ShowNarrationsDT_rows_selected)) {
      
      narration_df <<- narration_df[-as.numeric(input$ShowNarrationsDT_rows_selected),]
      
      # update the visual display of the table    
      replaceData(proxy, narration_df, resetPaging = FALSE, rownames = FALSE)
      narration_df_reactive(narration_df)
    }
    
  })
  
  
  # for when a cell in the narration data table/frame is edited
  observeEvent(input$ShowNarrationsDT_cell_edit, {
    
    row  <- input$ShowNarrationsDT_cell_edit$row
    clmn <- input$ShowNarrationsDT_cell_edit$col + 1  # not sure why I need to add 1 here, but ok
    narration_df[row, clmn] <<- input$ShowNarrationsDT_cell_edit$value

    narration_df_reactive(narration_df)
    
    print(narration_df)
    
  })
  
  
  # helper function to get a string of the narration options
  get_narration_options <- function() {
    
    options_string <- paste0("pan-to=", input$NarrationOptionsPanHorizontal, "%,",
                             input$NarrationOptionsPanVertical, "%",
                             " scale-by=", input$NarrationOptionsScale)
    
    options_string
    
  }
  
  
  
  
  
  
  # Display Quarto text (use either this, or DisplayQuartoDoc but not both)
  # output$QuartoText <- renderText({
  #  create_header_list(input)
  #  quarto_text <- generate_Closeread_Quarto_doc(narration_df, stickies_df, header_list)
  #  quarto_text
  #})
  
  
  # Display Quarto text in a Shiny Ace editor  (use either this, or QuartoText but not both)
  output$DisplayQuartoDoc <- renderUI({ 
    
    
    # if no narrations have been created yet, can't render the Quarto document
    if (nrow(narration_df) == 0) {
      validate(
        need((nrow(narration_df) != 0), 
             message = "\n\nYou need to first add narrations before you can generate a Quarto document")
      )
    }
    
    
    create_header_list(input)

    #quarto_text <- generate_Closeread_Quarto_doc(narration_df, stickies_df, header_list)
    quarto_text <- generate_Closeread_Quarto_doc(narration_df_reactive(), 
                                                 stickies_df, 
                                                 header_list)
    
    header_list_reactive()
    
    ace_editor <- shinyAce::aceEditor("DisplayQuartoDoc",
                                      quarto_text,
                                      mode = "markdown",
                                      readOnly = TRUE)
    
    ace_editor
    
  })
  
  
  
  
  # When HTML tab is entered run this code
  output$ClosereadOutput <- renderUI({
    
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

  

  getClosereadPage <- function(quarto_text, save_page_name = "closeread_doc", width, height) {
    
    writeLines(quarto_text, paste0(save_page_name, ".qmd"))
    
    # Adding a one second delay to make sure the file is down being written to before rendering
    # If this is not included, sometimes the .html rendering file is missing content
    # This is a pretty ugly solution, would be great if there is a better one
    Sys.sleep(1)  
    
    quarto_render(paste0(save_page_name, ".qmd"))
    
    # could perhaps alternatively use addResourcePath() to include _extensions directory
    temp_quarto_dir <- paste(sample(letters, 20, replace = TRUE), collapse = "")
    addResourcePath(temp_quarto_dir, getwd())
    
    closeread_html <- tags$iframe(
      src=paste0(temp_quarto_dir, "/", save_page_name, ".html"),
      width=width, height=height)
    
    closeread_html
    
  }
  
  
  
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
                                       options = get_narration_options())
    
    preview_sticky_df <- subset(stickies_df, name == preview_narration_df$sticky)
  
    quarto_preview <- generate_Closeread_Quarto_doc(preview_narration_df, preview_sticky_df, preview_header_list)
    
    html_output <- getClosereadPage(quarto_preview, "preview", 500, 500)
    
    html_output    
    
  })
  
  
  
  
  
  
  
  
  
    
  # Code to download the Quarto and HTML documents
  
  output$downloadQuarto <- downloadHandler(
    filename = "closeread_doc.qmd",   # can give any name here
    content = function(file) {
      file.copy("closeread_doc.qmd", file)
    }
  )
  
  
  
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
  


  
  
  
  
}   # closing brace for the server function 







# 3. Putting UI and the server together to run
shinyApp(ui = ui, server = server) 















