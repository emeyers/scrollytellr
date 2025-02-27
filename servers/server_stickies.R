

# Function called when the AddSticky button is pressed

add_sticky_button_pressed <- function(input, output, session) {
  
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
        
      } # end checking that an image was uploaded
      
      
      # 1. Create a directory (if it doesn't exist) to store images.
      image_dir <- "images"  #file.path(tempdir(), "images") # Use tempdir() for portability
      if (!dir.exists(image_dir)) {
        dir.create(image_dir)
      }
      
      # 2. Copy the uploaded image to the image directory
      image_path_server <- input$ImageUpload$datapath
      new_image_path <- file.path(image_dir, input$ImageUpload$name) # Use original filename
      file.copy(image_path_server, new_image_path, overwrite = TRUE)
      
      #addResourcePath("images", new_image_path)
      
      #curr_text <- file.path("images", input$ImageUpload$name)  #input$ImageUpload$datapath
      curr_text <- new_image_path
      
      
    } else if (input$StickyType == "Text") { 
      
      curr_text <- input$StickyTextContent
      updateTextInput(session, "StickyTextContent", value = "")   # Clear the text once the stikcy has been added
  
    } else if (input$StickyType == "R Code") {
      
      curr_text <- input$StickyRContent
      
      # Make sure the R code is valid
      tryCatch({
        #parse(text = curr_text)
        eval(parse(text = curr_text))  # a little more dangerous, but will catch things like trying to use a library that isn't installed
      }, error = function(e) {
        error_message = gsub("<text>:2:0: ", "", e$message)
        shinyalert("Invalid R code", 
                   paste("The R code you entered is invalid. Please check it and try again.\n\n\nError Message:\n", error_message), 
                   type = "warning")
        validate(
          need(FALSE, 
               message = "The R code you entered is not valid. Please check it and try again.")
        )
      })
      
      updateTextInput(session, "StickyRContent", value = "")   # Clear the text once the sticky has been added
      
    }
    
    
    # if the showcode checkbox is clicked for R code, add options to show the code
    if (input$StickyShowCode) {
      sticky_options <- "echo = TRUE"  #"showcode"
    } else {
      sticky_options <- ""
    }
    updateCheckboxInput(session, "StickyOptions", value = FALSE)   # Clear the text once the stikcy has been added
    
    
    
    curr_sticky_df <- data.frame(name = input$StickyName, 
                                 type = input$StickyType,
                                 text = curr_text,
                                 options = sticky_options)
    
    
    # Add to global variable stickies_df
    stickies_df <<- rbind(stickies_df, curr_sticky_df)
    stickies_df_reactive <- stickies_df_reactive(stickies_df)
    
    # update the narration tab so this sticky is avaliable to select
    updateSelectInput(session, "NarrationSticky",
                      label = "Select Sticky",
                      choices = stickies_df$name,
                      selected = stickies_df$name[1]
    )
    
    
    
  })  
  
  
} # end code for AddSticky button being pressed function







# Display the stickies table - using a DataTable which is editable, etc.

show_stickies_dt <- function(input, output, session) {
  
  output$ShowStickiesDT <- renderDT({
    
    input$AddSticky # update everytime a sticky is added
    
    datatable(stickies_df, escape = FALSE, selection = "none",
              options = list(dom = 't'))
    
  })
  
}















