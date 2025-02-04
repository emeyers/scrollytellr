

add_stickies_panel <- nav_panel(title = "Add Sticky", {
  
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
          condition = "input.StickyType == 'Text'",
          textAreaInput(
            inputId = "StickyTextContent",
            label = "Sticky Text",
            height = "250px",
            placeholder = "Add your sticky text here")
        ),
        
        
        conditionalPanel( 
          condition = "input.StickyType == 'R Code'",
          textAreaInput(
            inputId = "StickyRContent",
            label = "Sticky Code",
            height = "200px",
            placeholder = "Add your sticky code here"),
          checkboxInput("StickyShowCode", "Show code")
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
    
    
  )})  # end stickies panel



