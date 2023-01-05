#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(shiny)

# Load data---------------------------------------------------------------------

tech_obs <- readRDS("TechObservations2022.rds") %>%
  arrange(Observer)

# UI----------------------------------------------------------------------------
ui <- fluidPage(
  
  fluidRow(
    
    br(), 
    column(4,  
           HTML("<div style='height: 75px;'>"),
           imageOutput("logo"),
           HTML("</div>")),
    column(8, h1("2022 IMBCR Field Technician Observations"))
    
  ),
  
  br(),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput("observer", label = "Select Observer ID", 
                  choices = unique(tech_obs$Observer)),
      br(),
      textOutput("photo_caption"),
      br(),
      imageOutput("photo"),
    ),
    
    
    mainPanel(
      
      p("This year, 102 IMBCR field techs counted 264,813 individual birds of 394 
        species! Your contribution is essential for this long-term monitoring 
        program, which helps us understand how bird populations respond to 
        management action, climate change, and habitat loss. To read some examples
        of IMBCR data in action, ",
          a("click here.",
            href = "https://www.birdconservancy.org/imbcr2018/")),
      
      h3(textOutput("full_table_title")),
      br(),
      dataTableOutput("full_table"),
      br(),
      downloadButton("download", "Download my data"),
      br(),
      br(),
      "Questions? Contact jessie.reese@birdconservancy.org",
      
    )
    
  )
  
)

# Server------------------------------------------------------------------------

server <- function(input, output, session) {
  
  output$logo <- renderImage({
    
    list(
      src = file.path("IMBCR Horizontal PNG.png"),
      width = 300,
      height = 75)
  }, deleteFile = FALSE)
  
  
  output$photo <- renderImage({
    
    top_bird <- tech_obs %>%
      filter(Observer == input$observer) %>%
      slice_max(`Number Observed by Tech` , n = 1) %>%
      pull(BirdCode) %>%
      as.character()
    
    list(
      src = file.path("BirdCodePhotos", paste0(top_bird, ".jpg"))
    )
  }, deleteFile = FALSE)
  
  
  output$photo_caption <- renderText({
    
    top_bird_name <- tech_obs %>%
      filter(Observer == input$observer) %>%
      slice_max(`Number Observed by Tech`, n = 1) %>%
      pull(Species) %>%
      as.character()
    
    paste0("Your top observed bird was ", top_bird_name, "!")
  }
  )
  
  output$full_table_title <- renderText(
    
    paste("Total Observations for ", input$observer, sep = "")
  )
  
  
  output$full_table <- renderDataTable(
    
    tech_obs %>%
      filter(Observer == input$observer), 
    options = list(pageLength = 10, order = c(2, 'desc'))
    
  )
  
  output$download <- downloadHandler(
    
    filename = function() {
      paste0("2022IMBCRObservations", input$observer, ".csv")
    },
    content = function(file){
      write.csv(tech_obs %>%
                  filter(Observer == input$observer),
                file)
    }
  )
  
} 

shinyApp(ui, server)
