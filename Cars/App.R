library(shiny)
library(tibble)
library(magrittr)
library(gt)

# autoreload on file change
options(shiny.autoreload = TRUE)

# the error is because i had created the column already 
# during the analysis

# we can make the table better using the gt library

# lets add images.
# the names are saved based on the car brand name
# let us validate the images

# we will get a list of the available images in the www folder,
# if the selected brand is available in that list, we will 
# print it out, otherwise we will print an error 

# let us style the errors with css

# thanks for watching

mtcars <- mtcars %>%
  rownames_to_column(var="brand")

ui <- navbarPage(
  header = (
    tags$link(rel="stylesheet", type="text/css", href="styles.css")
  ),
  collapsible = TRUE, # responsive navigation
  title="MTCARS ANALYSIS",
  # tabs
  tabPanel("Dashboard",
           # app layout
           sidebarLayout(
             # sidebar
             sidebarPanel(
               # car brand input
               selectInput("brand", "Brand:", choices = mtcars$brand)
             ),
             # main
             mainPanel(
               # display image
               imageOutput("pic"),
               # table
               gt_output("table")
             )
           )
           
    )
)

server <- function(input, output, session) {
  # let us get the data and filter
  data <- reactive({
    mtcars %>%
      dplyr::filter(brand == input$brand)
  })
  
  # lets display the data
  output$table <- render_gt({
    data() %>%
      gt() %>%
      tab_header(
        title = md("**`mtcars`**"),
        subtitle = md("Motor Trend Car Road Tests")
      ) %>%
      tab_footnote(
        footnote = md("*Data Source*: data sets library")
      )
  })
  
  # read and display images
  Image <- reactive({
    validate(
      need(
        paste0(input$brand, ".png") %in% list.files(paste0(getwd(), "/www")),
        paste0(input$brand, ".png not found")
      )
    )
    list(
      src = file.path("www/",paste0(input$brand, ".png")),
      contentType = "image/png",
      width = "100%",
      height = "100%",
      alt=input$brand
    )
  })
  
  # lets render the images
  output$pic <- renderImage({
    Image()
  }, deleteFile = FALSE)
}

shinyApp(ui, server)