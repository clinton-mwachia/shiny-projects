library(shiny)
library(tidyverse)
library(lubridate)
library(nycflights13) # contains the data
library(plotly) # make plots interactive

options(shiny.autoreload = TRUE)

# we will use server side selectize because we have too many choices to
# select from.

# i will copy the summaries from the analysis files to save time, 
# these files are in the repo.

# to get the weather data, we will combine flights data with the weather data.

# for this video we will drop na, though you can handle them as you wish

# thanks for watching

ui <- navbarPage(
  header = (
    tags$link(rel="stylesheet", type="text/css", href="styles.css")
  ),
  collapsible = TRUE,
  title = h5("FMS"),
  # the header layout tabs
  ## start dashboard ---------------------------------------->
  tabPanel("Dashboard", icon = icon("gauge"),
    # dashboard layout
    ## header
    h3("Flight Monitoring System"),
    ## tabs
    tabsetPanel(
      # full data
      tabPanel("Data", dataTableOutput("allFlights")),
      # delay by flight
      tabPanel("Delay by Flight", 
               # data panel layout
               sidebarLayout(
                 # sidebar
                 sidebarPanel(
                   # SELECT TAIL NUMBERS
                   selectizeInput("tailnum", "Tail Num:", choices=NULL)
                 ),
                 # main panel
                 mainPanel(
                   plotlyOutput("flight_tailnum")
                 )
               )),
    )
           
           
           ),
  ## end dashboard ---------------------------------------->
  ## start weather ---------------------------------------->
  tabPanel("Weather", icon = icon("cloud"), 
           # heading
           div(
             tags$h3("Case Study: How does weather influence delays?")
           ),
    tabsetPanel(
      tabPanel("Data",
               dataTableOutput("allweatherFlight")),
      tabPanel("Humidity", 
               fluidRow(
                 column(6, plotlyOutput("humid_dep")),
                 column(6, plotlyOutput("humid_arr"))
               )
               ),
      tabPanel("Temperature", 
               fluidRow(
                 column(6, plotlyOutput("temp_dep")),
                 column(6, plotlyOutput("temp_arr"))
               )
               ),
      tabPanel("Dew", 
               fluidRow(
                 column(6, plotlyOutput("dew_dep")),
                 column(6, plotlyOutput("dew_arr"))
               )
               )
    )
           
           ),
  ## end weather ---------------------------------------->
  ## start plane ---------------------------------------->
  tabPanel("Plane", icon = icon("plane"), 
           tabsetPanel(
             tabPanel("Data",
                      dataTableOutput("allplaneFlight")),
             tabPanel("Manufacturer",
                      # layout
                      sidebarLayout(
                        sidebarPanel(
                          selectizeInput("manu", "Manufacturer:", choices=NULL)
                        ),
                        mainPanel(
                          fluidRow(
                            column(10, plotlyOutput("pf_manu")),
                            column(10, dataTableOutput("pf_table"))
                        )
                      )),
           ),
           tabPanel("Model",
                    # layout
                    sidebarLayout(
                      sidebarPanel(
                        selectizeInput("model", "Model:", choices=NULL)
                      ),
                      mainPanel(
                        fluidRow(
                          column(10, plotlyOutput("pf_model")),
                          column(10, dataTableOutput("pf_model_table"))
                        )
                      ))
           )
           )
  ## end plane ---------------------------------------->
))

server <- function(input, output, session) {
  ## start flights data----------------------------------->
  flights_data <- reactive({
    flights %>%
      mutate(
        date = make_date(year,month, day)
      ) %>%
      select(-c(1:3,17:18)) %>%
      relocate(date, .before = dep_time) %>% # move date to the beginning
      drop_na()
  })
  # lets display the flights to the user
  output$allFlights <- renderDataTable({
    flights_data()
  })
  
  # lets update the tail number choices
  observeEvent(flights_data(), {
    updateSelectizeInput(session, "tailnum", choices = flights_data()$tailnum,
                         server = TRUE, selected = flights_data()$tailnum[1])
  })
  
  # delays by flight tail number
  output$flight_tailnum <- renderPlotly({
    flights_data() %>%
      filter(tailnum == input$tailnum) %>%
      select(date, dep_delay, arr_delay, tailnum) %>%
      ggplot(aes(x=date)) +
      geom_line(aes(y=dep_delay,colour="dep")) +
      geom_line(aes(y=arr_delay,color="arr")) +
      scale_color_manual(
        name="Delays", values = c("dep" = "blue", "arr" = "red")) +
      ylab("Delay") +
      ggtitle("Arrival/Departure delay Per Plane") +
      theme_bw() 
  })
  ## end flights data----------------------------------->
  
  ## start weather data --------------------------------------->
  weather_flight_data <- reactive({
    weather %>%
      mutate(
        date = make_date(year,month, day)
      ) %>%
      select(-c(2:5)) %>%
      relocate(date, .before = origin) %>%
      inner_join(flights_data()) %>%
      select(-c(wind_gust,time_hour,sched_arr_time, sched_dep_time,arr_time, 
                dep_time,distance)) %>%
      drop_na()
  })
  
  # lets display the data
  output$allweatherFlight <- renderDataTable({
    weather_flight_data()
  })
  
  ## humidity data
  weather_flight_data_humid <- reactive({
    weather_flight_data() %>% 
      sample_n(10000) %>%
      select(
        humid, dep_delay, arr_delay
      )
      
  })
  
  # departure delay plot
  output$humid_dep <- renderPlotly({
    weather_flight_data_humid() %>%
      ggplot(aes(x=humid)) +
      geom_smooth(aes(y=dep_delay),se=FALSE) +
      ggtitle("EFFECTS OF humidity ON DEP DELAY") +
      theme_bw()
  })
  
  # arrival delay plot
  output$humid_arr <- renderPlotly({
    weather_flight_data_humid() %>%
      ggplot(aes(x=humid)) +
      geom_smooth(aes(y=arr_delay),se=FALSE) +
      ggtitle("EFFECTS OF humidity ON ARR DELAY") +
      theme_bw()
  })
  
  ## temperature data
  weather_flight_data_temp <- reactive({
    weather_flight_data() %>% 
      sample_n(10000) %>%
      select(
        temp, dep_delay, arr_delay
      )
    
  })
  
  # departure delay plot
  output$temp_dep <- renderPlotly({
    weather_flight_data_temp() %>%
      ggplot(aes(x=temp)) +
      geom_smooth(aes(y=dep_delay),se=FALSE) +
      ggtitle("EFFECTS OF htemp ON DEP DELAY") +
      theme_bw()
  })
  
  # arrival delay plot
  output$temp_arr <- renderPlotly({
    weather_flight_data_temp() %>%
      ggplot(aes(x=temp)) +
      geom_smooth(aes(y=arr_delay),se=FALSE) +
      ggtitle("EFFECTS OF temp ON ARR DELAY") +
      theme_bw()
  })

  ## dew data
  weather_flight_data_dew <- reactive({
    weather_flight_data() %>% 
      sample_n(10000) %>%
      select(
        dewp, dep_delay, arr_delay
      )
    
  })
  
  # departure delay plot
  output$dew_dep <- renderPlotly({
    weather_flight_data_dew() %>%
      ggplot(aes(x=dewp)) +
      geom_smooth(aes(y=dep_delay),se=FALSE) +
      ggtitle("EFFECTS OF dew ON DEP DELAY") +
      theme_bw()
  })
  
  # arrival delay plot
  output$dew_arr <- renderPlotly({
    weather_flight_data_dew() %>%
      ggplot(aes(x=dewp)) +
      geom_smooth(aes(y=arr_delay),se=FALSE) +
      ggtitle("EFFECTS OF dew ON ARR DELAY") +
      theme_bw()
  })
  ## end weather data --------------------------------------->
  
  ## start plane data ---------------------------------------->
  plane_flight_data <- reactive({
    flights_data() %>% 
      inner_join(planes, "tailnum") %>%
      select(
        date, dep_delay, arr_delay, tailnum, year, manufacturer,
        model, engines, engine, seats
      )
  })
  # display the data
  output$allplaneFlight <- renderDataTable({
    plane_flight_data()
  })
  # the summary data by manufacturer
  plane_flight_manu <- reactive({
    plane_flight_data() %>%
      group_by(manufacturer) %>%
      summarize(
        tot_dep_delay = sum(dep_delay),
        tot_arr_delay = sum(arr_delay)
      )
  })
  
  # update selectize input with manufacturers
  observeEvent(plane_flight_manu(), {
    updateSelectizeInput(session, "manu", 
                         choices = plane_flight_manu()$manufacturer,
                         server=TRUE,
                         selected = plane_flight_manu()$manufacturer[1])
  })
  
  # render the plot
  output$pf_manu <- renderPlotly({
    plane_flight_manu() %>%
      filter(manufacturer == input$manu) %>%
      melt(id="manufacturer", variable.name = "Delay",value.name = "delay") %>%
      ggplot(aes(y=delay, x=manufacturer, fill=Delay)) + 
      geom_bar(position="dodge", stat="identity") +
      ggtitle("dep/arrival delay by manufacturer") +
      theme_bw()
  })
  
  # the table
  output$pf_table <- renderDataTable({
    plane_flight_manu() %>%
      filter(manufacturer == input$manu) %>%
      melt(id="manufacturer", variable.name = "Delay",value.name = "delay")
  })
  
  # the summary data by model
  plane_flight_model <- reactive({
    plane_flight_data() %>%
      group_by(model) %>%
      summarize(
        tot_dep_delay = sum(dep_delay),
        tot_arr_delay = sum(arr_delay)
      )
  })
  
  # update selectize input with manufacturers
  observeEvent(plane_flight_model(), {
    updateSelectizeInput(session, "model", 
                         choices = plane_flight_model()$model,
                         server=TRUE,
                         selected = plane_flight_model()$model[1])
  })
  
  # render the plot
  output$pf_model <- renderPlotly({
    plane_flight_model() %>%
      filter(model == input$model) %>%
      melt(id="model", variable.name = "Delay",value.name = "delay") %>%
      ggplot(aes(y=delay, x=model, fill=Delay)) + 
      geom_bar(position="dodge", stat="identity") +
      ggtitle("dep/arrival delay by model") +
      theme_bw()
  })
  
  # the table
  output$pf_model_table <- renderDataTable({
    plane_flight_model() %>%
      filter(model == input$model) %>%
      melt(id="model", variable.name = "Delay",value.name = "delay")
  })
  ## end plane data ---------------------------------------->
}

shinyApp(ui, server)