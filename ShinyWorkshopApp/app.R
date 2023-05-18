library(shiny)
library(tidyverse)
library(nycflights13)
library(bslib)

data <- flights %>% filter(month == 1)
min_dist <- min(data$distance, na.rm = TRUE)
max_dist <- max(data$distance, na.rm = TRUE)

ui <- fluidPage(
  theme = bs_theme(bootswatch = "darkly"),
  
  titlePanel("NYC Flights Dashboard"),
  
  sidebarLayout(
    
    sidebarPanel(
      checkboxGroupInput("NY_airport", "Airports:",
                          choices = c("EWR", "LGA", "JFK"),
                          selected = c("EWR", "LGA", "JFK")),
      
      checkboxInput("include_delayed", "Include Delayed Flights",
                    value = TRUE),
      
      dateRangeInput("date_range", "Date Range:", 
                     start = "2013-01-01", end = "2013-01-31",
                     min = "2013-01-01", max = "2013-01-31"),
      
      sliderInput("distance", "Flight Distance:",
                  min = min_dist, max = max_dist,
                  value = c(min_dist, max_dist))
    ),
    
    mainPanel(
      h3("Data from January of 2013"),
      
      plotOutput("flights_per_day"),
      plotOutput("top_dest"),

      tableOutput("most_delayed")
    )
  )
)

server <- function(input, output) {
  filtered_data <- reactive({
    filtering <- data %>% filter(origin %in% input$NY_airport)
    if (input$include_delayed) {}
    else {
      filtering <- filtering %>% filter(dep_delay <= 0)
    }
    filtering <- filtering %>% filter(day >= as.integer(str_sub(input$date_range[1], 9, 10)),
                                      day <= as.integer(str_sub(input$date_range[2], 9, 10)))
    filtering <- filtering %>% filter(distance >= input$distance[1],
                                      distance <= input$distance[2])
    
    filtering
  })
  
  output$flights_per_day <- renderPlot({
    filtered_data() %>% select(day, origin) %>%
      group_by(day, origin) %>%
      summarize(total_flights = n()) %>%
      ggplot(aes(x = day, y = total_flights, group = origin, color = origin)) +
      geom_line() +
      labs(x = "Day in January", y = "Number of Flights", title = "Flights by Day and Origin")
  })
  
  output$top_dest <- renderPlot({
    filtered_data() %>% select(dest) %>%
      group_by(dest) %>%
      summarize(num_flights = n()) %>%
      slice_max(num_flights, n = 10) %>%
      arrange(desc(num_flights)) %>%
      ggplot(aes(x = fct_reorder(dest, num_flights, .desc = TRUE), y = num_flights)) +
      geom_col() +
      labs(x = "Destination", y = "Number of Flights", title = "Most Popular Destinations")
  })
  
  output$most_delayed <- renderTable({
    filtered_data() %>% slice_max(dep_delay, n = 10) %>%
      arrange(desc(dep_delay)) %>%
      select(day, dep_time, sched_dep_time, dep_delay, flight, dest, distance)
  })
}

shinyApp(ui = ui, server = server)
