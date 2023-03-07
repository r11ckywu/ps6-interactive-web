library(shiny)
library(tidyverse)
library(dplyr)


temperature <- read_delim("UAH-lower-troposphere-long.csv.bz2")

ui <- fluidPage(
  tabsetPanel(
  
    # About Page
    tabPanel("About",
             titlePanel("About"), 
             mainPanel(
               p(HTML(paste0("This app uses satellite temperature data from ", 
                             "<strong>UAH</strong><br><br><br>",
                             "Temperature is measured as deviation (deg C) from 1978-2023 baseline.<br><br>",
                             "The dataset contains ", nrow(temperature), " observations and ", ncol(temperature), " variables.<br>",
                             "Here is a small (random) sample of data:"))),
               tableOutput("sample") 
             )
    ),
    
    # Plot Page
    tabPanel("Plot",
             sidebarLayout(
               sidebarPanel(
                sliderInput("year", label = h3("Year Range"), min = 1978, max = 2023, value = c(1990, 2010)),
                p("You can analyze the global temperature for different regions. Select the regions you are interested in. 
                  You see a line graph of mean temperature by year."),
                checkboxGroupInput("region", 
                                   "Select regions:",
                                   choices = unique(temperature$region), 
                                   selected = "globe")),
               mainPanel(
                 plotOutput("tempplot"),
                 textOutput("year_range")
               )
             )
    ),
    
    # Table Page
    tabPanel("Table",
        sidebarPanel(
          width = 15,
          sliderInput("n", "Sample Size:",
                      min = 1,
                      max = nrow(temperature),
                      value = 5000),
          selectInput("month", label = h5("Month"), 
                      choices = sort(unique(temperature$month)),
                      selected = 1),
          textOutput("max_temp")
          ),
        fluidRow(
          column(width = 4),
          column(width = 8,
                 mainPanel(
                   tableOutput("table")
                 )          ),
          column(width = 4)
        )
        
   )
  
  )
)


server <- function(input, output) {
  # About Materials
  output$sample <- renderTable({
    temperature %>% 
      sample_n(5)
  })
 
  
  # Plot Materials
  output$range <- renderPrint({input$year})
  output$region <- renderPrint({input$region})
  output$tempplot <- renderPlot({
    cat("new plot\n")
    temperature %>%
      filter(year >= input$year[1] & year <= input$year[2]) %>%
      filter(region %in% input$region) %>% 
      group_by(year) %>%
      summarize(mean_temp = mean(temp)) %>% 
      ggplot(aes(year, mean_temp)) +
      geom_line()
})
  output$year_range <- renderText({
    temperature1 <- temperature %>%
      filter(year >= input$year[1] & year <= input$year[2]) %>%
      filter(region %in% input$region)
    obs_count <- nrow(temperature1)
      
      # Combine the text strings and the computed values
    paste("Year range: ", input$year[1], "-", input$year[2],
          ", Observations: ", obs_count)
  })
  
  # Table Materials
  output$max_temp <- renderText({
    temperature2 <- reactive({ 
      temperature %>% 
      sample_n(input$n) %>% 
      filter(month == input$month)
    })
    output$max_temp <- renderText({
      max_temp <- max(temperature2()$temp)
      min_temp <- min(temperature2()$temp)
      paste("Maximum Temperature: ", max_temp, ", ", 
            "Minimum Temperature: ", min_temp)
    })
  })
  output$table <- renderTable({
    temperature %>%
      sample_n(input$n) %>% 
      filter(month == input$month) %>% 
      arrange(desc(year))
  })
 
     
      
 
    

}
shinyApp(ui = ui, server = server)
