
library(shiny)
library(tidyverse)
library(ggplot2)

data <- read_delim("UAH-lower-troposphere-long.csv.bz2")

# ui
ui <- fluidPage(
  
  # Application title
  titlePanel("PS6 - Looking At Global Temperature Data"),
  
  mainPanel(
    tabsetPanel(
      tabPanel("About", 
               p("This App utilizes satellite temperature data in", em("Celsius"), "from the", strong("UAH"), "for the years 1978 to 2023."), 
               p("The data includes temperatures for various regions (14310 observations of 4 variables), but we will be looking at and comparing data for the", strong("Globe"), strong("Land"), strong("Ocean"), "and", strong("USA"), "."),
               p("The following image shows the dispersion of current global temperatures for January", em("2023"), "."),
               img(src = "https://www.nsstc.uah.edu/climate/2023/JANUARY/202301_Map.png")
      ),
      
      tabPanel("Plot", 
               p("The following plot shows temperature data for the globe, land, ocean, and U.S."),
               sidebarLayout(
                 sidebarPanel(
                        checkboxGroupInput("regions", label = "Pick Region(s)",
                                           choices = c("globe", "globe_land", "globe_ocean", "usa49"),
                                           selected = c("globe")),
                        textOutput("region_text"),
                        
                        selectInput("visuals", label = "Visuals:",
                                    choices = c("Line", "Point", "Both"),
                                    selected = "Both"),
                 ),
                 mainPanel(
                        plotOutput("plot", height = "600px", width = "1200px")),
              
               )
      ),     
      
      tabPanel("Table",
               p("The following table allows you to filter & explore the data set"),
               sidebarLayout(
                 sidebarPanel(
                   sliderInput("year_range", label = "Choose the year range",
                               min = min(data$year),
                               max = max(data$year),
                               value = c(1978, 2023)),
                   textOutput("year_text")
                 ),
                 mainPanel(
                   dataTableOutput("dataTable"),
                  
                 ))))))

# Define server logic
server <- function(input, output) {
  
  output$dataTable <- renderDataTable({
    data %>%
      filter(year >= input$year_range[1],
             year <= input$year_range[2])
    
  })
  
  output$plot <- renderPlot({
    visuals <- input$visuals
    
    data %>%
      filter(region %in% input$regions) %>%
      ggplot(aes(year, temp, group = region, col = factor(region))) +
      ggtitle("Global Temperature Data")+
      scale_color_manual(values = c("globe" = "darkgreen","globe_land" = "darkkhaki","globe_ocean" = "blue","usa49" = "black")) +
      geom_point() -> plot_obj
    
    if (visuals == "Line") {
      plot_obj <- plot_obj + geom_line(color = "grey")
    } else if (visuals == "Point") {
      plot_obj <- plot_obj + geom_point(color = "grey")
    }
    
    plot_obj
    
  })
  
  output$region_text <- renderText({
    paste("You chose: ", paste(input$regions, collapse = ", "))
  })
  
  output$year_text <- renderText({
    paste("You chose:", input$year_range[1], "-", input$year_range[2])
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)


