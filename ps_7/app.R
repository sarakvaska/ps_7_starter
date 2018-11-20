#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(plotly)
library(ggplot2)
library(shiny)
upshot_results <- read_rds("upshot.rds")
upshot_educ <- read_rds("upshot_educ.rds")
upshot_race <- read_rds("upshot_race.rds")
actual_results <- read_rds("actual_results.rds")
together <- bind_rows(upshot_results, actual_results)
# Define UI for application that draws a histogram
options <- c("Dem. Advantage" = "dem_adv_results", "Rep. Advantage" = "rep_adv_results")
ui <- fluidPage(
  radioButtons("dataframe", "Set of Data to Visualize:", 
               choices = c("Upshot Predictions by State", 
                           "Upshot Predictions by Race",
                           "Upshot Predictions by Education",
                           "Upshot vs Actual Results", 
                           "Upshot vs Actual Results Percent Error")),
   # Application title
   titlePanel("Predicted Advantages"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput("x", 
                    label = "X-axis:", #internal label 
                    choices = c(options), #vector of choices for user to pick from 
                    selected = "Dem. Advantage")), 
      
      # Show a plot of the generated distribution
      mainPanel(plotlyOutput("plot"))))

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  output$plot <- renderPlotly({
    if(identical(input$dataframe, "Upshot Predictions by State")) {
      ggplotly(ggplot(data = upshot_results, 
                      aes_string(y = "state", x = input$x, color = "state_district")) + 
                 geom_point() + theme(text = element_text(size = 8), 
                                      axis.text.x = element_text(angle=50, hjust=1)) + 
                 labs(x = names(options[which(options == input$x)]), 
                      y = "State", 
                      color = "state-district"))
    }
    else if(identical(input$dataframe, "Upshot Predictions by Education")) {
      ggplotly(ggplot(data = upshot_educ, 
                      aes_string(y = "educ", x = input$x, color = "state_district")) + 
                 geom_point() + theme(text = element_text(size = 8), 
                                      axis.text.x = element_text(angle=50, hjust=1)) + 
                 labs(x = names(options[which(options == input$x)]), 
                      y = "Education", 
                      color = "state-district"))
    }
    else if(identical(input$dataframe, "Upshot Predictions by Race")) {
      ggplotly(ggplot(data = upshot_race, 
                      aes_string(y = "race_eth", x = input$x, color = "state_district")) + 
                 geom_point() + theme(text = element_text(size = 8), 
                                      axis.text.x = element_text(angle=50, hjust=1)) + 
                 labs(x = names(options[which(options == input$x)]), 
                      y = "Race", 
                      color = "state-district"))
    }
    else if(identical(input$dataframe, "Upshot vs Actual Results")) {
      ggplotly(ggplot(data = together, 
                      aes_string(y = "state", x = input$x, color = "result", 
                                 key = "state_district")) + 
                 geom_point() + theme(text = element_text(size = 8), 
                                      axis.text.x = element_text(angle=50, hjust=1)) + 
                 labs(x = names(options[which(options == input$x)]), 
                      y = "Percentage of Advantage", 
                      color = "Data Results"))
    }
   })
}

# Run the application 
shinyApp(ui, server)

