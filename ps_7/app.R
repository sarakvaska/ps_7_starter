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
upshot_age <- read_rds("upshot_age_percent.rds")
upshot_age_adv <- read_rds("upshot_age_adv.rds")
age_together <- left_join(upshot_age, upshot_age_adv)

upshot_educ <- read_rds("upshot_educ_percent.rds")
upshot_educ_adv <- read_rds("upshot_educ_adv.rds")
educ <- left_join(upshot_educ, upshot_educ_adv)

upshot_race <- read_rds("upshot_race_percent.rds")
upshot_race_adv <- read_rds("upshot_race_adv.rds")
race_together <- left_join(upshot_race, upshot_race_adv)

actual_results <- read_rds("actual.rds")
upshot_results <- read_rds("upshot.rds")
upshot_results_states <- upshot_results %>%
  group_by(state, district)
actual_results_states <- actual_results %>%
  group_by(state, district)
upshot_error <- bind_rows(actual_results_states, upshot_results_states)
# together <- bind_rows(upshot_results, actual_results)
# Define UI for application that draws a histogram
options <- c("Dem. Advantage Percentage" = "dem_adv_results", "Rep. Advantage Percentage" = "rep_adv_results")
ui <- fluidPage(
  radioButtons("dataframe", "Set of Data to Visualize:", 
               choices = c("Upshot Predictions by Age", 
                           "Upshot Predictions by Education", 
                           "Upshot Predictions by Race", 
                           "Upshot Predictions vs Actual Results")),
   # Application title
   titlePanel("Predicted Advantages"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput("x", 
                    label = "X-axis:", #internal label 
                    choices = c(options), #vector of choices for user to pick from 
                    selected = "Dem. Advantage Percentage"), 
        checkboxInput("line", label = "Show Best Fit Line", value = FALSE)),
      
      # Show a plot of the generated distribution
      mainPanel(plotlyOutput("plot"))))

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  output$plot <- renderPlotly({
    if(identical(input$dataframe, "Upshot Predictions by Age")) {
      if(input$line == FALSE) {
        ggplotly(ggplot(data = age_together, 
                        aes_string(y = "age_percentage", x = input$x, color = "state_district", 
                                   key = "ager")) + facet_wrap(~ ager, ncol = 2) +
                   geom_point() + 
                   labs(x = names(options[which(options == input$x)]), 
                        y = "Percentage of Age Group", 
                        color = "state-district"))
      }
      else {
        ggplotly(ggplot(data = age_together, 
                        aes_string(y = "age_percentage", x = input$x, color = "state_district", 
                                   key = "ager")) + facet_wrap(~ ager, ncol = 2) +
                   geom_point() + geom_smooth(aes(group = 1), se = FALSE) +
                   labs(x = names(options[which(options == input$x)]), 
                        y = "Percentage of Age Group", 
                        color = "state-district")) 
      }
    }
    else if(identical(input$dataframe, "Upshot Predictions by Education")) {
      if(input$line == FALSE) {
        ggplotly(ggplot(data = educ,
                        aes_string(y = "educ_percentage", x = input$x, color = "state_district",
                                   key = "educ")) + facet_wrap(~ educ, ncol = 2) +
                   geom_point() +
                   labs(x = names(options[which(options == input$x)]),
                        y = "Percentage of Education Groups",
                        color = "state-district"))
      }
      else {
        ggplotly(ggplot(data = educ,
                        aes_string(y = "educ_percentage", x = input$x, color = "state_district",
                                   key = "educ")) + facet_wrap(~ educ, ncol = 2) +
                   geom_point() + geom_smooth(aes(group = 1), se = FALSE) +
                   labs(x = names(options[which(options == input$x)]),
                        y = "Percentage of Education Groups",
                        color = "state-district"))
      }
    }
    else if(identical(input$dataframe, "Upshot Predictions by Race")) {
      if(input$line == FALSE) {
        ggplotly(ggplot(data = race_together,
                        aes_string(y = "race_percentage", x = input$x, color = "state_district",
                                   key = "race_eth")) + facet_wrap(~ race_eth, ncol = 2) +
                   geom_point() + 
                   labs(x = names(options[which(options == input$x)]),
                        y = "Percentage of Racial/Ethnic Groups",
                        color = "state-district"))
      }
      else {
        ggplotly(ggplot(data = race_together,
                        aes_string(y = "race_percentage", x = input$x, color = "state_district",
                                   key = "race_eth")) + facet_wrap(~ race_eth, ncol = 2) +
                   geom_point() + geom_smooth(aes(group = 1), se = FALSE) +
                   labs(x = names(options[which(options == input$x)]),
                        y = "Percentage of Racial/Ethnic Groups",
                        color = "state-district"))
      }
    }
    else {
      if(input$line == FALSE) {
        ggplotly(ggplot(data = upshot_error,
                        aes_string(y = "state", x = input$x, color = "result", 
                                   key = "state_district")) +
                   geom_point() + 
                   labs(x = names(options[which(options == input$x)]),
                        y = "State",
                        color = "Upshot Predicted vs Actual Results"))
      }
      else {
        ggplotly(ggplot(data = upshot_error,
                        aes_string(y = "state", x = input$x, color = "result", 
                                   key = "state_district")) +
                   geom_point() + geom_smooth(aes(group = 1), se = FALSE) +
                   labs(x = names(options[which(options == input$x)]),
                        y = "State",
                        color = "Upshot Predicted vs Actual Results"))
      }
    }
  })
}

# Run the application 
shinyApp(ui, server)

