#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(readxl)
library(shiny)

data <- read_excel("data/SurveyData.xlsx") %>% 
  mutate(
    ResponseID = `Response ID`
  ) %>% 
  mutate(
    
  )
meta <- read_excel("data/MBship22Jobs.xlsx")


ind.var.set <- c(
  "Gender" = "Gender",
  "LGBTQ" = "LGBTQ"
)
dep.var.set <- c(
  "Value" = "Value Received",
  "Isolated" = "Felt Isolated",
  "Belong" = "Feel Belong",
  "DemeaningBehavior" = "Demeaning Behavior",
  "Standoffish" = "Standoffish",
  "ValueGenders" = "Values All Genders",
  "Intimidating" = "Intimidating",
  "Inclusive" = "Inclusive",
  "OpValued" = "Opinion Valued",
  "Recommend" = "Would Recommend",
  "Renew" = "Likley to Renew",
  "OverallSAT" = "Overall Satisfaction"
)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Survey Data"),
    
    sidebarLayout(
        sidebarPanel(
          selectInput(
            "ind.var",
            "Independant Variable",
            ind.var.set,
          ),
          
          selectInput(
            "dep.var",
            "Dependant Variable",
            dep.var.set,
          ),
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    ),
    
    dataTableOutput('table'),
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    output$table <- renderDataTable(data, options = list(pageLength = 10))

    output$plot <- renderPlot({
      # generate bins based on input$bins from ui.R
      df <- data %>% 
        select(
          input$ind.var,
          input$dep.var
        ) %>% drop_na()
      
      df %>% ggplot(aes_string(x = input$ind.var, y = input$dep.var)) + geom_bar()
      
      # x    <- data[, 2]
      # bins <- seq(min(x), max(x), length.out = input$bins + 1)
      # 
      # # draw the histogram with the specified number of bins
      # hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
    
    # output$distPlot <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white')
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)
