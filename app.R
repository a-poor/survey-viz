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
  "Value Received" = "Value",
  "Felt Isolated" = "Isolated",
  "Feel Belong" = "Belong",
  "Demeaning Behavior" = "DemeaningBehavior",
  "Standoffish" = "Standoffish",
  "Values All Genders" = "ValueGenders",
  "Intimidating" = "Intimidating",
  "Inclusive" = "Inclusive",
  "Opinion Valued" = "OpValued",
  "Would Recommend" = "Recommend",
  "Likley to Renew" = "Renew",
  "Overall Satisfaction" = "OverallSAT"
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
           plotOutput("plot")
        )
    ),
    
    dataTableOutput('table'),
    
)

getDF <- function (ind, dep) {
  data %>% select(!!ind, !!dep) %>% drop_na()
}

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$table <- renderDataTable(
    getDF(input$ind.var, input$dep.var), 
    options = list(pageLength = 10)
  )
  output$plot <- renderPlot({
    getDF(input$ind.var, input$dep.var) %>% 
      ggplot(aes(x = get(input$dep.var), fill = get(input$ind.var))) + 
      geom_bar(position="dodge")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
