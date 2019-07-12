
library(datasets)
library(dplyr)
library(shiny)
library(shinydashboard)

data(iris)

ui <- dashboardPage(
  
  dashboardHeader(title="Iris Data"),
  
  dashboardSidebar(
    sidebarMenu(
      selectInput("flowerGroup", h4("Flower"),
                  choices = c("Setosa", "Versicolor", "Virginica")),
      menuItem("Histogram", tabName = "histogram", icon = icon("chart-bar")),
      menuItem("Statistics", tabName = "statistics", icon = icon("square-root-alt"))
    )
  ),
  
  dashboardBody(
    
    tabItems(
      tabItem(tabName = "histogram",
              fluidRow(
                box(plotOutput("hist", height = 400)),
                box(title = "Controls",
                    radioButtons("flower_part", "Flower Part", choices = c("Sepal", "Petal")),
                    radioButtons("length_width", "Side of Part", choices = c("Length", "Width"))
                )
              )
      ),
      tabItem(tabName = "statistics",
              fluidRow(
                box(title = "TO BE ADDED")
              )
      )
    )
  )
)



server <- function(input, output) {
  
  df <- reactive({
    
    df_temp <- c()
    
    if (input$flower_part == "Sepal") {
      if (input$length_width == "Length") {
        df_temp <- iris$Sepal.Length[iris$Species == tolower(input$flowerGroup)]
      } else {
        df_temp <- iris$Sepal.Width[iris$Species == tolower(input$flowerGroup)]
      }
    } else {
      if (input$length_width == "Length") {
        df_temp <- iris$Petal.Length[iris$Species == tolower(input$flowerGroup)]
      } else {
        df_temp <- iris$Petal.Width[iris$Species == tolower(input$flowerGroup)]
      }
    }
    
    return(df_temp)
    
  })
  
  title <- reactive({
    paste("Histogram of", input$flowerGroup, input$flower_part, input$length_width)
  })
  
  
  
  output$hist <- renderPlot({
    hist(df(), main = title(), xlab = "Centimeters")
  })
  
}

shinyApp(ui, server)
