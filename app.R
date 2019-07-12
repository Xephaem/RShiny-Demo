
library(datasets)
library(dplyr)
library(shiny)
library(shinydashboard)

data(iris)

ui <- dashboardPage(
  
  dashboardHeader(title="Iris Data"),
  
  dashboardSidebar(
    sidebarMenu(
      selectInput("flowerGroup", h4("Iris Species"),
                  choices = c("Setosa", "Versicolor", "Virginica")),
      menuItem("Graphs", tabName = "graphs", icon = icon("chart-bar")),
      menuItem("Statistics", tabName = "statistics", icon = icon("square-root-alt"))
    )
  ),
  
  dashboardBody(
    
    tabItems(
      tabItem(tabName = "graphs",
              fluidRow(
                tabBox(tabPanel(title = "Histogram", status = "primary",
                                solidHeader = T, plotOutput("hist", height = 400)),
                       tabPanel(title = "Boxplot", status = "primary",
                                solidHeader = T, plotOutput("boxplot", height = 400))
                ),
                box(title = "Controls for Histogram Display", status = "warning", solidHeader = T,
                    "Use these controls to display certain iris elements.", br(), br(),
                    radioButtons("flower_part", "Flower Part", choices = c("Sepal", "Petal")),
                    radioButtons("length_width", "Dimension", choices = c("Length", "Width"))
                )
              )
      ),
      tabItem(tabName = "statistics",
              fluidRow(
                tabsetPanel(
                  type = "tab",
                  tabPanel("Data", tableOutput("iris")),
                  tabPanel("Summary", verbatimTextOutput("summ"))
                )
              )
              
      )
    )
  )
)



server <- function(input, output) {
  
  df <- reactive({
    iris[iris$Species == tolower(input$flowerGroup),]
  })
  
  col <- reactive({
    
    df_temp <- c()
    
    if (input$flower_part == "Sepal") {
      if (input$length_width == "Length") {
        df_temp <- df()$Sepal.Length
      } else {
        df_temp <- df()$Sepal.Width
      }
    } else {
      if (input$length_width == "Length") {
        df_temp <- df()$Petal.Length
      } else {
        df_temp <- df()$Petal.Width
      }
    }
    
    return(df_temp)
    
  })
  
  title <- reactive({
    paste(input$flowerGroup, input$flower_part, input$length_width)
  })
  
  output$hist <- renderPlot({
    hist(col(), main = title(), xlab = "Centimeters")
  })
  
  output$boxplot <- renderPlot({
    with(df(), boxplot(col()))
  })
  
  output$iris <- renderTable({
    select(df(), -c("Species"))
  })
  
  output$summ <- renderPrint({
    summary(select(df(), -c("Species")))
  })
  
}

shinyApp(ui, server)
