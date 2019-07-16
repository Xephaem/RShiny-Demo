
library(datasets)
library(dplyr)
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)

data(iris)



ui <- dashboardPage(
  
  skin = "green",
  
  dashboardHeader(title="Iris Data"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("home")),
      selectInput("flowerGroup", h4("Iris Species"),
                  choices = c("Setosa", "Versicolor", "Virginica")),
      menuItem("Graphs", tabName = "graphs", icon = icon("chart-bar")),
      menuItem("Data", tabName = "statistics", icon = icon("square-root-alt"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("num_of_species"),
                valueBoxOutput("num_of_records")
              ),
              fluidRow(
                box(title = "Iris Scatter Plot", solidHeader = T, status = "primary",
                    plotOutput("scatter")),
                box(title = "Controls for Plot Display", status = "warning", solidHeader = T,
                    "Use these controls to select iris elements to compare.", br(), br(),
                    selectInput("scatX", h4("x-axis"),
                                choices = c("Sepal Length" = "Sepal.Length",
                                            "Sepal Width" = "Sepal.Width",
                                            "Petal Length" = "Petal.Length",
                                            "Petal Width" = "Petal.Width")),
                    selectInput("scatY", h4("y-axis"),
                                choices = c("Sepal Length" = "Sepal.Length",
                                            "Sepal Width" = "Sepal.Width",
                                            "Petal Length" = "Petal.Length",
                                            "Petal Width" = "Petal.Width")))
              )
      ),
      tabItem(tabName = "graphs",
              fluidRow(
                tabBox(tabPanel(title = "Histogram", status = "primary",
                                solidHeader = T, plotOutput("hist", height = 400)),
                       tabPanel(title = "Boxplot", status = "primary",
                                solidHeader = T, plotOutput("boxplot", height = 400))
                ),
                box(title = "Controls for Graph Display", status = "warning", solidHeader = T,
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
                  tabPanel("Data", dataTableOutput("iris")),
                  tabPanel("Summary", verbatimTextOutput("summ"))
                )
              )
      )
    )
  )
)



server <- function(input, output) {
  
  # data with specified Species from sidemenu
  df <- reactive({
    iris[iris$Species == tolower(input$flowerGroup),]
  })
  
  # make histogram + boxplot radiobutton selectors reactive
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
  
  # reactive title for histogram
  title <- reactive({
    paste(input$flowerGroup, input$flower_part, input$length_width)
  })
  
  # count unique values of Species in iris
  output$num_of_species <- renderValueBox({
    iris %>%
      summarise(n_distinct(Species)) %>%
      valueBox(subtitle = "Number of Species", color = "aqua")
  })
  
  # count number of observations
  output$num_of_records <- renderValueBox({
    iris %>%
      nrow() %>%
      valueBox(subtitle = "Number of Records in Data", color = "blue")
  })
  
  # scatter plot for Overview, with selective elements to set as x/y-axis
  output$scatter <- renderPlot({
    ggplot(iris, aes(x = get(input$scatX), y = get(input$scatY), color = Species)) +
      geom_point(size = 3) +
      labs(x = input$scatX, y = input$scatY)
  })
  
  # histogram output
  output$hist <- renderPlot({
    hist(col(), main = title(), xlab = "Centimeters")
  })
  
  # boxplot output
  output$boxplot <- renderPlot({
    with(df(), boxplot(col(), ylab = "Centimeters"))
  })
  
  # data table output for specific Species, with Species column dropped
  output$iris <- renderDataTable({
    select(df(), -c("Species"))
  })
  
  # summary of specific Species, with Species column dropped
  output$summ <- renderPrint({
    summary(select(df(), -c("Species")))
  })
  
}



shinyApp(ui, server)
