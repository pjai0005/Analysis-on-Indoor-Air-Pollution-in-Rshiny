library(shiny)
library(DT)
library(tidyverse)
library(plotly)
library(here)
library(shinythemes)

# once you've prepared the data uncomment this line
tidy_fuels <- read_csv(here("data", "cooking.csv"))
# you might want to use highlight_key here

ui <- fluidPage(
  title = "Indoor Air Pollution",
  theme = shinytheme("slate"),
  tabsetPanel(

#tab1
    tabPanel("chart",
      icon = icon("line-chart"),
      fluidRow( tags$br(), tags$br()),
      fluidRow(column(
          6,
          offset = 1,
          # also possible to use plotly here
          selectizeInput("countries", "Select Countries",
            choices = NULL,
            multiple = TRUE
          )
        )),
      fluidRow(column(
          2,
          offset = 1,
          checkboxInput("small_countries",
            "Hide countries < 1 million",
            value = FALSE
          )
        )
      ),
      fluidRow(column(
        2,
        offset = 1,
        checkboxInput("linear_scale",
                      "Linearize x-axis",
                      value = FALSE
        )
      )
      ),
      plotlyOutput("chart", width = "100%"),
      sliderInput("year",
        "Year",
        min = 2000,
        max = 2016,
        value = 2016,
        sep = "",
        width = "100%"
      )
    ),

    #tab1
    tabPanel("table", dataTableOutput("table"), icon = icon("table")),

    #tab1
    tabPanel("about", icon = icon("info-circle"))



  )
)

server <- function(input, output, session) {
  # Define reactive expressions here for filtering data

  # Define outputs here
  output$chart <- renderPlotly({
    ggplotly(ggplot() +
               geom_blank())
  })

  output$table <- renderDataTable({
    NULL
  })
}


runApp(shinyApp(ui, server))
