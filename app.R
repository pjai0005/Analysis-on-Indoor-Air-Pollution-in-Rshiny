library(shiny)
library(DT)
library(tidyverse)
library(plotly)
library(here)
library(shinythemes)

# once you've prepared the data uncomment this line
tidy_fuels <- read_csv(here("data", "cooking.csv"))
# you might want to use highlight_key here


# UI


ui <- fluidPage(
  title = "Indoor Air Pollution",
  theme = shinytheme("united"),
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
               )  )  ),


             fluidRow(column(
               2,
               offset = 1,
               checkboxInput("small_countries",
                             "Hide countries < 1 million",
                             value = FALSE
               )  )  ),


             fluidRow(column(
               2,
               offset = 1,
               checkboxInput("linear_scale",
                             "Linearize x-axis",
                             value = FALSE
               )  )  ),
             plotlyOutput("chart", width = "100%"),
             sliderInput("year",
                         "Year",
                         min = 2000,
                         max = 2016,
                         value = 2016,
                         sep = "",
                         width = "100%"
             ) ),

    #tab2
    tabPanel("table", dataTableOutput("table"),
             icon = icon("table"),
             fluidRow( offset = 1, tags$br(), tags$br(),
                       tags$h1("Data Table"),
                       tags$br(), tags$br()),
             fluidRow(column(
               2,
               offset = 1,
               radioButtons("tab", "Variables:",
                                   c("Fuel Comsumption" = "cooking" ,
                                     "GDP Per Capita" ="gdp_per_capita"),
                                   selected = "cooking"
             ) ) ),

             fluidRow( tags$br(), tags$br()),
             sliderInput("Year",
                         "Year",
                         min = 2000,
                         max = 2016,
                         value = c(2000, 2016),
                         width = "90%",
                         sep = ""
             )),

    #tab3
    tabPanel("about", textOutput("about"),
             icon = icon("info-circle"))



  )
)




# SERVER

server <- function(input, output, session) {
  # Define reactive expressions here for filtering data

  # Define outputs here
  output$chart <- renderPlotly({
    ggplotly(ggplot() +
               geom_blank())
  })


  output$table <- renderDataTable({

    if(input$tab == "cooking"){

      tidy_fuels %>%
        filter(year == input$Year[1] | year == input$Year[2]) %>%
        select(-c(code, continent, gdp_per_capita, total_population)) %>%
        pivot_wider(names_from = year ,
                    values_from = cooking) %>%
        mutate(Relative = (.[[3]] - .[[2]])/.[[2]]*100 ) %>%

        #select(country, total_population, input$Year[1], input$Year[2], Relative) %>%
        datatable(escape = FALSE,
                  caption = htmltools::tags$caption (style = 'caption-side: top; text-align: center;
                                                              color: black; font-family: Arial;
                                                              font-size: 150% ;', 'Relative Change in Fule Consumption')) %>%
        formatRound('Relative', digits = 2) }



    else{
      tidy_fuels %>%
        filter(year == input$Year[1] | year == input$Year[2]) %>%
        select(-c(code, continent, cooking, total_population)) %>%
        pivot_wider(names_from = year ,
                    values_from = gdp_per_capita) %>%
        mutate(Relative = (.[[3]] - .[[2]])/.[[2]]*100 ) %>%

        #select(country, total_population, input$Year[1], input$Year[2], Relative)%>%
        datatable(escape = FALSE,
                  caption = htmltools::tags$caption (style = 'caption-side: top; text-align: center;
                                                              color: black; font-family: Arial;
                                                              font-size: 150% ;', 'Realtive Change in GDP per capita')) %>%
        formatRound('Relative', digits = 2) }


  })



  output$about <- renderText({
    NULL
  })





}


runApp(shinyApp(ui, server))
