library(shiny)
library(DT)
library(tidyverse)
library(plotly)
library(here)
library(shinythemes)


# once you've prepared the data uncomment this line
tidy_fuels <- read_csv(here("data", "cooking.csv"))

tidy_fuels$tooltip <-
  glue::glue_data(tidy_fuels,
                  "country: {country}",
                  "\nPopulation: {scales::label_number_auto()(total_population)}",
                  "\nProportion: {scales::percent(cooking, scale = 1, accuracy = 1)}",
                  "\nGDP per capita: {scales::dollar(gdp_per_capita)}")
# you might want to use highlight_key here


# UI


ui <- fluidPage( navbarPage(title = "Indoor Air Pollution",
                            windowTitle ="Indoor Air Pollution",
                            id="tabactive",
  theme = shinytheme("flatly")),
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
                         value = c(2000, 2016),
                         sep = "",
                         width = "100%"
             ) ),

    #tab2
    tabPanel("table", dataTableOutput("table"),
             icon = icon("table"),
             fluidRow(column(6, offset = 5,
               radioButtons("tab", "Variables:",
                                   c("Fuel Comsumption" = "cooking" ,
                                     "GDP Per Capita" ="gdp_per_capita",
                                     "Total Population" = "total_population"),
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
             ) ),

    #tab3
    tabPanel("about", fluidPage(uiOutput("about")),
             icon = icon("question"))
    )

)




# SERVER

server <- function(input, output, session)

{
  # Define reactive expressions here for filtering data

  # Graph

      output$chart <- renderPlotly({

      if (input$small_countries) {
        tidy_fuels <- tidy_fuels %>% filter(total_population >= 1000000)
      }


      if (!is.null(input$countries)){
          tidy_fuels <- tidy_fuels %>% filter(country %in% input$countries)
      }

      if(input$linear_scale){
        plot <- tidy_fuels %>%
          group_by(country, year, continent)%>%
          summarise(gdp_per_capita = mean(gdp_per_capita),
                    cooking = mean(cooking)) %>%
          filter(year >= input$year[1]) %>%
          filter(year <= input$year[2]) %>%
          ggplot(aes( x = gdp_per_capita,
                      y = cooking,
                      color = continent,
                      size = gdp_per_capita) ) +
          geom_point(aes(text = tooltip),
                     alpha=0.4) +
          scale_x_log10() +
          scale_size_continuous(trans = "log10") +
          scale_y_continuous(labels = scales::label_percent(scale = 1)) +
          labs(x = "GDP per capita (int.-$)",
               y = "Access to clean fuels and technologies for cooking",
               color = "")
          #gghighlight::gghighlight(country %in% input$countries)
      }



      else{ plot <- tidy_fuels %>%
        group_by(country, year, continent)%>%
        summarise(gdp_per_capita = mean(gdp_per_capita),
                  cooking = mean(cooking)) %>%
        filter(year >= input$year[1]) %>%
        filter(year <= input$year[2]) %>%
        ggplot(aes( x = gdp_per_capita,
                    y = cooking,
                    color = continent,
                    size = gdp_per_capita) ) +
        geom_point(aes(text = tooltip),
                   alpha=0.4) +
        #scale_x_log10() +
        #scale_size_continuous(trans = "log10") +
        scale_y_continuous(labels = scales::label_percent(scale = 1)) +
        labs(x = "GDP per capita (int.-$)",
             y = "Access to clean fuels and technologies for cooking",
             color = "")
      }

     ggplotly(plot, tooltip = "text") %>%
           config(displaylogo = FALSE, modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d",
                                                                  "zoom2d", "pan2d"))



  })


      # TABLE

  output$table <- renderDataTable({

    if(input$tab == "cooking"){

      tidy_fuels %>%
        filter(year == input$Year[1] | year == input$Year[2]) %>%
        select(-c(code, continent, gdp_per_capita, total_population, tooltip)) %>%
        pivot_wider(names_from = year ,
                    values_from = cooking) %>%
        mutate("Absolute Change %" = .[[3]] - .[[2]],
               "Relative Change %" = (.[[3]] - .[[2]])/.[[2]]*100 ) %>%

       datatable(escape = FALSE, class = 'cell-border stripe',
                  caption = htmltools::tags$caption (style = 'caption-side: top; text-align: center;
                                                              color: black; font-family: Arial;
                                                              font-size: 150% ;', 'Relative Change in Fule Consumption'))%>%
        formatRound("Absolute Change %", digits = 2) %>%
        formatRound("Relative Change %", digits = 2)}



    else if(input$tab == "gdp_per_capita"){
      tidy_fuels %>%
        filter(year == input$Year[1] | year == input$Year[2]) %>%
        select(-c(code, continent, cooking, total_population, tooltip)) %>%
        pivot_wider(names_from = year ,
                    values_from = gdp_per_capita) %>%
        mutate("Absolute Change %" = .[[3]] - .[[2]],
               "Relative Change %" = (.[[3]] - .[[2]])/.[[2]]*100 ) %>%

        datatable(escape = FALSE, class = 'cell-border stripe',
                  caption = htmltools::tags$caption (style = 'caption-side: top; text-align: center;
                                                              color: black; font-family: Arial;
                                                              font-size: 150% ;', 'Realtive Change in GDP per capita'))%>%

        formatRound("Absolute Change %", digits = 2) %>%
        formatRound("Relative Change %", digits = 2)}


    else{
        tidy_fuels %>%
          filter(year == input$Year[1] | year == input$Year[2]) %>%
          select(-c(code, continent, cooking, gdp_per_capita, tooltip)) %>%
          pivot_wider(names_from = year ,
                      values_from = total_population) %>%
        mutate("Absolute Change %" = .[[3]] - .[[2]],
               "Relative Change %" = (.[[3]] - .[[2]])/.[[2]]*100 ) %>%

        datatable(escape = FALSE, class = 'cell-border stripe',
                    caption = htmltools::tags$caption (style = 'caption-side: top; text-align: center;
                                                                  color: black; font-family: Arial;
                                                                  font-size: 150% ;', 'Realtive Change in Total Population')) %>%

        formatRound("Absolute Change %", digits = 2) %>%
        formatRound("Relative Change %", digits = 2)}



  })



  # About


  url <- a("Our world in Data", href="https://ourworldindata.org/grapher/access-to-clean-fuels-for-cooking-vs-gdp-per-capita")

  world <- a("World Bank", href="https://ourworldindata.org/grapher/access-to-clean-fuels-for-cooking-vs-gdp-per-capita")

  output$about <- renderUI({

    tagList(tags$h1("About"),
            tags$hr(),
            tags$h2("Information about the data"),
            tags$br(),
            tags$h3("What are clean fuels?"),
            tags$br(),
            tags$p("Fuels that reduces the level of air pollution and are fully renewable energy options for energy use.
                   I hope, with the help of this ShinyApp, you have learnt important aspects about clean energy comsumtion by each country."),
            tags$p("This is shinyApp, generated by RStudio.
                       The data used for the visualisation is acquired from "), url,
            tags$p("And the original dataset is borrowed from "), world,
            tags$hr(),
            tags$hr(),
            tags$h2("About the author"),
            tags$br(),
            tags$p("I am Prachi, a student at Monash University.I am currently pursuing a Master's degree in Business Analytics (2021 to 2023),
                   and I have graduated in Engineering of Information Technology from India. "),
            tags$hr(),
            tags$hr())




  })





}


runApp(shinyApp(ui, server))
