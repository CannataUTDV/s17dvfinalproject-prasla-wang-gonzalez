#ui.R
require(shiny)
require(shinydashboard)
require(DT)
require(plotly)
require(leaflet)

dashboardPage(
  dashboardHeader(title = "U.S. Companies"
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction",tabName = "Introduction", icon= icon("home")),
      menuItem("Boxplot", tabName = "boxplot", icon = icon("archive")),
      menuItem("Histogram", tabName = "histogram", icon = icon("signal")),
      menuItem("Scatterplot", tabName = "scatterplot", icon = icon("line-chart")),
      menuItem("Crosstab",tabName = "crosstab", icon = icon("th")),
      menuItem("Barchart",tabName = "barchart",icon=icon("bar-chart")),
      menuItem("Map vs. Income", tabName = "map", icon=icon("map"))
    )
  ),
  dashboardBody(    
    tabItems(
      # Begin content.
      tabItem(tabName = "Introduction", 
              h3("Analysis on Company Data"),
              p("Using publicly traded corprate data, we tried to find meaninful insights on companies.")),
      tabItem(tabName = "boxplot",
        tabsetPanel(
            tabPanel("data", 
                     sliderInput("tax1","income tax paid range:",
                                 min = 0, max = 10000, value = c(500,4000)),
                     hr(),
                     actionButton(inputId = "click1", label = "Get Data"),
                     DT::dataTableOutput("boxplotData1")
                    
          ),
          tabPanel("Boxplot by state",
                   uiOutput("state"),
                   hr(),
                   plotlyOutput("boxplot1", height = 500)),
          tabPanel("Boxplot by Region",
                   uiOutput("region"),
                   hr(),
                   plotlyOutput("boxplot2", height = 500))
        )),
      tabItem(tabName = "histogram",
        tabsetPanel(
          tabPanel("Histogram", 
                   plotOutput("histogram1", height=500)))
      ),
      tabItem(tabName = "scatterplot",
              tabsetPanel(
                tabPanel("data",
                         sliderInput("tax2","income tax paid range:",
                                     min = 0, max = 10440, value = c(500,4000)),
                         sliderInput("asset1","total assests range:",
                                     min = 0, max = 4575, value = c(500,2000)),
                         actionButton(inputId = "click3", label = "Get Data"),
                         hr(),
                         DT::dataTableOutput("scatterplotData1")),
                tabPanel("Scatterplot",
                         plotlyOutput("scatterplot1",height=500))) 
              ),
      tabItem(tabName = "crosstab",
              tabsetPanel(
                tabPanel("crosstab",
                         sliderInput("KPI1","KPI low:",
                                     min= -30, max = 10, value = -20),
                         sliderInput("KPI2","KPI Medium:",
                                     min=10, max= 30, value = 20),
                         hr(),
                       plotOutput("crosstab1"))
                       )
              ),
      tabItem(tabName = "barchart",
              tabsetPanel(
                tabPanel("Data",
                         radioButtons("rb1", "tax",
                                      c("With income tax" = "Y", 
                                        "Without income tax"= "N")),
                         actionButton(inputId = "click5", label = "Get Data"),
                         DT::dataTableOutput("barchartData1")),
                tabPanel("Barchart",
                         plotOutput("barchart1", height = 500)
                         )
              )),
      tabItem(tabName = "map",
              tabsetPanel(
                tabPanel("Map",
                         leafletOutput("map1"), height = 1000)
              ))
    )
  ), skin = 'yellow'
)

