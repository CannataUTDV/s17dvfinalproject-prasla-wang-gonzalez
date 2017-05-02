#ui.R
require(shiny)
require(shinydashboard)
require(DT)
require(plotly)

dashboardPage(
  dashboardHeader(title = "U.S. Companies"
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction",tabName = "Introduction", icon= icon("home")),
      menuItem("Boxplot", tabName = "boxplot", icon = icon("archive")),
      menuItem("Histogram", tabName = "histogram", icon = icon("signal")),
      menuItem("Scatterplot", tabName = "scatterplot", icon = icon("line-chart")),
      menuItem("Corsstab",tabName = "crosstab", icon = icon("bar-chart"))
    )
  ),
  dashboardBody(    
    tabItems(
      # Begin content.
      tabItem(tabName = "Introduction", 
              h3("Describtion:"),
              p("blahh.....")),
      tabItem(tabName = "boxplot",
        tabsetPanel(
            tabPanel("data", 
                     sliderInput("tax1","income tax paid range:",
                                 min = 0, max = 10000, value = c(500,4000)),
                     hr(),
                     actionButton(inputId = "click1", label = "Get Data"),
                     DT::dataTableOutput("boxplotData1")
                    
          ),
          tabPanel("Boxplot",
                   uiOutput("state"),
                   hr(),
                   plotOutput("boxplot1", height = 500))
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
              )
    )
  ), skin = 'yellow'
)

