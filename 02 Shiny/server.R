# server.R
require(ggplot2)
require(dplyr)
require(shiny)
require(shinydashboard)
require(data.world)
require(readr)
require(DT)
require(plotly)

# The following query is for joining data
df = query(
  data.world(propsfile = ".data.world"),
  dataset="chriswongwr/s-17-dv-final-project", type="sql",
  query="SELECT *
        FROM US_Companies
        INNER JOIN LargestIndustry ON US_Companies.abbreviation = LargestIndustry.abbreviation
        INNER JOIN Region ON LargestIndustry.state = Region.State"
)  #%>% View()


regions <- df


shinyServer(function(input, output) { 
  # These widgets are for the Crosstabs tab.
  online1 = reactive({input$rb1})
  KPI_Low = reactive({input$KPI1}) 
  KPI_Medium = reactive({input$KPI2})
  taxfilterlo = reactive({input$tax1})
  taxfilterhi = reactive({input$tax2})
  
# Begin Boxplot tab--------------------------------------------
  
  boxplotdata <- eventReactive(input$click1,{
    query(
      data.world(propsfile = ".data.world"),
      dataset="chriswongwr/s-17-dv-final-project", type="sql",
      query="SELECT *
      FROM US_Companies
      INNER JOIN LargestIndustry ON US_Companies.abbreviation = LargestIndustry.abbreviation
      INNER JOIN Region ON LargestIndustry.state = Region.State"
    ) %>% dplyr::filter(.,income_tax_paid >= input$tax1[1],income_tax_paid <= input$tax1[2])

  })
  
  output$state <-renderUI({
    statelist <- as.data.frame(boxplotdata())$abbreviation
    statelist[51] <- ''
    selectInput("chooseState", "filter single state:", sort(statelist), multiple = FALSE)
  })
  
  output$boxplot1 <- renderPlot({
    if (input$chooseState != ''){
      boxplotdf <- as.data.frame(boxplotdata()) %>% filter(abbreviation == input$chooseState)
      ggplot(boxplotdf) + geom_boxplot(aes(x = abbreviation, y = income_tax_paid),fill = "orange") + theme(axis.text.x=element_text(angle=90, size=10, vjust=0.5))
    }else{
      ggplot(boxplotdata()) + geom_boxplot(aes(x = abbreviation, y = income_tax_paid),fill = "orange") + theme(axis.text.x=element_text(angle=90, size=10, vjust=0.5))
    }
    
    
  })
  
  output$boxplotData1 <- renderDataTable({DT::datatable(boxplotdata(), rownames = FALSE,
                                                        extensions = list(Responsive = TRUE, 
                                                                          FixedHeader = TRUE)
  )
  })
  
# End Boxplot tab----------------------------------------------------
  
# Begin Historm tab--------------------------------------------------
  output$histogram1 <- renderPlot({
    histodf = df
    histodf = mutate(histodf, greaterThanAvgEmp = ifelse(employee > mean(histodf$employee),"Yes","No"))
    ggplot(histodf,aes(income_tax_paid, colour = greaterThanAvgEmp)) + geom_freqpoly()+ coord_trans(y = "sqrt")
  })
# End Historm tab---------------------------------------------------

# Begin scatterplot tab---------------------------------------------
  output$scatterplot1 <- renderPlotly({
    ##make these dynamic
    scatterplotdf = dplyr::filter(df,income_tax_paid >= input$tax2[1],income_tax_paid <= input$tax2[2])
    
    ##make these dynamic
    scatterplotdf = dplyr::filter(scatterplotdf,total_assets >= input$asset1[1]*10^3, total_assets<= input$asset1[2]*10^3)
    
    
    plot_ly(scatterplotdf,x = ~total_assets, y = ~income_tax_paid)
    ##display data table of scatterplotdf here.

  })
  
  
  output$scatterplotData1 <- renderDataTable({
    scatterplotdf = dplyr::filter(df,income_tax_paid >= input$tax2[1],income_tax_paid <= input$tax2[2])
    
    ##make these dynamic
    scatterplotdf = dplyr::filter(scatterplotdf,total_assets >= input$asset1[1]*10^3, total_assets<= input$asset1[2]*10^3)
    
    DT::datatable(scatterplotdf, rownames = FALSE,
                                                        extensions = list(Responsive = TRUE, 
                                                                          FixedHeader = TRUE)
  )
  })
# End scatterplot tab-----------------------------------------------
  
# Beging Crosstab tab-----------------------------------------------
  output$crosstab1 <- renderPlot({
    
    #kpiLo = -20
    #kpiMed = 20
    crosstabdf = mutate(df, returnOnAssets = round(income/total_assets*100,5),kpiROA = ifelse(returnOnAssets < input$KPI1, "Low",ifelse(returnOnAssets < input$KPI2,"Medium","High")))
    crosstabdf = crosstabdf %>% dplyr::group_by(Region,kpiROA) %>% dplyr::summarize(avg_income = round(mean(income),3),avg_ROA = round(mean(returnOnAssets),2))
    positions = c("Low", "Medium","High")
    crosstabplot = ggplot(crosstabdf) + geom_text(aes(x = kpiROA,y = Region, label = avg_income,color = avg_ROA)) + scale_x_discrete(limits = positions) + scale_colour_gradientn(colours=rainbow(4))
    crosstabplot
  })
# End Crosstab tab-------------------------------------------------

  
})
