# server.R
require(ggplot2)
require(dplyr)
require(shiny)
require(shinydashboard)
require(data.world)
require(readr)
require(DT)
require(plotly)
require(leaflet)
library(geojson)
library(geojsonio)

url <- "http://leafletjs.com/examples/choropleth/us-states.js"
doc <- readLines(url)
doc2 <- gsub("var statesData = ", "", doc)
write(doc2, file = "us-states.json")


# The following query is for joining data
df <- query(
  data.world(propsfile = ".data.world"),
  dataset="chriswongwr/s-17-dv-final-project", type="sql",
  query="SELECT *
        FROM US_Companies
        INNER JOIN LargestIndustry ON US_Companies.abbreviation = LargestIndustry.abbreviation
        INNER JOIN Region ON LargestIndustry.state = Region.State"
)  #%>% View()



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
  
  output$boxplotData1 <- renderDataTable({DT::datatable(boxplotdata(), rownames = FALSE,
                                                        extensions = list(Responsive = TRUE, 
                                                                          FixedHeader = TRUE)
  )
  })
  
  output$state <-renderUI({
    statelist <- as.data.frame(boxplotdata())$abbreviation
    statelist[51] <- 'ALL'
    selectInput("chooseState", "filter single state:", sort(statelist), multiple = FALSE)
  })
  
  
  output$boxplot1 <- renderPlotly({
    if (input$chooseState == 'ALL'){
      p <- ggplot(boxplotdata()) + geom_boxplot(aes(x = abbreviation, y = income_tax_paid),fill = "orange") + theme(axis.text.x=element_text(angle=90, size=10, vjust=0.5))
      ggplotly(p)
    }else{
      boxplotdf <- as.data.frame(boxplotdata()) %>% filter(abbreviation == input$chooseState)
      p <- ggplot(boxplotdf) + geom_boxplot(aes(x = abbreviation, y = income_tax_paid),fill = "orange") + theme(axis.text.x=element_text(angle=90, size=10, vjust=0.5))
      ggplotly(p)
    }
    
  })
  output$region <-renderUI({
    regionlist <- as.data.frame(boxplotdata())$Region
    regionlist[6] <- 'ALL'
    selectInput("chooseRegion", "filter single region:", sort(regionlist), multiple = FALSE)
  })
  
  output$boxplot2 <- renderPlotly({
    if(input$chooseRegion == 'ALL'){
      p <- ggplot(boxplotdata()) + geom_boxplot(aes(x = Region, y = income_tax_paid),fill = "orange") + theme(axis.text.x=element_text(angle=90, size=10, vjust=0.5))
      ggplotly(p)
    }else{
      boxplotdf <- as.data.frame(boxplotdata()) %>% filter(Region == input$chooseRegion)
      p <- ggplot(boxplotdf) + geom_boxplot(aes(x = Region, y = income_tax_paid),fill = "orange") + theme(axis.text.x=element_text(angle=90, size=10, vjust=0.5))
      ggplotly(p)
    }
  })
  

  output$map1 <- renderLeaflet({
    cidf <- df %>% group_by(state) %>% summarise(mean = mean(income)) %>% arrange(state)#%>%View()
    states <- geojson_read("us-states.json", what = "sp") #%>% View()
    pal <- colorNumeric("YlGnBu", domain = cidf$mean)
     labels <- sprintf("<br/>%g Average Income", 
       cidf$mean) %>% lapply(htmltools::HTML)
   
    leaflet(states) %>%
       setView(lng = -96, lat = 37.8, zoom = 4) %>% 
       addTiles() %>%
       addPolygons(
         fillColor = ~pal(cidf$mean),
         weight = 2,
         opacity = 1,
         color = "white",
         dashArray = "3",
         fillOpacity = 0.7,
         label = labels,
         highlight = highlightOptions(
           weight = 5,
           color = "#666",
           dashArray = "",
           fillOpacity = 0.7,
           bringToFront = TRUE),
         labelOptions = labelOptions(
           style = list("font-weight" = "normal", padding = "3px 8px"),
           textsize = "15px",
           direction = "auto")) %>%
       addLegend(pal = pal, values = ~cidf$mean, opacity = 0.6, title = "2016 Ave_Income", position = "bottomright")
    
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
  
  scatterplotdata <-  eventReactive(input$click3,{
      query(
        data.world(propsfile = ".data.world"),
        dataset="chriswongwr/s-17-dv-final-project", type="sql",
        query="SELECT *
        FROM US_Companies
        INNER JOIN LargestIndustry ON US_Companies.abbreviation = LargestIndustry.abbreviation
        INNER JOIN Region ON LargestIndustry.state = Region.State"
      ) %>% dplyr::filter(.,income_tax_paid >= input$tax2[1],income_tax_paid <= input$tax2[2]) %>% dplyr::filter(.,total_assets >= input$asset1[1]*10^3, total_assets<= input$asset1[2]*10^3)
      
    })
  output$scatterplot1 <- renderPlotly({
    
    
    plot_ly(scatterplotdata(),x = ~total_assets, y = ~income_tax_paid)
    ##display data table of scatterplotdf here.

  })
  
  
  output$scatterplotData1 <- renderDataTable({
    
    DT::datatable(scatterplotdata(), rownames = FALSE,
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

# Begin Barchart tab-----------------------------------------------
  barchartdata <- eventReactive(input$click5,{
    if (input$rb1 == "Y"){
      bardf <- query(
        data.world(propsfile = ".data.world"),
        dataset="chriswongwr/s-17-dv-final-project", type="sql",
        query="SELECT *
        FROM US_Companies
        INNER JOIN LargestIndustry ON US_Companies.abbreviation = LargestIndustry.abbreviation
        INNER JOIN Region ON LargestIndustry.state = Region.State") %>% mutate(.,has_state_income_tax = ifelse(abbreviation %in% c("TX","FL","AL","WA","WY","NV","SD"),"No","Yes"))
     df_Tax = dplyr::filter(bardf,has_state_income_tax == "Yes")%>% select(employee,abbreviation)
     df_Tax = df_Tax %>%  group_by(abbreviation) %>% summarise(avg_emp = mean(employee))
     df_Tax
     
    }else{
      bardf <- query(
        data.world(propsfile = ".data.world"),
        dataset="chriswongwr/s-17-dv-final-project", type="sql",
        query="SELECT *
        FROM US_Companies
        INNER JOIN LargestIndustry ON US_Companies.abbreviation = LargestIndustry.abbreviation
        INNER JOIN Region ON LargestIndustry.state = Region.State") %>%mutate(.,has_state_income_tax = ifelse(abbreviation %in% c("TX","FL","AL","WA","WY","NV","SD"),"No","Yes"))
      df_noTax = dplyr::filter(bardf,has_state_income_tax == "No") %>% select(employee,abbreviation)
      df_noTax = df_noTax %>% group_by(abbreviation) %>% summarise(avg_emp = mean(employee))
      df_noTax
    }
    
  })
  
  output$barchartData1 <- renderDataTable({DT::datatable(barchartdata(), rownames = FALSE,
                                                        extensions = list(Responsive = TRUE, 
                                                                          FixedHeader = TRUE))
                                                        })
  output$barchart1 <- renderPlot({
    if(input$rb1 == "Y"){
      ggplot(barchartdata(), aes(x = abbreviation,y=  avg_emp)) + geom_bar(stat = "identity",fill = "blue") + geom_hline(yintercept = mean(barchartdata()$avg_emp)) + geom_text(aes(12,mean(barchartdata()$avg_emp)+10, label = paste("Average: ", round(mean(barchartdata()$avg_emp),2)),vjust = -2, fontface = "bold"))
      }else{
      ggplot(barchartdata(), aes(x = abbreviation,y=  avg_emp)) + geom_bar(stat = "identity",fill = "blue") + geom_hline(yintercept = mean(barchartdata()$avg_emp)) + geom_text(aes(3,mean(barchartdata()$avg_emp)+1, label = paste("Average: ", round(mean(barchartdata()$avg_emp),2)),vjust = -2, fontface = "bold"))
    }
  })
  
# End Barchart tab-------------------------------------------------
  
})
