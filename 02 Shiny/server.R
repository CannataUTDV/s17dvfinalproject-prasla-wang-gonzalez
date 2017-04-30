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
  
  # These widgets are for the first chart tab.
  # online2 = reactive({input$rb2})
  # output$regions2 <- renderUI({selectInput("selectedRegions", "Choose Categories:", region_list, multiple = TRUE) })
  # 
  # df1 <- df %>% dplyr::group_by(sex, school) %>% 
  #   dplyr::summarize(avg_wage = round(mean(wage),3),kpi = sum(wage/school))
  
  output$boxplot1 <- renderPlot({
    ##make these dynamic
    
    boxplotdf = dplyr::filter(df,income_tax_paid >= input$tax1[1],income_tax_paid <= input$tax1[2])
    ggplot(boxplotdf) + geom_boxplot(aes(x = abbreviation, y = income_tax_paid),fill = "orange") + theme(axis.text.x=element_text(angle=90, size=10, vjust=0.5))
  })
  
  output$boxplotData1 <- renderDataTable({
    boxplotdf = dplyr::filter(df,income_tax_paid >= input$tax1[1],income_tax_paid <= input$tax1[2])
    DT::datatable(boxplotdf, rownames = FALSE,
                                                        extensions = list(Responsive = TRUE, 
                                                                          FixedHeader = TRUE)
  )
  })
  
  output$histogram1 <- renderPlot({
    histodf = df
    histodf = mutate(histodf, greaterThanAvgEmp = ifelse(employee > mean(histodf$employee),"Yes","No"))
    ggplot(histodf,aes(income_tax_paid, colour = greaterThanAvgEmp)) + geom_freqpoly()+ coord_trans(y = "sqrt")
  })
  
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
  
  output$crosstab1 <- renderPlot({
    
    #kpiLo = -20
    #kpiMed = 20
    crosstabdf = mutate(df, returnOnAssets = round(income/total_assets*100,5),kpiROA = ifelse(returnOnAssets < input$KPI1, "Low",ifelse(returnOnAssets < input$KPI2,"Medium","High")))
    crosstabdf = crosstabdf %>% dplyr::group_by(Region,kpiROA) %>% dplyr::summarize(avg_income = round(mean(income),3),avg_ROA = round(mean(returnOnAssets),2))
    positions = c("Low", "Medium","High")
    crosstabplot = ggplot(crosstabdf) + geom_text(aes(x = kpiROA,y = Region, label = avg_income,color = avg_ROA)) + scale_x_discrete(limits = positions) + scale_colour_gradientn(colours=rainbow(4))
    crosstabplot
  })
  
  # output$plot2 <- renderPlot({
  #   df3 = df %>% dplyr::group_by(school)  %>% dplyr::summarize(average_wage = mean(wage))
  #   ## No High School Set
  #   
  #   set1 = df3 %>% filter(school <= 8)
  #   set1avg = sum(set1$average_wage)/8
  #   ## High School Set
  #   set2 = df3 %>% filter(school > 8, school <= 12)
  #   set2avg = sum(set2$average_wage)/4
  #   ## College Set
  #   set3 = df3 %>% filter(school > 12)
  #   set3avg = sum(set3$average_wage)/2
  #   
  #   plot2_1 = ggplot(set1,aes(x = school, y = average_wage)) + geom_bar(stat = "identity",fill = "#0033cc", color = "#0033cc") + coord_cartesian(ylim = c(0,1800)) + coord_cartesian(xlim = c(0,15)) + geom_hline(aes(yintercept = set1avg))
  #   plot2_2 = ggplot(set2,aes(x = school, y = average_wage)) + geom_bar(stat = "identity",fill = "#0033cc", color = "#0033cc") + coord_cartesian(ylim = c(0,1800)) + coord_cartesian(xlim = c(0,15))+ geom_hline(aes(yintercept = set2avg))
  #   plot2_3 = ggplot(set3,aes(x = school, y = average_wage)) + geom_bar(stat = "identity",fill = "#0033cc", color = "#0033cc") + coord_cartesian(ylim = c(0,1800)) + coord_cartesian(xlim = c(0,15))+ geom_hline(aes(yintercept = set3avg))
  #   if (online1()=="school"){
  #     plot2_1
  #   }else if(online1()=='coll'){
  #     plot2_2
  #   }else{
  #     plot2_3
  #   }
  # })
  
  # output$plot1 <- renderPlot({
  #   df1 <- df %>% dplyr::filter(., sex == "female") %>%
  #     dplyr::summarize(avg_wage = sum(wage)/16) #%>% View()
  #   df2 <- df %>% dplyr::filter(., sex == "male") %>%
  #     dplyr::summarize(avg_wage = sum(wage)/16) #%>% View()
  #   
  #     df %>% ggplot(.,aes(x = exper, y = wage)) + geom_bar(stat="identity",fill = "Blue") + geom_hline(aes(yintercept = df1$avg_wage)) + facet_grid(sex ~.) 
  #   })
  # 
# End Crosstab Tab ___________________________________________________________
# Begin Barchart Tab ------------------------------------------------------------------
  df2 <- eventReactive(input$click2, {
    if(online2() == "SQL") {
      print("Getting from data.world")
      print(input$selectedRegions)
      tdf = query(
        data.world(propsfile = "www/.data.world"),
        dataset="cannata/superstoreorders", type="sql",
        query="select Category, Region, sum(Sales) sum_sales
                from SuperStoreOrders
                where Region = ? or ? = 'All'
                group by Category, Region",
        queryParameters = list(unlist(input$selectedRegions), unlist(input$selectedRegions))
      ) # %>% View()
    }
    else {
      print("Getting from csv")
      file_path = "www/SuperStoreOrders.csv"
      df <- readr::read_csv(file_path)
      tdf = df %>% dplyr::filter(Region %in% input$selectedRegions | input$selectedRegions == "All") %>%
        dplyr::group_by(Category, Region) %>% 
        dplyr::summarize(sum_sales = sum(Sales)) # %>% View()
    }
   
    tdf2 = tdf %>% group_by(Category) %>% summarize(window_avg_sales = mean(sum_sales))
    dplyr::inner_join(tdf, tdf2, by = "Category")
   
  })
  
  
  
  # output$plot3 <- renderPlot({
  #   df4 <- df %>% dplyr::group_by(state,sex) %>% dplyr::summarize(sum_school = sum(school))
  #   ggplot(df4,aes(x = state,y = sum_school)) + geom_bar(stat = "identity", fill = "#0033cc") +
  #     facet_wrap(~sex) + coord_flip() +
  #     geom_text(mapping=aes(x=state, y=sum_school, label=round(sum_school,digits = 2)),colour="black", hjust=-.5)
  # })
  
  # End Barchart Tab ___________________________________________________________
  
})
