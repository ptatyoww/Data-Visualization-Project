#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library("tidyverse")
library("ggplot2")
library(scales)
library(rgdal)
library(RColorBrewer)
library(leaflet)
library(ggrepel)
library("sf")
library("plotly")
library(shiny)

dataframe <- read_csv("co2-emission-energy.csv")
print(dataframe)
shp <-readOGR("Aus_state.shp")


m <- leaflet(shp)
bins_kw <- c(0, 5000, 15000, 45000, 150000, 450000, 1000000, 1500000, 3000000)
bins_co <- c(-2,5,10,20,30,40,50,60,70)
pal_kw <- colorBin("PuBu",domain = shp@data$output_2017, bins = bins_kw)
pal_co <- colorBin("Reds",domain = shp@data$co2_2017, bins = bins_co)
tg<- dataframe %>%  select(State,Year,Total_output_kW,`CO2e emissions (tonnes per capita)`)
colnames(tg)[4] <- "Co2_emission"
tg <- tg %>% mutate_at(c("Total_output_kW","Co2_emission"),~(scale(.) %>% as.vector))
tg <- tg %>% gather(key="Data",value = "Value",3:4)
economic_sector <- data.frame("Economic_sector" = c("Energy","Stationary energy","Transport","Agriculture","Fugitive emission","Industrial processes","Waste"),"Percentage" = c(33.6,20.4,17.6,14.6,10.0,6.2,2.7))
gas_type <- data.frame("Gas" = c("Carbon Dioxide","Methane","Nitrous Oxide","Fluorinated Gases"),"Percentage" = c(79,11,7,3))
economic_sector<- economic_sector %>% 
  arrange(desc(Economic_sector)) %>%
  mutate(prop = Percentage / sum(economic_sector$Percentage) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )
gas_type<- gas_type %>% 
  arrange(desc(gas_type)) %>%
  mutate(prop = Percentage / sum(gas_type$Percentage) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )
data_boxplot <- dataframe %>% select(State,Year) %>% filter(Year != 2010)
data_boxplot["output"] <-0
data_boxplot["quantity"] <-0
for (i in data_boxplot$State){
  for (j in data_boxplot$Year){
    data_boxplot$output[(data_boxplot$State==i)&(data_boxplot$Year==j)] = (dataframe$Total_output_kW[(dataframe$State==i)&(dataframe$Year==j)]-dataframe$Total_output_kW[(dataframe$State==i)&(dataframe$Year==(as.numeric(j)-1))]);
    data_boxplot$quantity[(data_boxplot$State==i)&(data_boxplot$Year==j)] = (dataframe$Total_quantity[(dataframe$State==i)&(dataframe$Year==j)]-dataframe$Total_quantity[(dataframe$State==i)&(dataframe$Year==(as.numeric(j)-1))]);
    
  }
}
  
# Define UI for application that draws a histogram (layout use this one for result page)
ui <- navbarPage("",
                 tabPanel("Cause & Solution",headerPanel("Cause of global warming and the total amount of renewable energy"),
                          fluidPage(fluidRow(column(8,plotOutput("pie_chart")),
                                             column(4,radioButtons("greenhouse","Greenhouse Data",c("Economic sector"="eco_sector","Gas type"="gas")),
                                                    verbatimTextOutput("pie_text"))),
                                    fluidRow(br()),
                                    fluidRow(verbatimTextOutput("text1")),
                                    fluidRow(column(4,plotlyOutput("line_graph")),
                                             column(4,radioButtons("renewable","Renewable energy Data",c("Quantity"="quantity","Energy Output"="energy")),
                                                    verbatimTextOutput("line_text"),
                                                    verbatimTextOutput("boxplot_text")),
                                             column(4,plotlyOutput("boxplot"),
                                                    mainPanel(plotOutput("piechart"),position="left")
                                             )))),
                 tabPanel("Result",headerPanel("Analyse the effectiveness of renewable energy toward with Co2 emission"),
                          fluidPage(fluidRow(column(8,verbatimTextOutput("text3")),
                                             column(4,sliderInput("slider",h3("Year"),min=2010,max=2017,value=2010))),
                                    fluidRow(column(6,leafletOutput("co2_map")),
                                             column(6,leafletOutput("kw_map"))),
                                    fluidRow(br()),
                                    fluidRow(verbatimTextOutput("text2")),
                                    fluidRow(plotlyOutput("trend_line")))
                 ))


# Define server logic required to draw a histogram
server <- function(input, output,session) {
  #output$text1 is the brief description of the context
  output$pie_text <- renderText({
    if (input$greenhouse=="eco_sector"){
      paste("This graph visualises the proportion of the","economic sector which produce greenhouse gas",
            "in Australia. As we can see from the graph,",
            "the percentage of burning fossil fuels to","generate electricity is the main part that",
            "makes greenhouse gas. Hence, to reduce the","greenhouse gas emission the government is",
            "encouraging the use of renewable energy in","order to decrease the burning of fossil fuels",
            "to generate electricity by encouraging the", "resident to install and use it in the","household.",sep = "\n")}
    else if (input$greenhouse == "gas"){
      paste("This pie chart shows the proportion of each","gas type that is categorised as a greenhouse",
            "gas. Carbon dioxide is the major greenhouse","gas which takes 79% of the total and follow",
            "by methane 11%. Therefore, the reduction of","greenhouse gas may focus on reducing",
            "of greenhouse gas may focus on reducing the","the production of carbon dioxide should",
            "affect much to help in resolving","the global climate change.",sep="\n")}}
  )
  #output$text1 is the brief description of the context
  output$line_text <- renderText({
    if(input$renewable == "quantity"){
      paste("- The line chart on the left-hand side shows",
            "the cumulative amount of renewable energy",
            "installation each year and each line", 
            "represents state in Australia. From",
            "the graph, we can see that Queensland",
            "has the most number of renewable energy",
            "installations, whereas the least number",
            "of installations is in the North territory.",sep = "\n")}
    else if(input$renewable == "energy"){
      paste("- This line graph shows the cumulative",
            "amount of energy that generate from ",
            "renewable energy installed in the household.",
            "Each line also represents each state of",
            "Australia. Queensland also produces the ",
            "most energy compared with other states.", 
            "Besides, if we compare output energy with",
            "the quantity we will see that Western",
            "Australia has a number quantity higher",
            "than South Australia in 2015 but in the",
            "case of output energy, it gets higher in",
            "the year 2017. Thus to compare the",
            "effectiveness of renewable energy we may",
            "use output instead of quantity."
            ,sep="\n")}
  })
  output$boxplot_text <- renderText({
    if(input$renewable == "quantity"){
      paste("- The boxplot on the right-hand side shows",
            "the average number of renewable energy",
            "installations each year as same as the",
            "line chart Queensland is also the one",
            "that has the highest number of renewable",
            "energy","increases each year.",sep = "\n")}
    else if(input$renewable == "energy"){
      paste("- The boxplot on the right-hand side show",
            "average energy output is increasing each",
            " year by each state. For the energy output,",
            "that generated from Queensland is also", 
            "greater than the other states.",sep="\n")}
  })
  
  output$text1 <- renderText({paste("Since the data from the pie chart show us that the economic sector that produces greenhouse gas the most is generating electricity and Carbon dioxide is",
    "the major gas that make global climate change. Thus the strategy of the government to reduce greenhouse gas is using renewable energy so the line graph", 
    "below presents the increase in renewable energy usage for each year in each state including the boxplot that depicts the average renewable energy", "increase in each year.",sep="\n")})
  output$text2 <- renderText({
    paste("The choropleth map on the left-hand side (red colour map) represents the amount of co2 emission in each state by using a shade of colour to represent the",
          "amount of co2 emission and the map on the right-hand side (blue colour) represents the amount of energy output that generate from renewable energy for",
          "each state.  We can see the improvement in the reduction of co2 emission in some states by using a slider bar to change the year from 2010 to 2017 and",
          "see the changes in colour shade which bright shade represents the less amount in both maps and the dark shade will be the high amount. The state that",
          "shows a lot of improvement in co2 reduction from 2010 to 2017 is Tasmania and the least one is the North territory. However, Tasmania is not the state",
          "that has the highest number in generate energy output by renewable energy so we plot the line graph to see the trend between co2 emission and renewable",
          "energy output of each state.",
          "From the line graph below, we can see that the line graph of renewable energy output has a negative correlation with co2 emission but in some states",
          "seem different from others such as North territory. Furthermore, Queensland has the highest number of renewable energy output hence co2 emission is not",
          "reduced as much compare with Tasmania so we may need to analyse other factors that may have much more effect on the co2 emission.",sep="\n")})
  output$text3 <- renderText({paste("After we introduce the cause and the solution to reduce greenhouse gas, on this page we will focus",
                                    "on the result of the increase in renewable energy. This page will include the map to present the",
                                    "changes in both Co2 emissions and the renewable energy output and support with the line graph",
                                    "to clearly see the trend throughout all year.",sep="\n")})
  
  datarange <- reactive({input$slider-2010})
  output$co2_map <- renderLeaflet({m %>% setView(133.7751, -25.2744, 3)  %>% addPolygons(fillColor = ~pal_co(unlist(shp@data[25+datarange()])),weight = 2,opacity = 0.5,color = "black",dashArray = "3",fillOpacity = 0.7, label = sprintf("<strong>State: </strong>%s<br/><strong>Co2 emission:</strong>%g<br/><strong>Year:</strong>%g",shp@data$STE_NAM,unlist(shp@data[25+datarange()]),datarange()+2010)%>% lapply(htmltools::HTML),popup = paste("State:",shp@data$STE_NAM,"<br>","Co2 emission:",unlist(shp@data[25+datarange()]),"<br>","Year: ",datarange()+2010),labelOptions = labelOptions(direction = "auto",textsize = "12px"),highlightOptions = highlightOptions(weight = 5,color = "black",bringToFront = TRUE)) %>% addLegend(pal = pal_co, values = (unlist(shp@data[25+datarange()])),opacity = 0.5,title = "Co2 emission",position = "topright")})
  output$kw_map <- renderLeaflet({m %>% setView(133.7751, -25.2744, 3)  %>% addPolygons(fillColor = ~pal_kw(unlist(shp@data[9+datarange()])),weight = 2,opacity = 0.5,color = "black",dashArray = "3",fillOpacity = 0.7,label = sprintf("<strong>State: </strong>%s<br/><strong>Energy output:</strong>%g<br/><strong>Year: </strong>%g",shp@data$STE_NAM,unlist(shp@data[9+datarange()]),datarange()+2010)%>% lapply(htmltools::HTML),popup = paste("State:",shp@data$STE_NAM,"<br>","Energy output:",unlist(shp@data[9+datarange()]),"<br>","Year: ",datarange()+2010),labelOptions = labelOptions(direction = "auto",textsize = "12px"),highlightOptions = highlightOptions(weight = 5,color = "black",bringToFront = TRUE)) %>% addLegend(pal = pal_kw, values = (unlist(shp@data[9+datarange()])),opacity = 0.5,title = "Renewable energy output",position = "topright")})
  output$trend_line <- renderPlotly({ggplotly(ggplot(tg,aes(x=Year,y=Value,colour = Data, group = Data))+geom_point()+geom_line()+facet_wrap(~State,nrow=2)+labs(title = "Co2 emission and Energy output",subtitle = "Trend of co2 emission and renewable energy output")+theme_bw()+theme(plot.title =element_text(size = 17) ,axis.text.x = element_text(angle = 90),strip.text.x = element_text(size = 9))+ylab("Scale"))})
  
  
  
  output$pie_chart <- renderPlot({
    if (input$greenhouse=="eco_sector"){
      ggplot(economic_sector,aes("",Percentage,fill=Economic_sector))+geom_bar(stat = "identity",color = "white")+coord_polar(theta='y',start = 0)+theme_void()+theme(plot.title = element_text(size = 20),plot.caption.position = "plot")+geom_label_repel(data = economic_sector,
                                                                                                                                                                                                                             aes(y = ypos, label = paste0(Percentage, "%")),
                                                                                                                                                                                                                             size = 4.5, nudge_x = 1, show.legend = FALSE)+guides(fill=guide_legend(title = "Economic sector"))+scale_fill_brewer(palette = "Set1")+labs(title= "Greenhouse gas emission by Economic",caption = "Source: https://www.csiro.au/")
    }
    else if (input$greenhouse == "gas"){
      ggplot(gas_type,aes("",Percentage,fill=Gas))+geom_bar(stat = "identity",color = "white")+coord_polar(theta='y',start = 0)+theme_void()+theme(plot.title = element_text(size = 20),plot.caption.position = "plot")+geom_label_repel(data = gas_type,
                                                                                                                                                                                                          aes(y = ypos, label = paste0(Percentage, "%")),
                                                                                                                                                                                                          size = 4.5, nudge_x = 1, show.legend = FALSE)+guides(fill=guide_legend(title = "Gas type"))+scale_fill_brewer(palette = "Set1")+labs(title= "Greenhouse gas emission by gas type",caption = "Source: https://www.epa.gov")
    }})
  output$boxplot <- renderPlotly({
    if (input$renewable == "energy"){
      ggplotly(ggplot(data_boxplot,aes(State,output,fill=State))+geom_boxplot()+scale_fill_brewer(palette="Accent")+theme_bw()+theme(plot.title = element_text(size = 10),axis.text.x = element_text(angle = 90),strip.text.x = element_text(size = 8))+labs(title = "Average output energy", subtitle="average output increasing")+ylab("output kW"))
    }
    else if (input$renewable == "quantity"){
      ggplotly(ggplot(data_boxplot,aes(State,quantity,fill=State))+geom_boxplot()+scale_fill_brewer(palette="Paired")+theme_bw()+theme(plot.title = element_text(size = 10),axis.text.x = element_text(angle = 90),strip.text.x = element_text(size = 8))+labs(title = "Average amount installation",subtitle="average installation increasing")+ylab("Quantity"))
    }})
  output$line_graph <- renderPlotly({
    if (input$renewable == "energy"){
      ggplotly(ggplot(dataframe,aes(x=Year,y=Total_output_kW,colour = State, group = State))+geom_point()+geom_line()+theme_bw()+theme(plot.title = element_text(size = 10),axis.text.x = element_text(angle = 90),strip.text.x = element_text(size = 7))+labs(title = "Electricity output",subtitle = "Energy output from 2010 to 2017")+scale_y_continuous()+ylab("Total output(kW)"))
    }
    else if (input$renewable == "quantity"){
      ggplotly(ggplot(dataframe,aes(x=Year,y=Total_quantity,colour = State, group = State))+geom_point()+geom_line()+theme_bw()+theme(plot.title = element_text(size = 10),axis.text.x = element_text(angle = 90),strip.text.x = element_text(size = 7))+labs(title = "Total renewable energy installation",subtitle = "Total renewable energy installation from 2010 to 2017")+scale_y_continuous()+ylab("Total quantity"))
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
