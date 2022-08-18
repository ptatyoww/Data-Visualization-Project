library("shiny")
#Interface for show the graph using fixed page and each row contain the data that keep in server and we have slider input for user to interact with graph
ui <- fixedPage(titlePanel("Survey Of Butterflies in Melbourne 2017"),
                fixedRow(column(12,verbatimTextOutput("text1")))
                ,
                fixedRow(column(4,),
                         column(8,sliderInput("slider",label = h3("Total number of butterflies"), min= min(avg$Total_butterfly),max = max(avg$Total_butterfly), value = c(0,60),step=1)))
                ,
                fixedRow(column(4,verbatimTextOutput("text2") ),
                         column(8,leafletOutput("mymap"))
                ),
                fixedRow(column(6,plotOutput("vis1")),
                         column(6,plotOutput("vis2"))),
                fixedRow(column(12,verbatimTextOutput("text3")))
)
#server part contain the data that will show on interface              
server <- function(input,output,session){
  
  #output$text1 is the brief description of the context
  output$text1 <- renderText({paste("This project aim to observe the butterfly biodiversity and flower-butterfly interactions in each site around Melbourne City", "between Janruary-March 2017. In the original study, the researcher recorded all the butterflies that they saw when they walked", "around Melbourne areas. There are various types of plants and location in each site around Melbourne. The researcher visits", "each sites several times to collect the butterflies types. This allow them to analyse about the location and the time they could", "find particular butterfly types which alco consider about the weather condition",sep = "\n")})
  
  #output$text2 is the description of the leaflet map   
  output$text2 <- renderText({paste("All of the locations of the survey is","around city of Melbourne  which the","size of radiusrepresent the total","number of butterfliesthat are founded", "in each site.The name of the sites are:",
                                    "Argyle Square",
                                    "Canning/Neil St Reserve",
                                    "Carlton Gardens South",
                                    "Fitzroy-Treasury Gardens",
                                    "Gardiner Reserve",
                                    "GarrardStreet Reserve",
                                    "Lincoln Square",
                                    "Murchinson Square",
                                    "Pleasance Gardens",
                                    "Princes Park",
                                    "Royal Park",
                                    "State Library of Victoria",
                                    "University Square",
                                    "Westgate Park",
                                    "Womens Peace Gardens",sep="\n")})
  
  #this is reactive to change the data value from slide bar inout which interact with user and the value of data will change from the value in slide bar  
  datarange <- reactive({avg[avg$Total_butterfly >= input$slider[1] & avg$Total_butterfly <= input$slider[2],]})
  
  #output$mymap is the spatial graph to show the amount of butterflies in each site by using size of radius to represent the amout of butterflies and the graph will change from the slide bar input
  output$mymap <-renderLeaflet({leaflet(datarange()) %>% addTiles() %>% addCircleMarkers(lng = ~datarange()$avg_lon,lat = ~datarange()$avg_lat ,radius = ~(datarange()$Total_butterfly),color = ~site_color(avg$Site),fillOpacity = 0.7, popup = paste("Site:",datarange()$Site,"<br>","Total butterfly:",datarange()$Total_butterfly))})
  
  #output$vis1 is the graph of top 5 that have the highest number of total butterflies
  output$vis1 <- renderPlot({ggplot(top_,aes(x=Total_butterfly,y=reorder(Site,Total_butterfly),fill = Total_butterfly))+geom_col(width = 0.4)+scale_color_hue()+labs(title = "Visualisation 1",subtitle = "Top 5 sites of total butterflies",x="Total butterflies",y ="Site")})
  
  #output$text3 is the description of visualisation1 and visualisation2
  output$text3 <- renderText({paste("The visualisation 1 represents top 5 sites that have the high number of butterflies are founded. From the graph we can see there","are 2 sites which have the highest number of butterflies are Womens Peace Gardens and, Royal Park. The third is Calton Gardens","South. The fourth is Fitzroy-Treasury Gardens and the last is Westgate ParkAccording to visualisation 2, the graph represent the","timeseries of each observation site. The point in the graph shows us the date that we observed that site and we also plot the","line graph to observe the trend of amount of butterflies",sep = "\n")})
  
  #output$vis2 is the graph that show the number of butterflies that researcher observed in each site and each day
  output$vis2 <- renderPlot({ggplot(each_day,aes(x=Datetime,y=Total_butterfly,colour = Site))+geom_point()+geom_line()+labs(title = "Visualisation 2",subtitle = "The number of amount of butterfiles that are founded in each date for particular site",y="Total butterfiles")})
  
  
}
#call the application
shinyApp(ui, server)
