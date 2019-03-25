#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#To Do Items:
#Make Top 5 into graph instead. When clicking on chart, zoom to station. 
#Permit Image/Data Export
#Shapefile Export

library(shiny)
library(leaflet)
library(dplyr)
library(rgdal)
library(RColorBrewer)
library(colorRamps)
library(sf)
library(data.table)
library(plotly)
library(shinycssloaders)

monthStart <- function(x) {
  x <- as.POSIXlt(x)
  x$mday <- 1
  as.Date(x)
}

BikeIcon <- makeIcon(
  iconUrl = "http://www.altairapps.com/images/apps/icon_velov.png",
  iconWidth = 50, iconHeight = 50,
  iconAnchorX = 25, iconAnchorY = 50)

BikeIcon2 <- makeIcon(
  iconUrl = "http://www.altairapps.com/images/apps/icon_cogo.png",
  iconWidth = 50, iconHeight = 50,
  iconAnchorX = 25, iconAnchorY = 50)


# Define UI for application 
ui <- fluidPage(
  #Input Station Drop Down
  selectizeInput("Selectedstation", "Select Station", 
                 stationfinal$Var1, selected = "3000 Connecticut Ave NW / National Zoo", multiple = FALSE,
                 options = NULL),
  
        
  #Insert Slider
  sliderInput("YearMonthRange", "Date of Bikeshare",min(as.Date("2018-01-01")), 
              max(as.Date("2018-12-01")),
              value = c(min(as.Date("2018-01-01")),max(as.Date("2018-12-01"))),
              timeFormat="%b %Y",
              step=1),
  

  #Insert Radio Button - Origin vs. Destination
  radioButtons("Direction", "Review Trips That",
               c("Start At Selected Station" = "OriginTrips",
                 "End At Selected Station" = "DestinationTrips")),
  
  radioButtons("DisplayType", "Display:",
               c("Circles" = "Circles",
                 "Heat Map" = "HeatMap")),  
  
  conditionalPanel("input.DisplayType == 'HeatMap'",
                checkboxInput("DisplayStation", "Display Stations", value = FALSE)),
  
  #Insert Map
  leafletOutput("mymap",width="800px", height="800px") %>% withSpinner(color="#0dc5c1"),

  plotlyOutput("OverTime"),
  
  
  tableOutput("Top5Stations"),
  
  verbatimTextOutput("clickevent"))
  

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  sliderMonthRangeMin <- reactiveValues()
  sliderMonthRangeMax <- reactiveValues()
  
  
  observe({
    full.date.range.min <- as.Date(input$YearMonthRange[1])
    full.date.range.max <- as.Date(input$YearMonthRange[2])
    
    sliderMonthRangeMin$Month <- as.character(monthStart(full.date.range.min))
    sliderMonthRangeMax$Month <- as.character(monthStart(full.date.range.max))
    
    
  })
  
  
  points.all2 <- reactive({if(input$Direction == "OriginTrips"){
      points.all2 <-  Bikeshare_Data_Aggreggated2[ which(Bikeshare_Data_Aggreggated2$Start.station == input$Selectedstation & 
                                                          !is.na(Bikeshare_Data_Aggreggated2$lon.dest)) ,]
      setkey(points.all2,FixedDate)
      return(as.data.table(points.all2))
      
      
    }else{
      points.all2 <-  Bikeshare_Data_Aggreggated2[ which(Bikeshare_Data_Aggreggated2$End.station == input$Selectedstation & 
                                                          !is.na(Bikeshare_Data_Aggreggated2$lon.dest)) ,]
      setkey(points.all2,FixedDate)
      return(as.data.table(points.all2))
  }
    
      })
  
  
  observe({
    if(input$Selectedstation != "3000 Connecticut Ave NW / National Zoo"){
      updateSliderInput(session = session,
                        inputId = "YearMonthRange", 
                        label = "Date of BikeshareNew",
                        min = as.Date(min(points.all2()$FixedDate[points.all2()$Trips > 0])),
                        max = as.Date(max(points.all2()$FixedDate[points.all2()$Trips > 0])),
                        value = c(as.Date(min(points.all2()$FixedDate[points.all2()$Trips > 0])),as.Date(max(points.all2()$FixedDate[points.all2()$Trips >0]))),
                        timeFormat="%b %Y",
                        step=1
      )}
  })
  
  
  points.all <- reactive({
    points.all<-points.all2()[ which(points.all2()$FixedDate >= sliderMonthRangeMin$Month & points.all2()$FixedDate <= sliderMonthRangeMax$Month),]
    points.all$FullName<-paste(points.all$Start.station,points.all$End.station, sep = "|")
    points.all$CumTrips <- ave(points.all$Trips, points.all$FullName, FUN=cumsum)
    points.all$FullName<-NULL
    return(as.data.table(points.all))
    
    
  })
  
  


  
  output$mymap <- renderLeaflet({
    leaflet() %>% addTiles()  %>%  setView(lng = -76.99, lat = 38.93, zoom = 12)
    
  }) 



#prepare point data
#We need to make this a reactive element. To control the order.
  display.points<-  reactive({ 
    display.points<-points.all()[points.all()$CumTrips>0,]
    display.points$Trips<-display.points$CumTrips
      if(input$YearMonthRange[1] != input$YearMonthRange[2]){
        display.points$Radius<-sqrt(display.points$Trips)}
      else {
        display.points$Radius<-display.points$Trips}   
      
      if(input$Direction == "OriginTrips") {
        display.points$lon<-display.points$lon.dest
        display.points$lat<-display.points$lat.dest
        display.points$DisplayLon<-unique(display.points$lon.org)
        display.points$DisplayLat<-unique(display.points$lat.org)
        display.points$Station<-display.points$End.station}
      if(input$Direction == "DestinationTrips") {
        display.points$lon<-display.points$lon.org
        display.points$lat<-display.points$lat.org
        display.points$DisplayLon<-unique(display.points$lon.dest)
        display.points$DisplayLat<-unique(display.points$lat.dest)
        display.points$Station<-display.points$Start.station}
    return(as.data.frame(display.points))
    
})
  
    observe({ 
        if(input$DisplayType == "HeatMap"){
          
          
          
      #prepare heatmap data
    Bikeshare_sf = st_as_sf(display.points(), coords = c("lon", "lat"), crs = 4269, agr = "constant")
    All_States_Joined<-st_join(AllStates,Bikeshare_sf) %>% 
      group_by(GEOID, NAMELSAD) %>% 
      summarise(stations = n(),
                Trips = sum(Trips)) %>%
      mutate(stations = ifelse(is.na(Trips),0,stations))
    
    heatmap_labels<-paste0("<strong>Census Tract: </strong>", 
                           All_States_Joined$NAMELSAD,
                           "<br><strong>Stations in CT: </strong>",
                           All_States_Joined$stations,
                           "<br><strong>Number of Trips: </strong>",
                           All_States_Joined$Trips) %>% lapply(htmltools::HTML)

    isolate(leafletProxy("mymap")  %>% 
              clearMarkers() %>%   
              clearShapes())
    
    leafletProxy("mymap", data = display.points() )  %>%
      flyTo(display.points()$DisplayLon[1],
            display.points()$DisplayLat[1], 
            zoom = 13)  %>% 
 
      addPolygons(data = All_States_Joined, group = "Polygons", 
                  color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5,
                  fillColor = ~colorBin("YlOrRd", Trips)(Trips),
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE),
                  label = heatmap_labels,
                  labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                              textsize = "15px",
                                              direction = "auto")) %>%
      
      
      addMarkers(data = display.points(), 
                 lng = display.points()$DisplayLon,
                 lat = display.points()$DisplayLat, 
                 icon = BikeIcon) 
      }
      if(input$DisplayStation == TRUE){
        leafletProxy("mymap") %>%  addMarkers(data = display.points(), 
                                              group = "Stations",
                                              layerId= display.points()$Station,
                                              lng = display.points()$lon,
                                              lat = display.points()$lat,
                                              icon = BikeIcon2,
                                              options = markerOptions(riseOnHover = TRUE),
                                              popup = paste0("<strong>Station Name: </strong>",
                                                             display.points()$Station,
                                                             "<br><strong>Number of Trips: </strong>",
                                                             display.points()$Trips)) 
        
      }  
        
      if(input$DisplayType == "Circles"){
      #Show Aggregated TRips
CircleColors<-colorQuantile(colorRamp(c("#008000", "#FF0000"), interpolate="spline"), unique(quantile(display.points()$Trips)))

    leafletProxy("mymap", data = display.points() ) %>%
      flyTo(display.points()$DisplayLon[1],
            display.points()$DisplayLat[1],
            zoom = 13) %>% 
      clearMarkers() %>%   
      clearShapes() %>% 
      addMarkers(data = display.points(), 
                 lng = display.points()$DisplayLon,
                 lat = display.points()$DisplayLat,
                 icon = BikeIcon) %>%
      addCircleMarkers(data = display.points(), group = "Circles",
                       layerId= display.points()$Station,
                       lng = display.points()$lon,
                       lat = display.points()$lat,
                       radius = display.points()$Radius, 
                       popup = paste0("<strong>Station Name: </strong>",
                                      display.points()$Station,
                                      "<br><strong>Number of Trips: </strong>",
                                      display.points()$Trips),
                       color = CircleColors(display.points()$Trips),
                       stroke = TRUE) 
    

    }
      


 
        
})


  
  clicked_leaflet <- reactiveValues(clickedMarker=NULL)
  observeEvent(input$mymap_marker_click,{
    clicked_leaflet$clickedMarker <- input$mymap_marker_click
  })
  
  
  clicked_station = reactive(({
    clicked_leaflet$clickedMarker$id
  }))
  
  
  selected_data= reactive(({
    if(is.null(clicked_leaflet$clickedMarker))
      return(NULL)
    else if(input$Direction == "OriginTrips") {
    filter(Bikeshare_Data_Aggreggated2, Start.station == input$Selectedstation, End.station == as.character(clicked_station()[1]))}
    else if(input$Direction == "DestinationTrips") {
        
    filter(Bikeshare_Data_Aggreggated2, End.station == input$Selectedstation, Start.station == as.character(clicked_station()[1]))}
      }))


  output$OverTime=renderPlotly({
    req(!is.na(selected_data()))
    temp<-selected_data()
    
    if(is.null(temp))
      return(NULL)
    plot_ly() %>%
      add_lines(x = temp$FixedDate, y = temp$Trips, name = "Trips Over Time",xaxis = "x1", yaxis = "y1" ) %>%
      add_lines(x = temp$FixedDate[temp$FixedDate >= sliderMonthRangeMin$Month & temp$FixedDate <= sliderMonthRangeMax$Month] , y = temp$Trips[temp$FixedDate >= sliderMonthRangeMin$Month & temp$FixedDate <= sliderMonthRangeMax$Month] ,name = "Selected Month(s)",  line = list(width  = 4), xaxis = "x1", yaxis = "y1") %>%
      add_lines(x = RidershipByMonth$FixedDate, y = RidershipByMonth$TotalTrips, name = "Total Trips by Month", xaxis = "x1", yaxis = "y2") %>%
      
    layout(title = '',
             legend = list(orientation = 'h'),
             xaxis = list(title = ""),
             yaxis = list(
               range = c(0,max(temp$Trips)),
               title = "Station Bikeshare Trips"),
             yaxis2 = list( 
               anchor = "x", 
               overlaying = "y",
               range = c(0, max(RidershipByMonth$TotalTrips)), 
               side = "right", 
               title = "Total Bikeshare Trips",
               showgrid = FALSE))
    
  })
    
 # observeEvent(event_data("plotly_click"), {
    
    
  #  Date_Click<-as.Date("2018-06-01")
    
   # updateSliderInput(session, "YearMonth", value =  as.Date(as.character(event_data("plotly_click")$x)),timeFormat="%b %Y")})
  

  output$Top5Stations  <- renderTable({
    if(input$Direction == "OriginTrips") {
      display.points() %>% 
      select("Origin" = Start.station,"Destination" = End.station, "Trips" = Trips) %>% 
      arrange(-Trips) %>% 
      distinct(Origin,Destination, .keep_all = TRUE) %>% 
      mutate_if(is.numeric, format, 1)  %>% 
      as.data.frame() %>% 
      head(5)} else {
        display.points() %>% 
          select("Origin" = End.station,"Destination" = Start.station, "Trips" = Trips) %>% 
          arrange(-Trips) %>% 
          distinct(Destination,Origin, .keep_all = TRUE) %>% 
          mutate_if(is.numeric, format, 1)  %>% 
          as.data.frame() %>% 
          head(5)}
  })
}

# Run the application 
shinyApp(ui, server)

  



