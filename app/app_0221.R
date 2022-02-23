library(dplyr)
library(shinydashboard)
library(shiny)
library(plotly)
library(htmltools)
library(shinyWidgets)
library(tidyverse)
library(leaflet)
library(htmlwidgets)
library(sf)
library(lubridate)
library(RColorBrewer)
library(zoo)
if (!require("remotes")) {
  install.packages("remotes")
  library(remotes)
  remotes::install_github("rstudio/leaflet")
}

#c("shinydashboard", "shiny", "tidyverse", "htmlwidgets", "lubridate", "RColorBrewer")

library(tidyr)
# convert data type
citibike_covid_line <- read.csv("../data/citibike/cleaned/linechart_covid.csv")
citibike_covid_line$week <- as.Date(citibike_covid_line$week, "%Y-%m-%d")
class(citibike_covid_line$week)
# long 
citibike_covid_line <- citibike_covid_line %>% 
  gather(variables, values, tripduration, totaltrip) %>%
  mutate(values = ifelse(variables == 'totaltrip',round(values/20000),values))


citibike_covid <- readRDS("../data/citibike/cleaned/citibike_covid.RDS")
class(citibike_covid$week)

caserate_zcta <- readRDS("../data/citibike/cleaned/caserate_zcta.RDS")

data = readRDS("../data/Subway/cleaned/Subway_Data_Processed.RDS")

data_per_week_days = readRDS("../data/Subway/cleaned/Weekday_Subway_Data_Processed.RDS")

data_per_week_days$Weekdays<-ordered(data_per_week_days$Weekdays,levels=c("lundi", "mardi", "mercredi", "jeudi","vendredi", "samedi", "dimanche"))

names(caserate_zcta)[7]<-"value_per_week"
zip_polygon<-data%>%select(zcta,geometry)%>%unique
weekly_data<-aggregate(list(value_per_week=data$value_per_week),list(zcta=data$zcta,Year_Week=data$Year_Week),sum)
weekly_data<-merge(weekly_data,zip_polygon,by="zcta")
weekly_data<-st_as_sf(weekly_data)

# ----- TAXI DATA PROCESSING BEGIN -----
# Read in csv files for taxi data (change columns to workable formats)
taxi_data <- read.csv("../data/Taxi/cleaned/taxi_data_by_month_boro.csv")
taxi_data$Date <- as.yearmon(paste(taxi_data$Year, taxi_data$Month), "%Y %m")
taxi_data$weighted_trip_time = taxi_data$weighted_trip_time*60
covid_data_taxi <- read.csv("../data/Taxi/cleaned/covid_data_by_month_boro.csv")
covid_data_taxi$Date <- as.yearmon(paste(covid_data_taxi$Year, covid_data_taxi$Month), "%Y %m")
# ----- TAXI DATA PROCESSING END -----

# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "black",
    dashboardHeader(title="Transporation Dashboard"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Home", tabName = "home", icon = icon("tree")),
            menuItem("Citibike", tabName = "citibike", icon = icon("bicycle")),
            menuItem("Taxi", tabName = "taxi", icon = icon("car")),
            menuItem("Subway", tabName = "subway", icon = icon("train")),
            menuItem("Reference", tabName = "reference", icon = icon("print"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem("home",
                fluidPage(
                    h1("Home"),
                    titlePanel("Impact on Transportation by COVID-19"),
                    sidebarLayout(
                        sidebarPanel(
                          p("Once Upon the Time,",
                            style = "font-family: 'Marker Felt', serif;  font-weight: normal; font-thickness: 500%; font-size: 200%; text-shadow: 3px #aaa; line-height: 1"),
                          p("There was a gang race each other by using a different form of transportation, from the pub to a restaurant seven miles away.",
                            style = "font-family: 'Marker Felt', serif;  font-weight: normal; font-thickness: 300%; font-size: 150%; line-height: 1.2"),
                          p("Barney's plan is to use the ambulance ride;"),
                          p("Lily took the train;"), 
                          p("Robin hailed a cab;"),
                          p("Ted rode the bus;"),
                          p("Marshall took himself as the engine."),
                          tags$a(href = "https://www.imdb.com/title/tt1733369/", "<How I met your morth>", target = "_blank"),
                          
                          p(),
                          p("While Today, Under COVID Pandamic,",
                            style = "font-family: 'Marker Felt', serif;  font-weight: normal; font-thickness: 500%; font-size: 200%; text-shadow: 3px #aaa; line-height: 1.4"),
                          p("Here is a gang trying to figure out what patterns have changed about those transportation...",
                            style = "font-family: 'Marker Felt', serif;  font-weight: normal; font-thickness: 300%; font-size: 150%; line-height: 1.2")
                          
                        ),
                        mainPanel(
                            tabsetPanel(
                              tags$img(
                                img (src = "NYC-Trans.png", height="100%", width="100%", align="right")
                              )
                            )
                        )
                    )
                )
            ),
    
            tabItem("citibike",
                fluidPage(
                    h1("Citibike"),
                    titlePanel("Impact on Usage by Covid-19"),
                    sidebarLayout(
                        sidebarPanel(
                            h5("The map shows the # of trips by ZCTAs (ZIP Code Tabulation Areas) with corona case rate.
                               The data is from 2019/1/6 until now. You can see how citibike usage has been impacted by COVID-19."),
                            
                            #### for slider
                            sliderInput("dateSlider",
                                        "Select a Date (by week):",
                                        min = min(citibike_covid$week),
                                        max = max(citibike_covid$week),
                                        value=min(citibike_covid$week),
                                        step= days(7),
                                        timeFormat="%Y-%m-%d"
                            ),
                            selectizeInput("typeInput", "Type:",
                                           choices = unique(citibike_covid_line$variables),
                                           selected = "tripduration", multiple=FALSE),
                            sliderInput("dateSlider2",
                                        "Select a Date Range (by week):",
                                        min = min(citibike_covid_line$week),
                                        max = max(citibike_covid_line$week),
                                        value=c(min(citibike_covid_line$week), max(citibike_covid_line$week)), 
                                        step= days(7),
                                        timeFormat="%Y-%m-%d")
                        ),
                        mainPanel(
                            tabsetPanel(
                                tabPanel("Citibike Use Map",
                                         leafletOutput("cases"),
                                         plotOutput("timePlot"),
                                         plotOutput("casePlot"))
                            )
                        )
                    )
                )
        ),
        tabItem("taxi",
            fluidPage(
              titlePanel("Taxi Service"),
              
              # Sidebar layout with input and output definitions ----
              sidebarLayout(
                
                # Sidebar panel for inputs ----
                sidebarPanel(
                  
                  # Input: Select for the borough ----
                  selectInput(inputId = "borough",
                              label = "Choose a borough:",
                              choices = c("Manhattan", "Bronx", "Brooklyn", "Queens", "Staten Island")),
                  
                  # Input: Select for the business type ----
                  selectInput(inputId = "metric_type",
                              label = "Choose a metric:",
                              choices = c("Count", "Passenger Count", "Trip Distance", "Trip Time", "Total Amount", "Average Speed")),
                  
                  selectInput(inputId = "covid_data_boolean",
                              label = "Display Covid Rate Data?",
                              choices = c("Yes","No")),
                  
                  selectInput(inputId = "covid_metric",
                              label = "Choose a metric for Covid:",
                              choices = c("Cases", "Deaths", "Hospitalizations"))
                  
                ),
                
                # Main panel for displaying outputs ----
                mainPanel(
                  
                  # Output: tsPlot on borough ----
                  plotOutput(outputId = "tsPlot1"),
                  
                  plotOutput(outputId = "tsPlot2"),
                  
                )
              )
            )
        ),
        tabItem("subway",
              tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
              navbarPage(strong("NYC Subway during Covid",style="color: white;"),position = "fixed-bottom", theme="styles.css",
              tabPanel("Subway Map",
                       div(class="outer",
                           leafletOutput("map",width="100%",height="100%"),
                           absolutePanel(id = "controls", class="panel panel-default", fixed = TRUE, draggable = TRUE,
                                         top = 120, left = 20, right = "auto", bottom = "auto", width = 200, height = "auto",
                                         h3("Controls"),
                                         checkboxInput("show", "Show cluster of stations", value = F),
                                         conditionalPanel(condition="input.show==true",
                                                          checkboxInput("out", "Show output of stations", value = F)
                                         ),
                                         selectInput(
                                           inputId = "transport",
                                           label = "Choose which transport/Covid-19 to display on the choropleth map",
                                           choices = c("Covid-19","Subway"),
                                           selected = "Covid-19"
                                         ),
                                         sliderInput(
                                           inputId = "date",
                                           label = "Choose the date",
                                           min = min(data$Year_Week),
                                           max = max(data$Year_Week),
                                           value = as.Date("2020-10-05"),
                                           step= days(7),
                                           timeFormat="%Y-%m-%d" 
                                         )
                                         
                           ),
                           absolutePanel(id = "controls",class="panel panel-default", fixed = TRUE, draggable = TRUE,
                                         top = 40, right = 200, left = "auto", bottom = "auto", width = 250,
                                         conditionalPanel(condition= "input.out == true",
                                                          titlePanel("Outputs"),
                                                          p(htmlOutput("click_station_name")),
                                                          p(htmlOutput("click_value")),
                                                          h4("Average number of uses of this station"),
                                                          plotlyOutput("click_weekday",height = 150),
                                                          p(h4("Lines passing")),
                                                          p(htmlOutput("click_html",inline=TRUE))
                                                          )
                                         )
                       )
                       ),
                tabPanel("Subway Statistics",
                         titlePanel("Statistics"),
                         plotlyOutput("satistic_output",height=350)
                         )
                )
        ),
        tabItem("reference",
            fluidPage(
                h1("References"),
                sidebarLayout(
                    sidebarPanel(
                      p(strong("Data Resources"),
                        style = "font-family: 'normal', serif;  font-weight: normal; font-thickness: 300%; font-size: 160%; line-height: 1.2"),
                        tags$a(href = "https://github.com/nychealth/coronavirus-data", "NYC Corona Virus Data", target = "_blank"),
                        p(),
                        tags$a(href = "https://ride.citibikenyc.com/system-data", "NYC Citibike Data", target = "_blank"),
                        p(),
                      p(strong("Figure Resources"),
                        style = "font-family: 'normal', serif;  font-weight: normal; font-thickness: 300%; font-size: 160%; line-height: 1.2"),
                        tags$a(href = "https://www.wired.com/story/new-york-city-cap-uber-lyft/", "Cover figure", target = "_blank"),
                        p(),
                        tags$a(href = "https://feedbacklabs.org/blog/2020/04/03/citizen-input-matters-in-the-fight-against-covid-19/", "Ending figure", target = "_blank"),
                        p()
                      
                      ),
                    mainPanel(
                        tabsetPanel(tags$img(
                          img (src = "ending.png", height="100%", width="100%", align="right")
                        )
                        )
                    )
                )
            )
        )
    )
)
)

# Define server logic required to draw
server <- function(input, output, session) {

  #---------- TAXI BEGIN ----------
  
  metric_data <- reactive({
    if ( "Count" %in% input$metric_type){
      return("Count")
    }
    if ( "Passenger Count" %in% input$metric_type){
      return("weighted_passenger_count")
    }
    if ( "Trip Distance" %in% input$metric_type){
      return("weighted_trip_distance")
    }
    if ( "Trip Time" %in% input$metric_type){
      return("weighted_trip_time")
    }
    if ( "Total Amount" %in% input$metric_type){
      return("weighted_total_amount")
    }
    if ( "Average Speed" %in% input$metric_type){
      return("weighted_avg_speed")
    }
  })
  
  borough_data <- reactive({
    if ( "Manhattan" %in% input$borough){
      return("Manhattan")
    }
    if ( "Bronx" %in% input$borough){
      return("Bronx")
    }
    if ( "Brooklyn" %in% input$borough){
      return("Brooklyn")
    }
    if ( "Queens" %in% input$borough){
      return("Queens")
    }
    if ( "Staten Island" %in% input$borough){
      return("Staten Island")
    }
  })
  
  covid_metric <- reactive({
    if ( "Cases" %in% input$covid_metric){
      return("avg_case_count")
    }
    if ( "Deaths" %in% input$covid_metric){
      return("avg_death_count")
    }
    if ( "Hospitalizations" %in% input$covid_metric){
      return("avg_hosp_count")
    }
  })
  
  covid_data_table <- reactive({
    if ( "Yes" %in% input$covid_data_boolean){
      return(TRUE)
    }
    else{
      return (FALSE)
    }
  })
  
  output$tsPlot1 <- renderPlot({
    borough = borough_data()
    metric = metric_data()
    if(metric == "Count"){
      metric_label = "Total Taxi Rides"
    }
    if(metric == "weighted_passenger_count"){
      metric_label = "Passengers per Taxi Ride"
    }
    if(metric == "weighted_trip_distance"){
      metric_label = "Distance per Taxi Ride (miles)"
    }
    if(metric == "weighted_trip_time"){
      metric_label = "Time per Taxi Ride (minutes)"
    }
    if(metric == "weighted_total_amount"){
      metric_label = "Charge Amount per Taxi Ride (USD)"
    }
    if(metric == "weighted_avg_speed"){
      metric_label = "Average Speed per Taxi Ride (mph)"
    }
    data <- taxi_data[taxi_data$Borough == borough,c("Date", metric)]
    y_max = max(data[,2])
    ggplot(data = data, mapping = aes(x = data[,1], y=data[,2])) + geom_line() + geom_point()+ ylim(0,1.5*y_max) + ggtitle(borough) + labs(y=metric_label, x = "Month, Year") + theme(plot.title = element_text(hjust = 0.5, size=22, margin = margin(t = 0, r = 0, b = 20, l = 0)), axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), axis.title.x = element_text(margin = margin(t=15, r = 0, b = 0, l = 0)))
  })
  
  output$tsPlot2 <- renderPlot({
    if(covid_data_table()){
      borough = borough_data()
      if(borough == "Manhattan"){
        boro = "mn"
      }
      if(borough == "Bronx"){
        boro = "bx"
      }
      if(borough == "Brooklyn"){
        boro = "bk"
      }
      if(borough == "Queens"){
        boro = "qn"
      }
      if(borough == "Staten Island"){
        boro = "si"
      }
      
      if(covid_metric() == "avg_case_count"){
        covid_metric_label = "Cases per Day"
      }
      if(covid_metric() == "avg_death_count"){
        covid_metric_label = "Deaths per Day"
      }
      if(covid_metric() == "avg_hosp_count"){
        covid_metric_label = "Hospitalizations per Day"
      }
      
      string = paste(covid_metric(), "_", boro, sep="")
      data <- covid_data_taxi[,c("Date", string)]
      y_max = max(data[,2])
      ggplot(data = data, mapping = aes(x = data[,1], y=data[,2])) + geom_line() + geom_point()+ ylim(0,1.5*y_max) + labs(x = "Month, Year",y=covid_metric_label) + theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), axis.title.x = element_text(margin = margin(t=15, r = 0, b = 0, l = 0)))
    }
  })
  
  #---------- TAXI END ----------
  
    week_zcta <- reactive({
        w <- citibike_covid %>% filter(week == input$dateSlider)
        return(w)
    })
    output$cases <- renderLeaflet({
        pal <- colorBin(palette = "OrRd", 9, domain = citibike_covid$count)
        
        labels = sprintf(
            "<strong>ZCTA:</strong> %s<br/>
            <strong># of Trips:</strong> %g<br/>
            <strong>Case Rate(Per 100k people):</strong> %g",
            week_zcta()$zcta,
            week_zcta()$count,
            week_zcta()$caserate) %>%
            lapply(htmltools::HTML)
    week_zcta() %>%
        st_transform(crs = "+init=epsg:4326") %>%
        leaflet() %>%
        addProviderTiles(provider = "CartoDB.DarkMatter",
                         options = providerTileOptions(noWrap = TRUE)) %>%
        setView(-73.95, 40.75, zoom = 11) %>%
        addPolygons(label = labels, fillColor = ~pal(week_zcta()$count),
                    weight = 2, opacity = 1, color = "grey", dashArray="3",
                    stroke = FALSE, smoothFactor = .7,  fillOpacity = 0.7,
                    highlightOptions = highlightOptions(weight =5,
                                                        fillOpacity = 0.7,
                                                        color = "#666",
                                                        opacity = 1,
                                                        bringToFront = TRUE)) %>%
        addLegend(pal = pal,
                  values = ~count, title = "Total # of Start/End points",
                  opacity = 0.7, position = "bottomright")
    })
    week_line <- reactive({
        citibike_covid_line %>%
            filter(variables==input$typeInput,
                   week >= input$dateSlider2[1],
                   week <= input$dateSlider2[2])
    })
    output$timePlot <- renderPlot({
        ggplot(week_line(),aes(x=week)) +
        geom_density(aes(y = values),size=2, fill="#69b3a2", color="#69b3a2", alpha=.6, stat="identity") +
        geom_density(aes(y = case_rate/110), fill="darkred", color="darkred", alpha=.6, stat="identity") +
        labs(title = "Trip Duration & Total Trips in NYC" , x = 'Week', y = 'Trip duration')+
        scale_y_continuous(
          name = "Trip Duration(mins) or Total Trips(20000 trips)",
          sec.axis = sec_axis(~.*110, name="COVID cases")) + 
        #
        theme(panel.background = element_rect(fill = "gray10",
                                                colour = "gray10",
                                                size = 0.5, linetype = "solid")) +
        theme(plot.title=element_text(size=15,hjust = 0.5))
    })
#-------------------Server logic for mapping the subway---------------------------
#-------------------------For the map panel----------------------------------------
Transformation_of_the_domain<-function(x){
      i = as.integer(log10(x))
      return(i+log(x/10**i,5)/2)
    }
    week_subway_zcta <- reactive({
      if(input$transport == "Covid-19"){
        w <- caserate_zcta %>% filter(week_ending-days(5) == input$date)}
      else if(input$transport == "Subway"){
        w<-weekly_data%>%filter(Year_Week == input$date)
      }
      return(w)
    })
    week_subway<-reactive({
      w<-data%>%filter(Year_Week == input$date)
      w<-w%>%select(GTFS.Longitude,GTFS.Latitude,station,
                    value_per_week, Year_Week, html_color)%>%unique
      return(w)
    })
    
    get_label_and_domain<-reactive({
      if(input$transport == "Covid-19"){
        w <- caserate_zcta
        return(list("label"="number or cases<br/> per 100,000 people","domain"=Transformation_of_the_domain(w$value_per_week+1)))
      }
      else if(input$transport == "Subway"){
        w<-weekly_data
        return(list("label"="number of entries<br/> in the subway","domain"=Transformation_of_the_domain(w$value_per_week+1)))
      }
    })
    output$map <- renderLeaflet({
      label_and_domain<-get_label_and_domain()
      pal <- colorBin(palette = "OrRd", 10, domain =label_and_domain$domain)
      labels = sprintf(
        paste("<strong>%s</strong><br/>%g",label_and_domain$label),
        week_subway_zcta()$zcta, week_subway_zcta()$value_per_week) %>%
        lapply(htmltools::HTML)
      
      week_subway_zcta() %>%st_transform(crs = "+init=epsg:4326")%>%leaflet() %>% addProviderTiles("Stamen.TonerLite")%>% setView(-73.983,40.7639,zoom = 13)%>%addPolygons(
          label = labels,
          stroke = FALSE,
          smoothFactor = .5,
          opacity = 1,
          fillOpacity = 0.7,
          fillColor = ~pal(Transformation_of_the_domain(week_subway_zcta()$value_per_week+1)),
          highlightOptions = highlightOptions(weight =5,
                                              fillOpacity = 1,
                                              color = "black",
                                              opacity = 1,
                                              bringToFront = FALSE)) %>%
        addLegend("bottomright",
                  pal = pal,
                  values = ~Transformation_of_the_domain(value_per_week+1),
                  title = paste("Log10 of the",label_and_domain$label),
                  opacity = 0.7)
    })
    observe({
      anglerIcon <- makeIcon(
        iconUrl = "icon_subway.png",
        iconWidth = 50, iconHeight = 50
      )
      pal<-colorFactor(palette = c(rgb(186, 255, 246 , 0.4,maxColorValue = 255),
                                   rgb(181, 226, 140, 0.6, maxColorValue = 255),
                                   rgb(241, 211, 87, 0.7, maxColorValue = 255),
                                   rgb(241, 55, 0, 0.7, maxColorValue = 255)
      ),
      levels=c("10+","100+","1000+","10000+")
      )
      proxy<-leafletProxy("map",data=week_subway(),session)%>%clearMarkerClusters()
      proxy%>%addMarkers(~GTFS.Longitude,
                         ~GTFS.Latitude,
                         icon=anglerIcon,
                         label = ~station,
                         layerId = ~paste(station,GTFS.Longitude,GTFS.Latitude,value_per_week,sep="/"),
                         group = "stations",
                         labelOptions = labelOptions(style = list(
                           "color" = "black",
                           "font-family" = "Arial",
                           "font-weight" = "bold",
                           "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                           "font-size" = "12px",
                           "border-color" = "rgba(0,0,0,0.5)"
                         )),
                         options = markerOptions(value =~value_per_week),
                         clusterOptions = markerClusterOptions(iconCreateFunction=JS("function(cluster){
let label = 0
for ( const marker of cluster.getAllChildMarkers()){
 label += parseInt(marker.options.value)
}
label = parseInt(label/1000)
var digits = (label + '').length
		return L.divIcon({
		html: '<div><span>' + label+ '</span></div>',
		className: 'cluster digits-'+ digits,
		iconSize: null
		});
	}")))
      if(input$show){
        proxy%>%showGroup("stations")
        proxy%>%addLegend("topright",
                          pal = pal,
                          values = c("10+","100+","1000+","10000+"),
                          title = "Number of uses<br/> of subway per thousands",
                          opacity = 0.7,layerId = "MarkerClusterLegend")
      }
      else{
        proxy%>%hideGroup("stations")
        proxy%>%removeControl("MarkerClusterLegend")
      }
    })
    observeEvent(input$map_marker_click,{
      click<-input$map_marker_click
      info<-click$id
      info<-unlist(strsplit(info,split = "/"))
      station_name<-info[1]
      long<-as.numeric(info[2])
      lat<-as.numeric(info[3])
      turnstile<-as.numeric(info[4])
      station<-week_subway()[as.logical(
        (abs(week_subway()$GTFS.Latitude-lat)<1e-4)*
          (abs(week_subway()$GTFS.Longitude-long)<1e-4)*
          (week_subway()$station==station_name)*
          (abs(week_subway()$value_per_week-turnstile)<1e-4)
      ),]
      output$click_station_name = renderText({paste("<strong>Station :</strong>",station$station)})
      output$click_html = renderText({as.character(station$html_color)})
      output$click_value = renderText({paste("<strong>Number of uses this week:</strong>",turnstile)})
      station_weekdays<-data_per_week_days[as.logical((data_per_week_days$Year==unlist(strsplit(as.character(input$date),split="-"))[1])*
                                                        (abs(data_per_week_days$GTFS.Latitude-lat)<1e-4)*
                                                        (abs(data_per_week_days$GTFS.Longitude-long)<1e-4)*
                                                        (data_per_week_days$station==station_name)
      ),]
      fig<-plot_ly(data=st_drop_geometry(station_weekdays), x=~Weekdays,y=~TURNSTILE_PER_WEEKDAY,type="bar")
      fig<- fig %>%layout(xaxis = list(
        title="",
        tickangle=45,
        zerolinecolor = '#ffff',
        zerolinewidth = 2,
        tickfont=list(size=9),
        gridcolor = 'ffff'),
        yaxis = list(
          title="Average count",
          tickfont=list(size=9),
          scaleanchor = "x",
          scaleratio = 1,
          zerolinecolor = '#ffff',
          zerolinewidth = 2,
          gridcolor = 'ffff'),
        plot_bgcolor='#e5ecf6'
      )
      output$click_weekday<-renderPlotly(fig)
    })
#------------------------For the statistic panel------------------------------
    Total_Subway<-aggregate(list("value"=data$value_per_week),list("date"=data$Year_Week),sum)
    fig <- plot_ly()
    fig<-fig%>%add_trace(x=~Total_Subway$date,y=~Total_Subway$value,type="scatter",mode="lines+markers",fill='tozeroy', name = "Total subway uses")
    Total_Covid<-aggregate(list("value"=caserate_zcta$value_per_week),list("date"=caserate_zcta$week_ending-days(5)),sum)
    ay <- list(
      tickfont = list(color = "red"),
      overlaying = "y",
      side = "right",
      title = "Covid case rate per week")
    fig<-fig%>%add_trace(x=~Total_Covid$date,y=~Total_Covid$value, name = "Total Covid rate cases",yaxis="y2",type="scatter",mode="lines+markers",fill='tozeroy')
    fig <- fig %>% layout(
      title = "", yaxis2 = ay,
      xaxis = list(title=""),
      yaxis = list(title="Number of subaway uses per week")
    )%>%
      layout(plot_bgcolor='#e5ecf6',
             xaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff'),
             yaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff')
      )
    output$satistic_output<-renderPlotly(fig)
}

# Run the application 
shinyApp(ui = ui, server = server)
