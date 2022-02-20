
library(shinydashboard)
library(shiny)
library(tidyverse)
library(leaflet)
library(htmlwidgets)
library(sf)
library(lubridate)
library(RColorBrewer)

#c("shinydashboard", "shiny", "tidyverse", "htmlwidgets", "lubridate", "RColorBrewer")


citibike_covid <- readRDS("../data/citibike/cleaned/citibike_covid.RDS")
class(citibike_covid$week)

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
                            style = "font-family: 'Marker Felt', serif;  font-weight: normal; font-thickness: 300%; font-size: 150%; line-height: 1.2"),
                          
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
                            )
                            ####
                        ),
                        mainPanel(
                            tabsetPanel(
                                tabPanel("Citibike Use Map", leafletOutput("cases"))                            )
                        )
                    )
                )
        ),
        tabItem("taxi",
            fluidPage(
                h1("Taxi")
            )
        ),
        tabItem("subway",
            fluidPage(
                h1("Subway")    
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
                        p(),
                      
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
server <- function(input, output) {

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
        addProviderTiles(provider = "Stamen.TonerLite",
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
                  values = ~count, title = "# of start and end points",
                  opacity = 0.7, position = "bottomright")
    })


}

# Run the application 
shinyApp(ui = ui, server = server)
