
library(dplyr)
library(ggplot2)
library(leaflet)
library(shiny)
library(shinyjs)
library(sf)
library(jsonlite)
library(tidyr)
library(httr)


#data loading and processing
#-------------------------------------------------------------------------------

# boundaries data
geo_data <- st_read("./data/LGA_2023_AUST_GDA2020/LGA_2023_AUST_GDA2020.shp")
geo_data <- st_transform(geo_data, crs = st_crs("+proj=longlat +datum=WGS84"))
selected_geo <- geo_data %>%
  filter(LGA_CODE23 == 24600)

#bars data
data_bar <- fromJSON("data/bars-and-pubs-with-patron-capacity.json")
data_bar_2021 <- data_bar %>%
  filter(census_year == 2021)
bars <- data_bar_2021 %>%
  group_by(longitude, latitude) %>%
  summarize(trading_name = paste(trading_name, collapse = "-"))

#cafes and restaurants
data_food <- fromJSON("data/cafes-and-restaurants-with-seating-capacity.json")
data_food_2021 <- data_food %>%
  filter(!is.na(census_year), census_year == 2021, industry_anzsic4_description == "Cafes and Restaurants")%>%
  distinct(trading_name, .keep_all = TRUE)
result <- data_food_2021 %>%
  group_by(longitude, latitude) %>%
  summarize(trading_name = paste(trading_name, collapse = "-"))

restaurants <- anti_join(result, bars, by = c("longitude", "latitude"))

###Top10 Restaurants
data_top10restaurants <- read.csv("data/top10-restaurants.csv")
restpic <- gsub(" ", "", data_top10restaurants$restaurant_name)
restpic[1] <- "Farmer"
###Top10 Sites
data_top10site <- read.csv("data/top10-sites.csv")
sitespic <- gsub(" ", "", data_top10site$site_name)

#hotels data
data_hotel_2021 <- data_food %>%
  filter(!is.na(census_year), census_year == 2021,industry_anzsic4_code ==4400) %>%
  distinct(trading_name, .keep_all = TRUE)
hotels <- data_hotel_2021 %>%
  group_by(longitude, latitude) %>%
  summarize(trading_name = paste(trading_name, collapse = "-"))

#Points of interest data
data_site <- read.csv("data/landmarks-and-places-of-interest-including-schools-theatres-health-services-spor.csv")
data_site_2021 <- data_site %>%
  filter(Theme != "Health Services", Theme != "Office", Sub.Theme != "Police Station", Sub.Theme != "Railway Station") %>%
  rename(trading_name = Feature.Name) %>%
  separate(Co.ordinates, into = c("latitude", "longitude"), sep = ",") %>%
  distinct(trading_name, .keep_all = TRUE)
data_site_2021$longitude <- as.numeric(data_site_2021$longitude)
data_site_2021$latitude <- as.numeric(data_site_2021$latitude)


#Transport data
## train station
station <- read.csv("data/PTV_METRO_TRAIN_STATION.shp.csv")
## tram routes
tram_routes <- st_read("data/tram-tracks/tram-tracks.shp")
#tram stop
stop <- read.csv("data/PTV_METRO_TRAM_STOP.csv")

#filtering data
cleaned_stop <- stop[complete.cases(stop$longitude) & complete.cases(stop$latitude), ]
cleaned_station <- station[complete.cases(station$longitude) & complete.cases(station$latitude), ]


stop_sf <- st_as_sf(cleaned_stop, coords = c("longitude","latitude"), crs = st_crs("+proj=longlat +datum=WGS84"))

station_sf <- st_as_sf(cleaned_station, coords = c("longitude","latitude"), crs = st_crs("+proj=longlat +datum=WGS84"))

joined_data1 <- st_join(stop_sf, selected_geo, join = st_within)
joined_data2 <- st_join(station_sf, selected_geo, join = st_within)
filtered_stop <- joined_data1 %>%
  filter(!is.na(LGA_CODE23))
filtered_station <- joined_data2 %>%
  filter(!is.na(LGA_CODE23))


tram_stop <- left_join(filtered_stop, stop, by = "STOP_ID")
train_station <- left_join(filtered_station, station, by = "STOP_ID")

# pedestrian-counting-system
loc <- fromJSON("data/pedestrian-counting-system-sensor-locations.json")
p_counting <- fromJSON("data/pedestrian-counting-system-monthly-counts-per-hour.json")

p_counting_2023 <- p_counting[grepl("^2023", p_counting$time), ]

p_counting_2023_avg <- p_counting_2023 %>%
  group_by(locationid) %>%
  summarize(Avg_Count = mean(total_of_directions))

p_counting_2023_avg$locationid <- as.integer(p_counting_2023_avg$locationid)

counting <- loc %>% 
  left_join(p_counting_2023_avg, by = c("location_id" = "locationid"))%>%
  filter(!is.na(Avg_Count))

#search data
search_data <- "none"
tram_stop1 <- tram_stop %>%
  rename(trading_name = STOP_NAME.x)

train_station1 <- train_station %>%
  rename(trading_name = STOP_NAME.x)


all_data <- bind_rows(data_food_2021, data_bar_2021, data_hotel_2021, data_site_2021, tram_stop1, train_station1)

#weather
api_key <- "7f3b6c241a6b467d8d9110909231910"
city <- "Melbourne"
api_url <- paste0("http://api.weatherapi.com/v1/current.json?key=", api_key, "&q=", city)
response <- GET(api_url)

if (http_type(response) == "application/json") {
  weather_json <- content(response, "text")
  weather_data <- jsonlite::fromJSON(weather_json)
  srcdata <- weather_data$current$condition$icon
}


#User interfaces
#-------------------------------------------------------------------------------
ui <- fluidPage(
  
  titlePanel(" "),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  
  sidebarLayout(
    sidebarPanel(
      class = "my-sidebar",
      h3("Melbourne City Tourists Guide"),
      p(HTML("Discover Melbourne city with our Tourists Guide app, your perfect companion for an unforgettable visiting experience. Explore attractions, dining spots, and transportation information in this vibrant city.")),
      br(),
      tags$div(actionButton("show_top_points", "Featured Points of Interest", class = "btn-success"), class = "top-button" ),
      br(),
      tags$div(actionButton("show_top_Restaurants", "Recommended Restaurants", , class = "btn-warning"), class = "top-button" ),
      br(),
      checkboxGroupInput("markerType",  "Types of Locations：",choices = c("Bars and Pubs", "Cafes and Restaurants", "Hotels", "Points of Interest"),selected = "Points of Interest"), 
      checkboxGroupInput("transMarkerType", "Transport：",choices = c("Train Stations", "Tram Station", "Tram Routes", "Pedestrian flow"),selected = ""), 
      tags$div(actionButton("show_modal1", "Temperature in Melbourne", class = "btn-primary"), class = "my-checkbox-group" ),
      br(),
      tags$div(actionButton("show_modal2", "Air Quality in Melbourne", class = "btn-primary"), class = "my-checkbox-group" )
    ),
    mainPanel(
      class = "my-main",
      width = "100%",
      leafletOutput("mapPlot", height = "100vh"), 
      
      #search bar
      tags$div( textInput("user_input", "Search Information by Keyword", value = ""),
                uiOutput("trading_name_buttons"),
                textOutput("selected_trading_name"), class = "search-bar"),
      #weather
      uiOutput("image_div")
    )
  )
)

server <- function(input, output, session) {
  

  output$mapPlot <- renderLeaflet({
    
    leaflet() %>%
      addTiles() %>%
      setView(lat = -37.82, lng = 144.96, zoom = 15) %>%
      addProviderTiles("CartoDB.Positron")%>%
      addPolygons(data = selected_geo, color = "#31b0d5", fillOpacity = 0)
    })
    
    observe({
      leafletProxy("mapPlot") %>%
        clearPopups()
      selected_markers <- input$markerType
      if (isTruthy(selected_markers)) {
        #leafletProxy("mapPlot") %>%
          #clearGroup("search_markers")
        leafletProxy("mapPlot") %>%
          clearGroup("topsite_markers")
        leafletProxy("mapPlot") %>%
          clearGroup("toprestaurant_markers")
      }
      

      if ("Bars and Pubs" %in% selected_markers) {
        leafletProxy("mapPlot") %>%
          addMarkers(
            data = bars,
            group = "beer_markers",
            lng = ~longitude,
            lat = ~latitude,
            popup = ~{
              lapply(trading_name, function(name) {
                HTML(paste("<span>Trading Name:</span><ul class='popup'><li>", gsub("-", "</li><li>", name), "</li></ul>"))
              })
            },
            icon = customIcon("beer.png"),
            options = markerOptions(zIndex = 999)
          )
      }else{
        leafletProxy("mapPlot") %>%
          clearGroup("beer_markers") 
      }
      
      if ("Cafes and Restaurants" %in% selected_markers) {
        leafletProxy("mapPlot") %>%
          addMarkers(
            data = restaurants,
            group = "restaurant_markers",
            lng = ~longitude,
            lat = ~latitude,
            popup = ~{
              lapply(trading_name, function(name) {
                HTML(paste("<span>Trading Name:</span><ul class='popup'><li>", gsub("-", "</li><li>", name), "</li></ul>"))
              })
            },
            icon = customIcon("restaurant1.png")
          )
      }else{
        leafletProxy("mapPlot") %>%
          clearGroup("restaurant_markers")
      }
      
      if ("Hotels" %in% selected_markers) {
        leafletProxy("mapPlot") %>%
          addMarkers(
            data = hotels,
            group = "hotel_markers",
            lng = ~longitude,
            lat = ~latitude,
            popup = ~{
              lapply(trading_name, function(name) {
                HTML(paste("<span>Trading Name:</span><ul class='popup'><li>", gsub("-", "</li><li>", name), "</li></ul>"))
              })
            },
            icon = customIcon("hotel.png"),
            options = markerOptions(zIndex = 9999)
          )
      }else{
        leafletProxy("mapPlot") %>%
          clearGroup("hotel_markers")
      }
      
      if ("Points of Interest" %in% selected_markers) {
        leafletProxy("mapPlot") %>%
          addMarkers(
            data = data_site_2021,
            group = "site_markers",
            lng = ~longitude,
            lat = ~latitude,
            popup = ~{
              lapply(trading_name, function(name) {
                HTML(paste("<span>Sites Name:</span><ul class='popup'><li>", gsub("-", "</li><li>", name), "</li></ul>"))
              })
            },
            icon = customIcon("site.png")
          )
      }else{
        leafletProxy("mapPlot") %>%
          clearGroup("site_markers")
      }
    })
    
    # top buttons
    observeEvent(input$show_top_points, {
      updateCheckboxGroupInput(session, "markerType", selected = character(0))
      updateCheckboxGroupInput(session, "transMarkerType", selected = character(0))
      leafletProxy("mapPlot") %>%
        clearGroup("toprestaurant_markers")
      leafletProxy("mapPlot") %>%
        clearGroup("search_markers")
      leafletProxy("mapPlot") %>%
        addMarkers(
          data = data_top10site,
          group = "topsite_markers",
          lng = ~longitude,
          lat = ~latitude,
          popup = ~{
            lapply(1:length(site_name), function(i) {
              name <- site_name[i]
              popup_content <- paste0(
                "<div class='top-popup'><strong>", name, "</strong><br>",
                "<img src='", sitespic[i], ".jpg' /><br><p>",
                description[i], "</p></div>"
              )
              HTML(popup_content)
            })
          },
          icon = customIcon("star.png"),
          options = markerOptions(zIndex = 9999)
        )
    })
    
    observeEvent(input$show_top_Restaurants, {
      updateCheckboxGroupInput(session, "markerType", selected = character(0))
      updateCheckboxGroupInput(session, "transMarkerType", selected = character(0))
      leafletProxy("mapPlot") %>%
        clearGroup("topsite_markers")
      leafletProxy("mapPlot") %>%
        clearGroup("search_markers")
      leafletProxy("mapPlot") %>%
        addMarkers(
          data = data_top10restaurants,
          group = "toprestaurant_markers",
          lng = ~longitude,
          lat = ~latitude,
          popup = ~{
            lapply(1:length(restaurant_name), function(i) {
              name <- restaurant_name[i]
              popup_content <- paste0(
                "<div class='top-popup'><strong>", name, "</strong><br>",
                "<img src='", restpic[i], ".jpg' width='100' height='100' /><br><p>",
                description[i], "</p></div>"
              )
              HTML(popup_content)
            })
          },
          icon = customIcon("star.png"),
          options = markerOptions(zIndex = 9999)
        )
    })
    
    #transport boxes
    observe({
      selected_transMarkerType <- input$transMarkerType
      leafletProxy("mapPlot") %>%
        clearGroup("counting")
      if (isTruthy(selected_transMarkerType)) {
        #leafletProxy("mapPlot") %>%
          #clearGroup("search_markers")
        leafletProxy("mapPlot") %>%
          clearGroup("topsite_markers")
        leafletProxy("mapPlot") %>%
          clearGroup("toprestaurant_markers")

      }
      
      if ("Pedestrian flow" %in% selected_transMarkerType) {
        leafletProxy("mapPlot") %>%
          addCircleMarkers(
            data = counting,
            group = "counting",
            lng = ~longitude,
            lat = ~latitude,
            radius = ~sqrt((Avg_Count - 8) / (1628 - 8)) * 30,
            fill = TRUE,
            stroke = F,
            fillOpacity = 0.3,
            fillColor = "red",
            popup = ~paste("Sensor: ", sensor_description, "<br>Avg Persons Counting (01/2023 - 08/2023): ", ceiling(Avg_Count))
          )
        shinyjs::show("text_div")
      }else{
        leafletProxy("mapPlot") %>%
          clearGroup("counting")
        
      }
      if ("Train Stations" %in% selected_transMarkerType) {
        leafletProxy("mapPlot") %>%
          addMarkers(
            data = train_station,
            group = "train_markers",
            lng = ~longitude,
            lat = ~latitude,
            popup = ~paste("Train Station Name:<br>", STOP_NAME.x),
            icon = customIcon("train.png")
          )
      }else{
        leafletProxy("mapPlot") %>%
          clearGroup("train_markers")
      }
      
      if ("Tram Station" %in% selected_transMarkerType) {
        leafletProxy("mapPlot") %>%
          addMarkers(
            data = tram_stop,
            group = "tram_markers",
            lng = ~longitude,
            lat = ~latitude,
            popup = ~paste("Tram Station Name:<br>", STOP_NAME.x),
            icon = customIcon("metro.png")
          )
      }else{
        leafletProxy("mapPlot") %>%
          clearGroup("tram_markers")
      }
      
      if ("Tram Routes" %in% selected_transMarkerType){
        leafletProxy("mapPlot") %>%
          addPolylines(data = tram_routes,
                       group = "tramroutes_markers",
                       color = ~pal(name),
                       popup = ~name)
      }else{
        leafletProxy("mapPlot") %>%
          clearGroup("tramroutes_markers")
      }
    })      
    pal <- colorFactor("viridis", domain = tram_routes$name)
    customIcon <- function(iconUrl) {
      makeIcon(
        iconUrl = iconUrl,
        iconWidth = 20, 
        iconHeight = 20,  
        iconAnchorX = 10, 
        iconAnchorY = 10  
      )
    }
    
    observeEvent(input$show_modal1, {
      showModal(modalDialog(
        tags$iframe(src="https://public.tableau.com/views/MelbourneTemperatureandAirQuality2/Temperature?:language=en-US&:display_count=n&:origin=viz_share_link&:showVizHome=no&:embed=true", 
                    width="100%", height="100%"),
        footer = NULL,  
        easyClose = TRUE,  
        id = "tableau_modal"
      ))
    })
    
    observeEvent(input$show_modal2, {
      showModal(modalDialog(
        tags$iframe(src="https://public.tableau.com/shared/T4Y4FWXHY?:language=en-US&:display_count=n&:origin=viz_share_link&:showVizHome=no&:embed=true", 
                    width="100%", height="100%"),
        footer = NULL,  
        easyClose = TRUE,  
        id = "tableau_modal"
      ))
    })
    
#-----------search bar---START-------------------------------------------------
    observeEvent(input$user_input, {
      input_text <- input$user_input
      if (nchar(input_text) > 0) {
        leafletProxy("mapPlot") %>%
          clearGroup("search_markers")
        leafletProxy("mapPlot") %>%
          clearPopups()
        filtered <- all_data %>%
          filter(grepl(input_text, trading_name, ignore.case = TRUE))
        search_data <<- filtered
        output$trading_name_buttons <- renderUI({
          button_list <- lapply(1:5, function(i) {
            trading_name <- filtered$trading_name[i]
            if(!is.na(trading_name)){
              tags$div(
                style = "margin-bottom: 5px;",
                actionButton(inputId = paste0("trading_name_button_", i), label = trading_name)
              )
            }
          })
          do.call(tagList, button_list)
        })
      } else {
        output$trading_name_buttons <- renderUI({
          p("")
        })
      }
    })
    
    observeEvent(input[[paste0("trading_name_button_", 1)]], {
      selected_trading_name <- search_data$trading_name[1]
      lat <- search_data$latitude[1]
      long <- search_data$longitude[1]
      updateTextInput(session, "user_input", value = "")
      if (is.na(selected_trading_name)) {
        output$trading_name_buttons <- renderUI({
          p("")
        })
      } else {
        proxy <- leafletProxy("mapPlot")
        proxy %>% clearPopups()  
        proxy %>% addPopups(
          lng = long,  
          lat = lat,  
          popup = selected_trading_name
        )
        proxy %>%addMarkers(
          data = data_food_2021,
          group = "search_markers",
          lng = long,
          lat = lat,
          popup = selected_trading_name,
          icon =  makeIcon(
            iconUrl = "search.png",
            iconWidth = 30, 
            iconHeight = 30

          ),
          options = markerOptions(zIndex = 9999)
        )
        proxy %>% setView(lng = long, lat = lat, zoom = 20)
      }
    })
    
    observeEvent(input[[paste0("trading_name_button_", 2)]], {
      selected_trading_name <- search_data$trading_name[2]
      lat <- search_data$latitude[2]
      long <- search_data$longitude[2]
      updateTextInput(session, "user_input", value = "")
      if (is.na(selected_trading_name)) {
        output$trading_name_buttons <- renderUI({
          p("")
        })
      } else {
        proxy <- leafletProxy("mapPlot")
        proxy %>% clearPopups()  
        proxy %>% addPopups(
          lng = long,  
          lat = lat,  
          popup = selected_trading_name
        )
        proxy %>%addMarkers(
          data = data_food_2021,
          group = "search_markers",
          lng = long,
          lat = lat,
          popup = selected_trading_name,
          icon =  makeIcon(
            iconUrl = "search.png",
            iconWidth = 30, 
            iconHeight = 30
            
          ),
          options = markerOptions(zIndex = 9999)
        )
        proxy %>% setView(lng = long, lat = lat, zoom = 20)
      }
    })
    
    observeEvent(input[[paste0("trading_name_button_", 3)]], {
      selected_trading_name <- search_data$trading_name[3]
      lat <- search_data$latitude[3]
      long <- search_data$longitude[3]
      updateTextInput(session, "user_input", value = "")
      if (is.na(selected_trading_name)) {
        output$trading_name_buttons <- renderUI({
          p("")
        })
      } else {
        proxy <- leafletProxy("mapPlot")
        proxy %>% clearPopups()  
        proxy %>% addPopups(
          lng = long,  
          lat = lat,  
          popup = selected_trading_name
        )
        proxy %>%addMarkers(
          data = data_food_2021,
          group = "search_markers",
          lng = long,
          lat = lat,
          popup = selected_trading_name,
          icon =  makeIcon(
            iconUrl = "search.png",
            iconWidth = 30, 
            iconHeight = 30
            
          ),
          options = markerOptions(zIndex = 9999)
        )
        proxy %>% setView(lng = long, lat = lat, zoom = 20)
      }
    })
    
    observeEvent(input[[paste0("trading_name_button_", 4)]], {
      selected_trading_name <- search_data$trading_name[4]
      lat <- search_data$latitude[4]
      long <- search_data$longitude[4]
      updateTextInput(session, "user_input", value = "")
      if (is.na(selected_trading_name)) {
        output$trading_name_buttons <- renderUI({
          p("")
        })
      } else {
        proxy <- leafletProxy("mapPlot")
        proxy %>% clearPopups()  
        proxy %>% addPopups(
          lng = long,  
          lat = lat,  
          popup = selected_trading_name
        )
        proxy %>%addMarkers(
          data = data_food_2021,
          group = "search_markers",
          lng = long,
          lat = lat,
          popup = selected_trading_name,
          icon =  makeIcon(
            iconUrl = "search.png",
            iconWidth = 30, 
            iconHeight = 30
            
          ),
          options = markerOptions(zIndex = 9999)
        )
        proxy %>% setView(lng = long, lat = lat, zoom = 20)
      }
    })
    
    observeEvent(input[[paste0("trading_name_button_", 5)]], {
      selected_trading_name <- search_data$trading_name[5]
      lat <- search_data$latitude[5]
      long <- search_data$longitude[5]
      updateTextInput(session, "user_input", value = "")
      if (is.na(selected_trading_name)) {
        output$trading_name_buttons <- renderUI({
          p("")
        })
      } else {
        proxy <- leafletProxy("mapPlot")
        proxy %>% clearPopups()  
        proxy %>% addPopups(
          lng = long,  
          lat = lat,  
          popup = selected_trading_name
        )
        proxy %>%addMarkers(
          data = data_food_2021,
          group = "search_markers",
          lng = long,
          lat = lat,
          popup = selected_trading_name,
          icon =  makeIcon(
            iconUrl = "search.png",
            iconWidth = 30, 
            iconHeight = 30
            
          ),
          options = markerOptions(zIndex = 9999)
        )
        proxy %>% setView(lng = long, lat = lat, zoom = 20)
      }
    })
    
#-----------search bar---END----------------------------------------------------
    
    output$image_div <- renderUI({
      if (!is.null(srcdata)) {
        tags$div(
          p(paste(weather_data$location$name, ", ", weather_data$location$country)),
          tags$img(src = srcdata, width = "100", height = "100"),
          br(),
          paste(weather_data$current$temp_c, "°C",weather_data$current$condition$text, "Day"),
          br(),
          paste("Wind: ", weather_data$current$wind_kph, "km/h"),
          br(),
          paste("Humidity: ", weather_data$current$humidity, "%")
        )
      } else {
        NULL
      }
    })
    

  
  
  
  
}

shinyApp(ui, server)