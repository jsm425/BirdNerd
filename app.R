### packages
library(tidyverse)
library(shiny)
library(shinyjs) # for hidden() to hide buttons
library(leaflet) # for the map and everything on it
library(geosphere) # for haversineDist() to calculate distances b/w guess and points


### script --------------------------------------------------------------------
## querying ebird for sightings (last ten days)
difficultySettings <- c("Very Common", "Common", "Uncommon", "Rare", "Very Rare")
sightingsTables <- list(level1Sightings, level2Sightings, level3Sightings, level4Sightings, level5Sightings)
names(sightingsTables) <- difficultySettings

## variables
chosenLat <- NULL
chosenLong <- NULL

## function to calc distance b/w guess and all sightings and return shortest
checkGuessDistance <- function(selectedSightingsTable) {
  selectedSightingsTable %>%
    rowwise() %>%
    mutate(distanceFromGuess_km = distHaversine(p1 = c(chosenLong, chosenLat),
                                                p2 = c(lng, lat)) / 1000) %>%
    ungroup() %>%
    summarise(closest = min(distanceFromGuess_km)) %>%
    pull() %>%
    round() %>%
    return()
}


### server --------------------------------------------------------------------
server <- function(input, output) {
  ## reactive variables -----------------------------------**
  ## reactive difficulty setting from radio buttons
  chosenDifficulty <- reactive({as.character(input$challengeLevel)})
  
  ## reactive scorecard based on guesses
  reScorecard <- reactiveVal({data.frame(Level = difficultySettings,
                                         GuessStatus = rep(0, length(difficultySettings)),
                                         GuessLat = rep(NA, length(difficultySettings)),
                                         GuessLong = rep(NA, length(difficultySettings)),
                                         DistanceOff = rep(NA, length(difficultySettings)))})
  
  
  ## outputs ----------------------------------------------**
  ## question text
  output$questionText <- renderUI({
    sc <- reScorecard()
    # change prompt if guess has been made
    if (sc$GuessStatus[sc$Level == chosenDifficulty()] == 0) {
      displayedPrompt <- "Try to guess as close as possible to a sighting of this bird by <b>clicking</b> on the map (US only):"
    } else {
      # if made, display distance
      displayedPrompt <- paste0("The nearest sighting was ", 
                                sc$DistanceOff[sc$Level == chosenDifficulty()],
                                "km from your guess!")
    }
    HTML(paste0("<b>", chosenDifficulty(),
                " Sighting:</b><br>",
                sightingsTables[[chosenDifficulty()]][1,]$comName, "<br>",
                "<i>", sightingsTables[[chosenDifficulty()]][1,]$sciName, "</i><br>",
                "<br>",
                displayedPrompt))
  })
  
  ## mapping
  output$myMap <- renderLeaflet({
    leaflet() %>%
      fitBounds(lng1 = -126, lng2 = -66, lat1 = 24, lat2 = 50) %>%
      #$ setMaxBounds(lng1 = -126, lng2 = -66, lat1 = 24, lat2 = 50) %>%
      addTiles(options = providerTileOptions(minZoom = 4, maxZoom = 14))
  })
  
  ## scoreout panel text
  output$myScoreout <- renderUI({
    sc <- reScorecard()
    HTML(paste0("Nice job! Your total distance off over five guesses was ",
                "<b>", sum(sc$DistanceOff), "</b> km!"))
  })
  
  
  ### observers -------------------------------------------**
  ## on radio button switch - update map based on guess status
  observeEvent(eventExpr = chosenDifficulty(),
               handlerExpr = {
                 ## clear the markers from the map
                 clearMarkers(map = leafletProxy("myMap"))
                 ## check guess status
                 # pull the reactive df and check it
                 sc <- reScorecard()
                 scGuessStatus <- sc$GuessStatus[sc$Level == chosenDifficulty()]
                 # if guessed already, show guess and sightings
                 if (scGuessStatus > 0) {
                   # add in red circle for guess
                   addCircleMarkers(map = leafletProxy("myMap"),
                                    lat = sc$GuessLat[sc$Level == chosenDifficulty()],
                                    lng = sc$GuessLong[sc$Level == chosenDifficulty()],
                                    color = "red")
                   # add in blue circles for all sightings
                   addCircleMarkers(map = leafletProxy("myMap"),
                                    lat = ~lat,
                                    lng = ~lng,
                                    color = "blue",
                                    data = sightingsTables[[chosenDifficulty()]])
                 }
               })
  
  ## on map click - place guess on map (condition dependent)
  observeEvent(eventExpr = input$myMap_click,
               handlerExpr = {
                 ## check guess status
                 # pull the reactive df and check it
                 sc <- reScorecard()
                 scGuessStatus <- sc$GuessStatus[sc$Level == chosenDifficulty()]
                 # if not guessed yet, allow placement
                 if (scGuessStatus == 0) {
                   # set lat/long vars
                   chosenLat <<- input$myMap_click$lat
                   chosenLong <<- input$myMap_click$lng
                   # clear old marker(s)
                   clearMarkers(map = leafletProxy("myMap"))
                   # add new one where the click was
                   addCircleMarkers(map = leafletProxy("myMap"),
                                    lat = chosenLat,
                                    lng = chosenLong,
                                    color = "red")
                 }
               })
  
  ## on guess button press
  observeEvent(eventExpr = input$submitGuess,
               handlerExpr = {
                 # clear old marker(s) (so they don't build up on repeat clicks)
                 clearMarkers(map = leafletProxy("myMap"))
                 # add in circles for all sightings
                 addCircleMarkers(map = leafletProxy("myMap"),
                                  lat = ~lat,
                                  lng = ~lng,
                                  color = "blue",
                                  data = sightingsTables[[chosenDifficulty()]])
                 # add new one where the click was ( so it's on top)
                 addCircleMarkers(map = leafletProxy("myMap"),
                                  lat = chosenLat,
                                  lng = chosenLong,
                                  color = "red")
                 # change GuessStatus and DistanceFromGuess for that difficulty on scorecard
                 distanceOff <- checkGuessDistance(sightingsTables[[chosenDifficulty()]])
                 # pull the reactive df
                 sc <- reScorecard()
                 # modify it locally
                 sc$DistanceOff[sc$Level == chosenDifficulty()] <- distanceOff
                 sc$GuessStatus[sc$Level == chosenDifficulty()] <- 1
                 sc$GuessLat[sc$Level == chosenDifficulty()] <- chosenLat
                 sc$GuessLong[sc$Level == chosenDifficulty()] <- chosenLong
                 # push back to reactive df
                 reScorecard(sc)
                 # hide guess button
                 hide("submitGuess")
               })
  
  ## on reactive scorecard change - show/hide guess button based on guess status
  observeEvent(eventExpr = chosenDifficulty(),
               handlerExpr = {
                 ## check guess status
                 # pull the reactive df and check it
                 sc <- reScorecard()
                 scGuessStatus <- sc$GuessStatus[sc$Level == chosenDifficulty()]
                 # if not guessed yet, show button
                 if (scGuessStatus == 0) {
                   show("submitGuess")
                 } else {
                   hide("submitGuess")
                 }
               })
  
  ## conditional tracking if all guesses are made
  observeEvent(eventExpr = reScorecard(),
               handlerExpr = {
                 ## check sum of guess statuses
                 # pull the reactive df and check it
                 sc <- reScorecard()
                 scGuessSum <- sum(sc$GuessStatus)
                 # if all guesses in, display the scoreout
                 if (scGuessSum >= 5) {
                   show("scoreoutPanel")
                 }
               })
}


### UI ------------------------------------------------------------------------
ui <- fluidPage(
  # allow shinyjs usage
  useShinyjs(),
  ## sidebar layout setup
  sidebarLayout(
    # side panel with question stuff
    sidebarPanel(radioButtons(inputId = "challengeLevel", label = "Bird Rarity",
                              choices = difficultySettings,
                              selected = "Very Common"),
                 br(),
                 uiOutput("questionText"), # for the text/question/bird name
                 br(),
                 hidden(actionButton(inputId = "submitGuess", label = "Guess"))), # for the guess button; option to hide
    # main panel with map
    mainPanel(leafletOutput("myMap")) # for the map
  ),
  ## pop up layout below when five guesses made
  hidden( mainPanel( id = "scoreoutPanel",
                     uiOutput("myScoreout") ) )
)


### app -----------------------------------------------------------------------
# run the application 
shinyApp(ui = ui, server = server)