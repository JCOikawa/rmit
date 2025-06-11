
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(sf)
library(leaflet)
library(DT)


# 2022 Election Data
seats_2022 <- read.csv("HouseCandidatesDownload-27966.csv",skip = 1, header = TRUE)
first_prefs_party_2022 <- read.csv("HouseFirstPrefsByPartyDownload-27966.csv",skip = 1, header = TRUE)
tpp_div_2022 <- read.csv("HouseTppByDivisionDownload-27966.csv",skip = 1, header = TRUE)
changed_seats_2022 <- read.csv("HouseSeatsWhichChangedHandsDownload-27966.csv",skip = 1, header = TRUE)

# 2025 Election Data
seats_2025 <- read.csv("HouseCandidatesDownload-31496.csv",skip = 1, header = TRUE)
first_prefs_party_2025 <- read.csv("HouseFirstPrefsByPartyDownload-31496.csv",skip = 1, header = TRUE)
tpp_div_2025 <- read.csv("HouseTppByDivisionDownload-31496.csv",skip = 1, header = TRUE)
changed_seats_2025 <- read.csv("HouseSeatsWhichChangedHandsDownload-31496.csv",skip = 1, header = TRUE)

# Load shapefiles
divisions_2022 <- st_read("CED_2016_AUST.shp")
divisions_2025 <- st_read("CED_2016_AUST.shp")

seats_2022_winners <- seats_2022 %>%
  filter(Elected == "Y") %>%
  group_by(PartyNm) %>%
  summarise(Seats = n(), .groups = "drop")

seats_2025_winners <- seats_2025 %>%
  filter(Elected == "Y") %>%
  group_by(PartyNm) %>%
  summarise(Seats = n(), .groups = "drop")

tpp_div_2022_agg <- tpp_div_2022 %>%
  mutate(TPP_Percentage = ifelse(PartyAb == "ALP", `Australian.Labor.Party.Votes` / `TotalVotes` * 100, 
                                 `Liberal.National.Coalition.Votes` / `TotalVotes` * 100)) %>%
  group_by(StateAb, PartyAb, Year = "2022") %>%
  summarise(TPP_Percentage = mean(TPP_Percentage, na.rm = TRUE), .groups = "drop")

tpp_div_2025_agg <- tpp_div_2025 %>%
  mutate(TPP_Percentage = ifelse(PartyAb == "ALP", `Australian.Labor.Party.Votes` / `TotalVotes` * 100, 
                                 `Liberal.National.Coalition.Votes` / `TotalVotes` * 100)) %>%
  group_by(StateAb, PartyAb, Year = "2025") %>%
  summarise(TPP_Percentage = mean(TPP_Percentage, na.rm = TRUE), .groups = "drop")

# UI
ui <- fluidPage(
  titlePanel("Australian Federal Elections 2022-2025: Political Changes"),
  tabsetPanel(
    # Tab 1: Party Performance and Seat Changes
    tabPanel("Party Performance",
             fluidRow(
               column(4, style = "height: 100vh; overflow-y: auto;",
                      selectInput("party_seats", "Select Parties:", 
                                     choices = unique(c(seats_2022$PartyNm, seats_2025$PartyNm)), 
                                     multiple = TRUE, selected = c("Australian Labor Party", "Liberal")),
                      selectInput("year_map", "Select Year:", choices = c("2022", "2025")),
                      textOutput("party_performance_summary")
                      
             ),
             column(8, style = "height: 100vh; overflow-y: auto;",
                    plotlyOutput("seats_bar", height = "400px"),
                    leafletOutput("seats_map", height = "calc(100vh - 400px)")
             ))),
    # Tab 2: Voting Patterns and Preferences
    tabPanel("Voting Patterns",
             fluidRow(
               column(4, style = "height: 100vh; overflow-y: auto;",
                      selectInput("party_votes", "Select Parties:", 
                                     choices = unique(c(first_prefs_party_2022$PartyNm, first_prefs_party_2025$PartyNm)), 
                                     multiple = TRUE, selected = c("Australian Labor Party", "Liberal")),
                      selectInput("year_pie", "Select Year:", choices = c("2022", "2025")),
                      textOutput("voting_patterns_summary")
               ),
               column(8,style = "height: 100vh; overflow-y: auto;",
                      plotlyOutput("votes_bar", height = "400px"),
                      plotlyOutput("votes_pie", height = "calc(100vh - 400px)")
               ))),
             
    # Tab 3: Regional Analysis
    tabPanel("Seats That Changed Hands",
             fluidRow(
               column(4, style = "height: 100vh; overflow-y: auto;",
                      selectInput("state_changed", "Select State:", 
                                     choices = c("All", unique(changed_seats_2025$StateAb),selected ="VIC")),
                      textOutput("seat_changes_summary")),
               column(8,style = "height: 100vh; overflow-y: auto;",
                      leafletOutput("changed_seats_map", height = "500px"),
                      plotlyOutput("changed_seats_chart", height = "400px"),
                      DT::dataTableOutput("changed_seats_table", height = "calc(100vh - 900px)")
               ))),

    # Tab 4: References
    tabPanel("References",
             HTML("
             <h3>Data Sources</h3>
             <p>Australian Electoral Commission. (2022). 2022 federal election - House of Representatives downloads. <a href='https://results.aec.gov.au/27966/Website/HouseDownloadsMenu-27966-Csv.htm'>https://results.aec.gov.au/27966/Website/HouseDownloadsMenu-27966-Csv.htm</a></p>
             <p>Australian Electoral Commission. (2025). 2025 federal election - House of Representatives downloads. <a href='https://tallyroom.aec.gov.au/HouseDownloadsMenu-31496-Csv.htm'>https://tallyroom.aec.gov.au/HouseDownloadsMenu-31496-Csv.htm</a></p>
             <p>Australian Electoral Commission. (n.d.). Federal electoral boundary GIS data. <a href='https://www.aec.gov.au/electorates/gis/gis_datadownload.htm'>https://www.aec.gov.au/electorates/gis/gis_datadownload.htm</a></p>
             ")
    )
  )
)

# Server
server <- function(input, output) {
  # Tab 1: Party Performance
  output$party_performance_summary <- renderText({
    "TThis project utilizes interactive visualizations to showcase the political changes in Australia between the 2022 and 2025 federal elections, focusing on party seat performance and electoral division distribution. By selecting parties and years and clicking on the map, you can explore how shifts in voter preferences and regional differences have shaped the political landscape."
  })
  output$seats_bar <- renderPlotly({
    data <- rbind(
      seats_2022_winners %>% mutate(Year = "2022"),
      seats_2025_winners %>% mutate(Year = "2025")
    ) %>% filter(PartyNm %in% input$party_seats)
    plot_ly(data, x = ~PartyNm, y = ~Seats, color = ~Year, type = "bar") %>%
      layout(barmode = "group", title = "Seats Won by Party")
  })
  
  output$seats_map <- renderLeaflet({
    if (input$year_map == "2022") {
      data <- merge(divisions_2022, seats_2022 %>% filter(Elected == "Y") %>% distinct(DivisionNm, .keep_all = TRUE), 
                    by.x = "CED_NAME16", by.y = "DivisionNm", all.x = TRUE)
      data$PartyAb[is.na(data$PartyAb)] <- "No Data"
    } else {
      data <- merge(divisions_2025, seats_2025 %>% filter(Elected == "Y") %>% distinct(DivisionNm, .keep_all = TRUE), 
                    by.x = "CED_NAME16", by.y = "DivisionNm", all.x = TRUE)
      data$PartyAb[is.na(data$PartyAb)] <- "No Data"
    }
    
    # Dynamically generate colors based on unique parties
    unique_parties <- unique(data$PartyAb)
    party_colors <- colorFactor(
      palette = rainbow(length(unique_parties)), domain = unique_parties)
    
    # Create map
    map <- leaflet(data) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~party_colors(PartyAb),
        fillOpacity = 0.7,
        weight = 1.5,
        color = "black",
        popup = ~paste(CED_NAME16, PartyNm)
      )
    

    
    map <- map %>% addControl(html = "<h4>Electoral Division Winners</h4>", position = "topright",className = "map-title")  })
  
  output$changed_seats <- DT::renderDataTable({
    if (input$year_map == "2022") {
      changed_seats_2022
    } else {
      changed_seats_2025
    }
  })
  
  # Tab 2: Voting Patterns
  output$voting_patterns_summary <- renderText({
    "This tab displays voting patterns based on first preference votes. The bar chart shows the vote share for selected parties in 2022 and 2025, allowing comparison of party support over time. The pie chart illustrates the overall distribution of first preference votes for the selected year, highlighting shifts in voter preferences in Australia's political landscape."
  })
  output$votes_bar <- renderPlotly({
    data <- rbind(
      first_prefs_party_2022 %>% mutate(Year = "2022"),
      first_prefs_party_2025 %>% mutate(Year = "2025")
    ) %>% filter(PartyNm %in% input$party_votes)
    plot_ly(data, x = ~PartyNm, y = ~TotalPercentage, color = ~Year, type = "bar") %>%
      layout(barmode = "group", title = "First Preference Vote Share")
  })
  
  output$votes_pie <- renderPlotly({
    data <- if(input$year_pie == "2022") first_prefs_party_2022 else first_prefs_party_2025
    data <- data %>% filter(TotalPercentage > 1)
    plot_ly(data, labels = ~PartyNm, values = ~TotalPercentage, type = "pie") %>%
      layout(title = paste("Vote Distribution", input$year_pie))
  })
  
  
  # Tab 3: Seats Change Analysis
  output$seat_changes_summary <- renderText({
    total_changed <- nrow(changed_seats_2025)
    gained_by_party <- changed_seats_2025 %>%
      group_by(PartyAb) %>%
      summarise(Gained = n(), .groups = "drop")
    lost_by_party <- changed_seats_2025 %>%
      group_by(PreviousPartyAb) %>%
      summarise(Lost = n(), .groups = "drop")
    changes_by_state <- changed_seats_2025 %>%
      group_by(StateAb) %>%
      summarise(Changes = n(), .groups = "drop")
    
    summary_text <- paste(
      "Between the 2022 and 2025 federal elections, a total of", total_changed, "seats changed hands, reflecting significant shifts in Australia's political landscape.\n",
      "The Australian Labor Party (ALP) gained", 
      ifelse(any(gained_by_party$PartyAb == "ALP"), gained_by_party$Gained[gained_by_party$PartyAb == "ALP"], 0), "seats.\n",
      "The Liberal Party (LP) gained", 
      ifelse(any(gained_by_party$PartyAb == "LP"), gained_by_party$Gained[gained_by_party$PartyAb == "LP"], 0), "seats.\n",
      "The states with the most seat changes were:\n",
      paste(changes_by_state$StateAb, "with", changes_by_state$Changes, "changes", sep = " ", collapse = "\n"),
      paste("\nPositive Aspects:\n",
      "- The Australian Labor Party (ALP) gained seats in states like Victoria (VIC) and New South Wales (NSW), indicating voter support for its policies.\n",
      "- The rise of independent candidates (e.g., IND) suggests a reevaluation of traditional parties, fostering political diversity.\n",
      "Negative Risks:\n",
      "- The Liberal Party (LP) and National Party (NP) lost seats in traditional strongholds, potentially weakening the coalition's influence.\n",
      "- Uneven regional distribution of seat changes, with stability in Queensland (QLD) and Western Australia (WA) but volatility in VIC and NSW, could lead to parliamentary division or policy-making challenges.\n",
      "Overall, these changes highlight both the vitality of Australian politics and the challenges traditional parties face in adapting to voter demands."
    ))
    summary_text
  })
  
  data <- merge(divisions_2025, changed_seats_2025, by.x = "CED_NAME16", by.y = "DivisionNm", all.x = TRUE)
  output$changed_seats_map <- renderLeaflet({
    if (input$state_changed == "All") {
      data_filtered <- data
    } else {
      data_filtered <- data %>% filter(StateAb == input$state_changed)
    }
    leaflet(data_filtered) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~ifelse(!is.na(PreviousPartyAb), "red", "gray"),
        fillOpacity = 0.7,
        weight = 1,
        color = "black",
        popup = ~paste(CED_NAME16, ifelse(!is.na(PreviousPartyNm), paste("Changed from", PreviousPartyNm, "to", PartyNm), "No change"))
      )
  })
  
  output$changed_seats_chart <- renderPlotly({
    changed_by_state <- changed_seats_2025 %>% 
      group_by(StateAb) %>% 
      summarise(Count = n(), .groups = "drop")
    plot_ly(changed_by_state, x = ~StateAb, y = ~Count, type = "bar") %>%
      layout(title = "Number of Seats Changed Hands by State", xaxis = list(title = "State"), yaxis = list(title = "Count"))
  })
  
  output$changed_seats_table <- DT::renderDataTable({
    if (input$state_changed == "All") {
      changed_seats_2025 %>% select(DivisionNm, StateAb, PreviousPartyAb, PartyAb)
    } else {
      changed_seats_2025 %>% filter(StateAb == input$state_changed) %>% select(DivisionNm, StateAb, PreviousPartyAb, PartyAb)
    }
  })
}

# Run the app
shinyApp(ui, server)