## Jacob Richey
## playerElo App
## August 2019

library(shiny)
library(DT)
library(rsconnect)
library(tidyverse)

# load data found in appconstruction.R
b19EloDisplay <- read_csv("data/b19EloDisplay.csv")
p19EloDisplay <- read_csv("data/p19EloDisplay.csv")
eloTeam19 <- read_csv("data/eloTeam19.csv")
allPlayerEloX <- read_csv("data/allPlayerEloX.csv")

# compute RGB color quantiles based on playerElo/teamElo for output aesthetics
allElo <- rbind(b19EloDisplay, p19EloDisplay)
brksPlayer <- quantile(allElo$playerElo, probs = seq(.05, .95, .05), na.rm = TRUE)
brksTeam <- quantile(eloTeam19$`Team Elo`, probs = seq(.05, .95, .05), na.rm = TRUE)
clrNum <- round(seq(120, 255, length.out = 10), 0)
clrs <- c(paste0("rgb(255,", clrNum, ",", clrNum, ")"), paste0("rgb(", 355-clrNum, ",", 355-clrNum, ",255)"))

# write ui for shiny app
ui <- fluidPage(
  tags$strong(tags$h1("playerElo 2019"), 
              tags$h3("Factoring Context of Performance and Quality of Competition into MLB Player Analysis")),
  tags$em(tags$h5("Jacob Richey, Professor Abraham Wyner | University of Pennsylvania | Updated as of September 4th")),
  
  tabsetPanel(
    tabPanel("playerElo Ranks", 
             tags$em(tags$h6("Column Header Notes: EV = Exit Velocity,
                             HH% = Hard Hit Percentage, Trend = Difference of playerElo 50 PA ago")),
             tags$p(tags$em(tags$h6("Colors are on a 5% quantile basis"))),
             fluidRow(column(4, selectInput("pos", "Position", 
                                            c("Batters", "Pitchers", "C", "1B", "2B", "SS", 
                                              "3B", "OF", "DH", "SP", "RP"), selected = "Batters")),
                      column(4, selectInput("team", "Team",
                                            c("All", "Arizona Diamondbacks", "Atlanta Braves", "Baltimore Orioles",
                                              "Boston Red Sox", "Chicago White Sox", "Chicago Cubs", "Cincinnati Reds",
                                              "Cleveland Indians", "Colorado Rockies", "Detroit Tigers", "Houston Astros",
                                              "Kansas City Royals", "Los Angeles Angels", "Los Angeles Dodgers", "Miami Marlins",
                                              "Milwaukee Brewers", "Minnesota Twins", "New York Yankees", "New York Mets",
                                              "Oakland Athletics", "Philadelphia Phillies", "Pittsburgh Pirates", "San Diego Padres",
                                              "San Francisco Giants", "Seattle Mariners", "St. Louis Cardinals", "Tampa Bay Rays",
                                              "Texas Rangers", "Toronto Blue Jays", "Washington Nationals"))
                      ),
                      column(4,
                             selectInput("pa",
                                         ">PA",
                                         c(1, 25, 50, 100, 200, 300, 400, 500),
                                         100)
                      )
             ), DT::dataTableOutput("playerElo")),
    tabPanel("teamElo Ranks", 
             DT::dataTableOutput("teamElo")),
    tabPanel("playerElo Graphically",
             selectInput(inputId = "plySelect", label = "Player", choices = sort(unique(allPlayerEloX$Name)), 
                         multiple = T, selected = NULL),
             tags$em("Please allow a few moments for the graph to update. 
                     The bold horizontal line denotes the league average playerElo (roughly 980)."),
             plotOutput("graph", height = 600))
  )
)

# write server for shiny app
server <- function(input, output) {
  # Filter data based on selections
  output$playerElo <- DT::renderDataTable(DT::datatable({
    if (input$pos == "Batters") {
      data <- b19EloDisplay
    } else if (input$pos == "Pitchers") {
      data <- p19EloDisplay
    } else if (input$pos %in% c("C", "1B", "2B", "SS", "3B", "OF", "DH")) {
      data <- b19EloDisplay[b19EloDisplay$Position == input$pos, ]
    } else {
      data <- p19EloDisplay[p19EloDisplay$Position == input$pos, ]
    }
    if (input$team != "All") {
      data <- data[data$Team == input$team, ]
    }
    data <- data[data$PA > as.numeric(input$pa), ] 
    
  }, options = list(lengthChange = FALSE, pageLength = 100,
                    columnDefs = list(list(className = 'dt-center', targets = 3:10)))) %>%
    formatRound(c("EV", "HH%"), 1) %>%
    formatRound(c("wOBA", "xwOBA"), 3) %>%
    formatStyle('playerElo', backgroundColor = styleInterval(brksPlayer, clrs))
  )
  
  output$teamElo <-
    renderDT({
      datatable(eloTeam19, options = list(
        lengthChange = FALSE,
        pageLength = 30,
        columnDefs = list(list(
          className = 'dt-center', targets = 2:10
        ))
      )) %>%
        formatRound(c("Pythagorean WPct", "WPct"), 3) %>%
        formatStyle(c('Team Elo', 'Batting Elo', 'Pitching Elo'), 
                    backgroundColor = styleInterval(brksTeam, clrs))
    })
  
  output$graph <- renderPlot({
    player_highlight <- input$plySelect
    player_ids <- allPlayerEloX[match(player_highlight, allPlayerEloX$Name), ]
    
    background_data <- filter(allPlayerEloX, !playerID %in% player_ids$playerID)
    highlight_data <- filter(allPlayerEloX, playerID %in% player_ids$playerID)
    last_point <- highlight_data %>%
      group_by(playerID) %>%
      filter(PA == max(PA))
    
    graph <- ggplot(background_data) +
      geom_path(aes(PA, playerElo, group = playerID), alpha = 0.1) +
      geom_path(data = highlight_data, aes(PA, playerElo, group = playerID, color = Name),
                size = 1.3) +
      geom_hline(yintercept = 980, size = 1.1) +
      geom_label(data = last_point, aes(PA, playerElo, group = playerID, color = Name,
                                        label = paste(Name, round(playerElo, 0), sep = ": ")), hjust = "inward") +
      labs(title = "2018-2019 MLB playerElo Progress by Plate Appearance",
           x = "Plate Appearance", y = "playerElo Rating") +
      theme_classic() +
      theme(legend.position = "none",
            strip.text = element_text(face = "bold", size = 10),
            strip.background = element_blank(),
            plot.title = element_text(face = "bold", size = 16),
            axis.title = element_text(face = "bold"),
            axis.ticks = element_blank()) +
      scale_x_continuous(expand = c(0.01, 0.01))
    graph
  })
  
}

shinyApp(ui = ui, server = server)

