## Jacob Richey
## playerElo App
## August 2019

require(shiny)
require(tidyverse)
require(DT)
require(baseballr)
setwd("~/Desktop/Jacob/Personal R/playerElo_App")

# Load previously calculated park factors, calculated playerElos for 2018,
# and a matrix of quadratic coefficients based on game state to be used in playerElo calculations
parkFactors <- read_csv("data/ParkFactors.csv")
b18Elo <- read_csv("data/b18Elo.csv") %>%
  mutate(player_id = as.character(player_id))
p18Elo <- read_csv("data/p18Elo.csv") %>%
  mutate(player_id = as.character(player_id))
stateMatrix <- read.csv("data/stateMatrix.csv")

# compile and clean data to a usable format
pbp.compile <- function(data) {
  data <- data[-(1:2), 
               ][ ,c("V1","V18","V19","V20","V21","V22","V23","V24","V25","V26","V27",
                     "V28","V29","V30","V31","V32","V33","V34","V35"):=NULL]
  names(data) <- c("GameID", "Date", "Inning", "Road_Score", "Home_Score",
                   "Batting_Team", "Batter", "Batter_MLB_ID", "Batter_Hand",
                   "Runner1B", "Runner2B", "Runner3B", "Pitching_Team",
                   "Pitcher", "Pitcher_MLB_ID", "Pitcher_Hand", "Play_Type",
                   "Runs_OnPlay", "Outs_OnPlay", "SB", "CS", "FC", "PB", "WP", "Description")
  # remove all plays in which there is not a batted ball event
  data <- data[Play_Type != '' & Play_Type != 'CATCHER INTERFERENCE' & Play_Type != 'FAN INTERFERENCE']
  # correct two instances of inaccurate scoring
  data$Play_Type[7558] <- 'FIELD ERROR'
  data$Description[7558] <- 'Eric Stamets attempted sac bunt, reached on error. Naquin to 3B'
  data$Outs_OnPlay[7558] <- ''
  data$Play_Type[7731] <- ''
  data$Description[7731] <- 'Andrew Benintendi attempted sac bunt, reached on FC. Betts to 2B. Benintendi to 1B'
  data$Outs_OnPlay[7731] <- ''
  return(data)
}
# compute run value of every play (inspired by work of Jim Albert and Max Marchi)
compute.runs.value <- function(data) { 
  data[, c('Road_Score','Home_Score','Runs_OnPlay','Outs_OnPlay') := lapply(.SD, as.numeric),
       .SDcols = c('Road_Score','Home_Score','Runs_OnPlay','Outs_OnPlay')
       ][, c('Runs_OnPlay','Outs_OnPlay') := lapply(.SD, function(x){ifelse(is.na(x), 0, x)}), 
         .SDcols = c('Runs_OnPlay','Outs_OnPlay')
         ]
  data[, Outs_Before := cumsum(Outs_OnPlay) - Outs_OnPlay, by = .(GameID, Inning)
       ][, Outs_After := Outs_Before + Outs_OnPlay]
  
  cols <- c("Runner1B","Runner2B","Runner3B")
  anscols <- paste(cols, "After", sep="_")
  data[, (anscols) := shift(.SD, 1, 0, "lead"), .SDcols=cols]
  
  get.state <- function(runner1, runner2, runner3, outs) {
    runners <- paste(runner1, runner2, runner3, sep = "")
    paste(runners, outs)
  }
  
  RUNNER1 <- ifelse(as.character(data[, Runner1B]) == "", 0, 1)
  RUNNER2 <- ifelse(as.character(data[, Runner2B]) == "", 0, 1)
  RUNNER3 <- ifelse(as.character(data[, Runner3B]) == "", 0, 1)
  NRUNNER1 <- ifelse(as.character(data[, Runner1B_After]) == "", 0, 1)
  NRUNNER2 <- ifelse(as.character(data[, Runner2B_After]) == "", 0, 1)
  NRUNNER3 <- ifelse(as.character(data[, Runner3B_After]) == "", 0, 1)
  
  data[, State_Before := get.state(RUNNER1, RUNNER2, RUNNER3, Outs_Before)
       ][, State_After := get.state(NRUNNER1, NRUNNER2, NRUNNER3, Outs_After)]
  
  RE = data.table(
    STATE = c("000 0", "000 1", "000 2", "001 0", "001 1", "001 2", 
              "010 0", "010 1", "010 2", "011 0", "011 1", "011 2", 
              "100 0", "100 1", "100 2", "101 0", "101 1", "101 2",
              "110 0", "110 1", "110 2", "111 0", "111 1", "111 2"),
    EX.RUNS = c(.5075024, 0.2713679, 0.1053122, 1.3891213, 0.9728302, 0.3566315, 
                1.1492537, 0.6856786, 0.3229104, 1.9654296, 1.4041317, 0.5583602, 
                0.8786512, 0.5246762, 0.2233199, 1.7736422, 1.1971584, 0.4843450, 
                1.4476052, 0.9289954, 0.4378484, 2.2099367, 1.5406000, 0.7514162)
  )
  
  data[RE, on =c("State_Before" = "STATE"), RE_State := EX.RUNS
       ][RE, on =c("State_After" = "STATE"), RE_NewState := EX.RUNS
         ][, c('RE_State','RE_NewState') := lapply(.SD, function(x){ifelse(is.na(x), 0, x)}), 
           .SDcols = c('RE_State','RE_NewState')
           ][, Runs_Value := RE_NewState - RE_State + Runs_OnPlay]
}

RE19 <- fread("data/08-18-2019-mlb-season-pbp-feed.csv")
RE19 <- pbp.compile(RE19)
RE19 <- compute.runs.value(RE19)

# find most recent team for each player
teamsIso19b <- RE19 %>%
  select(Batting_Team, Batter_MLB_ID, Date) %>%
  arrange(desc(Date)) %>%
  distinct(Batter_MLB_ID, .keep_all = TRUE)
teamsIso19p <- RE19 %>%
  select(Pitching_Team, Pitcher_MLB_ID, Date) %>%
  arrange(desc(Date)) %>%
  distinct(Pitcher_MLB_ID, .keep_all = TRUE)

# import savant expected stats, and add player team
b19 <- read_csv("data/savant-expected-stats-batters19.csv") %>%
  mutate(player_id = as.character(player_id)) %>%
  left_join(teamsIso19b, by = c("player_id" = "Batter_MLB_ID")) %>%
  select(first_name, last_name, year, Batting_Team, everything())
p19 <- read_csv("data/savant-expected-stats-pitchers19.csv") %>%
  mutate(player_id = as.character(player_id)) %>%
  left_join(teamsIso19p, by = c("player_id" = "Pitcher_MLB_ID")) %>%
  select(first_name, last_name, year, Pitching_Team, everything())

# compute standardized home field advantage for season
RE19Home <- filter(RE19, substr(Inning, 2, 2) == 'B')
RE19Away <- filter(RE19, substr(Inning, 2, 2) == 'T')
homeAdv <- mean(RE19Home$Runs_Value) - mean(RE19Away$Runs_Value)

# create df to compute 2019 Elo, including previous season Elo regressed 15% to mean, 
# savant stats, and player team
b19Elo <- data.frame(player_id = unique(RE19$Batter_MLB_ID)) %>%
  mutate(player_id = as.character(player_id)) %>%
  left_join(b18Elo, by = "player_id") %>%
  mutate(currentElo = ifelse(is.na(currentElo), 1000, ((currentElo - 1000) * 0.85) + 1000),
         preseasonElo = currentElo, PA.x = 0) %>%
  left_join(b19, by = "player_id") %>%
  select(player_id, preseasonElo, currentElo, first_name, last_name, Batting_Team, everything(), -Date)
p19Elo <- data.frame(player_id = unique(RE19$Pitcher_MLB_ID)) %>%
  mutate(player_id = as.character(player_id)) %>%
  left_join(p18Elo, by = "player_id") %>%
  mutate(currentElo = ifelse(is.na(currentElo), 1000, ((currentElo - 1000) * 0.85) + 1000),
         preseasonElo = currentElo, BFP.x = 0) %>%
  left_join(p19, by = 'player_id') %>%
  select(player_id, preseasonElo, currentElo, first_name, last_name, Pitching_Team, everything(), -Date)

# create dt to track playerElo progression
allPlayerElo <- data.table(
  playerID = NA,
  PA = NA,
  playerElo = NA
)

# compute playerElo
for (row in 1:nrow(RE19)) {
  # find playerIDs and current playerElo
  batterID <- RE19$Batter_MLB_ID[row]
  pitcherID <- RE19$Pitcher_MLB_ID[row]
  batterElo <- as.numeric(b19Elo$currentElo[which(b19Elo$player_id == batterID)])
  pitcherElo <- as.numeric(p19Elo$currentElo[which(p19Elo$player_id == pitcherID)])
  
  # add to plate appearance count
  b19Elo$PA.x[which(b19Elo$player_id == batterID)] <-
    b19Elo$PA.x[which(b19Elo$player_id == batterID)] + 1
  p19Elo$BFP.x[which(p19Elo$player_id == pitcherID)] <-
    p19Elo$BFP.x[which(p19Elo$player_id == pitcherID)] + 1
  
  # add playerElo for current at bat count to progression df
  allPlayerElo <- rbindlist(list(allPlayerElo,
                                 list(batterID,
                                      b19Elo$PA.x[which(b19Elo$player_id == batterID)],
                                      batterElo),
                                 list(pitcherID,
                                      p19Elo$BFP.x[which(p19Elo$player_id == pitcherID)],
                                      pitcherElo))
  )
  
  # compute expected runs value for matchup
  # stateMatrix values found by running elo computation many times, 
  matrixRow <- which(stateMatrix$State == RE19$State_Before[row])
  ex.rv.batter <- (stateMatrix$bA[matrixRow] * batterElo^2) +
    (stateMatrix$bB[matrixRow] * batterElo) + stateMatrix$bC[matrixRow]
  ex.rv.pitcher <- (stateMatrix$pA[matrixRow] * pitcherElo^2) +
    (stateMatrix$pB[matrixRow] * pitcherElo) + stateMatrix$pC[matrixRow]
  if (substr(RE19$Inning[row], 2, 2) == 'B') {
    ex.rv.batter <- ex.rv.batter + homeAdv
  }
  park.factor <- parkFactors$Park_Factor[
    which(parkFactors$Park == str_sub(RE19$GameID[row], -5, -3))]

  # compare expected RV to actual RV and update playerElo accordingly
  # stateMatrix coefficients computed by repeatadly running Elo calculation over the previous three years, 
  # and finding relationship between playerElo and performance in each run-out state
  rv.diff <- RE19$Runs_Value[row] - ((ex.rv.batter + ex.rv.pitcher) / 2) - park.factor
  bEloChange <- ((921.675 + (6046.370 * rv.diff) - batterElo) / 502)
  pEloChange <- ((965.754 - (4762.089 * rv.diff) - pitcherElo) / 502)
  
  # account for errors only if it still helps pitcher or hurts batter, otherwise disregard
  if (RE19$Play_Type[row] != 'FIELD ERROR' || RE19$Play_Type[row] == 'FIELD ERROR' && bEloChange < 0) {
    b19Elo$currentElo[which(b19Elo$player_id == batterID)] <- batterElo + bEloChange
  }
  if (RE19$Play_Type[row] != 'FIELD ERROR' || RE19$Play_Type[row] == 'FIELD ERROR' && pEloChange > 0) {
    p19Elo$currentElo[which(p19Elo$player_id == pitcherID)] <- pitcherElo + pEloChange
  }
  
  # completion percentage
  if (row %% 1000 == 0) {
    print(paste0(round(100 * row / nrow(RE19), 2), "%"))
  }
}

# load positions of players
batterStats19 <- read_csv("data/mlb-player-stats-Batters.csv") %>%
  separate(Player, into = c("first_name", "last_name"), sep = " ", extra = "merge") %>%
  select(first_name, last_name, Pos) %>%
  distinct(first_name, last_name, .keep_all= TRUE)
pitcherStats19 <- read_csv("data/mlb-player-stats-P.csv") %>%
  separate(Player, into = c("first_name", "last_name"), sep = " ", extra = "merge") %>%
  mutate(Pos = ifelse(G - GS < 5 | IP > 65, "SP", "RP")) %>%
  select(first_name, last_name, Pos, ERA, WHIP)

# load exit velo stats on players
batterVeloStats19 <- read_csv("data/savant-batter-exitvelo-19.csv") %>%
  mutate(player_id = as.character(player_id)) %>%
  rename(EV = avg_hit_speed,
         'HH%' = ev95percent) %>%
  select(player_id, EV, "HH%")
pitcherVeloStats19 <- read_csv("data/savant-pitcher-exitvelo-19.csv") %>%
  mutate(player_id = as.character(player_id)) %>%
  rename(EV = avg_hit_speed,
         'HH%' = ev95percent) %>%
  select(player_id, EV, "HH%")

# create new df to describe trends of players
allPlayerX <- allPlayerElo %>%
  mutate(PA = PA + 50) %>%
  arrange(desc(playerElo)) %>%
  distinct(playerID, PA, .keep_all = TRUE) %>%
  rename(Trend = playerElo)

# add positions and velo stats, clean data
b19EloX <- b19Elo %>%
  arrange(desc(currentElo)) %>%
  mutate(Rank = seq.int(nrow(b19Elo))) %>%
  left_join(batterStats19, by = c("first_name", "last_name")) %>%
  left_join(batterVeloStats19, by = 'player_id') %>%
  mutate(currentElo = round(currentElo, 0)) %>%
  rename(First_Name = first_name,
         Last_Name = last_name,
         Team = Batting_Team,
         Position = Pos,
         PA = pa,
         playerElo = currentElo,
         wOBA = woba,
         xwOBA = est_woba)
# add positions to players whose suffixes pose matching problems (e.g. Ronald Acuna Jr.)
b19EloX$Position[which(b19EloX$player_id == 660670)] <- "OF"
b19EloX$Position[which(b19EloX$player_id == 666971)] <- "OF"
b19EloX$Position[which(b19EloX$player_id == 670712)] <- "3B"
b19EloX$Position[which(b19EloX$player_id == 592261)] <- "OF"
b19EloX$Position[which(b19EloX$player_id == 596105)] <- "OF"
b19EloX$Position[which(b19EloX$player_id == 628356)] <- "3B"
# continue to clena and add labels to pitchers, include trend
b19EloX <- b19EloX %>%
  distinct(player_id, .keep_all = TRUE) %>%
  drop_na(EV) %>%
  mutate(Position = replace_na(Position, "P")) %>%
  left_join(allPlayerX, by = c("player_id" = "playerID", "PA")) %>%
  mutate(Trend = replace_na(Trend, 1000))
# select columns for display
b19EloDisplay <- b19EloX %>%
  unite("Name", First_Name, Last_Name, sep = " ") %>%
  mutate(Trend = round(playerElo - Trend, 0)) %>%
  select(Name, Team, Position, PA, EV, 
         "HH%", wOBA, xwOBA, Trend, playerElo)

# run same steps for pitchers
p19EloX <- p19Elo %>%
  arrange(desc(currentElo)) %>%
  mutate(Rank = seq.int(nrow(p19Elo))) %>%
  left_join(pitcherStats19, by = c("first_name", "last_name")) %>%
  left_join(pitcherVeloStats19, by = 'player_id') %>%
  mutate(currentElo = round(currentElo, 0)) %>%
  rename(First_Name = first_name,
         Last_Name = last_name,
         Team = Pitching_Team,
         Position = Pos,
         PA = pa,
         playerElo = currentElo,
         wOBA = woba,
         xwOBA = est_woba)
p19EloX$Position[which(p19EloX$player_id == 592346)] <- "SP"
p19EloX$Position[which(p19EloX$player_id == 605541)] <- "RP"
p19EloX$Position[which(p19EloX$player_id == 605479)] <- "RP"
p19EloX$Position[which(p19EloX$player_id == 595001)] <- "RP"
p19EloX$Position[which(p19EloX$player_id == 641482)] <- "RP"
p19EloX$Position[which(p19EloX$player_id == 597113)] <- "RP"
p19EloX <- p19EloX %>%
  distinct(player_id, .keep_all = TRUE) %>%
  drop_na(EV) %>%
  left_join(allPlayerX, by = c("player_id" = "playerID", "PA")) %>%
  mutate(Trend = replace_na(Trend, 1000))
p19EloDisplay <- p19EloX %>%
  unite("Name", First_Name, Last_Name, sep = " ") %>%
  mutate(Trend = round(playerElo - Trend, 0),
         Trend = ifelse(Trend < 0, as.character(Trend), paste0("+", Trend))) %>%
  select(Name, Team, Position, PA, EV, 
         "HH%", wOBA, xwOBA, Trend, playerElo)

# load team records from baseballr package
al_records <- data.frame(matrix(unlist(standings_on_date_bref(date = "2019-08-15", division = "AL Overall")), 
                                nrow=15), stringsAsFactors=FALSE) %>%
  select("Team" = 1, "W" = 2, "L" = 3, "WPct" = 4, "pythWPct" = 8)
nl_records <- data.frame(matrix(unlist(standings_on_date_bref(date = "2019-08-15", division = "NL Overall")), 
                                nrow=15), stringsAsFactors=FALSE) %>%
  select("Team" = 1, "W" = 2, "L" = 3, "WPct" = 4, "pythWPct" = 8)
team_records <- rbind(al_records, nl_records) %>%
  arrange(Team)
team_records$Team <- c("Arizona Diamondbacks", "Atlanta Braves", "Baltimore Orioles",
                       "Boston Red Sox", "Chicago Cubs", "Chicago White Sox", "Cincinnati Reds",
                       "Cleveland Indians", "Colorado Rockies", "Detroit Tigers", "Houston Astros",
                       "Kansas City Royals", "Los Angeles Angels", "Los Angeles Dodgers", "Miami Marlins",
                       "Milwaukee Brewers", "Minnesota Twins", "New York Mets", "New York Yankees",
                       "Oakland Athletics", "Philadelphia Phillies", "Pittsburgh Pirates", "San Diego Padres",
                       "Seattle Mariners", "San Francisco Giants", "St. Louis Cardinals", "Tampa Bay Rays",
                       "Texas Rangers", "Toronto Blue Jays", "Washington Nationals")

# calculate team playerEloand get runs scored / against stats, and summarize in team df
b19EloTeam <- b19EloX %>%
  mutate(eloWeight = playerElo * PA) %>%
  group_by(Team) %>%
  summarise(TeamBattingElo = sum(eloWeight) / sum(PA)) %>%
  drop_na()
p19EloTeam <- p19EloX %>%
  mutate(eloWeight = playerElo * PA) %>%
  group_by(Team) %>%
  summarise(TeamPitchingElo = sum(eloWeight) / sum(PA)) %>%
  drop_na()
RS19Team <- RE19 %>%
  group_by(Batting_Team) %>%
  summarise(Runs_Scored = sum(Runs_OnPlay))
RA19Team <- RE19 %>%
  group_by(Pitching_Team) %>%
  summarise(Runs_Against = sum(Runs_OnPlay))
eloTeam19 <- b19EloTeam %>%
  left_join(p19EloTeam, by = "Team") %>%
  left_join(RS19Team, by = c("Team" = "Batting_Team")) %>%
  left_join(RA19Team, by = c("Team" = "Pitching_Team")) %>%
  left_join(team_records, by = "Team")

# create model for pythWPctand team playerElo, and use coefficients to weight batting vs. pitching
model <- lm(pythWPct ~ TeamBattingElo + TeamPitchingElo, data = eloTeam19)
eloTeam19 <- eloTeam19 %>%
  mutate(aggTeamElo = ((model$coefficients[2] / (model$coefficients[2] + model$coefficients[3])) * TeamBattingElo) +
           ((model$coefficients[3] / (model$coefficients[2] + model$coefficients[3])) * TeamPitchingElo),
         pythWPct = signif(as.numeric(pythWPct), 3),
         WPct = signif(as.numeric(WPct), 3),
         TeamBattingElo = round(TeamBattingElo, 0),
         TeamPitchingElo = round(TeamPitchingElo, 0),
         aggTeamElo = round(aggTeamElo, 0)) %>%
  arrange(desc(aggTeamElo)) %>%
  select(Team, TeamBattingElo, TeamPitchingElo, aggTeamElo, pythWPct, everything()) %>%
  rename('Batting Elo' = TeamBattingElo,
         'Pitching Elo' = TeamPitchingElo,
         'Team Elo' = aggTeamElo,
         'Runs Scored' = Runs_Scored,
         'Runs Against' = Runs_Against,
         'Pythagorean WPct' = pythWPct,
         "Wins" = W,
         "Losses" = L)

# clean and modify playerElo progression df, add names and boolean isPitcher variable 
isolateBNames <- b19Elo %>%
  select(player_id, first_name, last_name)
isolatePNames <- p19Elo %>%
  select(player_id, first_name, last_name)
allPlayerEloX <- allPlayerElo %>%
  slice(1:n()) %>%
  left_join(isolateBNames, by = c("playerID" = "player_id")) %>%
  left_join(isolatePNames, by = c("playerID" = "player_id")) %>%
  mutate(isPitcher = !is.na(last_name.y),
         Name = ifelse(isPitcher, paste(first_name.y, last_name.y),
                       paste(first_name.x, last_name.x))) %>%
  arrange(PA) %>%
  group_by(playerID, PA) %>%
  filter(playerElo == max(playerElo)) %>%
  ungroup() %>%
  mutate(isPitcher = factor(isPitcher, levels = c(T, F),
                            labels = c("Pitcher", "Batter"))) %>%
  distinct(playerID, Name, .keep_all = TRUE) %>%
  select(playerID, PA, playerElo, Name)

# write ui for shiny app
ui <- fluidPage(
  tags$strong(tags$h1("playerElo 2019")),
  tags$em(tags$h5("Jacob Richey | University of Pennsylvania | Updated as of August 18th")),
  
  tabsetPanel(
    tabPanel("playerElo Ranks", 
             tags$em(tags$h6("Column Header Notes: EV = Exit Velocity,
                             HH% = Hard Hit Percentage, Trend = playerElo 50 PA ago")),
             fluidRow(
               column(4,
                      selectInput("pos",
                                  "Position",
                                  c("Batters", "Pitchers", "C", "1B", "2B", "SS", "3B",
                                    "OF", "DH", "SP", "RP"),
                                  "Batters")
               ),
               column(4,
                      selectInput("team",
                                  "Team",
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
             ),
             # Create a new row for the table.
             DT::dataTableOutput("playerElo")),
    tabPanel("teamElo Ranks", 
             DT::dataTableOutput("teamElo")),
    tabPanel("playerElo Graphically",
             selectInput(inputId = "plySelect", label = "Player", choices = player_map$Name, 
                         multiple = T, selected = NULL),
             tags$em("Please allow a few moments for the graph to update."),
             tags$p(""),
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
                    columnDefs = list(list(className = 'dt-center', targets = 3:10)))))
  
  output$teamElo <-
    renderDT(
    eloTeam19,
      options = list(
        lengthChange = FALSE,
        pageLength = 30,
        columnDefs = list(list(
          className = 'dt-center', targets = 2:10
        ))
      )
    )

  output$graph <- renderPlot({
    player_highlight <- input$plySelect
    player_ids <- allPlayerEloX[match(player_highlight, allPlayerEloX$Name), ]
    
    background_data <- filter(positions, !playerID %in% player_ids)
    highlight_data <- filter(positions, playerID %in% player_ids)
    last_point <- highlight_data %>%
      group_by(playerID) %>%
      filter(PA == max(PA))
    
    graph <- ggplot(background_data) +
      geom_path(aes(PA, playerElo, group = playerID), alpha = 0.1) +
      geom_path(data = highlight_data, aes(PA, playerElo, group = playerID, color = Name),
                size = 1.3) +
      geom_hline(data = highlight_data, aes(yintercept = 980), size = 1.1) +
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
