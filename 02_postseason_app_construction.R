## playerElo Computation
## October 2019
## 02_postseason_app_construction
## Data sourced from Baseball Savant, RotoWire, 
## Baseball Reference, Retrosheet, and BigDataBall.

####### POSTSEASON ########################################################

# Load data found in 01_regular_app_construction.R.
b19Elo_dispID <- read_csv("data/b19Elo_dispID.csv") %>%
  mutate(player_id = as.character(player_id)) %>%
  select(player_id, playerElo)
p19Elo_dispID <- read_csv("data/p19Elo_dispID.csv") %>%
  mutate(player_id = as.character(player_id)) %>%
  select(player_id, playerElo)
teamElo19 <- read_csv("data/teamElo19.csv")
all_playerElo <- read_csv("data/all_playerElo.csv")
park_factors <- read_csv("data/ParkFactors.csv")
state_matrix <- read.csv("data/stateMatrix.csv")

# Clean data to a usable format.
pbp_compile_PS <- function(data) {
  data <- data[-(1:2), 
               ][, c("V18", "V19", "V20", "V21", "V22", "V23", "V24", 
                     "V25", "V26", "V27", "V28", "V29", "V30", "V31", 
                     "V32", "V33", "V34", "V35", "V45") := NULL]
  names(data) <- c("Dataset", "GameID", "Date", "Inning", "Road_Score", "Home_Score",
                   "Batting_Team", "Batter", "Batter_MLB_ID", "Batter_Hand",
                   "Runner1B", "Runner2B", "Runner3B", "Pitching_Team", 
                   "Pitcher", "Pitcher_MLB_ID", "Pitcher_Hand", "Play_Type",
                   "Runs_OnPlay", "Outs_OnPlay", "SB", "CS", "FC", "PB", "WP", 
                   "Description")
  # Remove all plays in which there is not a batted ball event
  data <- data[Play_Type != "" &
                 Play_Type != "CATCHER INTERFERENCE" &
                 Play_Type != "FAN INTERFERENCE"]
  # Correct instances of inaccurate scoring
  data$Outs_OnPlay[1195] <- "2"
  data$Play_Type[1961] <- "FIELD ERROR"
  data$Outs_OnPlay[1961] <- ""
  data$Play_Type[7307] <- "FIELD ERROR"
  data$Outs_OnPlay[7307] <- ""
  data$Play_Type[7475] <- "FIELD ERROR"
  data$Outs_OnPlay[7475] <- ""
  data$Outs_OnPlay[9467] <- "2"
  data
}

# Compute run value of every play, 
# inspired by work of Jim Albert and Max Marchi.
compute_rv <- function(data) { 
  # Add additional variables recording game state.
  data[, c("Road_Score", "Home_Score", "Runs_OnPlay", "Outs_OnPlay") := 
         lapply(.SD, as.numeric), 
       .SDcols = c("Road_Score", "Home_Score", "Runs_OnPlay", "Outs_OnPlay")
       ][, c("Runs_OnPlay", "Outs_OnPlay") := 
           lapply(.SD, function(x) { ifelse(is.na(x), 0, x) }),
         .SDcols = c("Runs_OnPlay", "Outs_OnPlay")]
  data[, Outs_Before := cumsum(Outs_OnPlay) - Outs_OnPlay, 
       by = .(GameID, Inning)][, Outs_After := Outs_Before + Outs_OnPlay]
  
  # Find change in runner positions after play.
  cols <- c("Runner1B", "Runner2B", "Runner3B")
  anscols <- paste(cols, "After", sep = "_")
  data[, (anscols) := shift(.SD, 1, 0, "lead"), .SDcols=cols]
  
  # Get base-out state for any play.
  get_state <- function(runner1, runner2, runner3, outs) {
    runners <- paste(runner1, runner2, runner3, sep = "")
    paste(runners, outs)
  }
  
  # Use binary numbers to represent runner positions.
  RUNNER1 <- ifelse(as.character(data[, Runner1B]) == "", 0, 1)
  RUNNER2 <- ifelse(as.character(data[, Runner2B]) == "", 0, 1)
  RUNNER3 <- ifelse(as.character(data[, Runner3B]) == "", 0, 1)
  NRUNNER1 <- ifelse(as.character(data[, Runner1B_After]) == "", 0, 1)
  NRUNNER2 <- ifelse(as.character(data[, Runner2B_After]) == "", 0, 1)
  NRUNNER3 <- ifelse(as.character(data[, Runner3B_After]) == "", 0, 1)
  
  # Compute base-out state before and after play.
  data[, State_Before := get_state(RUNNER1, RUNNER2, RUNNER3, Outs_Before)
       ][, State_After := get_state(NRUNNER1, NRUNNER2, NRUNNER3, Outs_After)]
  
  # Load expected runs matrix (found using 2016-2018 Retrosheet pbp data).
  RE <- data.table(
    STATE = c("000 0", "000 1", "000 2", "001 0", "001 1", "001 2", 
              "010 0", "010 1", "010 2", "011 0", "011 1", "011 2", 
              "100 0", "100 1", "100 2", "101 0", "101 1", "101 2",
              "110 0", "110 1", "110 2", "111 0", "111 1", "111 2"),
    EX.RUNS = c(0.5075024, 0.2713679, 0.1053122, 1.3891213, 0.9728302, 
                0.3566315, 1.1492537, 0.6856786, 0.3229104, 1.9654296, 
                1.4041317, 0.5583602, 0.8786512, 0.5246762, 0.2233199, 
                1.7736422, 1.1971584, 0.4843450, 1.4476052, 0.9289954, 
                0.4378484, 2.2099367, 1.5406000, 0.7514162)
  )
  
  # Compute run value of every play.
  data[RE, on = c("State_Before" = "STATE"), RE_State := EX.RUNS
       ][RE, on = c("State_After" = "STATE"), RE_NewState := EX.RUNS
         ][, c("RE_State","RE_NewState") := 
             lapply(.SD, function(x) { ifelse(is.na(x), 0, x) }), 
           .SDcols = c("RE_State","RE_NewState")
           ][, Runs_Value := RE_NewState - RE_State + Runs_OnPlay]
}

# Load play-by-play data for 2019 postseason (source: Retrosheet or BigDataBall).
RE19 <- fread("data/10-30-2019-mlb-season-pbp-feed.csv")
RE19 <- pbp_compile_PS(RE19)
RE19 <- compute_rv(RE19)

# Find most recent team for each player.
batter_teams <- RE19 %>%
  select(Batting_Team, Batter_MLB_ID, Date) %>%
  arrange(desc(Date)) %>%
  distinct(Batter_MLB_ID, .keep_all = TRUE)

pitcher_teams <- RE19 %>%
  select(Pitching_Team, Pitcher_MLB_ID, Date) %>%
  arrange(desc(Date)) %>%
  distinct(Pitcher_MLB_ID, .keep_all = TRUE)

# Import expected stats, and add player team (source: Baseball Savant).
b19 <- read_csv("data/PS_expected_stats.csv", 
                col_types = list(player_id = col_character())) %>%
  left_join(batter_teams, by = c("player_id" = "Batter_MLB_ID")) %>%
  select(first_name, last_name, year, Batting_Team, everything())

p19 <- read_csv("data/PS_expected_stats-2.csv", 
                col_types = list(player_id = col_character())) %>%
  left_join(pitcher_teams, by = c("player_id" = "Pitcher_MLB_ID")) %>%
  select(first_name, last_name, year, Pitching_Team, everything())

# Compute standardized home field advantage based on run value.
RE19_home <- filter(RE19, substr(Inning, 2, 2) == "B")
RE19_away <- filter(RE19, substr(Inning, 2, 2) == "T")
home_adv <- mean(RE19_home$Runs_Value) - mean(RE19_away$Runs_Value)

# Filter only postseason games
RE19PS <- filter(RE19, Dataset == "MLB 2019 Playoffs")

# Create df to update 2019 playerElo, including previous season Elo regressed 
# 15% to mean, savant stats, and player team.
b19EloPS <- data.frame(player_id = unique(RE19PS$Batter_MLB_ID),
                       stringsAsFactors = F) %>%
  left_join(b19Elo_dispID, by = "player_id") %>%
  mutate(regSeasonElo = playerElo,
         avgEloFaced = 0,
         PA.x = 0) %>%
  left_join(b19, by = "player_id") %>%
  rename(postseasonElo = playerElo) %>%
  mutate(postseasonElo = ifelse(is.na(postseasonElo), 1000, postseasonElo),
         regSeasonElo = ifelse(is.na(regSeasonElo), 1000, regSeasonElo)) %>%
  select(player_id, regSeasonElo, postseasonElo, first_name, 
         last_name, Batting_Team, everything(), -Date)

p19EloPS <- data.frame(player_id = unique(RE19PS$Pitcher_MLB_ID),
                     stringsAsFactors = F) %>%
  left_join(p19Elo_dispID, by = "player_id") %>%
  mutate(regSeasonElo = playerElo,
         avgEloFaced = 0,
         BFP.x = 0) %>%
  left_join(p19, by = "player_id") %>%
  rename(postseasonElo = playerElo) %>%
  mutate(postseasonElo = ifelse(is.na(postseasonElo), 1000, postseasonElo),
         regSeasonElo = ifelse(is.na(regSeasonElo), 1000, regSeasonElo)) %>%
  select(player_id, regSeasonElo, postseasonElo, first_name, 
         last_name, Pitching_Team, everything(), -Date)

# Compute playerElo.
for (row in 1:nrow(RE19PS)) {
  # Get playerIDs and current playerElo.
  batterID <- RE19PS$Batter_MLB_ID[row]
  pitcherID <- RE19PS$Pitcher_MLB_ID[row]
  batterElo <- b19EloPS$postseasonElo[which(b19EloPS$player_id == batterID)]
  pitcherElo <- p19EloPS$postseasonElo[which(p19EloPS$player_id == pitcherID)]
  
  # Add to PA or BFP count.
  b19EloPS$PA.x[which(b19EloPS$player_id == batterID)] <-
    b19EloPS$PA.x[which(b19EloPS$player_id == batterID)] + 1
  p19EloPS$BFP.x[which(p19EloPS$player_id == pitcherID)] <-
    p19EloPS$BFP.x[which(p19EloPS$player_id == pitcherID)] + 1
  
  b19EloPS$avgEloFaced[which(b19EloPS$player_id == batterID)] <-
    b19EloPS$avgEloFaced[which(b19EloPS$player_id == batterID)] + pitcherElo
  p19EloPS$avgEloFaced[which(p19EloPS$player_id == pitcherID)] <-
    p19EloPS$avgEloFaced[which(p19EloPS$player_id == pitcherID)] + batterElo
  
  # Compute expected runs value for matchup, based on current base-out
  # state and playerElo of batter and pitcher.
  matrix_row <- which(state_matrix$State == RE19PS$State_Before[row])
  xRV_batter <- (state_matrix$bA[matrix_row] * batterElo^2) +
    (state_matrix$bB[matrix_row] * batterElo) + state_matrix$bC[matrix_row]
  xRV_pitcher <- (state_matrix$pA[matrix_row] * pitcherElo^2) +
    (state_matrix$pB[matrix_row] * pitcherElo) + state_matrix$pC[matrix_row]
  
  # Add in home field advantage (if applicable).
  if (substr(RE19PS$Inning[row], 2, 2) == "B") {
    xRV_batter <- xRV_batter + home_adv
  }
  
  # Find park factor.
  park_factor <- park_factors$Park_Factor[
    which(park_factors$Park == str_sub(RE19PS$GameID[row], -5, -3))]
  
  # Compare expected RV to actual RV.
  rv_diff <- RE19PS$Runs_Value[row] - 
    ((xRV_batter + xRV_pitcher) / 2) - park_factor
  
  # Compute Elo change, with formulas are reflective of relationship 
  # between wOBA or FIP and Run Value per PA or BFP.
  bElo_delta <- (921.675 + (6046.370 * rv_diff) - batterElo) / 70
  pElo_delta <- (965.754 - (4762.089 * rv_diff) - pitcherElo) / 70
  
  # Account for errors only if it still helps pitcher or hurts batter, 
  # otherwise disregard, and update playerElo of batter and pitcher.
  if (RE19PS$Play_Type[row] != "FIELD ERROR" || 
      RE19PS$Play_Type[row] == "FIELD ERROR" && bElo_delta < 0) {
    b19EloPS$postseasonElo[which(b19EloPS$player_id == batterID)] <- 
      batterElo + bElo_delta
  }
  if (RE19$Play_Type[row] != "FIELD ERROR" || 
      RE19$Play_Type[row] == "FIELD ERROR" && pElo_delta > 0) {
    p19EloPS$postseasonElo[which(p19EloPS$player_id == pitcherID)] <-
      pitcherElo + pElo_delta
  }
}

# Load positions of players (source: RotoWire).
batter_positions <- read_csv("data/mlb-player-stats-Batters.csv") %>%
  separate(Player, into = c("first_name", "last_name"), 
           sep = " ", extra = "merge") %>%
  select(first_name, last_name, Pos) %>%
  distinct(first_name, last_name, .keep_all= TRUE)

pitcher_positions <- read_csv("data/mlb-player-stats-P.csv") %>%
  separate(Player, into = c("first_name", "last_name"), 
           sep = " ", extra = "merge") %>%
  mutate(Pos = ifelse(G - GS < 10, "SP", "RP")) %>%
  select(first_name, last_name, Pos, ERA, WHIP, IP)

# Load exit velo stats on players (source: Baseball Savant).
batter_ev <- read_csv("data/PS_exit_velocity.csv", 
                      col_types = list(player_id = col_character())) %>%
  rename(EV = avg_hit_speed, 
         "HH%" = ev95percent) %>%
  select(player_id, EV, "HH%")
pitcher_ev <- read_csv("data/PS_exit_velocity-2.csv", 
                       col_types = list(player_id = col_character())) %>%
  rename(EV = avg_hit_speed,
         "HH%" = ev95percent) %>%
  select(player_id, EV, "HH%")

# Add positions and velo stats to playerElo df, clean data.
b19Elo_modPS <- b19EloPS %>%
  arrange(desc(postseasonElo)) %>%
  filter(player_id != '593372') %>%
  mutate(Rank = seq.int(nrow(b19EloPS)-1),
         avgEloFaced = round(avgEloFaced / PA.x, 1)) %>%
  left_join(batter_positions, by = c("first_name", "last_name")) %>%
  left_join(batter_ev, by = "player_id") %>%
  rename(First_Name = first_name,
         Last_Name = last_name,
         Team = Batting_Team,
         Position = Pos,
         PA = PA.x,
         wOBA = woba,
         xwOBA = est_woba)
# Continue to clean and add labels to pitchers, include trend over last 50 PA.
b19Elo_modPS <- b19Elo_modPS %>%
  distinct(player_id, .keep_all = TRUE) %>%
  drop_na(EV) %>%
  mutate(Position = replace_na(Position, "P"),
         PostTrend = round(postseasonElo - regSeasonElo), 0)
# Select columns for display in Shiny App.
b19Elo_dispPS <- b19Elo_modPS %>%
  unite("Name", First_Name, Last_Name, sep = " ") %>%
  mutate(postseasonElo = round(postseasonElo)) %>%
  select(Name, Team, Position, PA, regSeasonElo, PostTrend, postseasonElo)

# Run same steps for pitchers.
p19Elo_modPS <- p19EloPS %>%
  arrange(desc(postseasonElo)) %>%
  mutate(Rank = seq.int(nrow(p19EloPS)),
         avgEloFaced = round(avgEloFaced / BFP.x, 1)) %>%
  left_join(pitcher_positions, by = c("first_name", "last_name")) %>%
  left_join(pitcher_ev, by = "player_id") %>%
  rename(First_Name = first_name,
         Last_Name = last_name,
         Team = Pitching_Team,
         Position = Pos,
         PA = BFP.x,
         wOBA = woba,
         xwOBA = est_woba)
p19Elo_modPS <- p19Elo_modPS %>%
  distinct(player_id, .keep_all = TRUE) %>%
  drop_na(EV, IP) %>%
  mutate(PostTrend = round(postseasonElo - regSeasonElo), 0)
p19Elo_dispPS <- p19Elo_modPS %>%
  unite("Name", First_Name, Last_Name, sep = " ") %>%
  mutate(postseasonElo = round(postseasonElo)) %>%
  select(Name, Team, Position, PA, regSeasonElo, PostTrend, postseasonElo)

# Load team records (source: Baseball Reference), compute pythWPct.
standings <- read_csv("data/standings.csv") %>%
  separate(pythWL, c("pythW", "pythL"), sep = "-") %>%
  arrange(Tm) %>%
  filter(Tm != "Avg") %>%
  mutate(Tm = c("Arizona Diamondbacks", "Atlanta Braves", "Baltimore Orioles",
                "Boston Red Sox", "Chicago Cubs", "Chicago White Sox", 
                "Cincinnati Reds", "Cleveland Indians", "Colorado Rockies", 
                "Detroit Tigers", "Houston Astros", "Kansas City Royals", 
                "Los Angeles Angels", "Los Angeles Dodgers", "Miami Marlins", 
                "Milwaukee Brewers", "Minnesota Twins", "New York Mets", 
                "New York Yankees", "Oakland Athletics", 
                "Philadelphia Phillies", "Pittsburgh Pirates", 
                "San Diego Padres", "Seattle Mariners", "San Francisco Giants", 
                "St. Louis Cardinals", "Tampa Bay Rays", "Texas Rangers", 
                "Toronto Blue Jays", "Washington Nationals"),
         pythWPct = round(as.numeric(pythW) / 
                            (as.numeric(pythW) + as.numeric(pythL)), 3)) %>%
  rename(Team = Tm, WPct = "W-L%") %>%
  select(Team, W, L, WPct, pythWPct)

# Calculate team playerElo, weighting individual playerElo's by PA / IP,
# get runs scored / against stats, and summarize in team df.
b19Elo_teamPS <- b19Elo_modPS %>%
  mutate(eloWeight = postseasonElo * PA) %>%
  group_by(Team) %>%
  summarise(TeamBattingElo = sum(eloWeight) / sum(PA)) %>%
  mutate(TeamBattingElo = TeamBattingElo + 1000 - mean(TeamBattingElo)) %>%
  drop_na()
p19Elo_teamPS <- p19Elo_modPS %>%
  mutate(eloWeight = postseasonElo * IP) %>%
  group_by(Team) %>%
  summarise(TeamPitchingElo = sum(eloWeight) / sum(IP)) %>%
  mutate(TeamPitchingElo = TeamPitchingElo + 1000 - mean(TeamPitchingElo)) %>%
  drop_na()
rs19_team <- RE19PS %>%
  group_by(Batting_Team) %>%
  summarise(Runs_Scored = sum(Runs_OnPlay))
ra19_team <- RE19PS %>%
  group_by(Pitching_Team) %>%
  summarise(Runs_Against = sum(Runs_OnPlay))
teamElo19PS <- b19Elo_teamPS %>%
  left_join(p19Elo_teamPS, by = "Team") %>%
  left_join(rs19_team, by = c("Team" = "Batting_Team")) %>%
  left_join(ra19_team, by = c("Team" = "Pitching_Team"))

# Compute team ranks using z scores of Team Batting and Pitching Elo.
teamElo19PS <- teamElo19PS %>%
  mutate(b_zscore = (TeamBattingElo - mean(TeamBattingElo)) / sd(TeamBattingElo),
         p_zscore = (TeamPitchingElo - mean(TeamPitchingElo)) / sd(TeamPitchingElo),
         aggTeamElo = round(1000 + 20 * b_zscore + 20 * p_zscore),
         pythWPct = round(Runs_Scored^2/(Runs_Scored^2 + Runs_Against^2), 3),
         TeamBattingElo = round(TeamBattingElo),
         TeamPitchingElo = round(TeamPitchingElo)) %>%
  arrange(desc(aggTeamElo)) %>%
  select(Team, pythWPct, Runs_Scored, Runs_Against, 
         TeamBattingElo, TeamPitchingElo, aggTeamElo) %>%
  rename("Batting Elo" = TeamBattingElo,
         "Pitching Elo" = TeamPitchingElo,
         "Team Elo" = aggTeamElo,
         "Runs Scored" = Runs_Scored,
         "Runs Against" = Runs_Against,
         "Pythagorean WPct" = pythWPct)

# Write csv's for use in 02_app.R
write_csv(b19Elo_dispPS, "data/b19Elo_dispPS.csv")
write_csv(p19Elo_dispPS, "data/p19Elo_dispPS.csv")
write_csv(teamElo19PS, "data/teamElo19PS.csv")
