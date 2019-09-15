## playerElo Computation
## September 2019
## 01_app_construction
## Data sourced from Baseball Savant, RotoWire, 
## Baseball Reference, Retrosheet, and BigDataBall.

library(tidyverse)
library(data.table)

# - Load previously calculated park factors.
# - Found by comparing avg run value of visting teams at every stadium to
#   average run value at all stadiums.
park_factors <- read_csv("data/ParkFactors.csv")

# - Load previously calculated ending playerElos for 2018.
# - Found by running the following script with 2018 data, starting all 
#   players with a preseason playerElo of 1000.
b18Elo <- read_csv("data/b18Elo.csv", 
                   col_types = list(player_id = col_character()))

p18Elo <- read_csv("data/p18Elo.csv", 
                   col_types = list(player_id = col_character()))

# - Load matrix of previously calculated quadratic coefficients 
#   to be used in playerElo calculations.
# - State Matrix coefficients computed by repeatadly running playerElo
#   calculation over the previous three years, and finding relationship
#   between playerElo and performance (measured by wOBA and run value)
#   in each base-out state.
state_matrix <- read.csv("data/stateMatrix.csv")

# Clean data to a usable format.
pbp_compile <- function(data) {
  data <- data[-(1:2), 
               ][, c("V1", "V18", "V19", "V20", "V21", "V22", "V23", "V24", 
                     "V25", "V26", "V27", "V28", "V29", "V30", "V31", 
                     "V32", "V33", "V34", "V35", "V45") := NULL]
  names(data) <- c("GameID", "Date", "Inning", "Road_Score", "Home_Score",
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

# Load play-by-play data for 2019 season (source: Retrosheet or BigDataBall).
RE19 <- fread("data/09-10-2019-mlb-season-pbp-feed.csv")
RE19 <- pbp_compile(RE19)
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
b19 <- read_csv("data/expected_stats-6.csv", 
                col_types = list(player_id = col_character())) %>%
  left_join(batter_teams, by = c("player_id" = "Batter_MLB_ID")) %>%
  select(first_name, last_name, year, Batting_Team, everything())

p19 <- read_csv("data/expected_stats-7.csv", 
                col_types = list(player_id = col_character())) %>%
  left_join(pitcher_teams, by = c("player_id" = "Pitcher_MLB_ID")) %>%
  select(first_name, last_name, year, Pitching_Team, everything())

# Compute standardized home field advantage based on run value.
RE19_home <- filter(RE19, substr(Inning, 2, 2) == "B")
RE19_away <- filter(RE19, substr(Inning, 2, 2) == "T")
home_adv <- mean(RE19_home$Runs_Value) - mean(RE19_away$Runs_Value)

# Create df to update 2019 playerElo, including previous season Elo regressed 
# 15% to mean, savant stats, and player team.
b19Elo <- data.frame(player_id = unique(RE19$Batter_MLB_ID), 
                     stringsAsFactors = F) %>%
  left_join(b18Elo, by = "player_id") %>%
  mutate(currentElo = ifelse(is.na(currentElo), 
                             1000, 
                             ((currentElo - 1000) * 0.85) + 1000),
         preseasonElo = currentElo, avgEloFaced = 0,
         PA.x = 0) %>%
  left_join(b19, by = "player_id") %>%
  select(player_id, preseasonElo, currentElo, first_name, 
         last_name, Batting_Team, everything(), -Date)

p19Elo <- data.frame(player_id = unique(RE19$Pitcher_MLB_ID),
                     stringsAsFactors = F) %>%
  left_join(p18Elo, by = "player_id") %>%
  mutate(currentElo = ifelse(is.na(currentElo), 
                             1000, 
                             ((currentElo - 1000) * 0.85) + 1000),
         preseasonElo = currentElo, avgEloFaced = 0,
         BFP.x = 0) %>%
  left_join(p19, by = "player_id") %>%
  select(player_id, preseasonElo, currentElo, first_name, 
         last_name, Pitching_Team, everything(), -Date)

# Create list to track playerElo progression by PA or BFP.
all_playerElo <- list()

# Compute playerElo.
for (row in 1:nrow(RE19)) {
  # Get playerIDs and current playerElo.
  batterID <- RE19$Batter_MLB_ID[row]
  pitcherID <- RE19$Pitcher_MLB_ID[row]
  batterElo <- b19Elo$currentElo[which(b19Elo$player_id == batterID)]
  pitcherElo <- p19Elo$currentElo[which(p19Elo$player_id == pitcherID)]
  
  # Add to PA or BFP count.
  b19Elo$PA.x[which(b19Elo$player_id == batterID)] <-
    b19Elo$PA.x[which(b19Elo$player_id == batterID)] + 1
  p19Elo$BFP.x[which(p19Elo$player_id == pitcherID)] <-
    p19Elo$BFP.x[which(p19Elo$player_id == pitcherID)] + 1
  
  b19Elo$avgEloFaced[which(b19Elo$player_id == batterID)] <-
    b19Elo$avgEloFaced[which(b19Elo$player_id == batterID)] + pitcherElo
  p19Elo$avgEloFaced[which(p19Elo$player_id == pitcherID)] <-
    p19Elo$avgEloFaced[which(p19Elo$player_id == pitcherID)] + batterElo

  # Add playerElo for current PA or BFP to progression list.
  all_playerElo[[row*2 - 1]] <- 
    list(playerID = batterID,
         PA = b19Elo$PA.x[which(b19Elo$player_id == batterID)], 
         playerElo = batterElo)
  all_playerElo[[row*2]] <- 
    list(playerID = pitcherID,
         PA = p19Elo$BFP.x[which(p19Elo$player_id == pitcherID)],
         playerElo = pitcherElo)

  # Compute expected runs value for matchup, based on current base-out
  # state and playerElo of batter and pitcher.
  matrix_row <- which(state_matrix$State == RE19$State_Before[row])
  xRV_batter <- (state_matrix$bA[matrix_row] * batterElo^2) +
    (state_matrix$bB[matrix_row] * batterElo) + state_matrix$bC[matrix_row]
  xRV_pitcher <- (state_matrix$pA[matrix_row] * pitcherElo^2) +
    (state_matrix$pB[matrix_row] * pitcherElo) + state_matrix$pC[matrix_row]
  
  # Add in home field advantage (if applicable).
  if (substr(RE19$Inning[row], 2, 2) == "B") {
    xRV_batter <- xRV_batter + home_adv
  }
  
  # Find park factor.
  park_factor <- park_factors$Park_Factor[
    which(park_factors$Park == str_sub(RE19$GameID[row], -5, -3))]

  # Compare expected RV to actual RV.
  rv_diff <- RE19$Runs_Value[row] - 
    ((xRV_batter + xRV_pitcher) / 2) - park_factor
  
  # Compute Elo change, with formulas are reflective of relationship 
  # between wOBA or FIP and Run Value per PA or BFP.
  bElo_delta <- (921.675 + (6046.370 * rv_diff) - batterElo) / 502
  pElo_delta <- (965.754 - (4762.089 * rv_diff) - pitcherElo) / 502
  
  # Account for errors only if it still helps pitcher or hurts batter, 
  # otherwise disregard, and update playerElo of batter and pitcher.
  if (RE19$Play_Type[row] != "FIELD ERROR" || 
      RE19$Play_Type[row] == "FIELD ERROR" && bElo_delta < 0) {
    b19Elo$currentElo[which(b19Elo$player_id == batterID)] <- 
      batterElo + bElo_delta
  }
  if (RE19$Play_Type[row] != "FIELD ERROR" || 
      RE19$Play_Type[row] == "FIELD ERROR" && pElo_delta > 0) {
    p19Elo$currentElo[which(p19Elo$player_id == pitcherID)] <-
      pitcherElo + pElo_delta
  }
  
  # Print completion percentage of for loop.
  if (row %% 1000 == 0) {
    print(paste0(round(100 * row / nrow(RE19), 2), "%"))
  }
}

# Convert list to df.
all_playerElo <- do.call("bind_rows", all_playerElo)

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
batter_ev <- read_csv("data/exit_velocity-2.csv", 
                      col_types = list(player_id = col_character())) %>%
  rename(EV = avg_hit_speed, 
         "HH%" = ev95percent) %>%
  select(player_id, EV, "HH%")
pitcher_ev <- read_csv("data/exit_velocity-3.csv", 
                       col_types = list(player_id = col_character())) %>%
  rename(EV = avg_hit_speed,
         "HH%" = ev95percent) %>%
  select(player_id, EV, "HH%")

# Create new df to describe trends of players, using playerElo progression dt.
all_playerElo_mod <- all_playerElo %>%
  mutate(PA = PA + 50) %>%
  arrange(desc(playerElo)) %>%
  distinct(playerID, PA, .keep_all = TRUE) %>%
  rename(Trend = playerElo)

# Add positions and velo stats to playerElo df, clean data.
b19Elo_mod <- b19Elo %>%
  arrange(desc(currentElo)) %>%
  mutate(Rank = seq.int(nrow(b19Elo)),
         avgEloFaced = round(avgEloFaced / PA.x, 1)) %>%
  left_join(batter_positions, by = c("first_name", "last_name")) %>%
  left_join(batter_ev, by = "player_id") %>%
  rename(First_Name = first_name,
         Last_Name = last_name,
         Team = Batting_Team,
         Position = Pos,
         PA = pa,
         playerElo = currentElo,
         wOBA = woba,
         xwOBA = est_woba)
# Add positions to players whose suffixes pose matching problems,
# (e.g. Ronald Acuna Jr.).
b19Elo_mod$Position[which(b19Elo_mod$player_id == 660670)] <- "OF"
b19Elo_mod$Position[which(b19Elo_mod$player_id == 666971)] <- "OF"
b19Elo_mod$Position[which(b19Elo_mod$player_id == 670712)] <- "3B"
b19Elo_mod$Position[which(b19Elo_mod$player_id == 592261)] <- "OF"
b19Elo_mod$Position[which(b19Elo_mod$player_id == 596105)] <- "OF"
b19Elo_mod$Position[which(b19Elo_mod$player_id == 628356)] <- "3B"
# Continue to clean and add labels to pitchers, include trend over last 50 PA.
b19Elo_mod <- b19Elo_mod %>%
  distinct(player_id, .keep_all = TRUE) %>%
  drop_na(EV) %>%
  mutate(Position = replace_na(Position, "P")) %>%
  left_join(all_playerElo_mod, by = c("player_id" = "playerID", "PA")) %>%
  mutate(Trend = replace_na(Trend, 1000))
# Select columns for display in Shiny App.
b19Elo_disp <- b19Elo_mod %>%
  unite("Name", First_Name, Last_Name, sep = " ") %>%
  mutate(Trend = round(playerElo - Trend),
         playerElo = round(playerElo + 1000 - mean(playerElo))) %>%
  select(Name, Team, Position, PA, EV, "HH%", wOBA, xwOBA, Trend, playerElo)

# Run same steps for pitchers.
p19Elo_mod <- p19Elo %>%
  arrange(desc(currentElo)) %>%
  mutate(Rank = seq.int(nrow(p19Elo)),
         avgEloFaced = round(avgEloFaced / BFP.x, 1)) %>%
  left_join(pitcher_positions, by = c("first_name", "last_name")) %>%
  left_join(pitcher_ev, by = "player_id") %>%
  rename(First_Name = first_name,
         Last_Name = last_name,
         Team = Pitching_Team,
         Position = Pos,
         PA = pa,
         playerElo = currentElo,
         wOBA = woba,
         xwOBA = est_woba)
p19Elo_mod$Position[which(p19Elo_mod$player_id == 592346)] <- "SP"
p19Elo_mod$Position[which(p19Elo_mod$player_id == 605541)] <- "RP"
p19Elo_mod$Position[which(p19Elo_mod$player_id == 605479)] <- "RP"
p19Elo_mod$Position[which(p19Elo_mod$player_id == 595001)] <- "RP"
p19Elo_mod$Position[which(p19Elo_mod$player_id == 641482)] <- "RP"
p19Elo_mod$Position[which(p19Elo_mod$player_id == 597113)] <- "RP"
p19Elo_mod <- p19Elo_mod %>%
  distinct(player_id, .keep_all = TRUE) %>%
  drop_na(EV, IP) %>%
  left_join(all_playerElo_mod, by = c("player_id" = "playerID", "PA")) %>%
  mutate(Trend = replace_na(Trend, 1000))
p19Elo_disp <- p19Elo_mod %>%
  unite("Name", First_Name, Last_Name, sep = " ") %>%
  mutate(Trend = round(playerElo - Trend),
         playerElo = round(playerElo + 1000 - mean(playerElo))) %>%
  select(Name, Team, Position, PA, EV, "HH%", wOBA, xwOBA, Trend, playerElo)

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
b19Elo_team <- b19Elo_mod %>%
  mutate(eloWeight = playerElo * PA) %>%
  group_by(Team) %>%
  summarise(TeamBattingElo = sum(eloWeight) / sum(PA)) %>%
  mutate(TeamBattingElo = TeamBattingElo + 1000 - mean(TeamBattingElo)) %>%
  drop_na()
p19Elo_team <- p19Elo_mod %>%
  mutate(eloWeight = playerElo * IP) %>%
  group_by(Team) %>%
  summarise(TeamPitchingElo = sum(eloWeight) / sum(IP)) %>%
  mutate(TeamPitchingElo = TeamPitchingElo + 1000 - mean(TeamPitchingElo)) %>%
  drop_na()
rs19_team <- RE19 %>%
  group_by(Batting_Team) %>%
  summarise(Runs_Scored = sum(Runs_OnPlay))
ra19_team <- RE19 %>%
  group_by(Pitching_Team) %>%
  summarise(Runs_Against = sum(Runs_OnPlay))
teamElo19 <- b19Elo_team %>%
  left_join(p19Elo_team, by = "Team") %>%
  left_join(rs19_team, by = c("Team" = "Batting_Team")) %>%
  left_join(ra19_team, by = c("Team" = "Pitching_Team")) %>%
  left_join(standings, by = "Team")

# Compute team ranks using z scores of Team Batting and Pitching Elo.
teamElo19 <- teamElo19 %>%
  mutate(b_zscore = (TeamBattingElo - mean(TeamBattingElo)) / sd(TeamBattingElo),
         p_zscore = (TeamPitchingElo - mean(TeamPitchingElo)) / sd(TeamPitchingElo),
         aggTeamElo = round(1000 + 20 * b_zscore + 20 * p_zscore),
         pythWPct = signif(as.numeric(pythWPct), 3),
         WPct = signif(as.numeric(WPct), 3),
         TeamBattingElo = round(TeamBattingElo),
         TeamPitchingElo = round(TeamPitchingElo)) %>%
  arrange(desc(aggTeamElo)) %>%
  select(Team, W, L, WPct, pythWPct, Runs_Scored, Runs_Against, 
         TeamBattingElo, TeamPitchingElo, aggTeamElo) %>%
  rename("Batting Elo" = TeamBattingElo,
         "Pitching Elo" = TeamPitchingElo,
         "Team Elo" = aggTeamElo,
         "Runs Scored" = Runs_Scored,
         "Runs Against" = Runs_Against,
         "Pythagorean WPct" = pythWPct,
         "Wins" = W,
         "Losses" = L)

# Clean and modify playerElo progression df, and 
# add names and boolean isPitcher variable.
batter_names <- b19Elo %>%
  select(player_id, first_name, last_name)
pitcher_names <- p19Elo %>%
  select(player_id, first_name, last_name)

all_playerElo_mod <- all_playerElo %>%
  left_join(batter_names, by = c("playerID" = "player_id")) %>%
  left_join(pitcher_names, by = c("playerID" = "player_id")) %>%
  mutate(isPitcher = !is.na(last_name.y),
         Name = ifelse(isPitcher, paste(first_name.y, last_name.y),
                       paste(first_name.x, last_name.x))) %>%
  arrange(PA) %>%
  group_by(playerID, PA) %>%
  filter(playerElo == max(playerElo)) %>%
  ungroup() %>%
  mutate(isPitcher = factor(isPitcher, levels = c(T, F),
                            labels = c("Pitcher", "Batter"))) %>%
  select(playerID, PA, playerElo, Name) %>%
  filter(Name != "NA NA")

# Write csv's for use in 02_app.R
write_csv(b19Elo_disp, "data/b19Elo_disp.csv")
write_csv(p19Elo_disp, "data/p19Elo_disp.csv")
write_csv(teamElo19, "data/teamElo19.csv")
write_csv(all_playerElo_mod, "data/all_playerElo.csv")
