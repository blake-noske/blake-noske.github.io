season = 2024
round = "GF"

# set weather replacement at start of year
weather_replacements <- c(
  "WINDY_RAIN" = "RAIN",
  "MOSTLY_CLEAR" = "CLEAR_NIGHT"
)

# Match results -----------------------------------------------------------

# Fetch match results and calculate margins
y <- season-2012
matches <- fetch_results_afl(season = 2012)
for (i in 1:y) {
  m <- fetch_results_afl(season = 2012+i)
  ss1 <- setdiff(names(m), names(matches))
  m <- m %>% select(-c(ss1))
  
  ss2 <- setdiff(names(matches), names(m))
  matches <- matches %>% select(-c(ss2))
  
  matches <- rbind(matches,m)
}

matches$match.homeTeam.name <- replace_teams(matches$match.homeTeam.name)
matches$match.awayTeam.name <- replace_teams(matches$match.awayTeam.name)
matches$venue.name <- replace_venues(matches$venue.name)


venue_name_replacements <- c(
  "Docklands" = "Marvel",
  "Sydney Showground" = "ENGIE Stadium",
  "Carrara" = "People First Stadium",
  "Perth Stadium" = "Optus Stadium",
  "Kardinia Park" = "GMHBA Stadium",
  "Marrara Oval" = "TIO Stadium",
  "Traeger Park" = "TIO Traeger Park",
  "Eureka Stadium" = "Mars Stadium",
  "York Park" = "UTAS Stadium",
  "Bellerive Oval" = "Blundstone Arena",
  "Stadium Australia" = "Accor Stadium",
  "Summit Sports Park" = "Adelaide Hills"
)

# Replace team names in Home.Team and Away.Team columns
matches <- matches %>%
  mutate(
    venue.name = recode(venue.name, !!!venue_name_replacements))

matches$weather.weatherType[matches$venue.name == "Marvel"] <- "CLEAR_NIGHT"
#unique(matches$weather.weatherType) Unhash at start of season
#unique(matches[matches$round.year >= 2023, ]$weather.weatherType) Unhash at start of season

# Replace weather to reflect weather collected in last 2 years
matches <- matches %>%
  mutate(
    weather.weatherType = recode(weather.weatherType, !!!weather_replacements))

# Calculate margin
matches <- matches %>%
  mutate(Margin = homeTeamScore.matchScore.totalScore - awayTeamScore.matchScore.totalScore)

# Define classification label based on margin
threshold_big_win <- 39  # Adjust as per market definition
matches <- matches %>%
  mutate(Result = ifelse(Margin > threshold_big_win,"BW",
                         ifelse(Margin > 0,"LW",
                                ifelse(Margin == 0,"D",
                                       ifelse(Margin < -threshold_big_win,"BL","LL")))))


# Team stats --------------------------------------------------------------

player_stats <- fetch_player_stats_afl(season = 2012)
for (i in 1:y) {
  p <- fetch_player_stats_afl(season = 2012+i)
  ss1 <- setdiff(names(p), names(player_stats))
  p <- p %>% select(-c(ss1))
  
  ss2 <- setdiff(names(player_stats), names(p))
  player_stats <- player_stats %>% select(-c(ss2))
  
  player_stats <- rbind(player_stats,p)
  
}
player_stats$team.name <- replace_teams(player_stats$team.name)
player_stats$venue.name <- replace_venues(player_stats$venue.name)

# Replace team names in Home.Team and Away.Team columns
player_stats <- player_stats %>%
  mutate(
    venue.name = recode(venue.name, !!!venue_name_replacements))

team_stats <- player_stats %>%
  group_by(utcStartTime, team.name) %>%
  summarise(
    Total.kicks = sum(kicks, na.rm = TRUE),
    Total.handballs = sum(handballs, na.rm = TRUE),
    Total.disposals = sum(disposals, na.rm = TRUE),
    Total.marks = sum(marks, na.rm = TRUE),
    Total.bounces = sum(bounces, na.rm = TRUE),
    Total.tackles = sum(tackles, na.rm = TRUE),
    Total.contestedPossessions = sum(contestedPossessions, na.rm = TRUE),
    Total.uncontestedPossessions = sum(uncontestedPossessions, na.rm = TRUE),
    Total.totalPossessions = sum(totalPossessions, na.rm = TRUE),
    Total.inside50s = sum(inside50s, na.rm = TRUE),
    Total.marksInside50 = sum(marksInside50, na.rm = TRUE),
    Total.contestedMarks = sum(contestedMarks, na.rm = TRUE),
    Total.hitouts = sum(hitouts, na.rm = TRUE),
    Total.onePercenters = sum(onePercenters, na.rm = TRUE),
    Total.disposalEfficiency = sum(disposalEfficiency, na.rm = TRUE),
    Total.clangers = sum(clangers, na.rm = TRUE),
    Total.freesAgainst = sum(freesAgainst, na.rm = TRUE),
    Total.rebound50s = sum(rebound50s, na.rm = TRUE),
    Total.turnovers = sum(turnovers, na.rm = TRUE),
    Total.intercepts = sum(intercepts, na.rm = TRUE),
    Total.tacklesInside50 = sum(tacklesInside50, na.rm = TRUE),
    Total.metresGained = sum(metresGained, na.rm = TRUE),
    Total.clearances.centreClearances = sum(clearances.centreClearances, na.rm = TRUE),
    Total.clearances.stoppageClearances = sum(clearances.stoppageClearances, na.rm = TRUE),
    Total.clearances.totalClearances = sum(clearances.totalClearances, na.rm = TRUE),
    Total.extendedStats.effectiveKicks = sum(extendedStats.effectiveKicks, na.rm = TRUE),
    Total.extendedStats.kickEfficiency = sum(extendedStats.kickEfficiency, na.rm = TRUE),
    Total.extendedStats.kickToHandballRatio = sum(extendedStats.kickToHandballRatio, na.rm = TRUE),
    Total.extendedStats.effectiveDisposals = sum(extendedStats.effectiveDisposals, na.rm = TRUE),
    Total.extendedStats.marksOnLead = sum(extendedStats.marksOnLead, na.rm = TRUE),
    Total.extendedStats.interceptMarks = sum(extendedStats.interceptMarks, na.rm = TRUE),
    Total.extendedStats.contestedPossessionRate = sum(extendedStats.contestedPossessionRate, na.rm = TRUE),
    Total.extendedStats.hitoutsToAdvantage = sum(extendedStats.hitoutsToAdvantage, na.rm = TRUE),
    Total.extendedStats.hitoutWinPercentage = sum(extendedStats.hitoutWinPercentage, na.rm = TRUE),
    Total.extendedStats.hitoutToAdvantageRate = sum(extendedStats.hitoutToAdvantageRate, na.rm = TRUE),
    Total.extendedStats.groundBallGets = sum(extendedStats.groundBallGets, na.rm = TRUE),
    Total.extendedStats.f50GroundBallGets = sum(extendedStats.f50GroundBallGets, na.rm = TRUE),
    Total.extendedStats.scoreLaunches = sum(extendedStats.scoreLaunches, na.rm = TRUE),
    Total.extendedStats.pressureActs = sum(extendedStats.pressureActs, na.rm = TRUE),
    Total.extendedStats.defHalfPressureActs = sum(extendedStats.defHalfPressureActs, na.rm = TRUE),
    Total.extendedStats.spoils = sum(extendedStats.spoils, na.rm = TRUE),
    Total.extendedStats.ruckContests = sum(extendedStats.ruckContests, na.rm = TRUE),
    Total.extendedStats.contestDefOneOnOnes = sum(extendedStats.contestDefOneOnOnes, na.rm = TRUE),
    Total.extendedStats.contestDefLosses = sum(extendedStats.contestDefLosses, na.rm = TRUE),
    Total.extendedStats.contestDefLossPercentage = sum(extendedStats.contestDefLossPercentage, na.rm = TRUE),
    Total.extendedStats.contestOffOneOnOnes = sum(extendedStats.contestOffOneOnOnes, na.rm = TRUE),
    Total.extendedStats.contestOffWins = sum(extendedStats.contestOffWins, na.rm = TRUE),
    Total.extendedStats.contestOffWinsPercentage = sum(extendedStats.contestOffWinsPercentage, na.rm = TRUE)
  )
team_stats$utcStartTime <- substr(team_stats$utcStartTime, 1,
                                  nchar(team_stats$utcStartTime) - 9)


matches <- matches %>%
  left_join(team_stats, by = c("match.utcStartTime" = "utcStartTime",
                               "match.homeTeam.name" = "team.name")) %>%
  rename_with(~ paste0("match.homeTeam.", .), starts_with("Total")) %>%
  left_join(team_stats, by = c("match.utcStartTime" = "utcStartTime",
                               "match.awayTeam.name" = "team.name")) %>%
  rename_with(~ paste0("match.awayTeam.", .), starts_with("Total"))


# Weighted average for each team ------------------------------------------

round_replacement_values <- c("FW1" = 25, "SF" = 26,"PF" = 27,"GF" = 28)

# Replace the old values with the new values
round <- as.numeric(replace(round, round %in% names(round_replacement_values),
                            round_replacement_values[as.character(round)]))

Lineup <- fetch_lineup(season = season,round_number = round)
Lineup <- Lineup[Lineup$position != "EMERG", ]
Lineup$teamName <- replace_teams(Lineup$teamName)

decay_rate <- 0.8
normalized_weighted_decay <- function(values, decay_rate) {
  n <- length(values)
  weights <- decay_rate^((n-1):0)
  
  # Normalize the weights so that they sum to 1
  normalized_weights <- weights / sum(weights)
  
  # Calculate the weighted sum with the normalized weights
  weighted_sum <- sum(values * normalized_weights)
  
  if (is.na(weighted_sum) == TRUE) {
    weighted_sum = 0
  }
  
  return(weighted_sum)
}


Lineup_stats <- data_frame()
for (i in c(1:length(Lineup$providerId))) {
  player_stats_filter <- player_stats[player_stats$player.player.player.playerId == Lineup$player.playerId[i], ] %>%
    arrange(utcStartTime)
  
  player_stats_filter <- player_stats_filter[rowSums(is.na(player_stats_filter)) != ncol(player_stats_filter), ]
  
  if (length(player_stats_filter[,1]) > 5) {
    l = length(player_stats_filter[,1])
    player_stats_filter <- player_stats_filter[c(l:(l-4)),]%>%
      arrange(utcStartTime)
  }
  
  player_stats_filter<- player_stats_filter %>%
    select(kicks,handballs,disposals,marks,bounces,tackles,contestedPossessions,
           uncontestedPossessions,totalPossessions,inside50s,marksInside50,
           contestedMarks,hitouts,onePercenters,disposalEfficiency,clangers,
           freesAgainst,rebound50s,turnovers,intercepts,tacklesInside50,metresGained,
           clearances.centreClearances,clearances.stoppageClearances,
           clearances.totalClearances,extendedStats.effectiveKicks,
           extendedStats.kickEfficiency,extendedStats.kickToHandballRatio,
           extendedStats.effectiveDisposals,extendedStats.marksOnLead,
           extendedStats.interceptMarks,extendedStats.contestedPossessionRate,
           extendedStats.hitoutsToAdvantage,extendedStats.hitoutWinPercentage,
           extendedStats.hitoutToAdvantageRate,extendedStats.groundBallGets,
           extendedStats.f50GroundBallGets,extendedStats.scoreLaunches,
           extendedStats.pressureActs,extendedStats.defHalfPressureActs,
           extendedStats.spoils,extendedStats.ruckContests,
           extendedStats.contestDefOneOnOnes,extendedStats.contestDefLosses,
           extendedStats.contestDefLossPercentage,extendedStats.contestOffOneOnOnes,
           extendedStats.contestOffWins,extendedStats.contestOffWinsPercentage
    )
  
  weighted_decay_df <- as.data.frame(lapply(player_stats_filter, normalized_weighted_decay,
                                            decay_rate = decay_rate))
  Lineup_stats <- rbind(Lineup_stats,cbind(Lineup$teamName[i],Lineup$player.playerId[i],
                                           weighted_decay_df))
}

Lineup_stats <- na.omit(Lineup_stats)

new_team_stats <- Lineup_stats %>%
  group_by(`Lineup$teamName[i]`) %>%
  summarise(
    kicks = sum(kicks),
    handballs = sum(handballs),
    disposals = sum(disposals),
    marks = sum(marks),
    bounces = sum(bounces),
    tackles = sum(tackles),
    contestedPossessions = sum(contestedPossessions),
    uncontestedPossessions = sum(uncontestedPossessions),
    totalPossessions = sum(totalPossessions),
    inside50s = sum(inside50s),
    marksInside50 = sum(marksInside50),
    contestedMarks = sum(contestedMarks),
    hitouts = sum(hitouts),
    onePercenters = sum(onePercenters),
    disposalEfficiency = sum(disposalEfficiency),
    clangers = sum(clangers),
    freesAgainst = sum(freesAgainst),
    rebound50s = sum(rebound50s),
    turnovers = sum(turnovers),
    intercepts = sum(intercepts),
    tacklesInside50 = sum(tacklesInside50),
    metresGained = sum(metresGained),
    clearances.centreClearances = sum(clearances.centreClearances),
    clearances.stoppageClearances = sum(clearances.stoppageClearances),
    clearances.totalClearances = sum(clearances.totalClearances),
    extendedStats.effectiveKicks = sum(extendedStats.effectiveKicks),
    extendedStats.kickEfficiency = sum(extendedStats.kickEfficiency),
    extendedStats.kickToHandballRatio = sum(extendedStats.kickToHandballRatio),
    extendedStats.effectiveDisposals = sum(extendedStats.effectiveDisposals),
    extendedStats.marksOnLead = sum(extendedStats.marksOnLead),
    extendedStats.interceptMarks = sum(extendedStats.interceptMarks),
    extendedStats.contestedPossessionRate = sum(extendedStats.contestedPossessionRate),
    extendedStats.hitoutsToAdvantage = sum(extendedStats.hitoutsToAdvantage),
    extendedStats.hitoutWinPercentage = sum(extendedStats.hitoutWinPercentage),
    extendedStats.hitoutToAdvantageRate = sum(extendedStats.hitoutToAdvantageRate),
    extendedStats.groundBallGets = sum(extendedStats.groundBallGets),
    extendedStats.f50GroundBallGets = sum(extendedStats.f50GroundBallGets),
    extendedStats.scoreLaunches = sum(extendedStats.scoreLaunches),
    extendedStats.pressureActs = sum(extendedStats.pressureActs),
    extendedStats.defHalfPressureActs = sum(extendedStats.defHalfPressureActs),
    extendedStats.spoils = sum(extendedStats.spoils),
    extendedStats.ruckContests = sum(extendedStats.ruckContests),
    extendedStats.contestDefOneOnOnes = sum(extendedStats.contestDefOneOnOnes),
    extendedStats.contestDefLosses = sum(extendedStats.contestDefLosses),
    extendedStats.contestDefLossPercentage = sum(extendedStats.contestDefLossPercentage),
    extendedStats.contestOffOneOnOnes = sum(extendedStats.contestOffOneOnOnes),
    extendedStats.contestOffWins = sum(extendedStats.contestOffWins),
    extendedStats.contestOffWinsPercentage = sum(extendedStats.contestOffWinsPercentage)
    # Add more statistical categories as needed
  )
names(new_team_stats)[names(new_team_stats) == "Lineup$teamName[i]"] <- "Team"

# Elo ranking -------------------------------------------------------------

matches <- matches %>%
  arrange(match.utcStartTime)

# Initialize Elo ratings
initialize_elo <- function(teams, initial_rating = 1500) {
  elo_ratings <- setNames(rep(initial_rating, length(teams)), teams)
  return(elo_ratings)
}

# Function to update Elo ratings after each match
update_elo <- function(home_elo, away_elo, home_points, away_points, k = 20) {
  expected_home_win_prob <- 1 / (1 + 10^((away_elo - home_elo) / 400))
  actual_home_win <- ifelse(home_points > away_points, 1, 0)
  home_elo_new <- home_elo + k * (actual_home_win - expected_home_win_prob)
  away_elo_new <- away_elo + k * ((1 - actual_home_win) - (1 - expected_home_win_prob))
  return(c(home_elo_new, away_elo_new))
}

# Function to adjust Elo ratings at the start of each season
adjust_elo_start_season <- function(elo_ratings, initial_rating = 1500,
                                    adjustment_factor = 0.5) {
  for (team in names(elo_ratings)) {
    elo_ratings[team] <- elo_ratings[team] + adjustment_factor * (initial_rating - elo_ratings[team])
  }
  return(elo_ratings)
}

# Calculate Elo ratings across matches
calculate_elo_ratings <- function(matches, k = 20, initial_rating = 1500,
                                  adjustment_factor = 0.5) {
  teams <- unique(c(matches$match.homeTeam.name, matches$match.awayTeam.name))
  elo_ratings <- initialize_elo(teams, initial_rating)
  
  matches <- matches %>%
    arrange(match.utcStartTime) %>%
    mutate(Home.Elo = 0, Away.Elo = 0)
  
  current_season <- matches$round.year[1]
  
  for (i in 1:nrow(matches)) {
    if (matches$round.year[i] != current_season) {
      # Adjust Elo ratings at the start of the new season
      elo_ratings <- adjust_elo_start_season(elo_ratings, initial_rating, adjustment_factor)
      current_season <- matches$round.year[i]
    }
    
    home_team <- matches$match.homeTeam.name[i]
    away_team <- matches$match.awayTeam.name[i]
    home_points <- matches$homeTeamScore.matchScore.totalScore[i]
    away_points <- matches$awayTeamScore.matchScore.totalScore[i]
    
    home_elo <- elo_ratings[home_team]
    away_elo <- elo_ratings[away_team]
    
    matches$Home.Elo[i] <- home_elo
    matches$Away.Elo[i] <- away_elo
    
    new_elos <- update_elo(home_elo, away_elo, home_points, away_points, k)
    elo_ratings[home_team] <- new_elos[1]
    elo_ratings[away_team] <- new_elos[2]
  }
  
  # Create dataframe with most up-to-date Elo ratings
  elo_current <- data.frame(Team = names(elo_ratings), Elo = as.numeric(elo_ratings))
  
  # Remove post Elo values from matches dataframe
  elo_matches <- select(matches, match.utcStartTime, match.homeTeam.name,
                        match.awayTeam.name,
                        homeTeamScore.matchScore.totalScore,
                        awayTeamScore.matchScore.totalScore, round.year, Home.Elo, Away.Elo)
  
  return(list(elo_matches = elo_matches, elo_current = elo_current))
}

# Calculate Elo ratings
elo_data <- calculate_elo_ratings(matches)
elo_matches <- elo_data$elo_matches
matches <- cbind(matches,elo_matches[,7:8])
elo_current <- elo_data$elo_current

elo_current <- elo_current %>%
  arrange(Team)

new_team_stats <- new_team_stats %>% left_join(elo_current, by = "Team")


# Team Steaks ------------------------------------------------------------------

# Sort the matches by date to ensure chronological order
matches <- matches %>%
  arrange(match.utcStartTime) %>%
  mutate(Home.Win.Streak = 0)

# Function to calculate home team's win and loss streaks
calculate_home_streaks <- function(data) {
  team_streaks <- list()
  streaks <- integer(nrow(data))
  
  for (i in 1:nrow(data)) {
    home_team <- data$match.homeTeam.name[i]
    away_team <- data$match.awayTeam.name[i]
    home_points <- data$homeTeamScore.matchScore.totalScore[i]
    away_points <- data$awayTeamScore.matchScore.totalScore[i]
    
    # Create unique keys for the match
    match_key_home <- paste(home_team, away_team, sep = "_")
    match_key_away <- paste(away_team, home_team, sep = "_")
    
    # Initialize the team_streaks entry if not present
    if (!match_key_home %in% names(team_streaks)) {
      team_streaks[[match_key_home]] <- 0
    }
    if (!match_key_away %in% names(team_streaks)) {
      team_streaks[[match_key_away]] <- 0
    }
    
    # Store the current streak from the perspective of the home team before updating it
    streaks[i] <- team_streaks[[match_key_home]]
    
    # Determine the result and update the streaks for the next game
    if (home_points > away_points) {
      # Home team wins
      if (team_streaks[[match_key_home]] > 0) {
        team_streaks[[match_key_home]] <- team_streaks[[match_key_home]] + 1
      } else {
        team_streaks[[match_key_home]] <- 1
      }
      team_streaks[[match_key_away]] <- -team_streaks[[match_key_home]]
    } else {
      # Home team loses
      if (team_streaks[[match_key_home]] < 0) {
        team_streaks[[match_key_home]] <- team_streaks[[match_key_home]] - 1
      } else {
        team_streaks[[match_key_home]] <- -1
      }
      team_streaks[[match_key_away]] <- -team_streaks[[match_key_home]]
    }
  }
  
  teams <- sort(unique(c(matches$match.homeTeam.name, matches$match.awayTeam.name)),
                decreasing = FALSE)
  num_teams <- length(teams)
  current <- matrix(0, nrow = num_teams, ncol = num_teams, dimnames = list(teams, teams))
  
  # Update streaks matrix
  for (i in 1:length(teams)) {
    for (j in 1:length(teams)) {
      home_team <- teams[j]
      away_team <- teams[i]
      if (home_team != away_team) {
        match_key_home <- paste(home_team, away_team, sep = "_")
        current[i, j] <- team_streaks[[match_key_home]]
      }
    }
  }
  
  return(list(streaks = streaks,current = current))
}


# Calculate home team's win and loss streaks
win_streaks <- calculate_home_streaks(matches)
matches$Home.Win.Streak <- win_streaks$streaks
current_team_streaks <- win_streaks$current


# Venue Streaks ------------------------------------------------------------

# Sort the matches by date to ensure chronological order
matches <- matches %>%
  mutate(Home.Team.Venue.Win.Streak = 0, Away.Team.Venue.Win.Streak = 0)

# Function to calculate venue win streaks for both home and away teams
calculate_venue_streaks <- function(data) {
  venue_streaks <- list()
  home_team_streaks_result <- integer(nrow(data))
  away_team_streaks_result <- integer(nrow(data))
  
  # Initialize matrix to store team venue streaks
  team_venue_streaks <- matrix(0, nrow = length(unique(data$match.homeTeam.name)), 
                               ncol = length(unique(data$venue.name)),
                               dimnames = list(sort(unique(data$match.homeTeam.name),
                                                    decreasing = FALSE), 
                                               sort(unique(data$venue.name),
                                                    decreasing = FALSE)))
  
  for (i in 1:nrow(data)) {
    home_team <- data$match.homeTeam.name[i]
    away_team <- data$match.awayTeam.name[i]
    home_points <- data$homeTeamScore.matchScore.totalScore[i]
    away_points <- data$awayTeamScore.matchScore.totalScore[i]
    venue <- data$venue.name[i]
    
    # Create unique keys for the venue streaks
    venue_key_home <- paste(home_team, venue, sep = "_")
    venue_key_away <- paste(away_team, venue, sep = "_")
    
    # Initialize the venue_streaks entries if not present
    if (!venue_key_home %in% names(venue_streaks)) {
      venue_streaks[[venue_key_home]] <- 0
    }
    if (!venue_key_away %in% names(venue_streaks)) {
      venue_streaks[[venue_key_away]] <- 0
    }
    
    # Store the current venue streak from the perspective of both teams before updating it
    home_team_streaks_result[i] <- venue_streaks[[venue_key_home]]
    away_team_streaks_result[i] <- venue_streaks[[venue_key_away]]
    
    # Determine the result and update the streaks for the next game
    if (home_points > away_points) {
      # Home team wins
      if (venue_streaks[[venue_key_home]] > 0) {
        venue_streaks[[venue_key_home]] <- venue_streaks[[venue_key_home]] + 1
      } else {
        venue_streaks[[venue_key_home]] <- 1
      }
      if (venue_streaks[[venue_key_away]] > 0) {
        venue_streaks[[venue_key_away]] <- -1
      } else  {
        venue_streaks[[venue_key_away]] <- venue_streaks[[venue_key_away]] - 1
      }
    } else {
      # Home team loses
      if (venue_streaks[[venue_key_home]] < 0) {
        venue_streaks[[venue_key_home]] <- venue_streaks[[venue_key_home]] - 1
      } else {
        venue_streaks[[venue_key_home]] <- -1
      }
      if (venue_streaks[[venue_key_away]] > 0) {
        venue_streaks[[venue_key_away]] <- venue_streaks[[venue_key_away]] + 1
      } else  {
        venue_streaks[[venue_key_away]] <- 1
      }
    }
    
    # Update team_venue_streaks matrix
    team_venue_streaks[home_team, venue] <- venue_streaks[[venue_key_home]]
    team_venue_streaks[away_team, venue] <- venue_streaks[[venue_key_away]]
  }
  
  # Assign venue streaks to the data frame
  Home.Team.Venue.Win.Streak <- home_team_streaks_result
  Away.Team.Venue.Win.Streak <- away_team_streaks_result
  
  # Return list with data frame and team_venue_streaks matrix
  return(list(home = Home.Team.Venue.Win.Streak, away = Away.Team.Venue.Win.Streak,
              team_venue_streaks = team_venue_streaks))
}


# Calculate venue win streaks for expanded test data
results <- calculate_venue_streaks(matches)

matches$Home.Team.Venue.Win.Streak <- results$home
matches$Away.Team.Venue.Win.Streak <- results$away
current_venue_streaks <- results$team_venue_streaks


# Form --------------------------------------------------------------------

# Create a helper function to calculate form based on the last 5 games' margin (excluding the current game)
calculate_form <- function(df, team_name) {
  df %>%
    filter(match.homeTeam.name == team_name | match.awayTeam.name == team_name) %>%
    arrange(match.utcStartTime) %>%
    mutate(
      Team.Margin = ifelse(match.homeTeam.name == team_name, Margin, -Margin),
      Lagged.Margin = lag(Team.Margin),  # Lag the margin to exclude the current game
      Last.5.Margin = rollapply(Lagged.Margin, width = 5, FUN = sum, align = "right",
                                fill = NA, partial = TRUE),
      Team = team_name
    ) %>%
    select(match.utcStartTime, Team, Last.5.Margin)
}

# Create a list of unique teams
teams <- unique(c(matches$match.homeTeam.name, matches$match.awayTeam.name))

# Calculate form for each team based on the last 5 games (excluding the current game)
team_form_list <- lapply(teams, function(team) {
  calculate_form(matches, team)
})

# Combine all team form data into one data frame
team_form_df <- bind_rows(team_form_list)

# Merge the form data with the original data
matches <- matches %>%
  left_join(team_form_df, by = c("match.utcStartTime", "match.homeTeam.name" = "Team")) %>%
  rename(Home.Team.Form = Last.5.Margin) %>%
  left_join(team_form_df, by = c("match.utcStartTime", "match.awayTeam.name" = "Team")) %>%
  rename(Away.Team.Form = Last.5.Margin)

# The resulting data frame `matches` now contains the form of the home and away teams based on the last 5 games' margin (excluding the current game).

# Step 2: Extract the most current form for each team
current_team_form <- team_form_df %>%
  group_by(Team) %>%
  filter(match.utcStartTime == max(match.utcStartTime, na.rm = TRUE)) %>%
  ungroup() %>%
  select(Team, Current.Form = Last.5.Margin)

current_team_form <- current_team_form %>%
  arrange(Team)

# Next Fixture ------------------------------------------------------------

Next_round <- fetch_fixture_afl(season = season,round_number = round)

Next_round$home.team.name <- replace_teams(Next_round$home.team.name)
Next_round$away.team.name <- replace_teams(Next_round$away.team.name)
Next_round$venue.name <- replace_venues(Next_round$venue.name)

# Replace team names in Home.Team and Away.Team columns
Next_round <- Next_round %>%
  mutate(
    venue.name = recode(venue.name, !!!venue_name_replacements))

Next_round$utcStartTime <- ymd_hms(Next_round$utcStartTime, tz = "UTC")

# Convert the UTC times to local time
Next_round$local_time <- with_tz(Next_round$utcStartTime, tzone = Next_round$venue.timezone)
Next_round$local_time <- as.character(Next_round$local_time)

round_name <- data.frame(
  Round_Name = Next_round$round.name[1]
)

Next_round <- Next_round %>%
  select(round.name,utcStartTime,local_time,home.team.name,away.team.name,venue.name,venue.location,venue.timezone)


api_key <- "ed96e7c2f1850c8b5c4000dd5619e08c"
country <- "AU"  # Australia
Next_round_weather <- vector(length = length(Next_round$round.name))
for (i in c(1:length(Next_round$round.name))) {
  game_day = substr(Next_round$utcStartTime[i],start = 1,stop = 10)
  
  if (game_day >= today()) {
    city <- Next_round$venue.location[i]
    
    # Construct the API URL with the city name and country code
    url <- paste0("http://api.openweathermap.org/data/2.5/forecast?q=", city, ",", country, "&appid=", api_key, "&units=metric")
    
    # Make the API call
    response <- GET(url)
    
    # Check the status code
    if (status_code(response) == 200) {
      # Parse the content of the response
      forecast_data <- fromJSON(content(response, "text"))
    } else {
      print("Failed to retrieve data")
    }  
    
    forecast_list <- forecast_data$list
    
    # Convert the forecast date and time to R's datetime format
    forecast_list$dt_txt <- as.POSIXct(forecast_list$dt_txt, format="%Y-%m-%d %H:%M:%S", tz="UTC")
    
    # Filter for future dates
    
    game_day_forecast <- forecast_list[as.Date(forecast_list$dt_txt) == game_day, ]
    
    game_day_forecast$offset <- game_day_forecast$dt_txt - Next_round$utcStartTime[i]
    
    game_day_forecast_filtered <- game_day_forecast[game_day_forecast$offset > 0, ][1,]
    wind_speed <- game_day_forecast_filtered[[5]][1][[1]]
    forecast_basic <- game_day_forecast_filtered$weather[[1]][2][[1]]
    forecast_adv <- game_day_forecast_filtered$weather[[1]][3][[1]]
    
    if (forecast_basic == "Mist" ||
        forecast_basic == "Smoke" ||
        forecast_basic == "Haze" ||
        forecast_basic == "Dust" ||
        forecast_basic == "Fog" ||
        forecast_basic == "Sand" ||
        forecast_basic == "Ash") {
      weather = "OVERCAST"
    }
    if (forecast_basic == "Clear") {
      if (hour(Next_round$local_time[i]) < 17) {
        weather = "SUNNY"
      }
      else weather = "CLEAR_NIGHT"
    }
    if (forecast_basic == "Clouds") {
      if (forecast_adv == "broken clouds" || forecast_adv == "overcast clouds") {
        weather = "OVERCAST"
      } else if ((hour(Next_round$local_time[i]) < 17)) {
        weather = "MOSTLY_SUNNY"
      } else weather = "CLEAR_NIGHT"
    }
    if (wind_speed >= 8) {
      weather = "WINDY"
    }
    if (forecast_basic == "Thunderstorm" || forecast_basic == "Squall" || forecast_basic == "Tornado") {
      weather = "THUNDERSTORMS"
    }
    if (forecast_basic == "Drizzle" || forecast_basic == "Rain" || forecast_basic == "Snow") {
      weather = "RAIN"
    }
    
    if (Next_round$venue.name[i] == "Marvel") {
      weather = "CLEAR_NIGHT"
    }
    
    Next_round_weather[i] <- weather
  } else Next_round_weather[i] = as.character(matches %>% filter(ymd_hms(match.utcStartTime,tz="UTC") == Next_round$utcStartTime[i]) %>% select(weather.weatherType))
  
}

Next_round <- cbind(Next_round,Next_round_weather)


# Export ------------------------------------------------------------------

matches_filtered <- matches %>%
  select(match.homeTeam.name, match.awayTeam.name, venue.name, Margin,Result,
         starts_with("match.homeTeam.Total"), Home.Elo,Home.Team.Form,
         starts_with("match.awayTeam.Total"), Away.Elo,Away.Team.Form,
         Away.Team.Venue.Win.Streak,Home.Team.Venue.Win.Streak,Home.Win.Streak,
         weather.weatherType)
matches_filtered <- na.omit(matches_filtered)

#Export cleaned data set to CSV or another format for Python
setwd("C:/Users/blake/Desktop/AFL Odds/Website code")
write_json(round_name, "Round.json")
setwd("C:/Users/blake/Desktop/AFL Odds/cleaned data")
write.csv(matches_filtered, "afl_match_results_cleaned.csv", row.names = FALSE)
write.csv(new_team_stats, "afl_team_stats_cleaned.csv", row.names = FALSE)
write.csv(current_team_streaks, "afl_team_streaks_cleaned.csv", row.names = TRUE)
write.csv(current_venue_streaks, "afl_venue_streaks_cleaned.csv", row.names = TRUE)
write.csv(current_team_form, "afl_team_form_cleaned.csv", row.names = TRUE)
write.csv(Next_round, "afl_fixture_cleaned.csv", row.names = TRUE)
setwd("C:/Users/blake/Desktop/AFL Odds/R scripts/R Studio/AFL Data Cleaning")


##################################################################WIP



##################### FIND OUT DAYS SINCE LAST PLAYED