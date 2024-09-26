season = 2024

r <- fetch_results_afl(season = season)

r <- r %>% filter(round.roundNumber >= 5)

# Calculate margin
r <- r %>%
  mutate(Margin = homeTeamScore.matchScore.totalScore - awayTeamScore.matchScore.totalScore)

# Define classification label based on margin
threshold_big_win <- 39  # Adjust as per market definition
r <- r %>%
  mutate(Result = ifelse(Margin > threshold_big_win,"BW",
                         ifelse(Margin > 0,"LW",
                                ifelse(Margin == 0,"D",
                                       ifelse(Margin < -threshold_big_win,"BL","LL")))))

r <- r %>% select(Result)
setwd("C:/Users/blake/Desktop/AFL Odds/Testing and Analysis")
write.csv(x = r,file = glue('{season} actual results.csv'))
setwd("C:/Users/blake/Desktop/AFL Odds/R scripts/R Studio/AFL Data Cleaning")
