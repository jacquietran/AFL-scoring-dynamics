##### Load libraries ###############################################################################

library(rvest)
library(purrr)
library(stringr)
library(plyr)
library(stringr)
# library(listviewer)

##### Seasons 2008 to 2016 inclusive ###############################################################

##### Scrape URLs for each match ###################################################################

map_df(c(2008:2016), function(i){
  
  # simple but effective progress indicator
  cat(".")
  
  url <- paste("http://afltables.com/afl/seas/",i,".html", sep="")
  
  df <- data.frame(
    read_html(url) %>%
      html_nodes(xpath = '//body//center//table') %>%
      html_nodes("a") %>%
      html_attr("href")
  )
  
  df2 <- read_html(url) %>%
    html_nodes("h1") %>%
    html_text()
  
  obs <- nrow(df)
  
  season <- rep(df2, times = obs)
  
  df3 <- cbind(season, df)
  
}) -> listOfURLs

##### Clean data ###################################################################################

# Set column names
colnames(listOfURLs) <- c("season", "url")

# Assign data classes
listOfURLs$url <- as.character(listOfURLs$url)

# Isolate URLs that link directly to match stats page for each unique game in a given season
listOfURLs$matchPage <- startsWith(listOfURLs$url, "../stats/games")
listOfURLs <- listOfURLs[ which(listOfURLs$matchPage == TRUE), -3]

# Keep only the year number in the $season variable
listOfURLs$season <- substring(listOfURLs$season, 1, 5)

# Remove .. from URLs
temp <- data.frame(str_split_fixed(listOfURLs$url, "..", 2))
listOfURLs$url <- temp$X2

# Assign data classes
listOfURLs$url <- as.character(listOfURLs$url)
listOfURLs$season <- as.numeric(as.character(listOfURLs$season))

# Pass URLs into season-specific vectors for use in further data scraping
URLs2016 <- listOfURLs[ which(listOfURLs$season == 2016), 2]
URLs2015 <- listOfURLs[ which(listOfURLs$season == 2015), 2]
URLs2014 <- listOfURLs[ which(listOfURLs$season == 2014), 2]
URLs2013 <- listOfURLs[ which(listOfURLs$season == 2013), 2]
URLs2012 <- listOfURLs[ which(listOfURLs$season == 2012), 2]
URLs2011 <- listOfURLs[ which(listOfURLs$season == 2011), 2]
URLs2010 <- listOfURLs[ which(listOfURLs$season == 2010), 2]
URLs2009 <- listOfURLs[ which(listOfURLs$season == 2009), 2]
URLs2008 <- listOfURLs[ which(listOfURLs$season == 2008), 2]

##### Scrape scoring progression table for each match ##############################################

# 2016 season
map_df(URLs2016, function(i) {
  
  # simple but effective progress indicator
  cat(".")
  
  url <- paste("http://afltables.com/afl",i, sep="")
  
  df <- data.frame(
    read_html(url) %>%
      html_nodes(xpath = '/html/body/center/table[8]') %>%
      html_table(fill = TRUE))

  df2 <- data.frame(
    read_html(url) %>%
      html_nodes(xpath = '/html/body/center/table[1]') %>%
      html_nodes("a") %>%
      html_text()
  )
  
  df3 <- data.frame(
    read_html(url) %>%
      html_nodes(xpath = '/html/body/center/table[1]') %>%
      html_table(fill = TRUE)
  )
  
  obs <- nrow(df)
  
  home <- df2[4,]
  home <- as.character(home)
  away <- df2[5,]
  away <- as.character(away)
  matchDetails <- df3[1,2]
  matchDetails <- as.character(matchDetails)
  
  home <- rep(home, times = obs)
  away <- rep(away, times = obs)
  matchDetails <- rep(matchDetails, times = obs)
  
  df <- cbind(matchDetails, home, away, df)
  
}) -> df_scoringProg2016

df_scoringProg2016 <- df_scoringProg2016[,c(1:5,7,8)]

# 2015 season
map_df(URLs2015, function(i) {
  
  # simple but effective progress indicator
  cat(".")
  
  url <- paste("http://afltables.com/afl",i, sep="")
  
  df <- data.frame(
    read_html(url) %>%
      html_nodes(xpath = '/html/body/center/table[8]') %>%
      html_table(fill = TRUE))
  
  df2 <- data.frame(
    read_html(url) %>%
      html_nodes(xpath = '/html/body/center/table[1]') %>%
      html_nodes("a") %>%
      html_text()
  )
  
  df3 <- data.frame(
    read_html(url) %>%
      html_nodes(xpath = '/html/body/center/table[1]') %>%
      html_table(fill = TRUE)
  )
  
  obs <- nrow(df)
  
  home <- df2[4,]
  home <- as.character(home)
  away <- df2[5,]
  away <- as.character(away)
  matchDetails <- df3[1,2]
  matchDetails <- as.character(matchDetails)
  
  home <- rep(home, times = obs)
  away <- rep(away, times = obs)
  matchDetails <- rep(matchDetails, times = obs)
  
  df <- cbind(matchDetails, home, away, df)
  
}) -> df_scoringProg2015

df_scoringProg2015 <- df_scoringProg2015[,c(1:5,7,8)]

# 2014 season
map_df(URLs2014, function(i) {
  
  # simple but effective progress indicator
  cat(".")
  
  url <- paste("http://afltables.com/afl",i, sep="")
  
  df <- data.frame(
    read_html(url) %>%
      html_nodes(xpath = '/html/body/center/table[8]') %>%
      html_table(fill = TRUE))
  
  df2 <- data.frame(
    read_html(url) %>%
      html_nodes(xpath = '/html/body/center/table[1]') %>%
      html_nodes("a") %>%
      html_text()
  )
  
  df3 <- data.frame(
    read_html(url) %>%
      html_nodes(xpath = '/html/body/center/table[1]') %>%
      html_table(fill = TRUE)
  )
  
  obs <- nrow(df)
  
  home <- df2[4,]
  home <- as.character(home)
  away <- df2[5,]
  away <- as.character(away)
  matchDetails <- df3[1,2]
  matchDetails <- as.character(matchDetails)
  
  home <- rep(home, times = obs)
  away <- rep(away, times = obs)
  matchDetails <- rep(matchDetails, times = obs)
  
  df <- cbind(matchDetails, home, away, df)
  
}) -> df_scoringProg2014

df_scoringProg2014 <- df_scoringProg2014[,c(1:5,7,8)]

# 2013 season
map_df(URLs2013, function(i) {
  
  # simple but effective progress indicator
  cat(".")
  
  url <- paste("http://afltables.com/afl",i, sep="")
  
  df <- data.frame(
    read_html(url) %>%
      html_nodes(xpath = '/html/body/center/table[8]') %>%
      html_table(fill = TRUE))
  
  df2 <- data.frame(
    read_html(url) %>%
      html_nodes(xpath = '/html/body/center/table[1]') %>%
      html_nodes("a") %>%
      html_text()
  )
  
  df3 <- data.frame(
    read_html(url) %>%
      html_nodes(xpath = '/html/body/center/table[1]') %>%
      html_table(fill = TRUE)
  )
  
  obs <- nrow(df)
  
  home <- df2[4,]
  home <- as.character(home)
  away <- df2[5,]
  away <- as.character(away)
  matchDetails <- df3[1,2]
  matchDetails <- as.character(matchDetails)
  
  home <- rep(home, times = obs)
  away <- rep(away, times = obs)
  matchDetails <- rep(matchDetails, times = obs)
  
  df <- cbind(matchDetails, home, away, df)
  
}) -> df_scoringProg2013

df_scoringProg2013 <- df_scoringProg2013[,c(1:5,7,8)]

# 2012 season
map_df(URLs2012, function(i) {
  
  # simple but effective progress indicator
  cat(".")
  
  url <- paste("http://afltables.com/afl",i, sep="")
  
  df <- data.frame(
    read_html(url) %>%
      html_nodes(xpath = '/html/body/center/table[8]') %>%
      html_table(fill = TRUE))
  
  df2 <- data.frame(
    read_html(url) %>%
      html_nodes(xpath = '/html/body/center/table[1]') %>%
      html_nodes("a") %>%
      html_text()
  )
  
  df3 <- data.frame(
    read_html(url) %>%
      html_nodes(xpath = '/html/body/center/table[1]') %>%
      html_table(fill = TRUE)
  )
  
  obs <- nrow(df)
  
  home <- df2[4,]
  home <- as.character(home)
  away <- df2[5,]
  away <- as.character(away)
  matchDetails <- df3[1,2]
  matchDetails <- as.character(matchDetails)
  
  home <- rep(home, times = obs)
  away <- rep(away, times = obs)
  matchDetails <- rep(matchDetails, times = obs)
  
  df <- cbind(matchDetails, home, away, df)
  
}) -> df_scoringProg2012

df_scoringProg2012 <- df_scoringProg2012[,c(1:5,7,8)]

# 2011 season
map_df(URLs2011, function(i) {
  
  # simple but effective progress indicator
  cat(".")
  
  url <- paste("http://afltables.com/afl",i, sep="")
  
  df <- data.frame(
    read_html(url) %>%
      html_nodes(xpath = '/html/body/center/table[8]') %>%
      html_table(fill = TRUE))
  
  df2 <- data.frame(
    read_html(url) %>%
      html_nodes(xpath = '/html/body/center/table[1]') %>%
      html_nodes("a") %>%
      html_text()
  )
  
  df3 <- data.frame(
    read_html(url) %>%
      html_nodes(xpath = '/html/body/center/table[1]') %>%
      html_table(fill = TRUE)
  )
  
  obs <- nrow(df)
  
  home <- df2[4,]
  home <- as.character(home)
  away <- df2[5,]
  away <- as.character(away)
  matchDetails <- df3[1,2]
  matchDetails <- as.character(matchDetails)
  
  home <- rep(home, times = obs)
  away <- rep(away, times = obs)
  matchDetails <- rep(matchDetails, times = obs)
  
  df <- cbind(matchDetails, home, away, df)
  
}) -> df_scoringProg2011

df_scoringProg2011 <- df_scoringProg2011[,c(1:5,7,8)]

# 2010 season
map_df(URLs2010, function(i) {
  
  # simple but effective progress indicator
  cat(".")
  
  url <- paste("http://afltables.com/afl",i, sep="")
  
  df <- data.frame(
    read_html(url) %>%
      html_nodes(xpath = '/html/body/center/table[8]') %>%
      html_table(fill = TRUE))
  
  df2 <- data.frame(
    read_html(url) %>%
      html_nodes(xpath = '/html/body/center/table[1]') %>%
      html_nodes("a") %>%
      html_text()
  )
  
  df3 <- data.frame(
    read_html(url) %>%
      html_nodes(xpath = '/html/body/center/table[1]') %>%
      html_table(fill = TRUE)
  )
  
  obs <- nrow(df)
  
  home <- df2[4,]
  home <- as.character(home)
  away <- df2[5,]
  away <- as.character(away)
  matchDetails <- df3[1,2]
  matchDetails <- as.character(matchDetails)
  
  home <- rep(home, times = obs)
  away <- rep(away, times = obs)
  matchDetails <- rep(matchDetails, times = obs)
  
  df <- cbind(matchDetails, home, away, df)
  
}) -> df_scoringProg2010

df_scoringProg2010 <- df_scoringProg2010[,c(1:5,7,8)]

# 2009 season
map_df(URLs2009, function(i) {
  
  # simple but effective progress indicator
  cat(".")
  
  url <- paste("http://afltables.com/afl",i, sep="")
  
  df <- data.frame(
    read_html(url) %>%
      html_nodes(xpath = '/html/body/center/table[8]') %>%
      html_table(fill = TRUE))
  
  df2 <- data.frame(
    read_html(url) %>%
      html_nodes(xpath = '/html/body/center/table[1]') %>%
      html_nodes("a") %>%
      html_text()
  )
  
  df3 <- data.frame(
    read_html(url) %>%
      html_nodes(xpath = '/html/body/center/table[1]') %>%
      html_table(fill = TRUE)
  )
  
  obs <- nrow(df)
  
  home <- df2[4,]
  home <- as.character(home)
  away <- df2[5,]
  away <- as.character(away)
  matchDetails <- df3[1,2]
  matchDetails <- as.character(matchDetails)
  
  home <- rep(home, times = obs)
  away <- rep(away, times = obs)
  matchDetails <- rep(matchDetails, times = obs)
  
  df <- cbind(matchDetails, home, away, df)
  
}) -> df_scoringProg2009

df_scoringProg2009 <- df_scoringProg2009[,c(1:5,7,8)]

# 2008 season
map_df(URLs2008, function(i) {
  
  # simple but effective progress indicator
  cat(".")
  
  url <- paste("http://afltables.com/afl",i, sep="")
  
  df <- data.frame(
    read_html(url) %>%
      html_nodes(xpath = '/html/body/center/table[8]') %>%
      html_table(fill = TRUE))
  
  df2 <- data.frame(
    read_html(url) %>%
      html_nodes(xpath = '/html/body/center/table[1]') %>%
      html_nodes("a") %>%
      html_text()
  )
  
  df3 <- data.frame(
    read_html(url) %>%
      html_nodes(xpath = '/html/body/center/table[1]') %>%
      html_table(fill = TRUE)
  )
  
  obs <- nrow(df)
  
  home <- df2[4,]
  home <- as.character(home)
  away <- df2[5,]
  away <- as.character(away)
  matchDetails <- df3[1,2]
  matchDetails <- as.character(matchDetails)
  
  home <- rep(home, times = obs)
  away <- rep(away, times = obs)
  matchDetails <- rep(matchDetails, times = obs)
  
  df <- cbind(matchDetails, home, away, df)
  
}) -> df_scoringProg2008

df_scoringProg2008 <- df_scoringProg2008[,c(1:5,7,8)]

# Assign column names
colnames(df_scoringProg2016) <- c(
  "matchDetails", "homeTeam", "awayTeam", "homeTeamScores", "homeTeamScoreTime", "awayTeamScoreTime", "awayTeamScores"
)
colnames(df_scoringProg2015) <- c(
  "matchDetails", "homeTeam", "awayTeam", "homeTeamScores", "homeTeamScoreTime", "awayTeamScoreTime", "awayTeamScores"
)
colnames(df_scoringProg2014) <- c(
  "matchDetails", "homeTeam", "awayTeam", "homeTeamScores", "homeTeamScoreTime", "awayTeamScoreTime", "awayTeamScores"
)
colnames(df_scoringProg2013) <- c(
  "matchDetails", "homeTeam", "awayTeam", "homeTeamScores", "homeTeamScoreTime", "awayTeamScoreTime", "awayTeamScores"
)
colnames(df_scoringProg2012) <- c(
  "matchDetails", "homeTeam", "awayTeam", "homeTeamScores", "homeTeamScoreTime", "awayTeamScoreTime", "awayTeamScores"
)
colnames(df_scoringProg2011) <- c(
  "matchDetails", "homeTeam", "awayTeam", "homeTeamScores", "homeTeamScoreTime", "awayTeamScoreTime", "awayTeamScores"
)
colnames(df_scoringProg2010) <- c(
  "matchDetails", "homeTeam", "awayTeam", "homeTeamScores", "homeTeamScoreTime", "awayTeamScoreTime", "awayTeamScores"
)
colnames(df_scoringProg2009) <- c(
  "matchDetails", "homeTeam", "awayTeam", "homeTeamScores", "homeTeamScoreTime", "awayTeamScoreTime", "awayTeamScores"
)
colnames(df_scoringProg2008) <- c(
  "matchDetails", "homeTeam", "awayTeam", "homeTeamScores", "homeTeamScoreTime", "awayTeamScoreTime", "awayTeamScores"
)

# Add $season variable
df_scoringProg2016$season <- "2016"
df_scoringProg2015$season <- "2015"
df_scoringProg2014$season <- "2014"
df_scoringProg2013$season <- "2013"
df_scoringProg2012$season <- "2012"
df_scoringProg2011$season <- "2011"
df_scoringProg2010$season <- "2010"
df_scoringProg2009$season <- "2009"
df_scoringProg2008$season <- "2008"

# Merge into one data frame
df_scoringProgALL <- rbind(
  df_scoringProg2016, df_scoringProg2015, df_scoringProg2014, df_scoringProg2013, df_scoringProg2012, df_scoringProg2011,
  df_scoringProg2010, df_scoringProg2009, df_scoringProg2008
)

# Save messy data set to CSV (for ease of access - import CSV isntead of repeating scraping processes)
write.csv(df_scoringProgALL, "df_scoringProgALL_messy.csv", row.names = FALSE)
