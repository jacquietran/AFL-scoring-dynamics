##### Load libraries ###############################################################################

library(TTR)
library(changepoint)

#### Scoring probability per time instance - all teams, all games ##################################
#### Fit simple moving average #####################################################################

# Use data frame: scoreProbability_allTeamsAllGames
movingAv <- SMA(scoreProbability_allTeamsAllGames$probOfScoring_both, n = 25)
scoreProbability_allTeamsAllGames <- cbind(scoreProbability_allTeamsAllGames, movingAv)

#### LOESS regression on moving average values #####################################################

scoreProb_allTeamsAllGames_LOESS <- loess(
  scoreProbability_allTeamsAllGames$movingAv ~ scoreProbability_allTeamsAllGames$scoreTime_pct,
  span = 0.1)
# Span chosen based on visual inspection of loess regression overlaid on moving average
# Value chosen to balance fit and smoothing
scoreTime_pct_range <- scoreProbability_allTeamsAllGames$scoreTime_pct
scoreProb_allTeamsAllGames_EstVec <- predict(scoreProb_allTeamsAllGames_LOESS, scoreTime_pct_range,
                                                 se = TRUE)
scoreProb_allTeamsAllGames_Est <- data.frame(scoreTime_pct_range, scoreProb_allTeamsAllGames_EstVec)
scoreProb_allTeamsAllGames_Est <- scoreProb_allTeamsAllGames_Est[,-c(4,5)]
scoreProb_allTeamsAllGames_Est$upper <- scoreProb_allTeamsAllGames_Est$fit + (
  1.96 * scoreProb_allTeamsAllGames_Est$se.fit)
scoreProb_allTeamsAllGames_Est$lower <- scoreProb_allTeamsAllGames_Est$fit - (
  1.96 * scoreProb_allTeamsAllGames_Est$se.fit)
colnames(scoreProb_allTeamsAllGames_Est) <- c(
  "scoreTime_pct", "probOfScoring_est", "probOfScoring_est.se",
  "probOfScoring_est.upper", "probOfScoring_est.lower")
# Use scoreProb_allTeamsAllGames_Est$probOfScoring_est vector in changepoint analysis

# Remove interstitial objects
rm(movingAv, scoreProb_allTeamsAllGames_LOESS, scoreTime_pct_range)

##### Decompose moving average time series #########################################################

# Format moving average as time series object
# Frequency = 250 to account for quarter-to-quarter 'seasonality'
ts.movingAv <- ts(scoreProbability_allTeamsAllGames$movingAv, frequency = 250)

# Decompose time series into seasonal, trend, and random components
d.movingAv <- decompose(ts.movingAv)
# plot(d.movingAv)

# Extract seasonal component
d.movingAv_seasonal <- d.movingAv$seasonal

# Extract trend component
d.movingAv_trend <- d.movingAv$trend

# Extract seasonally adjusted components (trend + random components)
d.movingAv_seasonallyAdj <- ts.movingAv - d.movingAv$seasonal

##### Decompose LOESS time series ##################################################################

# Format LOESS estimates as time series object
# Frequency = 250 to account for quarter-to-quarter 'seasonality'
ts.LOESS <- ts(scoreProb_allTeamsAllGames_EstVec$fit, frequency = 250)

# Decompose time series into seasonal, trend, and random components
d.LOESS <- decompose(ts.LOESS)
# plot(d.LOESS)

# Extract seasonal component
d.LOESS_seasonal <- d.LOESS$seasonal

# Extract trend component
d.LOESS_trend <- d.LOESS$trend

# Extract seasonally adjusted components (trend + random components)
d.LOESS_seasonallyAdj <- ts.LOESS - d.LOESS$seasonal

##### Changepoint detection ########################################################################

##### Moving average, trend component ##############################################################

# Minor data wrangling
d.movingAv_trend_cpts <- data.frame(d.movingAv_trend)
colnames(d.movingAv_trend_cpts) <- "probOfScoring"
d.movingAv_trend_cpts$drop <- is.na(d.movingAv_trend_cpts$probOfScoring)
d.movingAv_trend_cpts <- data.frame(d.movingAv_trend_cpts[ which(
  d.movingAv_trend_cpts$drop == FALSE), -2])
d.movingAv_trend_cpts$scoreTime_pctObs <- seq(150, 876, by = 1)
colnames(d.movingAv_trend_cpts) <- c("probOfScoring", "scoreTime_pctObs")

# Changepoint detection based on changes in mean and variance
d.movingAv_trend_cptsOut <- cpt.meanvar(d.movingAv_trend_cpts$probOfScoring, method = "BinSeg", Q = 9)
# plot(d.movingAv_trend_cptsOut)

# Extract changepoint values
d.movingAv_trend_cptsValues <- data.frame(cpts(d.movingAv_trend_cptsOut))
colnames(d.movingAv_trend_cptsValues) <- "scoreTime_pctObs"
# Add 149 to changepoint values to account for missing data through initial period of time series
d.movingAv_trend_cptsValues$scoreTime_pctObs <- d.movingAv_trend_cptsValues$scoreTime_pctObs + 149

# Convert time instances to scoreTime_pct
seq1 <- seq(1, 1001, by = 1)
seq2 <- seq(0.0, 100.0, by = 0.1)
temp <- data.frame(seq1, seq2)
colnames(temp) <- c("scoreTime_pctObs", "scoreTime_Pct")
rm(seq1, seq2)
d.movingAv_trend_cptsValues <- inner_join(d.movingAv_trend_cptsValues, temp)

# Convert to vector
d.movingAv_trend_cptsValues <- d.movingAv_trend_cptsValues$scoreTime_Pct
# Use d.movingAv_trend_cptsValues when plotting time series decomposition plots

##### LOESS estimates, trend component #############################################################

# Minor data wrangling
d.LOESS_trend_cpts <- data.frame(d.LOESS_trend)
colnames(d.LOESS_trend_cpts) <- "probOfScoring"
d.LOESS_trend_cpts$drop <- is.na(d.LOESS_trend_cpts$probOfScoring)
d.LOESS_trend_cpts <- data.frame(d.LOESS_trend_cpts[ which(d.LOESS_trend_cpts$drop == FALSE), -2])
d.LOESS_trend_cpts$scoreTime_pctObs <- seq(150, 876, by = 1)
colnames(d.LOESS_trend_cpts) <- c("probOfScoring", "scoreTime_pctObs")

# Changepoint detection based on changes in mean and variance
d.LOESS_trend_cptsOut <- cpt.meanvar(d.LOESS_trend_cpts$probOfScoring, method = "BinSeg", Q = 9)
# plot(d.LOESS_trend_cptsOut)

# Extract changepoint values
d.LOESS_trend_cptsValues <- data.frame(cpts(d.LOESS_trend_cptsOut))
colnames(d.LOESS_trend_cptsValues) <- "scoreTime_pctObs"
# Add 149 to changepoint values to account for missing data through initial period of time series
d.LOESS_trend_cptsValues$scoreTime_pctObs <- d.LOESS_trend_cptsValues$scoreTime_pctObs + 149

# Convert time instances to scoreTime_pct
d.LOESS_trend_cptsValues <- inner_join(d.LOESS_trend_cptsValues, temp)

# Convert to vector
d.LOESS_trend_cptsValues <- d.LOESS_trend_cptsValues$scoreTime_Pct
# Use d.LOESS_trend_cptsValues when plotting time series decomposition plots