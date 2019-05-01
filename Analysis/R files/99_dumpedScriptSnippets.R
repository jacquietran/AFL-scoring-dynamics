##### Changepoint analysis #########################################################################

##### Decomposed moving average - seasonal component ###############################################

# Extract seasonal component vector from d.movingAv_seasonal
cpts_movingAv_seasonal <- data.frame(scoreProbability_allTeamsAllGames$scoreTime_pct,
                                     d.movingAv_seasonal)
colnames(cpts_movingAv_seasonal) <- c("scoreTime_pct", "movingAv_seasonal")

# Remove NA values
cpts_movingAv_seasonal$drop <- is.na(cpts_movingAv_seasonal$movingAv_seasonal)
cpts_movingAv_seasonal <- cpts_movingAv_seasonal[ which(cpts_movingAv_seasonal$drop == FALSE), -3]

# Convert vector back to time series object
ts.movingAv_seasonal <- ts(cpts_movingAv_seasonal$movingAv_seasonal, frequency = 250)

# Changepoint analysis
mvalue <- cpt.var(ts.movingAv_seasonal, method = "PELT", Q = 10)
cpts(mvalue)
plot(mvalue)

##### Decomposed moving average - trend component ##################################################

# Extract trend component vector from d.movingAv_trend
cpts_movingAv_trend <- data.frame(scoreProbability_allTeamsAllGames$scoreTime_pct,
                                  d.movingAv_trend)
colnames(cpts_movingAv_trend) <- c("scoreTime_pct", "movingAv_trend")

# Remove NA values
cpts_movingAv_trend$drop <- is.na(cpts_movingAv_trend$movingAv_trend)
cpts_movingAv_trend <- cpts_movingAv_trend[ which(cpts_movingAv_trend$drop == FALSE), -3]

# Convert vector back to time series object


# Changepoint analysis
mvalue <- cpt.var(cpts_movingAv_trend$movingAv_trend, method = "BinSeg", Q = 10)
cpts(mvalue)
plot(mvalue)

##### Changepoint analysis on LOESS estimates ######################################################

# Extract LOESS estimates vector from scoreProb_allTeamsAllGames and remove NA values
LOESS_estVec <- scoreProb_allTeamsAllGames_Est[,c(1:2)]
LOESS_estVec$drop <- is.na(LOESS_estVec$probOfScoring_est)
LOESS_estVec <- LOESS_estVec[ which(LOESS_estVec$drop == FALSE), -3]

# Changepoint analysis
mvalue <- cpt.var(LOESS_estVec$probOfScoring_est, method = "PELT", Q = 20)
cpts(mvalue)
plot(mvalue)

vnvalue <- cpt.var(diff(LOESS_estVec$probOfScoring_est), method = "PELT", Q = 10)
cpts(vnvalue)
plot(vnvalue)
