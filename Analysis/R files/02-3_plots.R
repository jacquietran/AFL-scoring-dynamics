##### Load libraries ###############################################################################

library(ggplot2)
library(reshape2)

##### Plots: All teams, all games ##################################################################

##### Moving average and LOESS #####################################################################

# Raw probability over time, before decomposition
# Y axis originates at zero (no truncation)
p <- ggplot(scoreProbability_allTeamsAllGames, aes(x = scoreTime_pct, y = movingAv))
p <- p + geom_line(colour = "black")
p <- p + geom_smooth(method = "loess", span = 0.1, se = TRUE, size = 2)
p <- p + scale_x_continuous(breaks = c(0,25,50,75,100),
                            labels = c("0%", "25%", "50%", "75%", "100%"),
                            minor_breaks = seq(0,100,5))
p <- p + labs(
  x = "Time within game (% of total game duration)",
  y = "Probability of scoring event")
p <- p + coord_cartesian(ylim = c(0,0.004))
p <- p + theme_minimal()
p <- p + theme(
  panel.grid.minor.y = element_blank(),
  panel.grid.major.y = element_blank(),
  panel.grid.minor.x = element_line(linetype = "dotted", colour = "grey"),
  panel.grid.major.x = element_line(linetype = "dashed", size = 1, colour = "lightgrey"),
  axis.title = element_text(size = 16),
  axis.text = element_text(size = 14))

# Save to PNG
plot.probOfScoring_allTeamsAllGames <- p
ggsave("probOfScoring_allTeamsAllGames.png", width = 24, height = 16, units = "cm", dpi = 300)

##### Moving average and LOESS #####################################################################

# Raw probability over time, before decomposition
# Truncated Y axis to show small changes
p <- ggplot(scoreProbability_allTeamsAllGames, aes(x = scoreTime_pct, y = movingAv))
p <- p + geom_line(colour = "black")
p <- p + geom_smooth(method = "loess", span = 0.1, se = TRUE, size = 2)
p <- p + scale_y_continuous(breaks = c(0,0.0002,0.0004,0.0006,0.0008,
                                       0.001,0.0012,0.0014,0.0016,0.0018,
                                       0.002,0.0022,0.0024,0.0026,0.0028,
                                       0.003,0.0032,0.0034,0.0036,0.0038,
                                       0.004))
p <- p + scale_x_continuous(breaks = c(0,25,50,75,100),
                            labels = c("0%", "25%", "50%", "75%", "100%"),
                            minor_breaks = seq(0,100,5))
p <- p + labs(
  x = "Time within game (% of total game duration)",
  y = "Probability of scoring event")
p <- p + coord_cartesian(ylim = c(0.003,0.0037))
p <- p + theme_minimal()
p <- p + theme(
  panel.grid.minor.y = element_blank(),
  panel.grid.major.y = element_blank(),
  panel.grid.minor.x = element_line(linetype = "dotted", colour = "grey"),
  panel.grid.major.x = element_line(linetype = "dashed", size = 1, colour = "lightgrey"),
  axis.title = element_text(size = 16),
  axis.text = element_text(size = 14))

# Save to PNG
plot.probOfScoring_allTeamsAllGames_trunc <- p
ggsave("probOfScoring_allTeamsAllGames_trunc.png", width = 24, height = 16, units = "cm", dpi = 300)

##### Decomposed moving average ####################################################################

# Minor data wrangling
# Convert decomposed moving average data from list object to data frame
df_d.movingAv <- data.frame(
  scoreProbability_allTeamsAllGames$scoreTime_pct,
  d.movingAv$x, d.movingAv$seasonal, d.movingAv$trend, d.movingAv$random
)
colnames(df_d.movingAv) <- c(
  "scoreTime_pct", "Observed", "Seasonal", "Trend", "Random"
)

# Assign data classes
df_d.movingAv$Observed <- as.numeric(as.character(df_d.movingAv$Observed))
df_d.movingAv$Seasonal <- as.numeric(as.character(df_d.movingAv$Seasonal))
df_d.movingAv$Trend <- as.numeric(as.character(df_d.movingAv$Trend))
df_d.movingAv$Random <- as.numeric(as.character(df_d.movingAv$Random))

# Melt wide to long
df_d.movingAv <- melt(df_d.movingAv, id.vars = "scoreTime_pct")

# Plot decomposed moving average of scoring probability
p <- ggplot(df_d.movingAv, aes(x = scoreTime_pct, y = value))
p <- p + geom_line()
p <- p + facet_wrap(~variable,
                    nrow = 4, ncol = 1,
                    scales = "free_y")
p <- p + scale_y_continuous(labels = scales::comma)
p <- p + scale_x_continuous(breaks = c(0,25,50,75,100),
                            labels = c("0%", "25%", "50%", "75%", "100%"),
                            minor_breaks = seq(0,100,5))
p <- p + labs(
  x = "Time within game\n(% of total game duration)",
  y = "Probability of scoring event")
p <- p + theme(
  panel.background = element_blank(),
  panel.grid.minor.y = element_blank(),
  panel.grid.major.y = element_blank(),
  panel.grid.minor.x = element_line(linetype = "dotted", colour = "grey"),
  panel.grid.major.x = element_line(linetype = "dashed", size = 1, colour = "lightgrey"),
  axis.title = element_text(size = 16),
  axis.text = element_text(size = 14),
  strip.text = element_text(size = 14))

# Save to PNG
plot.decompMovingAv <- p
ggsave("decompMovingAv.png", width = 14, height = 18, units = "cm", dpi = 300)

##### Decomposed LOESS estimates ###################################################################

# Minor data wrangling
# Convert decomposed LOESS estimates data from list object to data frame
df_d.LOESS <- data.frame(
  scoreProbability_allTeamsAllGames$scoreTime_pct,
  d.LOESS$x, d.LOESS$seasonal, d.LOESS$trend, d.LOESS$random
)
colnames(df_d.LOESS) <- c(
  "scoreTime_pct", "Observed", "Seasonal", "Trend", "Random"
)

# Assign data classes
df_d.LOESS$Observed <- as.numeric(as.character(df_d.LOESS$Observed))
df_d.LOESS$Seasonal <- as.numeric(as.character(df_d.LOESS$Seasonal))
df_d.LOESS$Trend <- as.numeric(as.character(df_d.LOESS$Trend))
df_d.LOESS$Random <- as.numeric(as.character(df_d.LOESS$Random))

# Melt wide to long
df_d.LOESS <- melt(df_d.LOESS, id.vars = "scoreTime_pct")

# Plot decomposed LOESS of scoring probability
p <- ggplot(df_d.LOESS, aes(x = scoreTime_pct, y = value))
p <- p + geom_line(colour = "#3C5DE7")
p <- p + facet_wrap(~variable,
                    nrow = 4, ncol = 1,
                    scales = "free_y")
p <- p + scale_y_continuous(labels = scales::comma)
p <- p + scale_x_continuous(breaks = c(0,25,50,75,100),
                            labels = c("0%", "25%", "50%", "75%", "100%"),
                            minor_breaks = seq(0,100,5))
p <- p + labs(
  x = "Time within game\n(% of total game duration)",
  y = "Probability of scoring event")
p <- p + theme(
  panel.background = element_blank(),
  panel.grid.minor.y = element_blank(),
  panel.grid.major.y = element_blank(),
  panel.grid.minor.x = element_line(linetype = "dotted", colour = "grey"),
  panel.grid.major.x = element_line(linetype = "dashed", size = 1, colour = "lightgrey"),
  axis.title = element_text(size = 16),
  axis.text = element_text(size = 14),
  strip.text = element_text(size = 14))

# Save to PNG
plot.decompLOESS <- p
ggsave("decompLOESS.png", width = 36, height = 20, units = "cm", dpi = 600)
