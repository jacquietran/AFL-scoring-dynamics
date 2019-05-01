geelong <- subset(df_scoreDiffFinal, team %in% c("Geelong") )


geelong$season <- as.factor(geelong$season)       

geelong$result <- as.factor(geelong$result) 

geelong$scoreTime_per <- as.factor(geelong$scoreTime_per) 

geelong$scoreTime_perTemp <- c(tail(geelong$scoreTime_per,-1),NA)

geelong$scoreTime_perFinal <- ifelse(geelong$scoreTime_perTemp == geelong$scoreTime_per | (is.na(geelong$scoreTime_per) & is.na(geelong$scoreTime_perTemp)),NA,geelong$scoreTime_per )

geelong <- geelong[-c(8)]
  
df_geelong <- na.omit(geelong)

test <- ddply(df_geelong, .(season, round, scoreTime_perFinal), summarise,
    diff = round(scoreDiff)
  )


test1 <- ddply(test, .(season, scoreTime_perFinal), summarise,
               average = round(mean(diff), 2))

test$scoreTime_per <- as.numeric(test$scoreTime_per)
test1$scoreTime_perFinal <- as.numeric(test1$scoreTime_perFinal)

###

p <- ggplot(test, aes(x = scoreTime_perFinal, y = diff))
p <- p + geom_line(lwd = 0.2, aes(linetype = round), col = "grey")
p <- p + geom_smooth(method = "loess", span = 0.1, lwd = 1, se = FALSE, aes(y = diff), col = "Blue")
p <- p + scale_x_continuous(breaks = c(0,25,50,75,100),
                            labels = c("0%", "25%", "50%", "75%", "100%"),
                            minor_breaks = seq(0,100,5))
p <- p + scale_y_continuous(breaks = c(-100, -75, -50, -25, 0, 25, 50, 75, 100, 125),
                            labels = c(-100, -75, -50, -25, 0, 25, 50, 75, 100, 125),
                            minor_breaks = seq(-125,125,25))
p <- p + theme_bw() + theme(legend.title = element_blank(), legend.position = "blank", panel.grid = element_blank()) + xlab("% Game Time") + ylab("Winning Score Difference")


p <- p + facet_wrap(~season)

###

