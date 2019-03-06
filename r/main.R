# Load required libraries, credentials and dataframes
if(!exists("utilize", mode="function")){ 
  source("util.R")
  utilize()
}

source('spotify.R')

PopMusic <- rbind(top90s,top00s,top10s)

dance_hist_times=c(nrow(pharrell), nrow(top90s), nrow(top10s))
dance_hist <- data.frame(
    Playlist=rep(c("Pharrell", "Top90s", "Top10s"), times=dance_hist_times),
                  Danceability=c(pharrell$danceability, top90s$danceability, top10s$danceability)
                )

# Histograms for Danceability
ggplot(dance_hist, aes(x=Danceability, color=Playlist, fill=Playlist)) +
  geom_density() + 
  scale_color_manual(values=c("#FF000A", "#FF8700", "#00C9F8")) +
  scale_fill_manual(values=c("#FF000A", "#FF8700", "#00C9F8")) +
  ylab("") + 
  xlab("") +
  theme_classic() 

# See if there is any relationship between certain audio feautures and the popularity of the songs
LRPop <- popmusic$track_popularity ~ popmusic$danceability + popmusic$energy + popmusic$valence
LRPharrell <- pharrell$track_popularity ~ pharrell$danceability + pharrell$energy + pharrell$valence 

summary(lm(popmusic$track_popularity ~ popmusic$danceability + popmusic$energy + popmusic$valence))
summary(lm(pharrell$track_popularity ~ pharrell$danceability + pharrell$energy + pharrell$valence))

