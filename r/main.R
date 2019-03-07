# # Load required libraries, credentials and dataframes
# if(!exists("utilize", mode="function")){
#   source("util.R")
#   utilize()
# }
load('data.RData')
library(tidyverse)
library(spotifyr)
library(compmus)
# library(showtext)
# font.add('DIN', regular='DINCondensed-Bold.ttf')
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

# Week 8 Assignment, Chromagram
# When looking at the data frame for music by The Neptunes, we can clearly see that a quiet popular song by Tyler, The Creator
# titled 'IFHY' has quiet a low danceability, while still being really popular. What is going on in this song?

IFHY <- 
  get_tidy_audio_analysis('0NjW4SKY3gbfl2orl1p8hr') %>% 
  select(segments) %>% unnest(segments) %>% 
  select(start, duration, pitches)

IFHY %>% 
  mutate(pitches = map(pitches, compmus_normalise, 'chebyshev')) %>% 
  compmus_gather_chroma %>% 
  ggplot(
    aes(
      x = start + duration / 2, 
      width = duration, 
      y = pitch_class, 
      fill = value)) + 
  geom_tile() +
  labs(x = 'Time (s)', y = NULL, fill = 'Magnitude') +
  theme_minimal() +
  scale_fill_gradient(low = "#FFC200", high = "#FF000A")

HOT <-
  get_tidy_audio_analysis('0oXuKhuNkXiZtuoxrdt3Ca') %>% 
  select(segments) %>% unnest(segments) %>% 
  select(start, duration, pitches)

HOT %>% 
  mutate(pitches = map(pitches, compmus_normalise, 'euclidean')) %>% 
  compmus_gather_chroma %>% 
  ggplot(
    aes(
      x = start + duration / 2, 
      width = duration, 
      y = pitch_class, 
      fill = value)) + 
  geom_tile() +
  labs(x = 'Time (s)', y = NULL, fill = 'Magnitude') +
  theme_minimal() +
  scale_fill_gradient(low = "#FFC200", high = "#FF000A")