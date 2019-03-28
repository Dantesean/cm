# # Load required libraries, credentials and dataframes
# if(!exists("utilize", mode="function")){
#   source("util.R")
#   utilize()
# }
load('data.RData')
library(tidyverse)
library(tidymodels)
library(ggdendro)
library(protoclust)
library(heatmaply)
library(spotifyr)
library(compmus)
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

# Week 9 Assignment, Self-Similarity Matricces

#First, load the data for the "Hot in Herre" song
HOT_SSM <- 
  get_tidy_audio_analysis('0oXuKhuNkXiZtuoxrdt3Ca') %>% 
  compmus_align(bars, segments) %>% 
  select(bars) %>% unnest(bars) %>% 
  mutate(
    pitches = 
      map(segments, 
          compmus_summarise, pitches, 
          method = 'rms', norm = 'euclidean')) %>% 
  mutate(
    timbre = 
      map(segments, 
          compmus_summarise, timbre, 
          method = 'mean'))

# Then create the cepstogram
HOT_SSM %>% 
  compmus_gather_timbre %>% 
  ggplot(
    aes(
      x = start + duration / 2, 
      width = duration, 
      y = basis, 
      fill = value)) + 
  geom_tile() +
  labs(x = 'Time (s)', y = NULL, fill = 'Magnitude') +
  scale_fill_viridis_c(option = 'E') +
  theme_minimal() +
  scale_fill_gradient(low = "#FFC200", high = "#FF000A")

# Fooling around with the settings in the first part, I don't notice any difference at all. I also really don't know
# right now what I should be seeing tho..

# More insight my be gained when comparing the self similarity matrices of a couple of songs.
HOT_SSM_TIMBRE <- HOT_SSM %>% 
  compmus_self_similarity(timbre, 'cosine') %>% 
  ggplot(
    aes(
      x = xstart + xduration / 2, 
      width = xduration,
      y = ystart + yduration / 2,
      height = yduration,
      fill = d)) + 
  geom_tile() +
  coord_fixed() +
  scale_fill_viridis_c(option = 'E', guide = 'none') +
  theme_minimal() +
  labs(x = '', y = '')

HOT_SSM_PITCH <- HOT_SSM %>% 
  compmus_self_similarity(pitch, 'cosine') %>% 
  ggplot(
    aes(
      x = xstart + xduration / 2, 
      width = xduration,
      y = ystart + yduration / 2,
      height = yduration,
      fill = d)) + 
  geom_tile() +
  coord_fixed() +
  scale_fill_viridis_c(option = 'E', guide = 'none') +
  theme_minimal() +
  labs(x = '', y = '')

# There is a clear structure in this song, let's compare it to another popular song by The Neptunes to see if this structure is part of the "Neptunes Sound"
DROP_SSM <- 
  get_tidy_audio_analysis('4agi7jEwpU2PLKdoe7t1nU') %>% 
  compmus_align(bars, segments) %>% 
  select(bars) %>% unnest(bars) %>% 
  mutate(
    pitches = 
      map(segments, 
          compmus_summarise, pitches, 
          method = 'mean', norm = 'manhattan')) %>% 
  mutate(
    timbre = 
      map(segments, 
          compmus_summarise, timbre, 
          method = 'mean'))

DROP_SSM %>% 
  compmus_self_similarity(timbre, 'euclidean') %>% 
  ggplot(
    aes(
      x = xstart + xduration / 2, 
      width = xduration,
      y = ystart + yduration / 2,
      height = yduration,
      fill = d)) + 
  geom_tile() +
  coord_fixed() +
  scale_fill_viridis_c(option = 'E', guide = 'none') +
  theme_classic() +
  labs(x = '', y = '')

# In the latter song, it's clear that there is a lot of repetition in the songs. Apart from the spoken words, the beat remains the same,
# and this can be clearly seen in the self-similarity matrix by the blue lines throughout the song. When compared to the first song, it still
# consits of a lot of "new" parts. "Hot in Herre" contains way more repition. This might be due to the song, but by listening to it you 
# will find out that Nelly is not as good with words as Snoop Dogg is.



# Assignment Week 12: Clustering
# It could be really interesting to see if songs from particular years are put into clusters.