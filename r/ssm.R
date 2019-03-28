library(tidyverse)
library(spotifyr)
library(compmus)
source('spotify.R')

# SSM For Hot in Herre

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
          method = 'rms'))

HOT_SSM_TIMBRE <- HOT_SSM %>% 
  compmus_self_similarity(timbre, 'angular') %>% 
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
  labs(x = '', y = '') +
  theme(axis.text=element_text(size=14, color = "white"), axis.line = element_line(colour = "#2E3E47"))

HOT_SSM_PITCH <- HOT_SSM %>% 
  compmus_self_similarity(pitches, 'cosine') %>% 
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
  labs(x = '', y = '') +
  theme(axis.text=element_text(size=14, color = "white"), axis.line = element_line(colour = "#2E3E47"))

# SSM for Drop It Like It's Hot
DROP_SSM <- 
  get_tidy_audio_analysis('4agi7jEwpU2PLKdoe7t1nU') %>% 
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
          method = 'rms'))

DROP_SSM_TIMBRE <- DROP_SSM %>% 
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
  theme_classic() +
  labs(x = '', y = '') +
  theme(axis.text=element_text(size=14, color = "white"), axis.line = element_line(colour = "#2E3E47"))

DROP_SSM_PITCH <- DROP_SSM %>% 
  compmus_self_similarity(pitches, 'cosine') %>% 
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
  labs(x = '', y = '') +
  theme(axis.text=element_text(size=14, color = "white"), axis.line = element_line(colour = "#2E3E47"))

# SSM for Get Lucky
LUCKY_SSM <- 
  get_tidy_audio_analysis('2Foc5Q5nqNiosCNqttzHof') %>% 
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
          method = 'rms'))

LUCKY_SSM_TIMBRE <- LUCKY_SSM %>% 
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
  theme_classic() +
  labs(x = '', y = '') +
  theme(axis.text=element_text(size=14, color = "white"), axis.line = element_line(colour = "#2E3E47"))

LUCKY_SSM_PITCH <- LUCKY_SSM %>% 
  compmus_self_similarity(pitches, 'cosine') %>% 
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
  labs(x = '', y = '') +
  theme(axis.text=element_text(size=14, color = "white"), axis.line = element_line(colour = "#2E3E47"))

# Looking at both SSM of Hot in Herre, both the SSM from the Timbre Features as well as from the Chroma features
# show a clear structure with lots of repetition as can be seen from the darker diagonals running through the 
# checker boxes.

# The checkerboard pattern of the pitch SSM looks like a jacket that Pharrell would wear! Beside that, the
# structure of this song is even more obvious. It's nearly identical blocks of identical size, execpt
# for the endig of the song and the start, where some new things happen. This again shows a true
# aspect of the music by the Neptunes: predictability. That's probably the reason why everyone 
# in clubs will go crazy as soon as they hear the 4-tap bass start of Drop It Like It's Hot.

# Finally, I thought it would be of interest to look if the continuation of The Neptunes sound in popular music
# -- the songs produced solo by Pharrell -- showed a similar segmentation, and it doesn't. There is no clear
# structure in this song as there was in the previous songs, even after tweaking the settings and changing
# some of the norms and distances, it still remains the same. Like Dr. Burgoyne mentioned in a lecture,
# Get Lucky was a song that was everywhere at the time it was released, from popular clubs to the Albert Heijn.
# So, it was really popular. But, it pays no resemblance to the other songs investigated trough the self-
# similarity matrices. 