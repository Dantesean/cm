library(tidyverse)
library(tidymodels)
library(ggdendro)
library(protoclust)
library(heatmaply)
library(spotifyr)
library(compmus)
source('spotify.R')

# Let's first rearrange our dataset of The Neptunes and Pop Music, so that it will not contain all songs in
# the corpera, but summarise them to their means in each consecutive year for each playlist

nep_clust_pre <- 
  neptunes_90s %>%
  arrange(desc(track_popularity)) %>%
  filter(track_popularity > 0 ) %>%
  filter(track_name != "Lemon - Drake Remix") %>%
  slice(1:20) %>%
  bind_rows(
    neptunes_00s %>%
      arrange(desc(track_popularity)) %>%
      filter(track_popularity > 0 ) %>%
      filter(track_name != "Lemon - Drake Remix") %>%
      slice(1:20),
    neptunes_10s %>%
      arrange(desc(track_popularity)) %>%
      filter(track_popularity > 0 ) %>%
      filter(track_name != "Lemon - Drake Remix") %>%
      slice(1:20)
  )

nep_clust <- 
  nep_clust_pre %>%
  mutate(
    segments = 
      map2(segments, key, compmus_c_transpose)) %>% 
  mutate(
    pitches = 
      map(segments, 
          compmus_summarise, pitches, 
          method = 'mean', norm = 'manhattan'),
    timbre =
      map(
        segments,
        compmus_summarise, timbre,
        method = 'mean')) %>% 
  mutate(pitches = map(pitches, compmus_normalise, 'clr')) %>% 
  mutate_at(vars(pitches, timbre), map, bind_rows) %>% 
  unnest(pitches, timbre) %>%
  mutate(track_name = paste("N ", year, " ", track_name))

nep_clust_juice <- 
  recipe(track_name ~
           danceability +
           energy +
           loudness +
           speechiness +
           acousticness +
           instrumentalness +
           liveness +
           valence +
           tempo +
           duration_ms +
           C + `C#|Db` + D + `D#|Eb` +
           E + `F` + `F#|Gb` + G +
           `G#|Ab` + A + `A#|Bb` + B +
           c01 + c02 + c03 + c04 + c05 + c06 +
           c07 + c08 + c09 + c10 + c11 + c12,
         data = nep_clust) %>% 
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>%
  # step_range(all_predictors()) %>% -> If you unmute this, use the manhattan method in the next code chunck 
  prep(nep_clust %>% mutate(track_name = str_trunc(track_name, 20))) %>%
  juice %>% 
  column_to_rownames('track_name')

nep_clust_dist <- dist(nep_clust_juice, method = 'euclidean')
protoclust(nep_clust_dist) %>% dendro_data %>% ggdendrogram
kmeans(nep_clust_juice, 4)

# Looking at the cluster on the far right, we can see that it really clusters the songs from the era around
# the century switch. This is not strange, since in that era, The Neptunes really had it's own sound, and 
# this is what I hypothised on earlier. Then there is another big cluster of more recent songs, most of which
# were procuded solely by Pharrell, and then there is a smaller cluster on the left that contains all kinds of songs
# that it was not able to identify. 

