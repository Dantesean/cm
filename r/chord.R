library(tidyverse)
library(tidymodels)
library(ggdendro)
library(protoclust)
library(robustbase)
library(heatmaply)
library(spotifyr)
library(compmus)
source('spotify.R')

for (i in (1:12)) {
  # First, get audio analysis of track
  track_audio_analysis_1 <- 
    get_tidy_audio_analysis(pharrell$track_uri[i]) %>% 
    compmus_align(sections, segments) %>% 
    select(sections) %>% unnest(sections) %>% 
    mutate(
      pitches = 
        map(segments, 
            compmus_summarise, pitches, 
            method = 'mean', norm = 'manhattan'))
  
  # Then create matching template and transform to wide
  track_match_1 <- track_audio_analysis_1 %>% 
    compmus_match_pitch_template(chord_templates, 'euclidean', 'manhattan') %>%
    spread(name, d)
  
  # Repeat the previous steps for another song
  track_audio_analysis_2 <- 
    get_tidy_audio_analysis(pharrell$track_uri[i+1]) %>% 
    compmus_align(sections, segments) %>% 
    select(sections) %>% unnest(sections) %>% 
    mutate(
      pitches = 
        map(segments, 
            compmus_summarise, pitches, 
            method = 'mean', norm = 'manhattan'))
  
  track_match_2 <- track_audio_analysis_2 %>% 
    compmus_match_pitch_template(chord_templates, 'euclidean', 'manhattan') %>%
    spread(name, d)
  
  if(!exists("track_matrix")){
  track_matrix <- cbind(
    (assign(paste(pharrell$track_name[i]), colMeans(track_match_1))),
    assign(paste(pharrell$track_name[i + 1]), colMeans(track_match_2))
  )
  } else {
    track_matrix <- cbind(track_matrix,
      (assign(paste(pharrell$track_name[i]), colMeans(track_match_1))),
      assign(paste(pharrell$track_name[i + 1]), colMeans(track_match_2))
    )
  }
}

track_matrix <- as.data.frame(track_matrix)