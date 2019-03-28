library(tidyverse)
library(spotifyr)
library(compmus)
source('spotify.R')

twenty_five <- 
  get_tidy_audio_analysis('5UVsbUV0Kh033cqsZ5sLQi') %>% 
  compmus_align(sections, segments) %>% 
  select(sections) %>% unnest(sections) %>% 
  mutate(
    pitches = 
      map(segments, 
          compmus_summarise, pitches, 
          method = 'mean', norm = 'manhattan'))

twenty_five %>% 
  compmus_match_pitch_template(key_templates, 'euclidean', 'manhattan') %>% 
  ggplot(
    aes(x = start + duration / 2, width = duration, y = name, fill = d)) +
  geom_tile() +
  scale_fill_viridis_c(option = 'E', guide = 'none') +
  theme_minimal() +
  labs(x = 'Time (s)', y = '')

heatmaply(
  halloween_juice,
  hclustfun = hclust,
  # hclustfun = protoclust,
  # Comment out the hclust_method line when using protoclust.
  hclust_method = 'average',
  dist_method = 'euclidean')