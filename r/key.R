library(tidyverse)
library(spotifyr)
library(compmus)
source('spotify.R')

# # First get audio analysis
# twenty_five <- 
#   get_tidy_audio_analysis('5UVsbUV0Kh033cqsZ5sLQi') %>% 
#   compmus_align(sections, segments) %>% 
#   select(sections) %>% unnest(sections) %>% 
#   mutate(
#     pitches = 
#       map(segments, 
#           compmus_summarise, pitches, 
#           method = 'mean', norm = 'manhattan'))
# 
# # Then create matching template 
# twenty_five_match <- twenty_five %>% 
#   compmus_match_pitch_template(key_templates, 'euclidean', 'manhattan')
# 
# # Then change format from long to wide
# twenty_match <- spread(twenty_five_match, name, d)
# 
# # Repeat the previous steps for another song
# # Then combine the rows
# chordogram_wide <- rbind(twenty_match, other_match)
# 
# # Put it back to long format
# chordogram <- gather(chordogram_wide, names, key="name", value="d")
# 
# # Plot it!
# chrodogram %>% ggplot(
#   aes(x = start + duration / 2, width = duration, y = name, fill = d)) +
#   geom_tile() +
#   scale_fill_viridis_c(option = 'E', guide = 'none') +
#   theme_minimal() +
#   labs(x = 'Time (s)', y = '')

pharrell_popularity <- pharrell %>% 
  arrange(desc(track_popularity)) %>%
  filter(track_popularity > 0 ) %>% 
  slice(1:100)

# ---- The Loop ---- #
for (i in (1:100)) {
  # First, get audio analysis of track
track_audio_analysis_1 <- 
  get_tidy_audio_analysis(pharrell_popularity$track_uri[i]) %>% 
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
  get_tidy_audio_analysis(pharrell_popularity$track_uri[i+1]) %>% 
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

  # Then combine the rows and ut it back to long format
  names <- colnames(track_match_1[3:ncol(track_match_1)])
  if (i == 1) {
    chordogram_wide <- 
      rbind(track_match_1, track_match_2)
    chordogram <- chordogram_wide %>% 
      gather(names, key="name", value="d")
  } else {
    chordogram_wide <-
      rbind(chordogram_wide, track_match_1, track_match_2)
    chordogram <- chordogram_wide %>%
      gather(names, key="name", value="d")
  }
}

# Plot it!
chordogram %>% ggplot(
  aes(x = start + duration / 2, width = duration, y = name, fill = d)) +
  geom_tile() +
  scale_fill_viridis_c(option = 'E', guide = 'none') +
  theme_minimal() +
  labs(x = 'Time (s)', y = '') +
  theme(axis.text=element_text(size=14, color = "white"), panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                                                                          colour = "#2E3E47"))
track_match_1 %>% 
  gather(names, key="name", value = "d") %>%
  ggplot(
  aes(x = start + duration / 2, width = duration, y = name, fill = d)) +
  geom_tile() +
  scale_fill_viridis_c(option = 'E', guide = 'none') +
  theme_minimal() +
  labs(x = 'Time (s)', y = '')