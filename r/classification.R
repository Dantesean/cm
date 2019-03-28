library(tidyverse)
library(tidymodels)
library(ggdendro)
library(protoclust)
library(heatmaply)
library(spotifyr)
library(compmus)
source('spotify.R')

# Week 11 Assignment, Classification
# We will first focus on the classification questions. In a week we will update this with the information on the whole beat tracking stuff (if that's even important)

# To try the different classification options, let's fool arround with the release dates of The Neptunes songs first. After that, it might
# be interesting to look into classification of pop songs when training on The Neptunes data (or maybe that's too much of a hastle, let's ask)
# Since my hypothesis is that the sound of The Neptunes changed over time, it's a logical test to see if the classifier thinks so as well. 
# However, since changes may be really small in each subsequent year, I will categorize the playlists into 3 playlists for the release years:
# 90s, 00s, and 10s. Then, I let the classifier choose which songs should be classsified in which years, and thus find out if there are 
# truly major shifts in sound.

neptunes_90s <- 
  pharrell %>% 
  mutate(year = as.numeric(format(release_date, "%Y"))) %>% 
  filter(year < 2000) %>%
  add_audio_analysis
neptunes_00s <-
  pharrell %>%
  mutate(year = as.numeric(format(release_date, "%Y"))) %>% 
  filter(year >= 2000) %>%
  filter(year < 2010) %>%
  add_audio_analysis
neptunes_10s <-
  pharrell %>% 
  mutate(year = as.numeric(format(release_date, "%Y"))) %>% 
  filter(year >= 2010) %>%
  add_audio_analysis

neptunes <- rbind(neptunes_90s, neptunes_00s, neptunes_10s)

# The first observation that should be drawn is that the corpus for Neptunes songs prior to 2000 is really small, only 83 observations. 
# This might influence the power of the classifiers.

# Now let's create a dataframe to combine the previously made "playlists", and add the pitch and timbre information needed for classification

neptunes_class_matrix <- 
  neptunes_90s %>% mutate(playlist = "Neptunes 90s") %>% 
  bind_rows(
    neptunes_00s %>% mutate(playlist = "Neptunes 00s"),
    neptunes_10s %>% mutate(playlist = "Neptunes 10s")) %>% 
  mutate(playlist = factor(playlist)) %>% 
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
  unnest(pitches, timbre)

# Now let's create the model
neptunes_class <- 
  recipe(playlist ~
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
         data = neptunes_class_matrix) %>% 
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>%
  # step_range(all_predictors()) %>% 
  prep(neptunes_class_matrix) %>% 
  juice

# Set the Cross-Validation on 10
neptunes_cv <- neptunes_class %>% vfold_cv(10)

# First we will use the Random Forests to find out which attributes account for most of the information when trying to estimate the playlist
neptunes_forest <- rand_forest() %>% set_engine('randomForest')
predict_forest <- function(split)
  fit(neptunes_forest, playlist ~ ., data = analysis(split)) %>% 
  predict(assessment(split), type = 'class') %>%
  bind_cols(assessment(split))

neptunes_cv %>% 
  mutate(pred = map(splits, predict_forest)) %>% 
  unnest(pred) %>% 
  metric_set(accuracy, kap, j_index)(truth = playlist, estimate = .pred_class)

neptunes_class %>% 
  fit(neptunes_forest, playlist ~ ., data = .) %>% 
  pluck('fit') %>% 
  randomForest::varImpPlot()

# It's clear that the accuracy is really hight (0.681 is not a bad metric at all!). The Plot also shows that loudness, c07, valence and c02
# are most important when looking at the playlist selection. c09 is something that might be handy aswell. After testing, it seemed more
# reasonable to also add speechiness, c01, duration_ms, tempo, c04 and energy.
# Let's see if reducing the model to just these explanatory variables yields better results

predict_forest_reduced <- function(split)
  fit(neptunes_forest, 
      playlist ~ loudness + c07 + valence + c02 + c09 + speechiness + c01 +duration_ms + tempo + c04 + energy, data = analysis(split)) %>% 
  predict(assessment(split), type = 'class') %>%
  bind_cols(assessment(split))

neptunes_cv %>% 
  mutate(pred = map(splits, predict_forest_reduced)) %>% 
  unnest(pred) %>% 
  metric_set(accuracy, kap, j_index)(truth = playlist, estimate = .pred_class)

# This does indeed result in a slightly better result. Let's create a heatmap.
neptunes_cv %>% 
  mutate(pred = map(splits, predict_forest_reduced)) %>% unnest(pred) %>% 
  conf_mat(truth = playlist, estimate = .pred_class) %>% 
  autoplot(type = 'heatmap')

# What we see is that the classifier is really good in labeling the "Neptunes 00s" and "Neptunes 10s" playlists in either category. For 
# music by The Neptunes in the 90s, it finds it clearly harder to interpet to which category in results, with only 16 songs in the right
# category. 

# So, can we now use these classifiers to see if The Neptunes did influence modern pop music? From the
# Random Forest we found out what were the most important predictors for the release years of The Neptunes sound. One thin we can do
# is see if in the Pop playlists, these attributes changed over time. However, will this truly give us answers? Right now, the "danceability"
# index is clearly not used that much (4.11% in the DT). This could be due to the high danceability of all songs in the corpus of The Neptunes. 
# Danceability, however, was hypothesized as a key factor of the sound produced by Pharrell Williams and Chad Hugo. So, let's first do
# some classification on the total bodies of both The Neptunes and Pop Songs in the 90s, since The Neptunes had it's own sound then, which
# was not present in the Top Charts yet.

# First, we include the audio analysis features to the top90s playlist

top90s <- 
  top90s %>%
  mutate(year = as.numeric(format(release_date, "%Y"))) %>% 
  add_audio_analysis

# Than we unwrap it all

neptunes_90s_class_matrix <- 
  neptunes_90s %>% mutate(playlist = "Neptunes 90s") %>% 
  bind_rows(
    top90s %>% mutate(playlist = "Top 90s")) %>% 
  mutate(playlist = factor(playlist)) %>% 
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
  unnest(pitches, timbre)

# Then, for this music from the 90s, let's create the model
neptunes_90s_class <- 
  recipe(playlist ~
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
         data = neptunes_90s_class_matrix) %>% 
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>%
  # step_range(all_predictors()) %>% 
  prep(neptunes_90s_class_matrix) %>% 
  juice

# Set the Cross-Validation on 10
neptunes_90s_cv <- neptunes_90s_class %>% vfold_cv(10)

# Now, use the Random Forests to find out which attributes account for most of the information when trying to estimate the playlist
neptunes_90s_cv %>% 
  mutate(pred = map(splits, predict_forest)) %>% 
  unnest(pred) %>% 
  metric_set(accuracy, kap, j_index)(truth = playlist, estimate = .pred_class)

neptunes_90s_class %>% 
  fit(neptunes_forest, playlist ~ ., data = .) %>% 
  pluck('fit') %>% 
  randomForest::varImpPlot()

# Insane results! Really good accuracy. Let's see if, using the most important features speechiness, c11, danceability and c04 we 
# can better the model

predict_forest_reduced_90s <- function(split)
  fit(neptunes_forest, 
      playlist ~ speechiness + c11 + danceability + c04, data = analysis(split)) %>% 
  predict(assessment(split), type = 'class') %>%
  bind_cols(assessment(split))

neptunes_90s_cv %>% 
  mutate(pred = map(splits, predict_forest_reduced_90s)) %>% 
  unnest(pred) %>% 
  metric_set(accuracy, kap, j_index)(truth = playlist, estimate = .pred_class)

# It won't get any better, so we just stick to the full model and create the heatmap.

neptunes_90s_cv %>% 
  mutate(pred = map(splits, predict_forest)) %>% unnest(pred) %>% 
  conf_mat(truth = playlist, estimate = .pred_class) %>% 
  autoplot(type = 'heatmap')

# We can interpet these results as that there is a clear distinction between music that was popular in the 90s and music that was produced
# by The Neptunes in the 90s. The distinctions could best be made by using the "Speechiness", "C11", "Danceability" and "C04" features. 
# Thus, this is what the music by The Neptunes made it different from popular songs back then. Did these features grow during 00s for the 
# popular playlists?

# First, amend the playlists
top00s <- 
  top00s %>%
  mutate(year = as.numeric(format(release_date, "%Y"))) %>% 
  add_audio_analysis

top10s <- 
  top10s %>%
  mutate(year = as.numeric(format(release_date, "%Y"))) %>% 
  add_audio_analysis

# Then, unwrap the features we want to look at
top_00s_10s_class_matrix <- 
  top00s %>% mutate(playlist = "Top 00s") %>% 
  bind_rows(
    top10s %>% mutate(playlist = "Top 10s")) %>% 
  mutate(playlist = factor(playlist)) %>% 
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
  unnest(pitches, timbre)

top_00s_10s_class_matrix %>% 
  group_by(year) %>% 
  summarise(mSpeechiness = mean(speechiness)) %>%
  ggplot + 
  aes(x=year, y=(mSpeechiness)) +
  geom_point(color="#50CFE1") +
  geom_smooth(method=lm, color="#FF8700") +
  labs(x = 'Time (y)', y = 'Mean Speechiness') +
  theme(panel.background = element_rect(fill = "#00121C"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "#2E3E47"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "#2E3E47"),
        axis.text=element_text(size=14))

top_00s_10s_class_matrix %>% 
  group_by(year) %>% 
  summarise(mDanceability = mean(danceability)) %>%
  ggplot(
    aes(
      x = year,
      y = mDanceability)) + 
  geom_point(color="#50CFE1") +
  geom_smooth(method=lm, color="#FF8700") +
  labs(x = 'Time (y)', y = 'Mean Danceability') +
  theme(panel.background = element_rect(fill = "#00121C"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "#2E3E47"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "#2E3E47"),
        axis.text=element_text(size=14))

top_00s_10s_class_matrix %>% 
  group_by(year) %>% 
  summarise(mC11 = mean(c11)) %>%
  ggplot + 
  aes(x=year, y=(mC11)) +
  geom_point(color="#50CFE1") +
  geom_smooth(method=lm, color="#FF8700") +
  labs(x = 'Time (y)', y = 'Mean C11') +
  theme(panel.background = element_rect(fill = "#00121C"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "#2E3E47"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "#2E3E47"),
        axis.text=element_text(size=14))

top_00s_10s_class_matrix %>% 
  group_by(year) %>% 
  summarise(mC04 = mean(c04)) %>%
  ggplot + 
  aes(x=year, y=(mC04)) +
  geom_point(color="#50CFE1") +
  geom_smooth(method=lm, color="#FF8700") +
  labs(x = 'Time (y)', y = 'Mean C04') +
  theme(panel.background = element_rect(fill = "#00121C"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "#2E3E47"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "#2E3E47"),
        axis.text=element_text(size=14))

# We can conclude that indeed, speechiness gained popularity since 2000. This result is not strange, since Hip-Hop and R&B music has
# truly gained popularity in recent years. However, the scale of the other feautres (danceability, c11 and c04) did not increase
# over the years.