utilize <- function(){
  # Load libraries
  library(tidyverse)
  library(spotifyr)
  library(ggplot2)
  library(anytime)
  library(compmus)
  if(!exists("get_release_date", mode="function")){ 
    source("get_release_date.R")
  }
  
  # Set Environment Variables
  Sys.setenv(SPOTIFY_CLIENT_ID = "557ece0477ae4ea5807f7a809a85a597")
  Sys.setenv(SPOTIFY_CLIENT_SECRET = "24684514c966448b87e8809c1c5a4156")
  
  
  # Load Playlists for project
  pharrell_unfiltered <- get_playlist_audio_features('dantesean', '37LPgjZWSjykXkrhHYzPcR')
  top90s1 <- get_playlist_audio_features('popsounds', '6koeQRjLOmY03Y7KckWm9o')
  top90s2 <- get_playlist_audio_features('listanauta','0YVNcVAYCNzuEtw7Zdru8P')
  top90s <- rbind(top90s1, top90s2)
  top00s <- get_playlist_audio_features('11123270324', '2n8WeZO6kAku8y4FNpDEka')
  top10s <- get_playlist_audio_features('bricomayor','2AjCvsmOeC1cr5b2tHTaT7')
  
  # Clean the Pharrell playlist for duplicates due to different album versions
  
  pharrell <- pharrell_unfiltered[!duplicated(pharrell_unfiltered$track_name), ]
  
  # Clean the Top 90s playlist for duplicates
  top90s <- top90s[!duplicated(top90s$track_name), ]
  
  # Delete first 6 columns containing only info on the playlist and the 12th containing the date the song was added
  top90s[, c((1:6), 12)] <- list(NULL)
  top00s[, c((1:6), 12)] <- list(NULL)
  top10s[, c((1:6), 12)] <- list(NULL)
  pharrell[, c((1:6), 12)] <- list(NULL)
  
  # Get the specific release dates for each song
  ## First for top90s Playlist
  top90s$release_date <- rep(0, nrow(top90s))
  for(i in 1:nrow(top90s)) {
    tryCatch({
      top90s$release_date[i] <- get_release_date(top90s[i, ]$track_name, top90s[i, ]$artist_name, top90s[i, ]$album_name)
    }, error=function(e){})
  }
  
  ## Then for top00s Playlist
  top00s$release_date <- rep(0, nrow(top00s))
  for(i in 1:nrow(top00s)) {
    tryCatch({
      top00s$release_date[i] <- get_release_date(top00s[i, ]$track_name, top00s[i, ]$artist_name, top00s[i, ]$album_name)
    }, error=function(e){})
  }
  
  ## Then for top10s Playlist
  top10s$release_date <- rep(0, nrow(top10s))
  for(i in 1:nrow(top10s)) {
    tryCatch({
      top10s$release_date[i] <- get_release_date(top10s[i, ]$track_name, top10s[i, ]$artist_name, top10s[i, ]$album_name)
    }, error=function(e){})
  }
  
  ## Then for the Pharrell playlist
  for(i in 1:nrow(pharrell)) {
    tryCatch({
      pharrell$release_date[i] <- get_release_date(pharrell[i, ]$track_name, pharrell[i, ]$artist_name, pharrell[i, ]$album_name)
    }, error=function(e){})
  }
  
  pharrell$release_date <- anytime(pharrell$release_date)
  top90s$release_date <- anytime(top90s$release_date)
  top00s$release_date <- anytime(top00s$release_date)
  top10s$release_date <- anytime(top10s$release_date)
  
  # Delete songs prior or after date X in order to contain some sense of consistency
  top90s <- top90s %>% mutate(year = format(release_date, "%Y")) %>% filter(year > 1990) %>% filter(year < 1998)
  top10s <- rbind((top10s %>% mutate(year = format(release_date, "%Y")) %>% filter(year >= 2006)), (top00s %>% mutate(year = format(release_date, "%Y%")) %>% filter(year >= 2006)))
  top00s <- top00s %>% mutate(year = format(release_date, "%Y")) %>% filter(year >= 1998) %>% filter(year < 2006)
  
  # Assign variables to global evironment
  assign("top90s", top90s , envir = .GlobalEnv)
  assign("top00s", top00s , envir = .GlobalEnv)
  assign("top10s", top10s , envir = .GlobalEnv)
  assign("pharrell", pharrell , envir = .GlobalEnv)
}