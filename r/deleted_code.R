# Deleted Code, maybe used for future ref

# To find release dates for playlist "Pop2000"
for(i in 1:nrow(pop2000)) {
  tryCatch({
  temp_track <- get_tracks(track_name = pop2000[i, ]$track_name, artist_name = pop2000[i, ]$artist_name, album_name = pop2000[i, ]$album_name, return_closest_track = TRUE)
  if(nrow(temp_track) == 0) {
    temp_track <- get_tracks(track_name = pop2000[i, ]$track_name, artist_name = pop2000[i, ]$artist_name, return_closest_track = TRUE)
  }
  if(!nrow(temp_track) == 0) {
  temp_album <- get_albums(temp_track$album_id)
  pop2000$release_date[i] <- temp_album$release_date
  } else {
    pop2000$release_date[i] <- NA
  }
  print(i)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
)}