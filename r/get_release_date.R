get_release_date <- function(track_name, artist_name, album_name) {
  if(nrow(get_tracks(track_name = track_name, artist_name = artist_name, album_name = album_name, return_closest_track = TRUE)) == 0){
    get_albums(get_tracks(track_name = track_name, artist_name = artist_name, return_closest_track = TRUE)$album_id)$release_date
  } else {
    get_albums(get_tracks(track_name = track_name, artist_name = artist_name, album_name = album_name, return_closest_track = TRUE)$album_id)$release_date
  }
}