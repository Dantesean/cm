#----- SPOTIFY CREDENTIAL FOR SPOTIFYR -----#

if (Sys.getenv('SPOTIFY_CLIENT_ID') == '')
    Sys.setenv(SPOTIFY_CLIENT_ID = '557ece0477ae4ea5807f7a809a85a597')
if (Sys.getenv('SPOTIFY_CLIENT_SECRET') == '')
    Sys.setenv(SPOTIFY_CLIENT_SECRET = '24684514c966448b87e8809c1c5a4156')

#' Normalise vectors for Computational Musicology.
#'
#' We use a number of normalisation strategies in Computational Musicology.
#' This function brings them together into one place, along with common
#' alternative names.
compmus_normalise <- compmus_normalize <- function(v, method = "euclidean")
{
  ## Supported functions
  
  harmonic  <- function(v) v * sum(1 / abs(v))
  manhattan <- function(v) v / sum(abs(v))
  euclidean <- function(v) v / sqrt(sum(v^2))
  chebyshev <- function(v) v / max(abs(v))
  clr       <- function(v) {lv <- log(v); lv - mean(lv)}
  
  ## Method aliases
  
  METHODS <-
    list(
      harmonic  = harmonic,
      manhattan = manhattan,
      L1        = manhattan,
      euclidean = euclidean,
      L2        = euclidean,
      chebyshev = chebyshev,
      maximum   = chebyshev,
      aitchison = clr,
      clr       = clr)
  
  ## Function selection
  
  
  if (!is.na(i <- pmatch(method, names(METHODS))))
    METHODS[[i]](v)
  else 
    stop('The method name is ambiguous or the method is unsupported.')
}

#' Compute pairwise distances for Computational Musicology in long format.
#'
#' We use a number of distance measures in Computational Musicology.
#' This function brings them together into one place, along with common
#' alternative names. It is designed for convenience, not speed.
compmus_long_distance <- function(xdat, ydat, feature, method = "euclidean")
{
  
  feature <- enquo(feature)
  
  ## Supported functions
  
  manhattan <- function(x, y) sum(abs(x - y))
  euclidean <- function(x, y) sqrt(sum((x - y) ^ 2))
  chebyshev <- function(x, y) max(abs(x - y))
  pearson   <- function(x, y) 1 - cor(x, y)
  cosine    <- function(x, y)
  {
    1 - sum(compmus_normalise(x, "euc") * compmus_normalise(y, "euc"))
  }
  angular   <- function(x, y) 2 * acos(1 - cosine(x, y)) / pi
  aitchison <- function(x, y)
  {
    euclidean(compmus_normalise(x, "clr"), compmus_normalise(y, "clr"))
  }
  
  ## Method aliases
  
  METHODS <-
    list(
      manhattan   = manhattan,
      cityblock   = manhattan,
      taxicab     = manhattan,
      L1          = manhattan,
      totvar      = manhattan,
      euclidean   = euclidean,
      L2          = euclidean,
      chebyshev   = chebyshev,
      maximum     = chebyshev,
      pearson     = pearson,
      correlation = pearson,
      cosine      = cosine,
      angular     = angular,
      aitchison   = aitchison)
  
  ## Function selection
  
  if (!is.na(i <- pmatch(method, names(METHODS))))
    bind_cols(
      crossing(
        xdat %>% select(xstart = start, xduration = duration),
        ydat %>% select(ystart = start, yduration = duration)),
      xdat %>% select(x = !!feature) %>% 
        crossing(ydat %>% select(y = !!feature)) %>% 
        transmute(d = map2_dbl(x, y, METHODS[[i]])))
  else 
    stop('The method name is ambiguous or the method is unsupported.')
}

#' Gathers chroma vectors into long format.
#'
#' Gathers chroma vectors into long format for Computational Musicology.
compmus_gather_chroma <- function(data)
{
  data %>% 
    mutate(pitches = map(pitches, bind_rows)) %>% unnest(pitches) %>% 
    gather("pitch_class", "value", C:B) %>% 
    mutate(pitch_class = fct_shift(factor(pitch_class), 3))
}

# Tonal Template
circshift <- function(v, n) {if (n == 0) v else c(tail(v, n), head(v, -n))}

# C     C#    D     Eb    E     F     F#    G     Ab    A     Bb    B 
major_chord <- 
  c(1,    0,    0,    0,    1,    0,    0,    1,    0,    0,    0,    0)
minor_chord <- 
  c(1,    0,    0,    1,    0,    0,    0,    1,    0,    0,    0,    0)
seventh_chord <- 
  c(1,    0,    0,    0,    1,    0,    0,    1,    0,    0,    1,    0)

major_key <- 
  c(6.35, 2.23, 3.48, 2.33, 4.38, 4.09, 2.52, 5.19, 2.39, 3.66, 2.29, 2.88)
minor_key <-
  c(6.33, 2.68, 3.52, 5.38, 2.60, 3.53, 2.54, 4.75, 3.98, 2.69, 3.34, 3.17)

chord_templates <-
  tribble(
    ~name  , ~template,
    'Gb:7'  , circshift(seventh_chord,  6),
    'Gb:maj', circshift(major_chord,    6),
    'Bb:min', circshift(minor_chord,   10),
    'Db:maj', circshift(major_chord,    1),
    'F:min' , circshift(minor_chord,    5),
    'Ab:7'  , circshift(seventh_chord,  8),
    'Ab:maj', circshift(major_chord,    8),
    'C:min' , circshift(minor_chord,    0),
    'Eb:7'  , circshift(seventh_chord,  3),
    'Eb:maj', circshift(major_chord,    3),
    'G:min' , circshift(minor_chord,    7),
    'Bb:7'  , circshift(seventh_chord, 10),
    'Bb:maj', circshift(major_chord,   10),
    'D:min' , circshift(minor_chord,    2),
    'F:7'   , circshift(seventh_chord,  5),
    'F:maj' , circshift(major_chord,    5),
    'A:min' , circshift(minor_chord,    9),
    'C:7'   , circshift(seventh_chord,  0),
    'C:maj' , circshift(major_chord,    0),
    'E:min' , circshift(minor_chord,    4),
    'G:7'   , circshift(seventh_chord,  7),
    'G:maj' , circshift(major_chord,    7),
    'B:min' , circshift(minor_chord,   11),
    'D:7'   , circshift(seventh_chord,  2),
    'D:maj' , circshift(major_chord,    2),
    'F#:min', circshift(minor_chord,    6),
    'A:7'   , circshift(seventh_chord,  9),
    'A:maj' , circshift(major_chord,    9),
    'C#:min', circshift(minor_chord,    1),
    'E:7'   , circshift(seventh_chord,  4),
    'E:maj' , circshift(major_chord,    4),
    'G#:min', circshift(minor_chord,    8),
    'B:7'   , circshift(seventh_chord, 11),
    'B:maj' , circshift(major_chord,   11),
    'D#:min', circshift(minor_chord,    3),
  )

key_templates <-
  tribble(
    ~name    , ~template,
    'Gb:maj', circshift(major_key,  6),
    'Bb:min', circshift(minor_key, 10),
    'Db:maj', circshift(major_key,  1),
    'F:min' , circshift(minor_key,  5),
    'Ab:maj', circshift(major_key,  8),
    'C:min' , circshift(minor_key,  0),
    'Eb:maj', circshift(major_key,  3),
    'G:min' , circshift(minor_key,  7),
    'Bb:maj', circshift(major_key, 10),
    'D:min' , circshift(minor_key,  2),
    'F:maj' , circshift(major_key,  5),
    'A:min' , circshift(minor_key,  9),
    'C:maj' , circshift(major_key,  0),
    'E:min' , circshift(minor_key,  4),
    'G:maj' , circshift(major_key,  7),
    'B:min' , circshift(minor_key, 11),
    'D:maj' , circshift(major_key,  2),
    'F#:min', circshift(minor_key,  6),
    'A:maj' , circshift(major_key,  9),
    'C#:min', circshift(minor_key,  1),
    'E:maj' , circshift(major_key,  4),
    'G#:min', circshift(minor_key,  8),
    'B:maj' , circshift(major_key, 11),
    'D#:min', circshift(minor_key,  3))