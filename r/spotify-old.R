#----- SPOTIFY CREDENTIAL FOR SPOTIFYR -----#

Sys.setenv(SPOTIFY_CLIENT_ID = "557ece0477ae4ea5807f7a809a85a597")
Sys.setenv(SPOTIFY_CLIENT_SECRET = "24684514c966448b87e8809c1c5a4156")

#----- AUDIO ANALYSIS FUNCTIONS -----#

#' Get Spotify audio analysis tidily.
#'
#' Loads the Spotify audio analysis for a track without extra
get_tidy_audio_analysis <- function(track_uri, ...)
{
    get_track_audio_analysis(track_uri, ...) %>%
        list %>% transpose %>% as_tibble %>%
        mutate_at(vars(meta, track), . %>% map(as_tibble)) %>%
        unnest(meta, track) %>%
        select(
            analyzer_version,
            duration,
            contains('fade'),
            ends_with('confidence'),
            bars:segments) %>%
        mutate_at(
            vars(bars, beats, tatums, sections),
            . %>% map(bind_rows)) %>%
        mutate(
            segments =
                map(
                    segments,
                    . %>%
                        transpose %>% as_tibble %>%
                        unnest(.preserve = c(pitches, timbre)) %>%
                        mutate(
                            pitches =
                                map(
                                    pitches,
                                    . %>%
                                        flatten_dbl %>%
                                        set_names(
                                            c(
                                                'C', 'C#|Db', 'D', 'D#|Eb',
                                                'E', 'F', 'F#|Gb', 'G',
                                                'G#|Ab', 'A', 'A#|Bb', 'B'))),
                            timbre =
                                map(
                                    timbre,
                                    . %>%
                                        flatten_dbl %>%
                                        set_names(
                                            c(
                                                'c01', 'c02', 'c03', 'c04',
                                                'c05', 'c06', 'c07', 'c08',
                                                'c09', 'c10', 'c11', 'c12'))))))
}

#----- NORMS AND DISTANCE FUNCTIONS -----#

#' Normalise vectors for Computational Musicology.
#'
#' We use a number of normalisation strategies in Computational Musicology.
#' This function brings them together into one place, along with common
#' alternative names.
compmus_normalise <- compmus_normalize <- function(v, method = 'euclidean')
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
            identity  = identity,
            id        = identity,
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
compmus_long_distance <- function(xdat, ydat, feature, method = 'euclidean')
{

    feature <- enquo(feature)

    ## Supported functions

    manhattan <- function(x, y) sum(abs(x - y))
    euclidean <- function(x, y) sqrt(sum((x - y) ^ 2))
    chebyshev <- function(x, y) max(abs(x - y))
    pearson   <- function(x, y) 1 - cor(x, y)
    cosine    <- function(x, y)
    {
        1 - sum(compmus_normalise(x, 'euc') * compmus_normalise(y, 'euc'))
    }
    angular   <- function(x, y) 2 * acos(1 - cosine(x, y)) / pi
    aitchison <- function(x, y)
    {
        euclidean(compmus_normalise(x, 'clr'), compmus_normalise(y, 'clr'))
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


compmus_self_similarity <- function(dat, feature, method = 'euclidean')
{
    feature <- enquo(feature)
    compmus_long_distance(dat, dat, !!feature, method)
}


#----- SUMMARIES -----#

#' Summarise vector-based features in list columns.
#'
#' Summarise vector-based featrues in list columns. Does not work with classical tidyverse grouping.
compmus_summarise <- compmus_summarize <- function(dat, feature, method = 'mean', norm = 'id')
{
    feature <- enquo(feature)

    ## Support functions
    ## TODO: Add geometric median and Chebyshev center.
    ## TODO: Search for minimum sum of angular distances in hyper-quadrant I.

    clr     <- function(v) {lv = log(v); lv - mean(lv)}
    softmax <- function(v) {exp(v) / sum(exp(v))}
    square  <- function(v) v^2
    not_max  <- function(v) v != max(v)

    ## Method aliases

    METHODS <-
        list(
            ## Central tendencies
            mean      = list( identity , mean , identity ),
            aitchison = list( clr      , mean , softmax  ),
            acenter   = list( clr      , mean , softmax  ),
            acentre   = list( clr      , mean , softmax  ),
            rms       = list( square   , mean , sqrt     ),
            max       = list( identity , max  , identity ),
            ## Dispersions
            sd        = list( identity , sd   , identity ),
            asd       = list( clr      , sd   , identity ),
            sdsq      = list( square   , sd   , identity ),
            varratio  = list( not_max  , mean , identity ))

    ## Function selection

    if (!is.na(i <- pmatch(method, names(METHODS))))
        dat %>%
        transmute(
            !!feature :=
                map(
                    !!feature,
                    . %>%
                        compmus_normalise(norm) %>%
                        (METHODS[[i]][[1]]) %>%
                        bind_rows)) %>%
        unnest(!!feature) %>%
        summarise_all(METHODS[[i]][[2]]) %>%
        map_dbl(1) %>%
        (METHODS[[i]][[3]])
    else
        stop('The method name is ambiguous or the method is unsupported.')
}

compmus_align_helper <- function(start0, duration0, inner)
{
    end0 <- start0 + duration0

    inner %>%
        filter(start < end0) %>%
        filter(
            pmin(end, end0) - pmax(start, start0) >=
                pmin(duration, duration0) / 2) %>%
        select(-end)
}

compmus_align_reduce <- function(outer, inner, name)
{
    outer %>%
        mutate(
            !!name :=
                map2(
                    start,
                    duration,
                    compmus_align_helper,
                    inner %>% mutate(end = start + duration)))
}

#' Aligns lower-level Spotify segmentations with higher-level segmentations.
#'
#' Returns a list column with tibbles of the lower-level segment for each higher-level segement.
compmus_align <- function(dat, outer, inner)
{
    outer <- enquo(outer)
    inner <- enquo(inner)

    mutate(
        dat,
        !!outer := map2(!!outer, !!inner, compmus_align_reduce, inner)) %>%
        select(-!!inner)
}


#----- UNWRAPPING CHROMA VECTORS -----#

#' Gathers chroma vectors into long format.
#'
#' Gathers chroma vectors into long format for Computational Musicology.
compmus_gather_chroma <- function(data)
{
    data %>%
        mutate(pitches = map(pitches, bind_rows)) %>% unnest(pitches) %>%
        gather('pitch_class', 'value', C:B) %>%
        mutate(pitch_class = fct_shift(factor(pitch_class), 3))
}

#' Gathers timbre vectors into long format.
#'
#' Gathers timbre vectors into long format for Computational Musicology.
compmus_gather_timbre <- function(data)
{
    data %>%
        mutate(timbre = map(timbre, bind_rows)) %>% unnest(timbre) %>%
        gather('basis', 'value', c01:c12)
}

