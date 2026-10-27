library(data.table)
library(jsonlite)

# ------------------------------------------------------------
# RETRY ONLY GAMES WITH MISSING PBP
# ------------------------------------------------------------

pbp_cache_dir <- "pbp_cache_2025_26"

# Games currently in your final dataset
final_games <- sort(unique(final$game_id))

# Games that already have a cached PBP file
pbp_files <- list.files(
  pbp_cache_dir,
  pattern = "\\.rds$",
  full.names = TRUE
)

pbp_game_ids <- as.integer(
  sub(
    "\\.rds$",
    "",
    basename(pbp_files)
  )
)

# Games we need to retry
missing_pbp_games <- setdiff(
  final_games,
  pbp_game_ids
)

cat("Games to retry:", length(missing_pbp_games), "\n")

# ------------------------------------------------------------
# Helper: MM:SS -> seconds
# ------------------------------------------------------------

time_to_seconds <- function(x) {
  
  if (is.na(x) || x == "") {
    return(NA_real_)
  }
  
  parts <- strsplit(
    x,
    ":",
    fixed = TRUE
  )[[1]]
  
  as.numeric(parts[1]) * 60 +
    as.numeric(parts[2])
}


# ------------------------------------------------------------
# Retry one game
# ------------------------------------------------------------

retry_pbp_one <- function(
    gid,
    max_attempts = 4L
) {
  
  fpath <- file.path(
    pbp_cache_dir,
    paste0(gid, ".rds")
  )
  
  url <- paste0(
    "https://api-web.nhle.com/v1/gamecenter/",
    gid,
    "/play-by-play"
  )
  
  
  for (attempt in seq_len(max_attempts)) {
    
    cat(
      "Game", gid,
      "- attempt", attempt,
      "of", max_attempts, "\n"
    )
    
    
    raw <- tryCatch(
      
      jsonlite::fromJSON(
        url,
        flatten = TRUE
      ),
      
      error = function(e) {
        NULL
      }
    )
    
    
    # --------------------------------------------------------
    # Successful response
    # --------------------------------------------------------
    
    if (
      !is.null(raw) &&
      "plays" %in% names(raw) &&
      !is.null(raw$plays) &&
      nrow(raw$plays) > 0
    ) {
      
      res <- as.data.table(raw$plays)
      
      res[, game_id := gid]
      
      
      # ---------------------------
      # Home / away abbreviations
      # ---------------------------
      
      home_code <- NA_character_
      away_code <- NA_character_
      
      
      if (
        "homeTeam" %in% names(raw) &&
        !is.null(raw$homeTeam$abbrev)
      ) {
        home_code <- raw$homeTeam$abbrev[1]
      }
      
      
      if (
        "awayTeam" %in% names(raw) &&
        !is.null(raw$awayTeam$abbrev)
      ) {
        away_code <- raw$awayTeam$abbrev[1]
      }
      
      
      res[, home_abbr := home_code]
      res[, away_abbr := away_code]
      
      
      # ---------------------------
      # Period
      # ---------------------------
      
      if (
        "periodDescriptor.number" %in% names(res)
      ) {
        
        setnames(
          res,
          "periodDescriptor.number",
          "period"
        )
        
      } else {
        
        res[, period := NA_integer_]
      }
      
      
      # ---------------------------
      # Game seconds
      # ---------------------------
      
      if (
        "timeInPeriod" %in% names(res)
      ) {
        
        res[
          ,
          period_seconds := vapply(
            timeInPeriod,
            time_to_seconds,
            numeric(1)
          )
        ]
        
      } else {
        
        res[, period_seconds := NA_real_]
      }
      
      
      res[
        ,
        game_seconds := fifelse(
          period <= 3,
          
          (period - 1) * 1200 +
            period_seconds,
          
          3600 +
            (period - 4) * 300 +
            period_seconds
        )
      ]
      
      
      # ---------------------------
      # Event type
      # ---------------------------
      
      if (
        "typeDescKey" %in% names(res)
      ) {
        
        setnames(
          res,
          "typeDescKey",
          "event_type"
        )
        
      } else {
        
        res[, event_type := NA_character_]
      }
      
      
      # ---------------------------
      # Situation code
      # ---------------------------
      
      if (
        !"situationCode" %in% names(res)
      ) {
        
        res[, situationCode := NA_character_]
      }
      
      
      # Save successful game
      saveRDS(
        res,
        fpath
      )
      
      
      cat(
        "SUCCESS:",
        gid,
        "-",
        nrow(res),
        "events\n"
      )
      
      return(TRUE)
    }
    
    
    # --------------------------------------------------------
    # Failed attempt: wait longer before trying again
    # --------------------------------------------------------
    
    wait_time <- attempt * 2
    
    cat(
      "Failed attempt.",
      "Waiting",
      wait_time,
      "seconds...\n"
    )
    
    Sys.sleep(wait_time)
  }
  
  
  cat(
    "FAILED after all attempts:",
    gid,
    "\n"
  )
  
  FALSE
}

retry_results <- logical(
  length(missing_pbp_games)
)

for (i in seq_along(missing_pbp_games)) {
  
  gid <- missing_pbp_games[i]
  
  retry_results[i] <- retry_pbp_one(
    gid
  )
  
  
  cat(
    i,
    "/",
    length(missing_pbp_games),
    "missing games attempted\n"
  )
  
  
  # Important:
  # don't hammer the NHL API
  Sys.sleep(0.5)
}