# ============================================================
# NHL 2025-26: Player 5-second intervals + MoneyPuck xG
# + exact 5v5 / PP / PK / empty-net classification
# + defensive-zone faceoff event file for icing analysis
# ============================================================

library(data.table)
library(hockeyR)
library(jsonlite)

setDTthreads(0)

SEASON <- 2026L
SEASON_START_YEAR <- SEASON - 1L

POST_EVENT_WINDOW_SEC <- 30L
PRE_EVENT_WINDOW_SEC <- 20L
DZ_FACEOFF_X_MIN <- 50
BAD_GAME_IDS <- c(2025020565)

# ------------------------------------------------------------
# 1. Schedule / game IDs
# ------------------------------------------------------------

sched <- hockeyR::get_game_ids(season = SEASON)
setDT(sched)
game_ids <- sched$game_id

cat("Games in schedule:", length(game_ids), "\n")

# ------------------------------------------------------------
# 2. Shift data with robust cache handling
# ------------------------------------------------------------

shift_cache_dir <- "shift_cache"
dir.create(shift_cache_dir, showWarnings = FALSE)

scrape_shift_one <- function(gid) {
  fpath <- file.path(shift_cache_dir, paste0(gid, ".rds"))

  if (file.exists(fpath)) {
    cached <- tryCatch(readRDS(fpath), error = function(e) NULL)
    if (!is.null(cached)) {
      setDT(cached)
      return(cached)
    }
    cat("Corrupt shift cache for", gid, "- deleting and re-scraping\n")
    unlink(fpath)
  }

  res <- tryCatch(
    hockeyR::get_game_shifts(game_id = gid),
    error = function(e) {
      cat("Shift scrape failed for game", gid, "\n")
      NULL
    }
  )

  if (!is.null(res)) {
    setDT(res)
    res[, game_id := gid]
    saveRDS(res, fpath)
  }

  res
}

shift_list <- vector("list", length(game_ids))

for (i in seq_along(game_ids)) {
  shift_list[[i]] <- scrape_shift_one(game_ids[i])
  if (i %% 25 == 0) cat(i, "/", length(game_ids), "shift games processed\n")
}

raw_shifts <- rbindlist(shift_list, fill = TRUE)

cat("Games with shift data:", uniqueN(raw_shifts$game_id), "/", length(game_ids), "\n")

# ------------------------------------------------------------
# 3. Reconstruct individual player shifts
# ------------------------------------------------------------

raw_shifts <- raw_shifts[!is.na(num_on) | !is.na(num_off)]

split_vals <- function(x) {
  if (length(x) == 0L || is.na(x) || x == "" || x == "None") {
    return(character(0))
  }
  vals <- trimws(strsplit(x, ",", fixed = TRUE)[[1]])
  vals[vals != "" & vals != "0"]
}

reconstruct_team_period <- function(dt, this_period) {
  setorder(dt, game_seconds)

  active_names <- character(0)
  active_starts <- numeric(0)

  out <- list()
  k <- 1L

  event_times <- sort(unique(dt$game_seconds))

  for (t in event_times) {
    rows_t <- dt[game_seconds == t]

    for (r in seq_len(nrow(rows_t))) {
      off_ids <- split_vals(rows_t$ids_off[r])
      if (length(off_ids) == 0L) next

      for (pid in off_ids) {
        if (pid %in% names(active_names)) {
          start_time <- active_starts[[pid]]
          if (!is.na(start_time) && t > start_time) {
            out[[k]] <- data.table(
              player_id = as.numeric(pid),
              player_name = active_names[[pid]],
              shift_start = start_time,
              shift_end = t
            )
            k <- k + 1L
          }
          active_names <- active_names[names(active_names) != pid]
          active_starts <- active_starts[names(active_starts) != pid]
        }
      }
    }

    for (r in seq_len(nrow(rows_t))) {
      on_ids <- split_vals(rows_t$ids_on[r])
      on_names <- split_vals(rows_t$players_on[r])
      if (length(on_ids) == 0L) next

      for (j in seq_along(on_ids)) {
        pid <- on_ids[j]
        pname <- if (j <= length(on_names)) on_names[j] else NA_character_
        if (!(pid %in% names(active_names))) {
          active_names[pid] <- pname
          active_starts[pid] <- t
        }
      }
    }
  }

  if (this_period <= 3L) {
    period_end <- this_period * 1200L
  } else {
    observed_end <- suppressWarnings(max(dt$game_seconds, na.rm = TRUE))
    theoretical_end <- 3600L + (this_period - 3L) * 300L
    period_end <- if (is.finite(observed_end) && observed_end > 3600) {
      min(theoretical_end, observed_end)
    } else {
      theoretical_end
    }
  }

  if (length(active_names) > 0L) {
    for (pid in names(active_names)) {
      start_time <- active_starts[[pid]]
      if (!is.na(start_time) && period_end > start_time) {
        out[[k]] <- data.table(
          player_id = as.numeric(pid),
          player_name = active_names[[pid]],
          shift_start = start_time,
          shift_end = period_end
        )
        k <- k + 1L
      }
    }
  }

  if (length(out) == 0L) return(NULL)
  rbindlist(out)
}

shifts <- raw_shifts[
  ,
  reconstruct_team_period(.SD, period[1]),
  by = .(game_id, team = event_team_abbr, period)
]

shifts <- shifts[
  !is.na(player_id) & !is.na(shift_start) & !is.na(shift_end) &
    shift_end > shift_start
]

# ------------------------------------------------------------
# Canonical player names by majority vote
# ------------------------------------------------------------
name_votes <- raw_shifts[
  ,
  {
    on_ids   <- split_vals(ids_on)
    on_names <- split_vals(players_on)
    off_ids  <- split_vals(ids_off)
    off_nms  <- split_vals(players_off)
    data.table(
      player_id = as.numeric(c(on_ids, off_ids)),
      player_name = c(
        if (length(on_ids) > 0)  on_names[seq_along(on_ids)]  else character(0),
        if (length(off_ids) > 0) off_nms[seq_along(off_ids)]  else character(0)
      )
    )
  },
  by = seq_len(nrow(raw_shifts))
]
name_votes <- name_votes[!is.na(player_id) & !is.na(player_name) & player_name != ""]

canonical_names <- name_votes[, .N, by = .(player_id, player_name)]
setorder(canonical_names, player_id, -N)
canonical_names <- canonical_names[, .SD[1], by = player_id]
setnames(canonical_names, "player_name", "canonical_name")

n_mismatched <- name_votes[
  canonical_names, on = "player_id"
][player_name != canonical_name, .N]
cat("\nName/ID rows that disagreed with the majority-vote name:",
    n_mismatched, "/", nrow(name_votes), "\n")

shifts[, player_name := NULL]
shifts <- merge(shifts, canonical_names, by = "player_id", all.x = TRUE)
setnames(shifts, "canonical_name", "player_name")

# ------------------------------------------------------------
# Flag implausibly long shifts
# ------------------------------------------------------------
MAX_PLAUSIBLE_SHIFT_SEC <- 180L

shift_dur <- shifts$shift_end - shifts$shift_start
n_flagged <- sum(shift_dur > MAX_PLAUSIBLE_SHIFT_SEC)
pct_flagged <- round(100 * n_flagged / nrow(shifts), 3)

cat("\nFlagging", n_flagged, "shifts (", pct_flagged,
    "% of all shifts) exceeding", MAX_PLAUSIBLE_SHIFT_SEC,
    "seconds\n")

flagged_shifts <- shifts[shift_dur > MAX_PLAUSIBLE_SHIFT_SEC]
fwrite(flagged_shifts, "flagged_long_shifts.csv")

shifts <- shifts[shift_dur <= MAX_PLAUSIBLE_SHIFT_SEC]

cat("Reconstructed shifts:", nrow(shifts), "\n")

# ------------------------------------------------------------
# 4. Game-specific roster / position lookup
# ------------------------------------------------------------

roster_cache_dir <- "roster_cache_all_games"
dir.create(roster_cache_dir, showWarnings = FALSE)

get_roster_one <- function(gid) {
  fpath <- file.path(roster_cache_dir, paste0(gid, ".rds"))

  if (file.exists(fpath)) {
    cached <- tryCatch(readRDS(fpath), error = function(e) NULL)
    if (!is.null(cached)) {
      setDT(cached)
      return(cached)
    }
    cat("Corrupt roster cache for", gid, "- deleting and re-scraping\n")
    unlink(fpath)
  }

  res <- tryCatch(
    hockeyR::get_game_rosters(game_id = gid),
    error = function(e) {
      cat("Roster scrape failed for game", gid, "\n")
      NULL
    }
  )

  if (!is.null(res)) {
    setDT(res)
    res[, game_id := gid]
    saveRDS(res, fpath)
  }

  res
}

roster_list <- vector("list", length(game_ids))

for (i in seq_along(game_ids)) {
  roster_list[[i]] <- get_roster_one(game_ids[i])
  if (i %% 25 == 0) cat(i, "/", length(game_ids), "roster games processed\n")
}

roster <- rbindlist(roster_list, fill = TRUE)

if (!"position_type" %in% names(roster)) {
  roster[, position_type := position]
}

if ("player_name" %in% names(roster)) {
  name_lookup <- roster[!is.na(player_id) & !is.na(player_name) & player_name != ""]
  name_lookup <- name_lookup[, .N, by = .(player_id, player_name)]
  setorder(name_lookup, player_id, -N)
  name_lookup <- name_lookup[, .SD[1], by = player_id][, .(player_id, roster_name = player_name)]

  name_compare <- merge(
    unique(shifts[, .(player_id, shift_log_name = player_name)]),
    name_lookup, by = "player_id", all.x = TRUE
  )
  disagreements <- name_compare[!is.na(roster_name) & shift_log_name != roster_name]
  cat("\nplayer_ids where shift-log name disagreed with roster name:", nrow(disagreements), "\n")

  shifts[, player_name := NULL]
  shifts <- merge(shifts, name_lookup, by = "player_id", all.x = TRUE)
  shifts[!is.na(roster_name), player_name := roster_name]
  shifts[, roster_name := NULL]
} else {
  cat("\nWARNING: roster data has no player_name column -- keeping shift-log names.\n")
}

roster_small <- unique(roster[, .(game_id, player_id, position, position_type)])

roster_small[
  ,
  fwd_def := fifelse(
    position_type == "G" | position == "G", "G",
    fifelse(position_type == "D" | position == "D", "Defense", "Forward")
  )
]

shifts <- merge(shifts, roster_small, by = c("game_id", "player_id"), all.x = TRUE)

shifts <- shifts[is.na(position) | position != "G"]
shifts <- shifts[is.na(fwd_def) | fwd_def != "G"]

cat("Rows with unknown position after roster join:", sum(is.na(shifts$position)), "\n")

# ------------------------------------------------------------
# 5. Expand shifts to a common 5-second snapshot grid
# ------------------------------------------------------------

shifts[, shift_id := .I]

shift_ivl <- shifts[
  ,
  {
    first_interval <- ceiling(shift_start / 5) * 5
    last_interval <- floor((shift_end - 1) / 5) * 5
    if (first_interval <= last_interval) {
      .(interval_start = seq(first_interval, last_interval, by = 5))
    } else {
      .(interval_start = numeric(0))
    }
  },
  by = .(shift_id, game_id, player_id, player_name, team, period, position, fwd_def,
         shift_start, shift_end)
]

shift_ivl[, interval_start := as.integer(interval_start)]
shift_ivl[, interval_end := interval_start + 5L]
shift_ivl[, game_minute := interval_start %/% 60L]
shift_ivl[, shift_age_sec := pmax(0, interval_start - shift_start)]
shift_ivl[, shift_age_mid_sec := pmax(0, interval_start + 2.5 - shift_start)]
shift_ivl[, shift_duration_sec := shift_end - shift_start]

# ------------------------------------------------------------
# 6. Home / Away
# ------------------------------------------------------------

sched_small <- unique(sched[, .(game_id, home_abbr, away_abbr)])

shift_ivl <- merge(shift_ivl, sched_small, by = "game_id", all.x = TRUE)
shift_ivl[, home_away := fifelse(team == home_abbr, "Home", "Away")]
shift_ivl[, opp_team := fifelse(team == home_abbr, away_abbr, home_abbr)]

# ------------------------------------------------------------
# 7. Scrape raw NHL play-by-play directly from NHL API
# ------------------------------------------------------------

# situationCode is stored as four digits:
# away goalie, away skaters, home skaters, home goalie.
# For example, 1551 is standard 5-on-5 with both goalies in net.
normalize_situation_code <- function(x) {
  raw <- trimws(as.character(x))
  out <- rep(NA_character_, length(raw))

  ok <- !is.na(raw) & nzchar(raw)
  vals <- suppressWarnings(as.integer(raw[ok]))
  good <- !is.na(vals)

  idx <- which(ok)[good]
  out[idx] <- sprintf("%04d", vals[good])
  out
}

classify_game_state <- function(away_goalie, away_skaters,
                                home_skaters, home_goalie) {
  both_goalies <- away_goalie == 1L & home_goalie == 1L

  fcase(
    is.na(away_goalie) | is.na(away_skaters) |
      is.na(home_skaters) | is.na(home_goalie), NA_character_,

    both_goalies & away_skaters == 5L & home_skaters == 5L, "5v5",
    both_goalies & away_skaters == 4L & home_skaters == 4L, "4v4",
    both_goalies & away_skaters == 3L & home_skaters == 3L, "3v3",

    both_goalies & home_skaters > away_skaters, "HOME_PP",
    both_goalies & away_skaters > home_skaters, "AWAY_PP",

    home_goalie == 0L & away_goalie == 1L, "HOME_EMPTY_NET",
    away_goalie == 0L & home_goalie == 1L, "AWAY_EMPTY_NET",
    home_goalie == 0L & away_goalie == 0L, "BOTH_EMPTY_NET",

    default = "OTHER"
  )
}

classify_team_strength <- function(game_state, home_away) {
  fcase(
    is.na(game_state) | is.na(home_away), NA_character_,

    game_state == "5v5", "5v5",
    game_state %chin% c("4v4", "3v3"), "EV_OTHER",

    game_state == "HOME_PP" & home_away == "Home", "PP",
    game_state == "HOME_PP" & home_away == "Away", "PK",
    game_state == "AWAY_PP" & home_away == "Away", "PP",
    game_state == "AWAY_PP" & home_away == "Home", "PK",

    game_state == "HOME_EMPTY_NET" & home_away == "Home", "EN_ATTACK",
    game_state == "HOME_EMPTY_NET" & home_away == "Away", "EN_DEFEND",
    game_state == "AWAY_EMPTY_NET" & home_away == "Away", "EN_ATTACK",
    game_state == "AWAY_EMPTY_NET" & home_away == "Home", "EN_DEFEND",

    default = "OTHER"
  )
}

flip_team_strength <- function(x) {
  fcase(
    is.na(x), NA_character_,
    x == "PP", "PK",
    x == "PK", "PP",
    x == "EN_ATTACK", "EN_DEFEND",
    x == "EN_DEFEND", "EN_ATTACK",
    default = x
  )
}

as_binary01 <- function(x) {
  raw <- tolower(trimws(as.character(x)))
  out <- rep(NA_integer_, length(raw))

  out[raw %chin% c("1", "true", "t", "yes")] <- 1L
  out[raw %chin% c("0", "false", "f", "no")] <- 0L

  numeric_vals <- suppressWarnings(as.integer(raw))
  use_numeric <- is.na(out) & !is.na(numeric_vals)
  out[use_numeric] <- as.integer(numeric_vals[use_numeric] != 0L)
  out
}

pbp_cache_dir <- "pbp_cache_2025_26"
dir.create(pbp_cache_dir, showWarnings = FALSE)

scrape_pbp_one <- function(gid) {

  fpath <- file.path(pbp_cache_dir, paste0(gid, ".rds"))

  required_cols <- c(
    "game_id", "game_seconds", "situationCode",
    "away_goalie", "away_skaters", "home_skaters", "home_goalie",
    "game_state", "home_score", "away_score",
    "home_team_id", "away_team_id"
  )

  if (file.exists(fpath)) {
    cached <- tryCatch(readRDS(fpath), error = function(e) NULL)
    if (!is.null(cached)) {
      setDT(cached)
      if (all(required_cols %in% names(cached))) {
        return(cached)
      }
      cat("Stale PBP cache for game", gid, "(missing new state columns) - re-scraping\n")
    } else {
      cat("Corrupt PBP cache for game", gid, "- deleting and re-scraping\n")
    }
    unlink(fpath)
  }

  url <- paste0("https://api-web.nhle.com/v1/gamecenter/", gid, "/play-by-play")

  raw <- tryCatch(
    jsonlite::fromJSON(url, flatten = TRUE),
    error = function(e) {
      cat("PBP download failed for game", gid, "\n")
      NULL
    }
  )

  if (is.null(raw)) return(NULL)

  if (!"plays" %in% names(raw) || is.null(raw$plays) || nrow(raw$plays) == 0) {
    cat("No PBP plays found for game", gid, "\n")
    return(NULL)
  }

  res <- as.data.table(raw$plays)

  res[, game_id := gid]
  res[, home_abbr := if (!is.null(raw$homeTeam$abbrev)) raw$homeTeam$abbrev else NA_character_]
  res[, away_abbr := if (!is.null(raw$awayTeam$abbrev)) raw$awayTeam$abbrev else NA_character_]
  res[, home_team_id := if (!is.null(raw$homeTeam$id)) raw$homeTeam$id else NA_integer_]
  res[, away_team_id := if (!is.null(raw$awayTeam$id)) raw$awayTeam$id else NA_integer_]

  if ("periodDescriptor.number" %in% names(res)) {
    setnames(res, "periodDescriptor.number", "period")
  } else {
    res[, period := NA_integer_]
  }

  time_to_seconds <- function(x) {
    if (is.na(x) || x == "") return(NA_real_)
    parts <- strsplit(x, ":", fixed = TRUE)[[1]]
    as.numeric(parts[1]) * 60 + as.numeric(parts[2])
  }

  if ("timeInPeriod" %in% names(res)) {
    res[, period_seconds := vapply(timeInPeriod, time_to_seconds, numeric(1))]
  } else {
    res[, period_seconds := NA_real_]
  }

  res[
    ,
    game_seconds := fifelse(
      period <= 3,
      (period - 1) * 1200 + period_seconds,
      3600 + (period - 4) * 300 + period_seconds
    )
  ]

  if ("typeDescKey" %in% names(res)) {
    setnames(res, "typeDescKey", "event_type")
  } else {
    res[, event_type := NA_character_]
  }

  if (!"situationCode" %in% names(res)) {
    res[, situationCode := NA_character_]
  }

  res[, situationCode := normalize_situation_code(situationCode)]
  res[, away_goalie := suppressWarnings(as.integer(substr(situationCode, 1, 1)))]
  res[, away_skaters := suppressWarnings(as.integer(substr(situationCode, 2, 2)))]
  res[, home_skaters := suppressWarnings(as.integer(substr(situationCode, 3, 3)))]
  res[, home_goalie := suppressWarnings(as.integer(substr(situationCode, 4, 4)))]
  res[, game_state := classify_game_state(
    away_goalie, away_skaters, home_skaters, home_goalie
  )]

  if ("details.eventOwnerTeamId" %in% names(res)) {
    setnames(res, "details.eventOwnerTeamId", "event_team_id")
  } else {
    res[, event_team_id := NA_integer_]
  }

  if ("details.duration" %in% names(res)) {
    setnames(res, "details.duration", "penalty_minutes")
  } else {
    res[, penalty_minutes := NA_real_]
  }

  if ("details.typeCode" %in% names(res)) {
    setnames(res, "details.typeCode", "penalty_type")
  } else {
    res[, penalty_type := NA_character_]
  }

  if ("details.homeScore" %in% names(res)) {
    setnames(res, "details.homeScore", "home_score")
  } else {
    res[, home_score := NA_integer_]
  }

  if ("details.awayScore" %in% names(res)) {
    setnames(res, "details.awayScore", "away_score")
  } else {
    res[, away_score := NA_integer_]
  }

  tryCatch(saveRDS(res, fpath), error = function(e) {
    cat("WARNING: failed to write cache for game", gid, "-", conditionMessage(e), "\n")
  })

  res
}

pbp_list <- vector("list", length(game_ids))

for (i in seq_along(game_ids)) {
  pbp_list[[i]] <- tryCatch(
    scrape_pbp_one(game_ids[i]),
    error = function(e) {
      cat("Unexpected error on game", game_ids[i], "-", conditionMessage(e), "\n")
      NULL
    }
  )
  if (i %% 25 == 0) cat(i, "/", length(game_ids), "PBP games processed\n")
}

pbp <- rbindlist(pbp_list, fill = TRUE)

cat("Games with PBP data:", uniqueN(pbp$game_id), "/", length(game_ids), "\n")

if (uniqueN(pbp$game_id) != length(game_ids)) {
  stop(
    paste0(
      "PBP coverage incomplete: ",
      uniqueN(pbp$game_id),
      " / ",
      length(game_ids),
      " games"
    )
  )
}

# Make downstream event logic robust to columns that may be absent in a game.
if (!"sortOrder" %in% names(pbp)) {
  pbp[, sortOrder := seq_len(.N), by = game_id]
}
if (!"details.reason" %in% names(pbp)) pbp[, details.reason := NA_character_]
if (!"details.xCoord" %in% names(pbp)) pbp[, details.xCoord := NA_real_]
if (!"homeTeamDefendingSide" %in% names(pbp)) {
  pbp[, homeTeamDefendingSide := NA_character_]
}

setorder(pbp, game_id, sortOrder)
pbp[
  ,
  `:=`(
    prev_event_type = shift(event_type, 1L),
    prev_reason = shift(details.reason, 1L),
    next_event_type = shift(event_type, -1L),
    next_xCoord = shift(details.xCoord, -1L),
    next_defending_side = shift(homeTeamDefendingSide, -1L)
  ),
  by = game_id
]

# ------------------------------------------------------------
# 7b. Flag shifts that contain a faceoff partway through
# ------------------------------------------------------------

faceoff_events <- pbp[
  !is.na(game_seconds) & toupper(event_type) == "FACEOFF",
  .(game_id, game_seconds = as.integer(game_seconds))
]
faceoff_events <- unique(faceoff_events)

cat("\nFaceoff events available for stoppage-flagging:", nrow(faceoff_events), "\n")

stoppage_matches <- shifts[
  faceoff_events,
  on = .(game_id, shift_start < game_seconds, shift_end > game_seconds),
  nomatch = NULL,
  .(shift_id)
]
stoppage_shift_ids <- unique(stoppage_matches$shift_id)

shifts[, has_stoppage := shift_id %in% stoppage_shift_ids]

cat("Shifts containing a faceoff:", sum(shifts$has_stoppage),
    "/", nrow(shifts),
    "(", round(100 * mean(shifts$has_stoppage), 1), "%)\n")

shift_ivl <- merge(
  shift_ivl,
  shifts[, .(shift_id, has_stoppage)],
  by = "shift_id",
  all.x = TRUE
)

# ------------------------------------------------------------
# 7c. Defensive-zone faceoffs, exact icing state, and on-ice units
# ------------------------------------------------------------

# A faceoff at |x| >= DZ_FACEOFF_X_MIN is treated as an end-zone faceoff.
# The sign of x plus homeTeamDefendingSide identifies the defending team.
def_zone_faceoffs <- pbp[
  !is.na(game_seconds) &
    toupper(event_type) == "FACEOFF" &
    !is.na(details.xCoord) &
    abs(details.xCoord) >= DZ_FACEOFF_X_MIN &
    !is.na(homeTeamDefendingSide),
  .(
    game_id,
    faceoff_sort_order = sortOrder,
    faceoff_game_seconds = as.integer(game_seconds),
    period,
    faceoff_x = details.xCoord,
    home_defending_side = tolower(homeTeamDefendingSide),
    home_abbr,
    away_abbr,
    defending_team = fcase(
      tolower(homeTeamDefendingSide) == "right" & details.xCoord > 0, home_abbr,
      tolower(homeTeamDefendingSide) == "right" & details.xCoord < 0, away_abbr,
      tolower(homeTeamDefendingSide) == "left"  & details.xCoord < 0, home_abbr,
      tolower(homeTeamDefendingSide) == "left"  & details.xCoord > 0, away_abbr,
      default = NA_character_
    ),
    is_icing = !is.na(prev_reason) &
      toupper(prev_event_type) == "STOPPAGE" &
      tolower(prev_reason) == "icing",
    situation_code = situationCode,
    away_goalie,
    away_skaters,
    home_skaters,
    home_goalie,
    game_state,
    is_5v5 = !is.na(game_state) & game_state == "5v5"
  )
]

def_zone_faceoffs <- unique(
  def_zone_faceoffs[!is.na(defending_team)],
  by = c("game_id", "faceoff_sort_order")
)

def_zone_faceoffs[
  ,
  `:=`(
    attacking_team = fifelse(defending_team == home_abbr, away_abbr, home_abbr),
    defending_home_away = fifelse(defending_team == home_abbr, "Home", "Away")
  )
]

setorder(def_zone_faceoffs, game_id, faceoff_sort_order)
def_zone_faceoffs[, event_id := .I]

cat("Defensive-zone faceoff events:", nrow(def_zone_faceoffs), "\n")
cat("Icing faceoffs:", def_zone_faceoffs[is_icing == TRUE, .N], "\n")
cat("Exact 5v5 icing faceoffs:",
    def_zone_faceoffs[is_icing == TRUE & is_5v5 == TRUE, .N], "\n")

# Match the defending team's skaters to the exact faceoff time.
# Use <= at shift start and > at shift end so a skater beginning at the
# faceoff is included, while one whose shift ended at that time is not.
shift_lookup <- shifts[
  ,
  .(
    shift_game_id = game_id,
    shift_team = team,
    shift_id,
    player_id,
    player_name,
    position,
    fwd_def,
    player_shift_start = shift_start,
    player_shift_end = shift_end
  )
]

event_players <- shift_lookup[
  def_zone_faceoffs,
  on = .(
    shift_game_id = game_id,
    shift_team = defending_team,
    player_shift_start <= faceoff_game_seconds,
    player_shift_end > faceoff_game_seconds
  ),
  nomatch = NULL,
  allow.cartesian = TRUE,
  .(
    event_id = i.event_id,
    game_id = i.game_id,
    faceoff_game_seconds = i.faceoff_game_seconds,
    is_icing = i.is_icing,
    event_game_state = i.game_state,
    event_is_5v5 = i.is_5v5,
    defending_team = i.defending_team,
    attacking_team = i.attacking_team,
    shift_id,
    player_id,
    player_name,
    position,
    fwd_def,
    player_shift_start,
    player_shift_end,
    toi_at_event_sec = i.faceoff_game_seconds - player_shift_start
  )
]

# Defensive protection against duplicate reconstructed records for one player.
setorder(event_players, event_id, player_id, -player_shift_start)
event_players <- unique(event_players, by = c("event_id", "player_id"))

event_unit_summary <- event_players[
  ,
  .(
    n_skaters = as.integer(uniqueN(player_id)),
    
    n_forwards = as.integer(
      sum(fwd_def == "Forward", na.rm = TRUE)
    ),
    
    n_defense = as.integer(
      sum(fwd_def == "Defense", na.rm = TRUE)
    ),
    
    mean_toi_at_event = as.numeric(
      mean(toi_at_event_sec, na.rm = TRUE)
    ),
    
    median_toi_at_event = as.numeric(
      median(toi_at_event_sec, na.rm = TRUE)
    ),
    
    min_toi_at_event = as.numeric(
      min(toi_at_event_sec, na.rm = TRUE)
    ),
    
    max_toi_at_event = as.numeric(
      max(toi_at_event_sec, na.rm = TRUE)
    ),
    
    mean_forward_toi = as.numeric(
      if (any(fwd_def == "Forward", na.rm = TRUE)) {
        mean(
          toi_at_event_sec[fwd_def == "Forward"],
          na.rm = TRUE
        )
      } else {
        NA_real_
      }
    ),
    
    mean_defense_toi = as.numeric(
      if (any(fwd_def == "Defense", na.rm = TRUE)) {
        mean(
          toi_at_event_sec[fwd_def == "Defense"],
          na.rm = TRUE
        )
      } else {
        NA_real_
      }
    ),
    
    line_end_time = as.numeric(
      min(player_shift_end, na.rm = TRUE)
    ),
    
    unit_id = paste(
      sort(unique(as.character(player_id))),
      collapse = "-"
    ),
    
    forward_line_id = paste(
      sort(
        unique(
          as.character(
            player_id[fwd_def == "Forward"]
          )
        )
      ),
      collapse = "-"
    ),
    
    defense_pair_id = paste(
      sort(
        unique(
          as.character(
            player_id[fwd_def == "Defense"]
          )
        )
      ),
      collapse = "-"
    )
  ),
  by = event_id
]

event_units <- merge(
  def_zone_faceoffs,
  event_unit_summary,
  by = "event_id",
  all.x = TRUE
)

event_units[is.na(n_skaters), n_skaters := 0L]
event_units[is.na(n_forwards), n_forwards := 0L]
event_units[is.na(n_defense), n_defense := 0L]

event_units[
  ,
  `:=`(
    valid_5v5_skaters = is_5v5 & n_skaters == 5L,
    standard_3F2D_unit = is_5v5 & n_skaters == 5L &
      n_forwards == 3L & n_defense == 2L
  )
]

icing_event_players <- event_players[is_icing == TRUE]

cat("\nIcing event sizes by exact game state and skater count:\n")
print(
  event_units[
    is_icing == TRUE,
    .N,
    by = .(game_state, n_skaters)
  ][order(game_state, n_skaters)]
)

# Shift-level flags are retained for description, but should not be used as
# the causal/model indicator because they also mark pre-icing intervals.
icing_shift_ids <- unique(icing_event_players$shift_id)
icing_5v5_shift_ids <- unique(
  icing_event_players[event_game_state == "5v5", shift_id]
)

shifts[, shift_contains_icing := shift_id %in% icing_shift_ids]
shifts[, shift_contains_5v5_icing := shift_id %in% icing_5v5_shift_ids]
shifts[, has_icing := shift_contains_icing]

shift_ivl <- merge(
  shift_ivl,
  shifts[
    ,
    .(shift_id, shift_contains_icing, shift_contains_5v5_icing, has_icing)
  ],
  by = "shift_id",
  all.x = TRUE
)

# Mark the five-second bucket containing the exact icing faceoff.
icing_moment_lookup <- unique(
  icing_event_players[
    event_game_state == "5v5",
    .(
      shift_id,
      interval_start = as.integer((faceoff_game_seconds %/% 5L) * 5L)
    )
  ]
)
icing_moment_lookup[, is_icing_moment_5v5 := TRUE]

shift_ivl <- merge(
  shift_ivl,
  icing_moment_lookup,
  by = c("shift_id", "interval_start"),
  all.x = TRUE
)
shift_ivl[is.na(is_icing_moment_5v5), is_icing_moment_5v5 := FALSE]

# Mark only intervals after a 5v5 icing, never intervals before it.
icing_times_by_shift <- unique(
  icing_event_players[
    event_game_state == "5v5",
    .(
      shift_id,
      icing_game_seconds = faceoff_game_seconds,
      post_interval_start = as.integer(ceiling(faceoff_game_seconds / 5) * 5)
    )
  ]
)

post_icing_map <- merge(
  shift_ivl[
    shift_id %in% icing_times_by_shift$shift_id,
    .(shift_id, interval_start)
  ],
  icing_times_by_shift,
  by = "shift_id",
  allow.cartesian = TRUE
)

post_icing_map <- post_icing_map[interval_start >= post_interval_start]
post_icing_map <- post_icing_map[
  ,
  .(last_icing_game_seconds = max(icing_game_seconds)),
  by = .(shift_id, interval_start)
]
post_icing_map[
  ,
  `:=`(
    post_icing_5v5 = TRUE,
    seconds_since_icing = interval_start - last_icing_game_seconds
  )
]

shift_ivl <- merge(
  shift_ivl,
  post_icing_map,
  by = c("shift_id", "interval_start"),
  all.x = TRUE
)
shift_ivl[is.na(post_icing_5v5), post_icing_5v5 := FALSE]

# ------------------------------------------------------------
# 8. Build exact 5v5 / PP / PK / empty-net state timeline
# ------------------------------------------------------------

pbp_state_obs <- pbp[
  !is.na(game_seconds) & !is.na(game_state),
  .(
    game_id,
    game_seconds = as.integer(game_seconds),
    sortOrder,
    situation_code = situationCode,
    away_goalie,
    away_skaters,
    home_skaters,
    home_goalie,
    game_state
  )
]

setorder(pbp_state_obs, game_id, game_seconds, sortOrder)
pbp_state_obs <- pbp_state_obs[, .SD[.N], by = .(game_id, game_seconds)]

# Safe initial state before the first recorded play.
game_starts <- data.table(
  game_id = unique(shift_ivl$game_id),
  game_seconds = 0L,
  sortOrder = 0L,
  situation_code = "1551",
  away_goalie = 1L,
  away_skaters = 5L,
  home_skaters = 5L,
  home_goalie = 1L,
  game_state = "5v5"
)

pbp_state_obs <- rbindlist(list(game_starts, pbp_state_obs), fill = TRUE)
setorder(pbp_state_obs, game_id, game_seconds, sortOrder)
pbp_state_obs <- pbp_state_obs[, .SD[.N], by = .(game_id, game_seconds)]

cat("\nPBP state observations:", nrow(pbp_state_obs), "\n")
print(table(pbp_state_obs$game_state, useNA = "ifany"))

# ------------------------------------------------------------
# 9. Assign state to every five-second player observation
# ------------------------------------------------------------

state_grid <- unique(shift_ivl[, .(game_id, interval_start)])

setkey(pbp_state_obs, game_id, game_seconds)
setkey(state_grid, game_id, interval_start)

state_grid <- pbp_state_obs[
  state_grid,
  on = .(game_id, game_seconds = interval_start),
  roll = TRUE
]

setnames(state_grid, "game_seconds", "interval_start")

state_grid <- unique(
  state_grid[
    ,
    .(
      game_id,
      interval_start,
      situation_code,
      away_goalie,
      away_skaters,
      home_skaters,
      home_goalie,
      game_state
    )
  ]
)

shift_ivl <- merge(
  shift_ivl,
  state_grid,
  by = c("game_id", "interval_start"),
  all.x = TRUE
)

shift_ivl[, strength_detail := classify_team_strength(game_state, home_away)]
shift_ivl[
  ,
  strength := fcase(
    strength_detail %chin% c("5v5", "EV_OTHER"), "EV",
    strength_detail == "PP", "PP",
    strength_detail == "PK", "PK",
    strength_detail == "EN_ATTACK", "EN_ATTACK",
    strength_detail == "EN_DEFEND", "EN_DEFEND",
    default = "OTHER"
  )
]

shift_ivl[, is_5v5 := as.integer(strength_detail == "5v5")]
shift_ivl[, is_EV := as.integer(strength == "EV")]
shift_ivl[, is_PP := as.integer(strength == "PP")]
shift_ivl[, is_PK := as.integer(strength == "PK")]
shift_ivl[, is_empty_net := as.integer(strength %chin% c("EN_ATTACK", "EN_DEFEND"))]

cat("Missing strength classifications:", sum(is.na(shift_ivl$strength_detail)), "\n")
print(table(shift_ivl$strength_detail, useNA = "ifany"))

six_on_five_intervals <- state_grid[
  (home_goalie == 0L & home_skaters == 6L & away_goalie == 1L & away_skaters == 5L) |
    (away_goalie == 0L & away_skaters == 6L & home_goalie == 1L & home_skaters == 5L),
  .N
]
cat("Unique 6-on-5 empty-net intervals kept out of PP/PK:",
    six_on_five_intervals, "\n")

# ------------------------------------------------------------
# 10. MoneyPuck xG with exact shot-strength classification
# ------------------------------------------------------------

mp_zip_url <- paste0(
  "https://peter-tanner.com/moneypuck/downloads/shots_",
  SEASON_START_YEAR,
  ".zip"
)
mp_zip_path <- paste0("shots_", SEASON_START_YEAR, ".zip")
mp_dir <- "moneypuck_data"

if (!file.exists(mp_zip_path)) {
  download.file(mp_zip_url, mp_zip_path, mode = "wb")
}

dir.create(mp_dir, showWarnings = FALSE)

mp_csv_name <- unzip(mp_zip_path, list = TRUE)$Name[1]
mp_csv_path <- file.path(mp_dir, mp_csv_name)

if (!file.exists(mp_csv_path)) {
  unzip(mp_zip_path, exdir = mp_dir, overwrite = FALSE)
}

shots <- fread(mp_csv_path)

required_shot_cols <- c(
  "game_id", "teamCode", "xGoal", "period", "time",
  "homeTeamCode", "awayTeamCode",
  "homeEmptyNet", "awayEmptyNet",
  "homeSkatersOnIce", "awaySkatersOnIce"
)
missing_shot_cols <- setdiff(required_shot_cols, names(shots))

if (length(missing_shot_cols) > 0L) {
  stop(paste(
    "Missing required MoneyPuck columns:",
    paste(missing_shot_cols, collapse = ", ")
  ))
}

cat("\nRaw MoneyPuck game_id range:", range(shots$game_id), "\n")

if (max(nchar(as.character(shots$game_id))) <= 6) {
  shots[
    ,
    game_id := as.integer(paste0(SEASON_START_YEAR, sprintf("%06d", game_id)))
  ]
  cat("Rebuilt MoneyPuck game_id range:", range(shots$game_id), "\n")
}

shots[, teamCode := toupper(teamCode)]
shots[, homeTeamCode := toupper(homeTeamCode)]
shots[, awayTeamCode := toupper(awayTeamCode)]

shots[, period_offset := (period - 1L) * 1200L]
shots[, abs_time := period_offset + time]
shots[, interval_start := as.integer((abs_time %/% 5L) * 5L)]

shots[, mp_home_empty_net := as_binary01(homeEmptyNet)]
shots[, mp_away_empty_net := as_binary01(awayEmptyNet)]
shots[, mp_home_goalie := fifelse(is.na(mp_home_empty_net), NA_integer_, 1L - mp_home_empty_net)]
shots[, mp_away_goalie := fifelse(is.na(mp_away_empty_net), NA_integer_, 1L - mp_away_empty_net)]

shots[
  ,
  shot_game_state := classify_game_state(
    mp_away_goalie,
    as.integer(awaySkatersOnIce),
    as.integer(homeSkatersOnIce),
    mp_home_goalie
  )
]

shots[
  ,
  shooting_home_away := fcase(
    teamCode == homeTeamCode, "Home",
    teamCode == awayTeamCode, "Away",
    default = NA_character_
  )
]
shots[, shot_offense_strength := classify_team_strength(shot_game_state, shooting_home_away)]
shots[, shot_defense_strength := flip_team_strength(shot_offense_strength)]
shots[
  ,
  defendingTeamCode := fcase(
    teamCode == homeTeamCode, awayTeamCode,
    teamCode == awayTeamCode, homeTeamCode,
    default = NA_character_
  )
]

# Overall xG, retained for backward compatibility.
shot_agg <- shots[
  ,
  .(xG = sum(xGoal, na.rm = TRUE)),
  by = .(game_id, teamCode, interval_start)
]

shift_ivl <- merge(
  shift_ivl,
  shot_agg,
  by.x = c("game_id", "team", "interval_start"),
  by.y = c("game_id", "teamCode", "interval_start"),
  all.x = TRUE
)
setnames(shift_ivl, "xG", "xGF")
shift_ivl[is.na(xGF), xGF := 0]

shift_ivl <- merge(
  shift_ivl,
  shot_agg,
  by.x = c("game_id", "opp_team", "interval_start"),
  by.y = c("game_id", "teamCode", "interval_start"),
  all.x = TRUE
)
setnames(shift_ivl, "xG", "xGA")
shift_ivl[is.na(xGA), xGA := 0]

# Exact xG and shot counts by team-strength perspective.
shot_for_long <- shots[
  !is.na(shot_offense_strength),
  .(
    xGF = sum(xGoal, na.rm = TRUE),
    shots_for = .N
  ),
  by = .(
    game_id,
    team = teamCode,
    interval_start,
    offense_strength = shot_offense_strength
  )
]

shot_for_wide <- dcast(
  shot_for_long,
  game_id + team + interval_start ~ offense_strength,
  value.var = c("xGF", "shots_for"),
  fill = 0
)

shot_against_long <- shots[
  !is.na(shot_defense_strength) & !is.na(defendingTeamCode),
  .(
    xGA = sum(xGoal, na.rm = TRUE),
    shots_against = .N
  ),
  by = .(
    game_id,
    team = defendingTeamCode,
    interval_start,
    defense_strength = shot_defense_strength
  )
]

shot_against_wide <- dcast(
  shot_against_long,
  game_id + team + interval_start ~ defense_strength,
  value.var = c("xGA", "shots_against"),
  fill = 0
)

shift_ivl <- merge(
  shift_ivl,
  shot_for_wide,
  by = c("game_id", "team", "interval_start"),
  all.x = TRUE
)
shift_ivl <- merge(
  shift_ivl,
  shot_against_wide,
  by = c("game_id", "team", "interval_start"),
  all.x = TRUE
)

strength_levels <- c(
  "5v5", "EV_OTHER", "PP", "PK",
  "EN_ATTACK", "EN_DEFEND", "OTHER"
)
expected_strength_xg_cols <- c(
  paste0("xGF_", strength_levels),
  paste0("xGA_", strength_levels),
  paste0("shots_for_", strength_levels),
  paste0("shots_against_", strength_levels)
)

for (col_name in expected_strength_xg_cols) {
  if (!col_name %in% names(shift_ivl)) {
    shift_ivl[, (col_name) := 0]
  } else {
    shift_ivl[is.na(get(col_name)), (col_name) := 0]
  }
}

cat("\nRows with xGF > 0:", sum(shift_ivl$xGF > 0), "/", nrow(shift_ivl), "\n")
cat("MoneyPuck shot states:\n")
print(table(shots$shot_game_state, useNA = "ifany"))

# ------------------------------------------------------------
# 11. Score differential from PBP
# ------------------------------------------------------------

score_obs <- NULL

if (all(c("home_score", "away_score") %in% names(pbp))) {

  score_obs <- pbp[
    !is.na(game_seconds) & !is.na(home_score) & !is.na(away_score),
    .(home_score = home_score[.N], away_score = away_score[.N]),
    by = .(game_id, game_seconds = as.integer(game_seconds))
  ]

  score_starts <- data.table(
    game_id = unique(shift_ivl$game_id),
    game_seconds = 0L,
    home_score = 0L,
    away_score = 0L
  )

  score_obs <- rbindlist(list(score_starts, score_obs), fill = TRUE)
  setorder(score_obs, game_id, game_seconds)
  score_obs <- score_obs[, .SD[.N], by = .(game_id, game_seconds)]

  score_grid <- unique(shift_ivl[, .(game_id, interval_start)])

  setkey(score_obs, game_id, game_seconds)
  setkey(score_grid, game_id, interval_start)

  score_grid <- score_obs[
    score_grid,
    on = .(game_id, game_seconds = interval_start),
    roll = TRUE
  ]

  setnames(score_grid, "game_seconds", "interval_start")
  score_grid <- unique(
    score_grid[, .(game_id, interval_start, home_score, away_score)]
  )

  shift_ivl <- merge(
    shift_ivl,
    score_grid,
    by = c("game_id", "interval_start"),
    all.x = TRUE
  )

  shift_ivl[
    ,
    score_diff := fifelse(
      home_away == "Home",
      home_score - away_score,
      away_score - home_score
    )
  ]

  shift_ivl[is.na(score_diff), score_diff := 0L]

} else {
  warning("home_score / away_score missing from PBP; score_diff set to NA")
  shift_ivl[, score_diff := NA_integer_]
}

# ------------------------------------------------------------
# 12. Build one-row-per-faceoff data for the icing model
# ------------------------------------------------------------

icing_model_events <- copy(event_units)

if (!is.null(score_obs) && nrow(icing_model_events) > 0L) {
  event_score_query <- icing_model_events[
    ,
    .(event_id, game_id, event_time = faceoff_game_seconds)
  ]

  setkey(score_obs, game_id, game_seconds)
  setkey(event_score_query, game_id, event_time)

  event_scores <- score_obs[
    event_score_query,
    on = .(game_id, game_seconds = event_time),
    roll = TRUE,
    .(
      event_id = i.event_id,
      home_score,
      away_score
    )
  ]

  icing_model_events <- merge(
    icing_model_events,
    event_scores,
    by = "event_id",
    all.x = TRUE
  )

  icing_model_events[
    ,
    score_diff := fifelse(
      defending_home_away == "Home",
      home_score - away_score,
      away_score - home_score
    )
  ]
} else {
  icing_model_events[, `:=`(
    home_score = NA_integer_,
    away_score = NA_integer_,
    score_diff = NA_integer_
  )]
}

# Find the next play-stopping event after each faceoff. The same-time icing
# stoppage is skipped because the faceoff occurs at that same game clock time.
play_stop_types <- c("STOPPAGE", "GOAL", "PENALTY", "PERIOD_END", "GAME_END")
stoppage_times <- pbp[
  !is.na(game_seconds) & toupper(event_type) %chin% play_stop_types,
  .(game_seconds = sort(unique(as.integer(game_seconds)))),
  by = game_id
]

stop_time_list <- split(stoppage_times$game_seconds, stoppage_times$game_id)

next_time_after <- function(gid, event_time, time_lookup) {
  times <- time_lookup[[as.character(gid)]]
  if (is.null(times) || length(times) == 0L) return(Inf)

  idx <- findInterval(event_time, times) + 1L
  if (idx <= length(times)) times[idx] else Inf
}

icing_model_events[
  ,
  next_stoppage_time := mapply(
    next_time_after,
    game_id,
    faceoff_game_seconds,
    MoreArgs = list(time_lookup = stop_time_list)
  )
]

icing_model_events[
  ,
  `:=`(
    period_start_time = fifelse(
      period <= 3,
      (period - 1L) * 1200L,
      3600L + (period - 4L) * 300L
    ),
    period_end_time = fifelse(
      period <= 3,
      period * 1200L,
      3600L + (period - 3L) * 300L
    )
  )
]

icing_model_events[
  ,
  analysis_end_time := pmin(
    faceoff_game_seconds + POST_EVENT_WINDOW_SEC,
    fifelse(is.na(line_end_time), Inf, line_end_time),
    next_stoppage_time,
    period_end_time
  )
]

icing_model_events[
  ,
  `:=`(
    exposure_sec = pmax(0, analysis_end_time - faceoff_game_seconds),
    pre_window_start = pmax(
      period_start_time,
      faceoff_game_seconds - PRE_EVENT_WINDOW_SEC
    )
  )
]

icing_model_events[
  ,
  valid_model_row := valid_5v5_skaters & exposure_sec > 0
]

model_windows <- icing_model_events[
  valid_model_row == TRUE,
  .(
    event_id,
    game_id,
    defending_team,
    attacking_team,
    window_start = faceoff_game_seconds,
    window_end = analysis_end_time,
    pre_window_start,
    pre_window_end = faceoff_game_seconds
  )
]

# Opponent shots after the faceoff.
post_against_shots <- shots[
  model_windows,
  on = .(
    game_id,
    teamCode = attacking_team,
    abs_time >= window_start,
    abs_time < window_end
  ),
  nomatch = NULL,
  allow.cartesian = TRUE,
  .(
    event_id = i.event_id,
    xGoal,
    shot_game_state
  )
]

post_against_summary <- post_against_shots[
  ,
  .(
    post_xGA_all = sum(xGoal, na.rm = TRUE),
    post_shots_against_all = .N,
    post_xGA_5v5 = sum(xGoal[shot_game_state == "5v5"], na.rm = TRUE),
    post_shots_against_5v5 = sum(shot_game_state == "5v5", na.rm = TRUE)
  ),
  by = event_id
]

# Defending-team shots after the faceoff.
post_for_shots <- shots[
  model_windows,
  on = .(
    game_id,
    teamCode = defending_team,
    abs_time >= window_start,
    abs_time < window_end
  ),
  nomatch = NULL,
  allow.cartesian = TRUE,
  .(
    event_id = i.event_id,
    xGoal,
    shot_game_state
  )
]

post_for_summary <- post_for_shots[
  ,
  .(
    post_xGF_all = sum(xGoal, na.rm = TRUE),
    post_shots_for_all = .N,
    post_xGF_5v5 = sum(xGoal[shot_game_state == "5v5"], na.rm = TRUE),
    post_shots_for_5v5 = sum(shot_game_state == "5v5", na.rm = TRUE)
  ),
  by = event_id
]

# Opponent pressure in the 20 seconds before the faceoff. This is useful as
# a control because teams often ice the puck after being pinned in their zone.
pre_against_shots <- shots[
  model_windows,
  on = .(
    game_id,
    teamCode = attacking_team,
    abs_time >= pre_window_start,
    abs_time < pre_window_end
  ),
  nomatch = NULL,
  allow.cartesian = TRUE,
  .(
    event_id = i.event_id,
    xGoal,
    shot_game_state
  )
]

pre_against_summary <- pre_against_shots[
  ,
  .(
    pre_xGA_20_all = sum(xGoal, na.rm = TRUE),
    pre_shots_against_20_all = .N,
    pre_xGA_20_5v5 = sum(xGoal[shot_game_state == "5v5"], na.rm = TRUE),
    pre_shots_against_20_5v5 = sum(shot_game_state == "5v5", na.rm = TRUE)
  ),
  by = event_id
]

icing_model_events <- merge(
  icing_model_events,
  post_against_summary,
  by = "event_id",
  all.x = TRUE
)
icing_model_events <- merge(
  icing_model_events,
  post_for_summary,
  by = "event_id",
  all.x = TRUE
)
icing_model_events <- merge(
  icing_model_events,
  pre_against_summary,
  by = "event_id",
  all.x = TRUE
)

model_outcome_cols <- c(
  "post_xGA_all", "post_shots_against_all",
  "post_xGA_5v5", "post_shots_against_5v5",
  "post_xGF_all", "post_shots_for_all",
  "post_xGF_5v5", "post_shots_for_5v5",
  "pre_xGA_20_all", "pre_shots_against_20_all",
  "pre_xGA_20_5v5", "pre_shots_against_20_5v5"
)

for (col_name in model_outcome_cols) {
  if (!col_name %in% names(icing_model_events)) {
    icing_model_events[, (col_name) := 0]
  } else {
    icing_model_events[is.na(get(col_name)), (col_name) := 0]
  }
}

icing_model_events[
  exposure_sec > 0,
  `:=`(
    post_xGA_5v5_per60 = 3600 * post_xGA_5v5 / exposure_sec,
    post_xGF_5v5_per60 = 3600 * post_xGF_5v5 / exposure_sec,
    post_xG_diff_5v5_per60 =
      3600 * (post_xGF_5v5 - post_xGA_5v5) / exposure_sec
  )
]

icing_model_events[
  ,
  `:=`(
    is_icing_int = as.integer(is_icing),
    fatigue_10sec = mean_toi_at_event / 10,
    any_post_shot_against_5v5 = as.integer(post_shots_against_5v5 > 0)
  )
]

# ------------------------------------------------------------
# 13. Diagnostics
# ------------------------------------------------------------

cat("\n--- DIAGNOSTICS ---\n")
cat("Final player rows before bad-game exclusion:", nrow(shift_ivl), "\n")
cat("Games represented:", uniqueN(shift_ivl$game_id), "\n")
cat("Player-row strength detail:\n")
print(table(shift_ivl$strength_detail, useNA = "ifany"))

cat("\nIcing faceoffs by exact state:\n")
print(
  icing_model_events[
    is_icing == TRUE,
    .N,
    by = .(game_state, n_skaters)
  ][order(game_state, n_skaters)]
)

cat("Valid exact-5v5 icing events with exactly five defending skaters:",
    icing_model_events[
      is_icing == TRUE & valid_5v5_skaters == TRUE,
      .N
    ], "\n")

# ------------------------------------------------------------
# 14. Final outputs
# ------------------------------------------------------------

n_before <- nrow(shift_ivl)
shift_ivl <- shift_ivl[!(game_id %in% BAD_GAME_IDS)]
cat("\nExcluded", n_before - nrow(shift_ivl), "player rows from",
    length(BAD_GAME_IDS), "known-bad game(s):",
    paste(BAD_GAME_IDS, collapse = ", "), "\n")

icing_model_events <- icing_model_events[!(game_id %in% BAD_GAME_IDS)]
icing_event_players <- icing_event_players[!(game_id %in% BAD_GAME_IDS)]

xg_detail_cols <- grep(
  "^(xGF_|xGA_|shots_for_|shots_against_)",
  names(shift_ivl),
  value = TRUE
)

player_keep_cols <- unique(c(
  "player_name", "player_id", "shift_id", "game_id", "team", "opp_team",
  "period", "shift_start", "shift_end", "shift_duration_sec",
  "interval_start", "interval_end", "shift_age_sec", "shift_age_mid_sec",
  "position", "fwd_def", "home_away", "game_minute",
  "situation_code", "away_goalie", "away_skaters",
  "home_skaters", "home_goalie", "game_state",
  "strength_detail", "strength", "is_5v5", "is_EV", "is_PP", "is_PK",
  "is_empty_net", "score_diff", "home_score", "away_score",
  "xGF", "xGA", xg_detail_cols,
  "has_stoppage", "shift_contains_icing", "shift_contains_5v5_icing",
  "has_icing", "is_icing_moment_5v5", "post_icing_5v5",
  "seconds_since_icing", "last_icing_game_seconds"
))
player_keep_cols <- intersect(player_keep_cols, names(shift_ivl))

final <- shift_ivl[, ..player_keep_cols]
setnames(final, "player_name", "player")

icing_model_data <- icing_model_events[
  valid_model_row == TRUE &
    n_skaters == 5L &
    exposure_sec > 0
]

fwrite(final, "nhl_2025_26_player_shift_data.csv")
fwrite(icing_model_events, "nhl_2025_26_dz_faceoff_events_all.csv")
fwrite(icing_model_data, "nhl_2025_26_icing_faceoff_model_data.csv")
fwrite(icing_event_players, "nhl_2025_26_icing_on_ice_players.csv")

cat("\nDone. Player rows:", nrow(final), "\n")
cat("Saved: nhl_2025_26_player_shift_data.csv\n")
cat("Saved: nhl_2025_26_dz_faceoff_events_all.csv\n")
cat("Saved: nhl_2025_26_icing_faceoff_model_data.csv\n")
cat("Saved: nhl_2025_26_icing_on_ice_players.csv\n")

# ------------------------------------------------------------
# Optional model starter (requires glmmTMB)
# ------------------------------------------------------------
#
# The model file contains BOTH icing and non-icing defensive-zone faceoffs.
# It is therefore suitable for an icing indicator, fatigue, and interaction.
# Do not fit the causal question with the shift-level has_icing flag, because
# that flag is TRUE on intervals that occurred before the icing.
#
# library(glmmTMB)
#
# model_data <- fread("nhl_2025_26_icing_faceoff_model_data.csv")
# model_data <- model_data[
#   standard_3F2D_unit == TRUE & exposure_sec >= 5
# ]
# model_data[, fatigue10_c := fatigue_10sec - mean(fatigue_10sec, na.rm = TRUE)]
#
# fit_xga <- glmmTMB(
#   post_xGA_5v5 ~
#     is_icing_int * fatigue10_c +
#     score_diff + factor(period) + pre_xGA_20_5v5 +
#     (1 | game_id) + (1 | defending_team),
#   offset = log(exposure_sec),
#   family = tweedie(link = "log"),
#   data = model_data
# )
#
# summary(fit_xga)
