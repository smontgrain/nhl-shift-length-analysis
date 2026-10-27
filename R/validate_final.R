# ============================================================
# NHL 2025-26 SHIFT LENGTH PROJECT
# FINAL DATA VALIDATION
#
# Purpose:
#   Validate the completed player-interval dataset before analysis.
#
# Main questions:
#   1. Did we retain the expected games?
#   2. Does every game have usable PBP?
#   3. How many games contain penalties?
#   4. Are PP and PK states paired correctly?
#   5. Are 5v5 / PP / PK / empty-net states internally consistent?
#   6. Does reconstructed on-ice player count agree with NHL PBP?
#   7. Are xGF / xGA joins internally consistent?
#   8. Are shift ages and player rows structurally plausible?
#   9. Are icing-event datasets internally consistent?
#
# This script is diagnostic only.
# It does NOT modify the analysis dataset.
# ============================================================

library(data.table)

# ============================================================
# 0. SETTINGS
# ============================================================

EXPECTED_SCHEDULE_GAMES <- 1394L

BAD_GAME_IDS <- c(
  2025020565L
)

EXPECTED_FINAL_GAMES <-
  EXPECTED_SCHEDULE_GAMES - length(BAD_GAME_IDS)

FINAL_FILE <-
  "notebooks/nhl_2025_26_player_shift_data.csv"

ICING_ALL_FILE <-
  "notebooks/nhl_2025_26_dz_faceoff_events_all.csv"

ICING_MODEL_FILE <-
  "notebooks/nhl_2025_26_icing_faceoff_model_data.csv"

ICING_PLAYERS_FILE <-
  "notebooks/nhl_2025_26_icing_on_ice_players.csv"

PBP_CACHE_DIR <-
  "pbp_cache_2025_26"


# ============================================================
# 1. SIMPLE CHECK-REPORTING SYSTEM
# ============================================================

validation_results <- data.table(
  section = character(),
  check = character(),
  status = character(),
  value = character(),
  note = character()
)

add_check <- function(
    section,
    check,
    status,
    value = "",
    note = ""
) {
  
  validation_results <<- rbind(
    validation_results,
    data.table(
      section = section,
      check = check,
      status = status,
      value = as.character(value),
      note = note
    )
  )
  
  cat(
    sprintf(
      "%-5s | %-40s | %s\n",
      status,
      check,
      as.character(value)
    )
  )
}

section_header <- function(x) {
  
  cat(
    "\n",
    paste(rep("=", 70), collapse = ""),
    "\n",
    x,
    "\n",
    paste(rep("=", 70), collapse = ""),
    "\n",
    sep = ""
  )
}

safe_pct <- function(x, digits = 2) {
  
  if (length(x) == 0L || is.na(x)) {
    return(NA_real_)
  }
  
  round(100 * x, digits)
}

safe_mean <- function(x) {
  
  if (length(x) == 0L || all(is.na(x))) {
    return(NA_real_)
  }
  
  mean(x, na.rm = TRUE)
}


# ============================================================
# 2. LOAD FINAL OUTPUTS
# ============================================================

section_header("1. FILE AND DATA AVAILABILITY")

required_files <- c(
  FINAL_FILE,
  ICING_ALL_FILE,
  ICING_MODEL_FILE,
  ICING_PLAYERS_FILE
)

for (f in required_files) {
  
  exists_now <- file.exists(f)
  
  add_check(
    "Files",
    paste("Exists:", f),
    ifelse(exists_now, "PASS", "FAIL"),
    exists_now
  )
}

if (!file.exists(FINAL_FILE)) {
  
  stop(
    paste(
      "Main dataset is missing:",
      FINAL_FILE
    )
  )
}

final <- fread(
  FINAL_FILE,
  showProgress = TRUE
)

setDT(final)

icing_all <- if (file.exists(ICING_ALL_FILE)) {
  fread(ICING_ALL_FILE)
} else {
  NULL
}

icing_model <- if (file.exists(ICING_MODEL_FILE)) {
  fread(ICING_MODEL_FILE)
} else {
  NULL
}

icing_players <- if (file.exists(ICING_PLAYERS_FILE)) {
  fread(ICING_PLAYERS_FILE)
} else {
  NULL
}

cat(
  "\nFinal rows:",
  format(nrow(final), big.mark = ","),
  "\n"
)


# ============================================================
# 3. REQUIRED COLUMN CHECK
# ============================================================

section_header("2. REQUIRED COLUMN CHECK")

required_final_cols <- c(
  "player",
  "player_id",
  "shift_id",
  "game_id",
  "team",
  "opp_team",
  "period",
  "shift_start",
  "shift_end",
  "shift_duration_sec",
  "interval_start",
  "interval_end",
  "shift_age_sec",
  "shift_age_mid_sec",
  "position",
  "fwd_def",
  "home_away",
  "game_minute",
  "situation_code",
  "away_goalie",
  "away_skaters",
  "home_skaters",
  "home_goalie",
  "game_state",
  "strength_detail",
  "strength",
  "is_5v5",
  "is_EV",
  "is_PP",
  "is_PK",
  "is_empty_net",
  "score_diff",
  "xGF",
  "xGA"
)

missing_final_cols <- setdiff(
  required_final_cols,
  names(final)
)

add_check(
  "Columns",
  "Required final columns present",
  ifelse(
    length(missing_final_cols) == 0L,
    "PASS",
    "FAIL"
  ),
  length(missing_final_cols),
  ifelse(
    length(missing_final_cols) == 0L,
    "",
    paste(missing_final_cols, collapse = ", ")
  )
)

if (length(missing_final_cols) > 0L) {
  stop(
    paste(
      "Validation cannot continue. Missing required columns:",
      paste(missing_final_cols, collapse = ", ")
    )
  )
}


# ============================================================
# 4. GAME COVERAGE
# ============================================================

section_header("3. GAME COVERAGE")

n_final_games <- uniqueN(final$game_id)

add_check(
  "Games",
  "Games represented in final",
  ifelse(
    n_final_games == EXPECTED_FINAL_GAMES,
    "PASS",
    "WARN"
  ),
  paste(
    n_final_games,
    "/",
    EXPECTED_FINAL_GAMES
  )
)

bad_games_remaining <- intersect(
  unique(final$game_id),
  BAD_GAME_IDS
)

add_check(
  "Games",
  "Known bad games excluded",
  ifelse(
    length(bad_games_remaining) == 0L,
    "PASS",
    "FAIL"
  ),
  length(bad_games_remaining),
  paste(bad_games_remaining, collapse = ", ")
)


# ============================================================
# 5. PLAYER / SHIFT STRUCTURE
# ============================================================

section_header("4. PLAYER AND SHIFT STRUCTURE")

n_goalie_rows <- final[
  position == "G" |
    fwd_def == "G",
  .N
]

add_check(
  "Shifts",
  "Goalie rows removed",
  ifelse(
    n_goalie_rows == 0L,
    "PASS",
    "FAIL"
  ),
  n_goalie_rows
)

bad_shift_order <- final[
  shift_end <= shift_start,
  .N
]

add_check(
  "Shifts",
  "shift_end > shift_start",
  ifelse(
    bad_shift_order == 0L,
    "PASS",
    "FAIL"
  ),
  bad_shift_order
)

over_180 <- final[
  shift_duration_sec > 180,
  .N
]

add_check(
  "Shifts",
  "No retained shifts above 180 sec",
  ifelse(
    over_180 == 0L,
    "PASS",
    "WARN"
  ),
  over_180
)

bad_interval_width <- final[
  interval_end - interval_start != 5,
  .N
]

add_check(
  "Shifts",
  "Every interval is 5 sec",
  ifelse(
    bad_interval_width == 0L,
    "PASS",
    "FAIL"
  ),
  bad_interval_width
)

bad_shift_age <- final[
  shift_age_sec < 0 |
    shift_age_mid_sec < 0 |
    shift_age_sec > shift_duration_sec,
  .N
]

add_check(
  "Shifts",
  "Shift ages structurally valid",
  ifelse(
    bad_shift_age == 0L,
    "PASS",
    "FAIL"
  ),
  bad_shift_age
)

duplicate_player_intervals <- final[
  ,
  .N,
  by = .(
    game_id,
    team,
    player_id,
    interval_start
  )
][
  N > 1
]

add_check(
  "Shifts",
  "No duplicate player-team intervals",
  ifelse(
    nrow(duplicate_player_intervals) == 0L,
    "PASS",
    "FAIL"
  ),
  nrow(duplicate_player_intervals)
)


# ============================================================
# 6. PLAYER NAME COLLISIONS
# ============================================================

section_header("5. PLAYER NAME / ID CHECKS")

name_collisions <- final[
  ,
  .(
    n_ids = uniqueN(player_id)
  ),
  by = player
][
  n_ids > 1
]

add_check(
  "Players",
  "Names mapping to multiple IDs",
  "INFO",
  nrow(name_collisions),
  "Some may be legitimate namesakes."
)

if (nrow(name_collisions) > 0L) {
  
  print(
    name_collisions[
      order(-n_ids, player)
    ]
  )
}

unique_players <- uniqueN(
  final$player_id
)

add_check(
  "Players",
  "Unique player IDs",
  "INFO",
  unique_players
)


# ============================================================
# 7. PLAYER TOI SANITY
# ============================================================

section_header("6. PLAYER TOI SANITY")

league_toi <- final[
  ,
  .(
    games = uniqueN(game_id),
    total_minutes = .N * 5 / 60,
    fwd_def = first(fwd_def)
  ),
  by = .(
    player,
    player_id
  )
]

league_toi[
  ,
  avg_min_per_game :=
    total_minutes / games
]

cat("\nTOI distribution by position:\n")

print(
  league_toi[
    ,
    .(
      min = round(
        min(avg_min_per_game, na.rm = TRUE),
        1
      ),
      
      p25 = round(
        quantile(
          avg_min_per_game,
          0.25,
          na.rm = TRUE
        ),
        1
      ),
      
      median = round(
        median(
          avg_min_per_game,
          na.rm = TRUE
        ),
        1
      ),
      
      p75 = round(
        quantile(
          avg_min_per_game,
          0.75,
          na.rm = TRUE
        ),
        1
      ),
      
      max = round(
        max(avg_min_per_game, na.rm = TRUE),
        1
      )
    ),
    by = fwd_def
  ]
)

implausible_toi <- league_toi[
  avg_min_per_game > 28
]

add_check(
  "Players",
  "Players above 28 min/game",
  ifelse(
    nrow(implausible_toi) == 0L,
    "PASS",
    "WARN"
  ),
  nrow(implausible_toi)
)

if (nrow(implausible_toi) > 0L) {
  
  print(
    implausible_toi[
      order(-avg_min_per_game)
    ]
  )
}


# ============================================================
# 8. BUILD ONE TEAM ROW PER 5-SECOND INTERVAL
# ============================================================

section_header("7. TEAM-INTERVAL RECONSTRUCTION")

team_interval <- final[
  ,
  .(
    opp_team = first(opp_team),
    
    home_away = first(home_away),
    
    game_state = first(game_state),
    
    strength_detail = first(strength_detail),
    
    strength = first(strength),
    
    is_5v5 = first(is_5v5),
    
    is_PP = first(is_PP),
    
    is_PK = first(is_PK),
    
    is_empty_net = first(is_empty_net),
    
    away_skaters = first(away_skaters),
    
    home_skaters = first(home_skaters),
    
    away_goalie = first(away_goalie),
    
    home_goalie = first(home_goalie),
    
    n_reconstructed_skaters =
      uniqueN(player_id),
    
    n_forwards =
      uniqueN(
        player_id[
          fwd_def == "Forward"
        ]
      ),
    
    n_defense =
      uniqueN(
        player_id[
          fwd_def == "Defense"
        ]
      ),
    
    mean_shift_age_sec =
      mean(
        shift_age_mid_sec,
        na.rm = TRUE
      ),
    
    xGF = first(xGF),
    
    xGA = first(xGA)
  ),
  by = .(
    game_id,
    team,
    period,
    interval_start
  )
]

team_interval[
  ,
  declared_skaters := fifelse(
    home_away == "Home",
    home_skaters,
    away_skaters
  )
]

team_interval[
  ,
  skater_count_match :=
    n_reconstructed_skaters ==
    declared_skaters
]

cat("\nReconstructed skaters per team interval:\n")

print(
  team_interval[
    ,
    .N,
    by = n_reconstructed_skaters
  ][
    order(n_reconstructed_skaters)
  ]
)

valid_declared_count <- team_interval[
  !is.na(declared_skaters)
]

pct_skater_match <- safe_pct(
  mean(
    valid_declared_count$skater_count_match,
    na.rm = TRUE
  )
)

add_check(
  "On-ice",
  "Reconstructed count matches NHL PBP",
  ifelse(
    pct_skater_match >= 95,
    "PASS",
    "WARN"
  ),
  paste0(
    pct_skater_match,
    "%"
  )
)

cat("\nMismatch table:\n")

print(
  team_interval[
    !is.na(declared_skaters),
    .N,
    by = .(
      declared_skaters,
      n_reconstructed_skaters
    )
  ][
    order(
      declared_skaters,
      n_reconstructed_skaters
    )
  ]
)


# ============================================================
# 9. EXACT 5v5 UNIT CHECK
# ============================================================

section_header("8. EXACT 5v5 VALIDATION")

five_v_five <- team_interval[
  is_5v5 == 1L
]

bad_5v5_state <- five_v_five[
  game_state != "5v5" |
    strength_detail != "5v5" |
    strength != "EV",
  .N
]

add_check(
  "5v5",
  "5v5 flags agree with state fields",
  ifelse(
    bad_5v5_state == 0L,
    "PASS",
    "FAIL"
  ),
  bad_5v5_state
)

bad_5v5_goalies <- five_v_five[
  away_goalie != 1L |
    home_goalie != 1L,
  .N
]

add_check(
  "5v5",
  "Both goalies present at exact 5v5",
  ifelse(
    bad_5v5_goalies == 0L,
    "PASS",
    "FAIL"
  ),
  bad_5v5_goalies
)

bad_5v5_pbp_counts <- five_v_five[
  away_skaters != 5L |
    home_skaters != 5L,
  .N
]

add_check(
  "5v5",
  "PBP says 5 skaters each at 5v5",
  ifelse(
    bad_5v5_pbp_counts == 0L,
    "PASS",
    "FAIL"
  ),
  bad_5v5_pbp_counts
)

standard_3f2d_pct <- safe_pct(
  mean(
    five_v_five$n_reconstructed_skaters == 5L &
      five_v_five$n_forwards == 3L &
      five_v_five$n_defense == 2L,
    na.rm = TRUE
  )
)

add_check(
  "5v5",
  "Exact 5v5 intervals reconstructed as 3F/2D",
  "INFO",
  paste0(
    standard_3f2d_pct,
    "%"
  )
)


# ============================================================
# 10. STRENGTH-STATE LOGIC
# ============================================================

section_header("9. STRENGTH STATE VALIDATION")

cat("\nTeam-interval strength distribution:\n")

strength_summary <- team_interval[
  ,
  .(
    intervals = .N,
    games = uniqueN(game_id)
  ),
  by = strength
][
  order(-intervals)
]

print(strength_summary)

pp_games <- unique(
  team_interval[
    strength == "PP",
    game_id
  ]
)

pk_games <- unique(
  team_interval[
    strength == "PK",
    game_id
  ]
)

add_check(
  "Strength",
  "PP and PK game counts equal",
  ifelse(
    length(pp_games) ==
      length(pk_games),
    "PASS",
    "FAIL"
  ),
  paste(
    length(pp_games),
    "PP /",
    length(pk_games),
    "PK"
  )
)

pp_without_pk <- setdiff(
  pp_games,
  pk_games
)

pk_without_pp <- setdiff(
  pk_games,
  pp_games
)

add_check(
  "Strength",
  "Games with PP but no PK",
  ifelse(
    length(pp_without_pk) == 0L,
    "PASS",
    "FAIL"
  ),
  length(pp_without_pk)
)

add_check(
  "Strength",
  "Games with PK but no PP",
  ifelse(
    length(pk_without_pp) == 0L,
    "PASS",
    "FAIL"
  ),
  length(pk_without_pp)
)

special_pairs <- team_interval[
  strength %chin% c("PP", "PK"),
  .(
    n_PP = sum(strength == "PP"),
    n_PK = sum(strength == "PK"),
    n_teams = uniqueN(team)
  ),
  by = .(
    game_id,
    period,
    interval_start
  )
]

bad_special_pairs <- special_pairs[
  n_teams == 2L &
    (n_PP != 1L | n_PK != 1L)
]

add_check(
  "Strength",
  "PP/PK paired at same interval",
  ifelse(
    nrow(bad_special_pairs) == 0L,
    "PASS",
    "WARN"
  ),
  nrow(bad_special_pairs)
)

bad_empty_net_overlap <- team_interval[
  is_empty_net == 1L &
    (is_PP == 1L | is_PK == 1L),
  .N
]

add_check(
  "Strength",
  "Empty-net not mislabeled PP/PK",
  ifelse(
    bad_empty_net_overlap == 0L,
    "PASS",
    "FAIL"
  ),
  bad_empty_net_overlap
)


# ============================================================
# 11. PBP CACHE VALIDATION
# ============================================================

section_header("10. RAW PBP CACHE VALIDATION")

pbp_files <- list.files(
  PBP_CACHE_DIR,
  pattern = "^[0-9]{10}\\.rds$",
  full.names = TRUE
)

add_check(
  "PBP",
  "PBP cache game files",
  ifelse(
    length(pbp_files) ==
      EXPECTED_SCHEDULE_GAMES,
    "PASS",
    "WARN"
  ),
  paste(
    length(pbp_files),
    "/",
    EXPECTED_SCHEDULE_GAMES
  )
)

required_pbp_cols <- c(
  "game_id",
  "game_seconds",
  "event_type",
  "situationCode",
  "away_goalie",
  "away_skaters",
  "home_skaters",
  "home_goalie",
  "game_state",
  "home_score",
  "away_score",
  "home_team_id",
  "away_team_id"
)

pbp_game_check <- rbindlist(
  lapply(
    pbp_files,
    function(f) {
      
      x <- tryCatch(
        suppressWarnings(
          readRDS(f)
        ),
        error = function(e) NULL
      )
      
      file_gid <- as.integer(
        sub(
          "\\.rds$",
          "",
          basename(f)
        )
      )
      
      if (is.null(x)) {
        
        return(
          data.table(
            game_id = file_gid,
            readable = FALSE,
            schema_valid = FALSE,
            n_events = NA_integer_,
            n_penalty_events = NA_integer_,
            has_penalty = NA,
            has_unequal_strength = NA,
            missing_home_team_id = NA_real_,
            missing_away_team_id = NA_real_
          )
        )
      }
      
      setDT(x)
      
      schema_valid <-
        all(
          required_pbp_cols
          %in% names(x)
        )
      
      if (!schema_valid) {
        
        return(
          data.table(
            game_id = if (
              "game_id" %in% names(x)
            ) {
              x$game_id[1]
            } else {
              file_gid
            },
            
            readable = TRUE,
            schema_valid = FALSE,
            n_events = nrow(x),
            n_penalty_events = NA_integer_,
            has_penalty = NA,
            has_unequal_strength = NA,
            missing_home_team_id = NA_real_,
            missing_away_team_id = NA_real_
          )
        )
      }
      
      penalty_flag <- grepl(
        "PENALTY",
        toupper(x$event_type)
      )
      
      unequal_strength <- (
        !is.na(x$away_skaters) &
          !is.na(x$home_skaters) &
          x$away_skaters != x$home_skaters &
          x$away_goalie == 1L &
          x$home_goalie == 1L
      )
      
      data.table(
        game_id = x$game_id[1],
        readable = TRUE,
        schema_valid = TRUE,
        n_events = nrow(x),
        
        n_penalty_events =
          sum(
            penalty_flag,
            na.rm = TRUE
          ),
        
        has_penalty =
          any(
            penalty_flag,
            na.rm = TRUE
          ),
        
        has_unequal_strength =
          any(
            unequal_strength,
            na.rm = TRUE
          ),
        
        missing_home_team_id =
          mean(
            is.na(
              x$home_team_id
            )
          ),
        
        missing_away_team_id =
          mean(
            is.na(
              x$away_team_id
            )
          )
      )
    }
  ),
  fill = TRUE
)

n_unreadable <- pbp_game_check[
  readable == FALSE,
  .N
]

add_check(
  "PBP",
  "Readable PBP caches",
  ifelse(
    n_unreadable == 0L,
    "PASS",
    "FAIL"
  ),
  paste(
    nrow(pbp_game_check) -
      n_unreadable,
    "/",
    nrow(pbp_game_check)
  )
)

n_bad_schema <- pbp_game_check[
  readable == TRUE &
    schema_valid == FALSE,
  .N
]

add_check(
  "PBP",
  "PBP caches use current schema",
  ifelse(
    n_bad_schema == 0L,
    "PASS",
    "FAIL"
  ),
  n_bad_schema
)


# ============================================================
# 12. HOW MANY GAMES HAVE PENALTIES?
# ============================================================

section_header("11. PENALTY COVERAGE")

penalty_games <- pbp_game_check[
  schema_valid == TRUE &
    has_penalty == TRUE
]

no_penalty_games <- pbp_game_check[
  schema_valid == TRUE &
    has_penalty == FALSE
]

valid_pbp_games <- pbp_game_check[
  schema_valid == TRUE,
  .N
]

cat(
  "\nGames containing at least one penalty event:",
  nrow(penalty_games),
  "\n"
)

cat(
  "Games containing zero penalty events:",
  nrow(no_penalty_games),
  "\n"
)

cat(
  "Percent of valid-PBP games with a penalty:",
  round(
    100 *
      nrow(penalty_games) /
      valid_pbp_games,
    2
  ),
  "%\n"
)

add_check(
  "Penalties",
  "Games with >=1 penalty event",
  "INFO",
  nrow(penalty_games)
)

add_check(
  "Penalties",
  "Games with zero penalty events",
  "INFO",
  nrow(no_penalty_games),
  paste(
    no_penalty_games$game_id,
    collapse = ", "
  )
)

cat("\nDistribution of penalty-event counts per game:\n")

print(
  summary(
    penalty_games$n_penalty_events
  )
)


# ============================================================
# 13. PENALTIES VS SPECIAL-TEAMS STATES
# ============================================================

section_header("12. PENALTIES VS PP/PK COVERAGE")

special_team_games <- unique(
  team_interval[
    strength %chin% c(
      "PP",
      "PK"
    ),
    game_id
  ]
)

pbp_game_check[
  ,
  has_final_special_teams :=
    game_id %in%
    special_team_games
]

cat(
  "\nRaw penalty event vs final PP/PK state:\n"
)

print(
  table(
    penalty_event =
      pbp_game_check$has_penalty,
    
    final_PP_PK =
      pbp_game_check$
      has_final_special_teams,
    
    useNA = "ifany"
  )
)

penalty_no_special <- pbp_game_check[
  has_penalty == TRUE &
    has_final_special_teams == FALSE
]

add_check(
  "Penalties",
  "Penalty games with no PP/PK",
  ifelse(
    nrow(penalty_no_special) <= 5L,
    "PASS",
    "WARN"
  ),
  nrow(penalty_no_special),
  paste(
    penalty_no_special$game_id,
    collapse = ", "
  )
)

unequal_no_special <- pbp_game_check[
  has_unequal_strength == TRUE &
    has_final_special_teams == FALSE
]

add_check(
  "Penalties",
  "Unequal-strength games with no PP/PK",
  ifelse(
    nrow(unequal_no_special) == 0L,
    "PASS",
    "FAIL"
  ),
  nrow(unequal_no_special),
  paste(
    unequal_no_special$game_id,
    collapse = ", "
  )
)


# ============================================================
# 14. TEAM ID COVERAGE IN PBP
# ============================================================

section_header("13. PBP TEAM-ID COVERAGE")

mean_missing_home_id <- safe_mean(
  pbp_game_check$
    missing_home_team_id
)

mean_missing_away_id <- safe_mean(
  pbp_game_check$
    missing_away_team_id
)

add_check(
  "PBP",
  "Missing home team IDs",
  ifelse(
    mean_missing_home_id == 0,
    "PASS",
    "WARN"
  ),
  paste0(
    round(
      100 *
        mean_missing_home_id,
      4
    ),
    "%"
  )
)

add_check(
  "PBP",
  "Missing away team IDs",
  ifelse(
    mean_missing_away_id == 0,
    "PASS",
    "WARN"
  ),
  paste0(
    round(
      100 *
        mean_missing_away_id,
      4
    ),
    "%"
  )
)


# ============================================================
# 15. xGF / xGA DUPLICATION CHECK
# ============================================================

section_header("14. xG JOIN VALIDATION")

xgf_consistency <- final[
  ,
  .(
    n_distinct_xGF =
      uniqueN(xGF)
  ),
  by = .(
    game_id,
    team,
    interval_start
  )
]

bad_xgf_groups <- xgf_consistency[
  n_distinct_xGF != 1L
]

add_check(
  "xG",
  "Same-team interval has one xGF",
  ifelse(
    nrow(bad_xgf_groups) == 0L,
    "PASS",
    "FAIL"
  ),
  nrow(bad_xgf_groups)
)

xga_consistency <- final[
  ,
  .(
    n_distinct_xGA =
      uniqueN(xGA)
  ),
  by = .(
    game_id,
    team,
    interval_start
  )
]

bad_xga_groups <- xga_consistency[
  n_distinct_xGA != 1L
]

add_check(
  "xG",
  "Same-team interval has one xGA",
  ifelse(
    nrow(bad_xga_groups) == 0L,
    "PASS",
    "FAIL"
  ),
  nrow(bad_xga_groups)
)


# ============================================================
# 16. xGF OF ONE TEAM SHOULD MATCH OPPONENT xGA
# ============================================================

team_xg_pair <- team_interval[
  ,
  .(
    game_id,
    team,
    opp_team,
    interval_start,
    xGF,
    xGA
  )
]

opp_xg <- copy(team_xg_pair)

setnames(
  opp_xg,
  c(
    "team",
    "opp_team",
    "xGF",
    "xGA"
  ),
  c(
    "opp_team",
    "team",
    "opp_xGF",
    "opp_xGA"
  )
)

xg_pair_check <- merge(
  team_xg_pair,
  opp_xg,
  by = c(
    "game_id",
    "team",
    "opp_team",
    "interval_start"
  ),
  all = FALSE
)

xg_pair_check[
  ,
  gf_vs_opp_ga_diff :=
    abs(
      xGF -
        opp_xGA
    )
]

xg_pair_check[
  ,
  ga_vs_opp_gf_diff :=
    abs(
      xGA -
        opp_xGF
    )
]

bad_xg_symmetry <- xg_pair_check[
  gf_vs_opp_ga_diff > 1e-12 |
    ga_vs_opp_gf_diff > 1e-12
]

add_check(
  "xG",
  "Team xGF matches opponent xGA",
  ifelse(
    nrow(bad_xg_symmetry) == 0L,
    "PASS",
    "FAIL"
  ),
  nrow(bad_xg_symmetry)
)


# ============================================================
# 17. TEAM-GAME xG TOTALS
# ============================================================

team_game_xg <- team_interval[
  ,
  .(
    total_xGF =
      sum(
        xGF,
        na.rm = TRUE
      ),
    
    total_xGA =
      sum(
        xGA,
        na.rm = TRUE
      )
  ),
  by = .(
    game_id,
    team
  )
]

cat("\nTeam-game xGF distribution:\n")

print(
  summary(
    team_game_xg$total_xGF
  )
)

suspect_xg_games <- team_game_xg[
  total_xGF < 0.3 |
    total_xGF > 7
]

add_check(
  "xG",
  "Team-games outside broad xGF range",
  ifelse(
    nrow(suspect_xg_games) == 0L,
    "PASS",
    "WARN"
  ),
  nrow(suspect_xg_games)
)

if (nrow(suspect_xg_games) > 0L) {
  
  print(
    suspect_xg_games[
      order(-total_xGF)
    ]
  )
}


# ============================================================
# 18. xG BY STRENGTH
# ============================================================

section_header("15. xG BY STRENGTH")

strength_xg <- team_interval[
  ,
  .(
    mean_xGF =
      mean(
        xGF,
        na.rm = TRUE
      ),
    
    mean_xGA =
      mean(
        xGA,
        na.rm = TRUE
      ),
    
    mean_margin =
      mean(
        xGF - xGA,
        na.rm = TRUE
      ),
    
    intervals = .N,
    
    games =
      uniqueN(game_id)
  ),
  by = strength
][
  order(-intervals)
]

print(strength_xg)

pp_xgf <- strength_xg[
  strength == "PP",
  mean_xGF
]

ev_xgf <- strength_xg[
  strength == "EV",
  mean_xGF
]

pk_xgf <- strength_xg[
  strength == "PK",
  mean_xGF
]

if (
  length(pp_xgf) == 1L &&
  length(ev_xgf) == 1L &&
  length(pk_xgf) == 1L
) {
  
  ordered_strength_xgf <-
    pp_xgf >
    ev_xgf &&
    ev_xgf >
    pk_xgf
  
  add_check(
    "xG",
    "Expected PP > EV > PK xGF ordering",
    ifelse(
      ordered_strength_xgf,
      "PASS",
      "WARN"
    ),
    paste(
      "PP =",
      round(pp_xgf, 6),
      "| EV =",
      round(ev_xgf, 6),
      "| PK =",
      round(pk_xgf, 6)
    )
  )
}


# ============================================================
# 19. EMPTY-NET SANITY
# ============================================================

section_header("16. EMPTY-NET VALIDATION")

empty_net <- team_interval[
  is_empty_net == 1L
]

if (nrow(empty_net) > 0L) {
  
  empty_net_bad_goalie_state <-
    empty_net[
      away_goalie == 1L &
        home_goalie == 1L,
      .N
    ]
  
  add_check(
    "Empty net",
    "Empty-net state has a pulled goalie",
    ifelse(
      empty_net_bad_goalie_state == 0L,
      "PASS",
      "FAIL"
    ),
    empty_net_bad_goalie_state
  )
}


# ============================================================
# 20. ICING DATASET VALIDATION
# ============================================================

section_header("17. ICING DATA VALIDATION")

if (!is.null(icing_all)) {
  
  add_check(
    "Icing",
    "Defensive-zone faceoff events",
    "INFO",
    nrow(icing_all)
  )
  
  if ("is_icing" %in% names(icing_all)) {
    
    add_check(
      "Icing",
      "Icing faceoffs",
      "INFO",
      icing_all[
        is_icing == TRUE,
        .N
      ]
    )
  }
  
  if (
    all(
      c(
        "is_icing",
        "is_5v5"
      ) %in%
      names(icing_all)
    )
  ) {
    
    add_check(
      "Icing",
      "Exact 5v5 icing faceoffs",
      "INFO",
      icing_all[
        is_icing == TRUE &
          is_5v5 == TRUE,
        .N
      ]
    )
  }
}

if (!is.null(icing_model)) {
  
  model_bad_skaters <- if (
    "n_skaters" %in%
    names(icing_model)
  ) {
    icing_model[
      n_skaters != 5L,
      .N
    ]
  } else {
    NA_integer_
  }
  
  add_check(
    "Icing",
    "Model rows have five skaters",
    ifelse(
      is.na(model_bad_skaters),
      "WARN",
      ifelse(
        model_bad_skaters == 0L,
        "PASS",
        "FAIL"
      )
    ),
    model_bad_skaters
  )
  
  model_bad_exposure <- if (
    "exposure_sec" %in%
    names(icing_model)
  ) {
    icing_model[
      exposure_sec <= 0,
      .N
    ]
  } else {
    NA_integer_
  }
  
  add_check(
    "Icing",
    "Model exposure > 0",
    ifelse(
      is.na(model_bad_exposure),
      "WARN",
      ifelse(
        model_bad_exposure == 0L,
        "PASS",
        "FAIL"
      )
    ),
    model_bad_exposure
  )
  
  if (
    all(
      c(
        "standard_3F2D_unit",
        "n_skaters"
      ) %in%
      names(icing_model)
    )
  ) {
    
    cat(
      "\nIcing model 3F/2D unit rate:",
      round(
        100 *
          mean(
            icing_model$
              standard_3F2D_unit,
            na.rm = TRUE
          ),
        2
      ),
      "%\n"
    )
  }
}


# ============================================================
# 21. ICING PLAYER MATCHING
# ============================================================

if (!is.null(icing_players)) {
  
  duplicate_icing_players <-
    icing_players[
      ,
      .N,
      by = .(
        event_id,
        player_id
      )
    ][
      N > 1L
    ]
  
  add_check(
    "Icing",
    "No duplicate player within icing event",
    ifelse(
      nrow(
        duplicate_icing_players
      ) == 0L,
      "PASS",
      "FAIL"
    ),
    nrow(
      duplicate_icing_players
    )
  )
}


# ============================================================
# 22. STOPPAGE / ICING FLAGS IN FINAL
# ============================================================

section_header("18. STOPPAGE AND ICING FLAGS")

if (
  "has_stoppage" %in%
  names(final)
) {
  
  cat(
    "\nPercent player intervals on shifts containing a faceoff:",
    round(
      100 *
        mean(
          final$has_stoppage,
          na.rm = TRUE
        ),
      2
    ),
    "%\n"
  )
}

if (
  "has_icing" %in%
  names(final)
) {
  
  cat(
    "\nIcing-containing shift rows by strength:\n"
  )
  
  print(
    final[
      ,
      .(
        pct_has_icing =
          round(
            100 *
              mean(
                has_icing,
                na.rm = TRUE
              ),
            3
          )
      ),
      by = strength
    ]
  )
}


# ============================================================
# 23. PRIMARY 5v5 SHIFT-LENGTH ANALYSIS SAMPLE
# ============================================================

section_header("19. PRIMARY SHIFT-LENGTH ANALYSIS SAMPLE")

analysis_5v5 <- team_interval[
  is_5v5 == 1L &
    n_reconstructed_skaters == 5L &
    n_forwards == 3L &
    n_defense == 2L
]

add_check(
  "Analysis",
  "Valid exact-5v5 team intervals",
  "INFO",
  format(
    nrow(analysis_5v5),
    big.mark = ","
  )
)

add_check(
  "Analysis",
  "Games contributing valid exact 5v5",
  "INFO",
  uniqueN(
    analysis_5v5$game_id
  )
)

cat(
  "\nAverage unit shift-age distribution:\n"
)

print(
  summary(
    analysis_5v5$
      mean_shift_age_sec
  )
)


# ============================================================
# 24. LONG-SHIFT SAMPLE SIZES
# ============================================================

analysis_5v5[
  ,
  shift_age_bin :=
    5L *
    floor(
      mean_shift_age_sec / 5L
    )
]

cat(
  "\nSample size by 5-second unit shift-age bin:\n"
)

print(
  analysis_5v5[
    ,
    .(
      intervals = .N,
      games =
        uniqueN(game_id)
    ),
    by = shift_age_bin
  ][
    order(shift_age_bin)
  ]
)

analysis_5v5[
  ,
  shift_length_group :=
    cut(
      mean_shift_age_sec,
      
      breaks = c(
        -Inf,
        30,
        45,
        60,
        Inf
      ),
      
      labels = c(
        "0-30",
        "30-45",
        "45-60",
        "60+"
      ),
      
      right = FALSE
    )
]

cat(
  "\nPrimary manuscript shift-length groups:\n"
)

print(
  analysis_5v5[
    ,
    .(
      mean_xGF =
        mean(
          xGF,
          na.rm = TRUE
        ),
      
      mean_xGA =
        mean(
          xGA,
          na.rm = TRUE
        ),
      
      mean_margin =
        mean(
          xGF - xGA,
          na.rm = TRUE
        ),
      
      intervals = .N,
      
      games =
        uniqueN(game_id)
    ),
    by = shift_length_group
  ]
)


# ============================================================
# 25. FINAL VALIDATION SUMMARY
# ============================================================

section_header("20. FINAL VALIDATION SUMMARY")

print(
  validation_results
)

cat(
  "\nStatus counts:\n"
)

print(
  validation_results[
    ,
    .N,
    by = status
  ][
    order(
      factor(
        status,
        levels = c(
          "FAIL",
          "WARN",
          "PASS",
          "INFO"
        )
      )
    )
  ]
)

n_fail <- validation_results[
  status == "FAIL",
  .N
]

n_warn <- validation_results[
  status == "WARN",
  .N
]

cat(
  "\nFAIL checks:",
  n_fail,
  "\n"
)

cat(
  "WARN checks:",
  n_warn,
  "\n"
)

if (n_fail == 0L) {
  
  cat(
    "\nNo critical validation failures detected.\n"
  )
  
} else {
  
  cat(
    "\nIMPORTANT:",
    "Resolve FAIL checks before final analysis.\n"
  )
}

fwrite(
  validation_results,
  "validation_summary.csv"
)

cat(
  "\nSaved validation_summary.csv\n"
)
