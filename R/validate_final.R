# ============================================================
# Pre-analysis diagnostics for `final`
# Run this before starting any real modeling/analysis.
# ============================================================

library(data.table)

# ------------------------------------------------------------
# 1. Name collisions -- find every case where one displayed name
#    maps to more than one player_id. Some of these will be real
#    namesakes (like the two Elias Perssons on VAN) and not bugs --
#    but you need to know they exist before grouping by name.
# ------------------------------------------------------------
name_collisions <- final[, .(n_ids = uniqueN(player_id)), by = player][n_ids > 1]
setorder(name_collisions, -n_ids)
print(name_collisions)

# for each colliding name, show the actual ids + how much ice time
# each one has, so you can tell them apart
if (nrow(name_collisions) > 0) {
  collision_detail <- final[
    player %in% name_collisions$player,
    .(minutes = .N * 5 / 60, games = uniqueN(game_id), team = first(team)),
    by = .(player, player_id)
  ]
  setorder(collision_detail, player, -minutes)
  print(collision_detail)
}

# ------------------------------------------------------------
# 2. Build a disambiguated display name for anything downstream --
#    use this instead of `player` for any by-name grouping/plots so
#    namesakes never silently combine again.
# ------------------------------------------------------------
final[, player_display := paste0(player, " (", player_id, ")")]

# ------------------------------------------------------------
# 3. TOI/game spot-checks across different player roles.
#    Swap in player_id (not name) once you've confirmed IDs from
#    step 1, to be 100% sure which specific player you're checking.
# ------------------------------------------------------------
check_toi <- function(pid_or_name, by_id = FALSE) {
  if (by_id) {
    final[player_id == pid_or_name, .(
      player = first(player), games = uniqueN(game_id),
      total_minutes = .N * 5 / 60,
      avg_min_per_game = round(.N * 5 / 60 / uniqueN(game_id), 1)
    )]
  } else {
    final[player == pid_or_name, .(
      games = uniqueN(game_id), total_minutes = .N * 5 / 60,
      avg_min_per_game = round(.N * 5 / 60 / uniqueN(game_id), 1)
    ), by = player_id]
  }
}

# elite forward -- expect ~21-24 min/game
check_toi("Connor McDavid")

# elite two-way defenseman -- expect ~24-27 min/game
check_toi("Cale Makar")
check_toi("Jake Evans")
check_toi("Nazem Kadri")

# a depth/4th-line forward -- expect ~9-13 min/game
# (swap in any name you recognize as a depth player)
# check_toi("Some Depth Player")

# ------------------------------------------------------------
# 4. League-wide TOI distribution by position -- catch anyone still
#    implausible without having to know every player individually.
# ------------------------------------------------------------
league_toi <- final[, .(
  games = uniqueN(game_id),
  total_minutes = .N * 5 / 60,
  fwd_def = first(fwd_def)
), by = .(player, player_id)]

league_toi[, avg_min_per_game := total_minutes / games]

league_toi[, .(
  min = round(min(avg_min_per_game), 1),
  p25 = round(quantile(avg_min_per_game, .25), 1),
  median = round(median(avg_min_per_game), 1),
  p75 = round(quantile(avg_min_per_game, .75), 1),
  max = round(max(avg_min_per_game), 1)
), by = fwd_def]

# flag anyone still over a realistic ceiling (defensemen can run
# higher than forwards, so use a generous but not absurd cutoff)
implausible <- league_toi[avg_min_per_game > 28]
setorder(implausible, -avg_min_per_game)
print(implausible)

# ------------------------------------------------------------
# 5. Team-level TOI reconciliation -- the strongest structural
#    check available. At any moment, exactly 5 skaters (6 on a
#    delayed penalty) are on ice per team, minus empty-net/penalty
#    edge cases. Summed across a full game, one team's total skater
#    minutes should land close to 5 x regulation length (~300 min
#    for a 60-minute regulation game, a bit more with PP situations
#    roughly netting out, more again if the game went to OT).
#    A team that's WAY off this in a given game points to a data
#    problem in that specific game.
# ------------------------------------------------------------
team_game_toi <- final[, .(total_skater_minutes = .N * 5 / 60),
                       by = .(game_id, team)]

summary(team_game_toi$total_skater_minutes)

# flag games where a team's total is way outside a sane band
# (~280-340 covers regulation with normal PP/PK fluctuation; wider
# for OT games, so treat anything below 250 or above 400 as worth
# a manual look)
suspect_games <- team_game_toi[total_skater_minutes < 250 | total_skater_minutes > 400]
setorder(suspect_games, -total_skater_minutes)
print(suspect_games)

# ------------------------------------------------------------
# 6. Final overall sanity numbers before you move to analysis
# ------------------------------------------------------------
cat("\n--- SUMMARY ---\n")
cat("Total rows:", nrow(final), "\n")
cat("Unique players (by id):", uniqueN(final$player_id), "\n")
cat("Names with id collisions:", nrow(name_collisions), "\n")
cat("Player-seasons over 28 min/game:", nrow(implausible), "\n")
cat("Team-games outside sane TOI band:", nrow(suspect_games), "\n")
cat("Strength distribution:\n")
print(final[, .N, by = strength][, pct := round(100 * N / sum(N), 1)][])

# ============================================================
# xG diagnostics for `final` -- run before analysis.
#
# Key thing to keep in mind: xGF/xGA are ON-ICE metrics. Every
# skater on a team, in the same game+interval, shares the SAME
# xGF value (it's not "their" individual shot contribution, it's
# "how much xG did my team generate while I was on the ice"). This
# means summing xGF across all rows in `final` massively overcounts
# actual team totals (each shot gets counted once per teammate on
# ice, typically ~5x). The checks below account for that.
# ============================================================


# ------------------------------------------------------------
# A. Verify xGF is genuinely shared across teammates on ice.
#    Every player on the SAME team, in the SAME game+interval,
#    should show the IDENTICAL xGF value. If this isn't true, the
#    join logic is broken somewhere.
# ------------------------------------------------------------
xgf_consistency <- final[, .(n_distinct_xGF = uniqueN(xGF)),
                         by = .(game_id, team, interval_start)]
table(xgf_consistency$n_distinct_xGF)   # should be ~100% equal to 1

xga_consistency <- final[, .(n_distinct_xGA = uniqueN(xGA)),
                         by = .(game_id, opp_team, interval_start)]
table(xga_consistency$n_distinct_xGA)   # should also be ~100% equal to 1

# ------------------------------------------------------------
# B. True team-game xG totals, de-duplicated down to one row per
#    team/game/interval (undoing the on-ice duplication) before
#    summing -- THIS is the number to compare against reality.
# ------------------------------------------------------------
team_game_xg <- unique(final[, .(game_id, team, interval_start, xGF)])
team_game_totals <- team_game_xg[, .(total_xGF = sum(xGF)), by = .(game_id, team)]

summary(team_game_totals$total_xGF)
# realistic NHL range per team per game: roughly 1.0 - 5.5,
# occasionally higher in a wild game

# flag anything outside a sane range for manual review
suspect_xg_games <- team_game_totals[total_xGF < 0.3 | total_xGF > 7]
setorder(suspect_xg_games, -total_xGF)
print(suspect_xg_games)

# ------------------------------------------------------------
# C. Spot-check one specific game against a site you can verify
#    manually (Natural Stat Trick, MoneyPuck's own game page, etc.)
# ------------------------------------------------------------
sample_game <- team_game_totals$game_id[1]
team_game_totals[game_id == sample_game]

# ------------------------------------------------------------
# D. League-wide season xG totals per team -- real teams land
#    roughly 220-290 total xG across an 82-game season. Way outside
#    that (in either direction) suggests something's still off.
# ------------------------------------------------------------
season_team_xg <- team_game_totals[, .(
  season_total_xGF = sum(total_xGF), games = uniqueN(game_id)
), by = team]
season_team_xg[, xg_per_game := round(season_total_xGF / games, 2)]
setorder(season_team_xg, -season_total_xGF)
print(season_team_xg)

# ------------------------------------------------------------
# E. xG should scale sensibly with strength state -- average xGF
#    per interval should be clearly higher during PP than EV, and
#    EV clearly higher than PK (teams generate much less offense
#    shorthanded).
# ------------------------------------------------------------
final[, .(avg_xGF_per_interval = round(mean(xGF), 4)), by = strength]
# expect roughly: PP highest, EV middle, PK lowest

# ------------------------------------------------------------
# F. Check for implausible single-interval spikes. A single 5-second
#    window can have more than one shot, so values above 1 aren't
#    automatically wrong, but very large ones are worth a look.
# ------------------------------------------------------------
max(final$xGF)
final[xGF > 1.5][order(-xGF)][1:10]

# ------------------------------------------------------------
# G. Cross-check xG against ACTUAL goals at the team-game level.
#    These should NOT match exactly (that's the entire point of an
#    xG model), but across many games they should correlate
#    positively and meaningfully -- a team that generates more xG
#    should generally score more goals, on average.
# ------------------------------------------------------------
if ("event" %in% names(shots)) {
  actual_goals <- shots[event == "GOAL", .(actual_goals = .N), by = .(game_id, teamCode)]
  
  goals_vs_xg <- merge(
    team_game_totals, actual_goals,
    by.x = c("game_id", "team"), by.y = c("game_id", "teamCode"),
    all.x = TRUE
  )
  goals_vs_xg[is.na(actual_goals), actual_goals := 0]
  
  cat("\nCorrelation between team-game xGF and actual goals:",
      round(cor(goals_vs_xg$total_xGF, goals_vs_xg$actual_goals), 3), "\n")
  # expect something positive and meaningful, roughly 0.3-0.6 --
  # NOT close to 0 (would suggest a broken join) and NOT close to 1
  # (xG isn't supposed to predict goals that precisely game-to-game)
} else {
  cat("\n'event' column not found in shots -- skipping goals-vs-xG check.\n")
}

# ------------------------------------------------------------
# H. Player-level xG rate sanity (per 60 minutes), using the
#    disambiguated player_display so namesakes don't get combined.
# ------------------------------------------------------------
if (!"player_display" %in% names(final)) {
  final[, player_display := paste0(player, " (", player_id, ")")]
}

player_xg_rate <- final[, .(
  minutes = .N * 5 / 60,
  xGF = sum(xGF),
  xGA = sum(xGA)
), by = .(player_display, fwd_def)]

player_xg_rate[, xGF_per_60 := round(xGF / minutes * 60, 2)]
player_xg_rate[, xGA_per_60 := round(xGA / minutes * 60, 2)]

# realistic range: most players land 1.5-4 xGF/60, elite offensive
# players can push 4-6; anything way outside that is worth a look
setorder(player_xg_rate, -xGF_per_60)
head(player_xg_rate[minutes > 200], 15)   # min minutes filter avoids small-sample noise

cat("\n--- xG DIAGNOSTIC SUMMARY ---\n")
cat("Team-games with suspect xG totals:", nrow(suspect_xg_games), "\n")
cat("Max single-interval xGF:", round(max(final$xGF), 3), "\n")

# should be a meaningful minority, not near-0% or near-100%
mean(final$has_stoppage)

# spot check: does this line up with real hockey? PP/PK shifts should
# have a MUCH lower stoppage rate than EV, since PK units especially
# get changed on whistles when possible, whereas EV shifts often run
# through live play
final[, .(pct_stoppage = round(100 * mean(has_stoppage), 1)), by = strength]


final[, uniqueN(game_id)]  # should be 1393# does "ICING" exist as its own event type at all?
unique(pbp$event_type)

# if not, "stoppage" is the likely bucket -- check what raw fields
# are available on those rows that might carry a reason
pbp[toupper(event_type) == "STOPPAGE"][1:3]

# look specifically for any column name that might hold a reason
grep("reason", names(pbp), ignore.case = TRUE, value = TRUE)
final[, .N, by = has_icing]  # rough sense of how much data you have to work with
final[, .(pct_icing = round(100 * mean(has_icing), 2)), by = strength]  # icing should be ~0% on PP (rare to ice while up a man)

# team IDs should no longer be all-NA
mean(is.na(pbp$home_team_id))
mean(is.na(pbp$away_team_id))

final[, uniqueN(game_id)]                    # should be 1393
nrow(final)                                   # sanity check row count
"has_stoppage" %in% names(final)              # should be TRUE, cleanly
"has_icing" %in% names(final)                 # should be TRUE, cleanly
final[, .N, by = has_icing]
final[, .(pct_icing = round(100 * mean(has_icing), 2)), by = strength]