options(dplyr.summarise.inform = FALSE)
library(tcltk)
library(dplyr)
library(scorekeepeR)

# Data --------------------------------------------------------------------

data_dir = "data"
if (!dir.exists(data_dir)) dir.create(data_dir)
if (!dir.exists(file.path(data_dir, "gamelogs"))) dir.create(file.path(data_dir, "gamelogs"))

read_table <- function(file, init_fn, col_classes = NA){
  path = file.path(data_dir, file)
  if (file.exists(path)) read.csv(path, colClasses = col_classes) else init_fn()
}

# the saved environment holds what is on disk; the rv environment holds what is in memory
# comparing the two tells us whether there are unsaved changes
saved = new.env()
saved$teams = read_table("Teams.csv", init_teams_table, "character")
saved$players = read_table("Players.csv", init_players_table, "character")
saved$rosters = read_table("Rosters.csv", init_rosters_table, "character")
saved$games = read_table("Games.csv", init_games_table,
                         c(TeamID = "character", GameID = "character",
                           Date = "character", Opponent = "character",
                           TeamScore = "integer", OpponentScore = "integer"))
saved$game_stats = read_table("GameStats.csv", init_game_stats_table,
                              c(PlayerID = "character", GameID = "character"))

rv = new.env()
rv$teams = saved$teams
rv$players = saved$players
rv$rosters = saved$rosters
rv$roster = NULL          # roster view of team selected in teams table
rv$sk_roster = NULL       # roster set for scoring a game
rv$team_id = NULL
rv$game_id = NULL
rv$game_log = NULL
rv$game_dirty = FALSE     # TRUE when the game in progress has unsaved changes
rv$games = saved$games
rv$game_stats = saved$game_stats

# Helpers -----------------------------------------------------------------

# compare data frames ignoring row names (which drift with adding/deleting rows)
same_df <- function(x, y){
  rownames(x) = NULL
  rownames(y) = NULL
  isTRUE(all.equal(x, y))
}

# label used for missing values in filters
blank_lab = "(blank)"

fill_blank <- function(x){
  x = as.character(x)
  ifelse(is.na(x) | grepl("^\\s*$", x), blank_lab, x)
}

fmt_pct <- function(x) if (is.na(x)) "--" else paste0(x, "%")

# Styling -----------------------------------------------------------------

col_bg = "#ffffff"
col_border = "#dee2e6"
col_muted = "#6c757d"
col_text = "#1d1f21"

# create_player_name() errors on zero-length input
player_name <- function(first_name, last_name){
  if (length(first_name) == 0) return(character())
  create_player_name(first_name, last_name)
}
