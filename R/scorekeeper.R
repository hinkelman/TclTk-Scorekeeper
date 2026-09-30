# Scorekeeper tab: tally game stats for players on the set roster.

sk = new.env()

sk$date = tclVar(as.character(Sys.Date()))
sk$opponent = tclVar("")
sk$team_score = tclVar("")
sk$opp_score = tclVar("")
sk$player = tclVar("")   # PlayerID of selected player
sk$undo = tclVar(0)
sk$last_log = tclVar("")

# Scorekeeper buttons: label, stats to update, and the event recorded in the log
# (first stat is the logged event)
sk_buttons = list(
  miss_1 = list(label = "Miss", stats = "FTA"),
  make_1 = list(label = "Make", stats = c("FTM", "FTA")),
  miss_2 = list(label = "Miss", stats = "FGA2"),
  make_2 = list(label = "Make", stats = c("FGM2", "FGA2")),
  miss_3 = list(label = "Miss", stats = "FGA3"),
  make_3 = list(label = "Make", stats = c("FGM3", "FGA3")),
  tov = list(label = "Turnover", stats = "TOV"),
  stl = list(label = "Steal", stats = "STL"),
  dreb = list(label = "Def. Rebound", stats = "DREB"),
  oreb = list(label = "Off. Rebound", stats = "OREB"),
  blk = list(label = "Block", stats = "BLK"),
  ast = list(label = "Assist", stats = "AST"),
  pf = list(label = "Foul", stats = "PF")
)

value_boxes = c(pts = "Points", ft = "Free Throws", fg = "Field Goals",
                three = "3PT Field Goals", ts = "True Shooting", eff = "Efficiency",
                reb = "Rebounds", blk = "Blocks", stl = "Steals",
                ast = "Assists", tov = "Turnovers", pf = "Fouls")

build_scorekeeper_tab <- function(parent){
  tkgrid.columnconfigure(parent, 3, weight = 1)
  tkgrid.rowconfigure(parent, 0, weight = 1)

  # Game info ----
  info = ttklabelframe(parent, text = "Game", padding = 8)
  tkgrid(info, row = 0, column = 0, sticky = "nsw", padx = c(0, 10))
  tkgrid(ttklabel(info, text = "Date (YYYY-MM-DD)"), columnspan = 2, sticky = "w")
  sk$date_entry = ttkentry(info, textvariable = sk$date, width = 20)
  tkgrid(sk$date_entry, columnspan = 2, sticky = "ew", pady = c(0, 8))
  tkgrid(ttklabel(info, text = "Opponent"), columnspan = 2, sticky = "w")
  sk$opp_entry = ttkentry(info, textvariable = sk$opponent, width = 20)
  tkgrid(sk$opp_entry, columnspan = 2, sticky = "ew", pady = c(0, 8))
  tkgrid(ttklabel(info, text = "Final Score"), columnspan = 2, pady = c(8, 2))
  tkgrid(ttklabel(info, text = "Team", foreground = col_muted),
         ttklabel(info, text = "Opp", foreground = col_muted), sticky = "w")
  sk$team_score_entry = ttkentry(info, textvariable = sk$team_score, width = 8)
  sk$opp_score_entry = ttkentry(info, textvariable = sk$opp_score, width = 8)
  tkgrid(sk$team_score_entry, sk$opp_score_entry, sticky = "ew", padx = c(0, 4))
  for (ent in list(sk$date_entry, sk$opp_entry, sk$team_score_entry, sk$opp_score_entry))
    tkbind(ent, "<KeyRelease>", on_game_info_change)
  sk$save_btn = ttkbutton(info, text = "Save game stats", style = "Accent.TButton",
                          command = on_save_game)
  tkgrid(sk$save_btn, columnspan = 2, pady = c(20, 0))

  # Players ----
  players = ttkframe(parent, padding = c(0, 4))
  tkgrid(players, row = 0, column = 1, sticky = "nsw", padx = c(0, 10))
  tkgrid(ttklabel(players, text = "Select Player", font = "SkTitleFont"), sticky = "w")
  sk$players_frame = ttkframe(players)
  tkgrid(sk$players_frame, sticky = "nw")
  sk$no_roster_lbl = ttklabel(sk$players_frame, foreground = col_muted, wraplength = 160,
                              text = "Need to set roster before scoring a game")
  tkgrid(sk$no_roster_lbl, sticky = "w")
  tkgrid(ttklabel(players, text = "Did Not Play (DNP)", font = "SkTitleFont"),
         sticky = "w", pady = c(16, 2))
  sk$dnp_lb = make_listbox(players, height = 6, width = 20, on_select = on_dnp_change)
  tkgrid(sk$dnp_lb$frame, sticky = "ew")

  # Buttons ----
  btns = ttkframe(parent, padding = c(0, 4))
  tkgrid(btns, row = 0, column = 2, sticky = "n", padx = c(0, 14))
  for (i in 0:3) tkgrid.columnconfigure(btns, i, weight = 1, uniform = "btn")
  sk$btns = list()
  mk_btn <- function(id){
    sk$btns[[id]] = ttkbutton(btns, text = sk_buttons[[id]]$label, style = "Score.TButton",
                              command = function() record_event(id))
    sk$btns[[id]]
  }
  shot_row <- function(r, miss, make, label){
    tkgrid(mk_btn(miss), row = r, column = 0, sticky = "ew", padx = 3, pady = 3)
    tkgrid(ttklabel(btns, text = label, anchor = "center"), row = r, column = 1,
           columnspan = 2, sticky = "ew")
    tkgrid(mk_btn(make), row = r, column = 3, sticky = "ew", padx = 3, pady = 3)
  }
  pair_row <- function(r, a, b){
    tkgrid(mk_btn(a), row = r, column = 0, columnspan = 2, sticky = "ew", padx = 3, pady = 3)
    if (!is.null(b))
      tkgrid(mk_btn(b), row = r, column = 2, columnspan = 2, sticky = "ew", padx = 3, pady = 3)
  }
  shot_row(0, "miss_1", "make_1", "Free Throw")
  shot_row(1, "miss_2", "make_2", "Field Goal")
  shot_row(2, "miss_3", "make_3", "Three Point")
  pair_row(3, "tov", "stl")
  pair_row(4, "dreb", "oreb")
  pair_row(5, "blk", "ast")
  pair_row(6, "pf", NULL)
  sk$undo_chk = ttkcheckbutton(btns, text = "UNDO", variable = sk$undo,
                               style = "Undo.TCheckbutton")
  tkgrid(sk$undo_chk, row = 6, column = 2, columnspan = 2, pady = 3)
  tkgrid(ttklabel(btns, text = "Last game log entry:", font = "SkTitleFont"),
         row = 7, column = 0, columnspan = 4, pady = c(24, 2))
  tkgrid(ttklabel(btns, textvariable = sk$last_log), row = 8, column = 0, columnspan = 4)

  # Value boxes ----
  boxes = ttkframe(parent, padding = c(0, 4))
  tkgrid(boxes, row = 0, column = 3, sticky = "nsew")
  sk$boxes = list()
  for (i in seq_along(value_boxes)){
    id = names(value_boxes)[i]
    sk$boxes[[id]] = make_value_box(boxes, value_boxes[[id]])
    tkgrid(sk$boxes[[id]]$frame, row = (i - 1) %/% 3, column = (i - 1) %% 3,
           sticky = "nsew", padx = 5, pady = 5)
  }
  for (i in 0:2) tkgrid.columnconfigure(boxes, i, weight = 1, uniform = "vb")

  # keyboard shortcut to toggle undo
  tkbind(base, "<Control-z>", function() tclvalue(sk$undo) = 1 - as.integer(tclvalue(sk$undo)))

  refresh_scorekeeper()
}

# Game setup ---------------------------------------------------------------

start_game <- function(roster){
  roster$NameNum = create_player_namenum(roster$FirstName, roster$LastName, roster$Number)
  roster$NameNum = ifelse(is.na(roster$NameNum), "(unnamed)", roster$NameNum)
  rv$sk_roster = roster
  rv$team_id = roster$TeamID[1]  # same TeamID for all rows in roster
  rv$game_id = ids::random_id()
  rv$game_log = NULL
  rv$game_dirty = FALSE
  rv$games = saved$games
  rv$game_stats = add_game_stats(saved$game_stats, roster$PlayerID, rv$game_id)

  tclvalue(sk$opponent) = ""
  tclvalue(sk$team_score) = ""
  tclvalue(sk$opp_score) = ""
  tclvalue(sk$undo) = 0

  # rebuild player radio buttons
  for (w in as.character(tkwinfo("children", sk$players_frame))) tcl("destroy", w)
  for (i in seq_len(nrow(roster))){
    tkgrid(ttkradiobutton(sk$players_frame, text = roster$NameNum[i], value = roster$PlayerID[i],
                          variable = sk$player, command = refresh_value_boxes),
           sticky = "w", pady = 1)
  }
  tclvalue(sk$player) = roster$PlayerID[1]
  listbox_set(sk$dnp_lb, roster$PlayerID, roster$NameNum, select = "none")

  sync_game_row()
  refresh_scorekeeper()
}

sync_game_row <- function(){
  if (is.null(rv$team_id)) return()
  score <- function(x) suppressWarnings(as.integer(tclvalue(x)))
  rv$games = update_games_row(rv$games, rv$team_id, rv$game_id, tclvalue(sk$date),
                              tclvalue(sk$opponent), score(sk$team_score), score(sk$opp_score))
}

# Refresh ------------------------------------------------------------------

game_unsaved <- function(){
  !(same_df(saved$games, rv$games) && same_df(saved$game_stats, rv$game_stats))
}

refresh_scorekeeper <- function(){
  has_roster = !is.null(rv$sk_roster)
  for (b in sk$btns) set_state(b, has_roster)
  set_state(sk$undo_chk, has_roster)
  set_state(sk$save_btn, has_roster && game_unsaved())
  last = if (length(rv$game_log) > 0) rv$game_log[length(rv$game_log)] else ""
  tclvalue(sk$last_log) = last
  refresh_value_boxes()
}

refresh_value_boxes <- function(){
  pid = tclvalue(sk$player)
  gs = rv$game_stats
  gs = gs[gs$PlayerID == pid & gs$GameID %in% rv$game_id, ]
  if (is.null(rv$sk_roster) || nrow(gs) != 1){
    for (b in sk$boxes) value_box_set(b, "--")
    return()
  }
  gs = suppressWarnings(calc_game_stats(gs))
  b = sk$boxes
  value_box_set(b$pts, gs$PTS)
  value_box_set(b$ft, fmt_pct(gs$`FT%`), paste0(gs$FTM, "/", gs$FTA))
  value_box_set(b$fg, fmt_pct(gs$`FG%`), paste0(gs$FGM, "/", gs$FGA))
  value_box_set(b$three, fmt_pct(gs$`3P%`), paste0(gs$FGM3, "/", gs$FGA3))
  value_box_set(b$ts, fmt_pct(gs$`TS%`))
  value_box_set(b$eff, gs$EFF)
  value_box_set(b$reb, gs$REB, paste0(gs$DREB, " + ", gs$OREB))
  value_box_set(b$blk, gs$BLK)
  value_box_set(b$stl, gs$STL)
  value_box_set(b$ast, gs$AST)
  value_box_set(b$tov, gs$TOV)
  value_box_set(b$pf, gs$PF)
}

# Callbacks ----------------------------------------------------------------

record_event <- function(id){
  pid = tclvalue(sk$player)
  if (is.null(rv$sk_roster) || pid == "") return()
  stats = sk_buttons[[id]]$stats
  undo = tclvalue(sk$undo) == "1"
  name = rv$sk_roster$NameNum[rv$sk_roster$PlayerID == pid]
  rv$game_log = add_log_entry(rv$game_log, name, stats[1], undo)
  for (stat in stats){
    rv$game_stats = update_game_stat(rv$game_stats, pid, rv$game_id, stat, undo)
  }
  rv$game_dirty = TRUE
  refresh_scorekeeper()
}

on_dnp_change <- function(){
  if (is.null(rv$sk_roster)) return()
  rv$game_stats = update_dnp(rv$game_stats, listbox_selected(sk$dnp_lb), rv$game_id)
  rv$game_dirty = TRUE
  refresh_scorekeeper()
}

on_game_info_change <- function(){
  if (is.null(rv$sk_roster)) return()
  sync_game_row()
  rv$game_dirty = TRUE
  refresh_scorekeeper()
}

on_save_game <- function(){
  date = tclvalue(sk$date)
  if (is.na(as.Date(date, optional = TRUE))){
    tkmessageBox(title = "Invalid date", icon = "error",
                 message = "Enter the game date as YYYY-MM-DD before saving.")
    return()
  }
  sync_game_row()
  write.csv(rv$games, file.path(data_dir, "Games.csv"), row.names = FALSE)
  write.csv(rv$game_stats, file.path(data_dir, "GameStats.csv"), row.names = FALSE)
  saved$games = rv$games
  saved$game_stats = rv$game_stats
  header = create_log_header(rv$team_id, rv$game_id, date, tclvalue(sk$opponent))
  cat(c(header, rv$game_log), sep = "\n",
      file = file.path(data_dir, "gamelogs", paste0(date, "_GameID_", rv$game_id, ".txt")))
  rv$game_dirty = FALSE
  refresh_scorekeeper()
}
