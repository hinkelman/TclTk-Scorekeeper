# Stats Viewer tab: filter games and summarize stats by team, game, and/or player.

sv = new.env()

sv$group_team = tclVar(1)
sv$group_game = tclVar(0)
sv$group_player = tclVar(1)
sv$stats_type = tclVar("Per game")
sv$margin_min = tclVar("")
sv$margin_max = tclVar("")
sv$margin_range = NULL
sv$data = NULL
sv$sort_col = NULL
sv$sort_desc = TRUE
sv$status = tclVar("")

# filters in cascade order; changing a filter updates the choices of those below it
filter_levels = c("leagues", "teams", "seasons", "margin", "opponents", "dates")

build_statsviewer_tab <- function(parent){
  tkgrid.columnconfigure(parent, 1, weight = 1)
  tkgrid.rowconfigure(parent, 0, weight = 1)

  # Filters ----
  filters = ttklabelframe(parent, text = "Filters", padding = 8)
  tkgrid(filters, row = 0, column = 0, sticky = "nsw", padx = c(0, 10))
  add_filter <- function(id, label){
    level = match(id, filter_levels)
    tkgrid(ttklabel(filters, text = label), sticky = "w", pady = c(4, 0))
    sv[[id]] = make_listbox(filters, height = 3, width = 26, all_none = TRUE,
                            on_select = function() refresh_stats(level + 1))
    tkgrid(sv[[id]]$frame, sticky = "ew")
  }
  add_filter("leagues", "Leagues")
  add_filter("teams", "Teams")
  add_filter("seasons", "Seasons")

  tkgrid(ttklabel(filters, text = "Scoring margin"), sticky = "w", pady = c(8, 0))
  mf = ttkframe(filters)
  sv$min_spin = ttkspinbox(mf, textvariable = sv$margin_min, width = 6, from = -100, to = 100,
                           command = function() refresh_stats(5))
  sv$max_spin = ttkspinbox(mf, textvariable = sv$margin_max, width = 6, from = -100, to = 100,
                           command = function() refresh_stats(5))
  for (s in list(sv$min_spin, sv$max_spin)){
    tkbind(s, "<Return>", function() refresh_stats(5))
    tkbind(s, "<FocusOut>", function() refresh_stats(5))
  }
  tkpack(sv$min_spin, side = "left")
  tkpack(ttklabel(mf, text = "  to  "), side = "left")
  tkpack(sv$max_spin, side = "left")
  tkgrid(mf, sticky = "w")

  add_filter("opponents", "Opponents")
  add_filter("dates", "Dates")

  # Table options ----
  right = ttkframe(parent)
  tkgrid(right, row = 0, column = 1, sticky = "nsew")
  tkgrid.columnconfigure(right, 0, weight = 1)
  tkgrid.rowconfigure(right, 1, weight = 1)

  opts = ttkframe(right)
  tkgrid(opts, row = 0, column = 0, sticky = "ew", pady = c(0, 8))

  gb = ttklabelframe(opts, text = "Group by", padding = 6)
  tkgrid(gb, row = 0, column = 0, sticky = "nsw", padx = c(0, 10))
  for (x in list(list("Team", sv$group_team), list("Game", sv$group_game),
                 list("Player", sv$group_player))){
    tkpack(ttkcheckbutton(gb, text = x[[1]], variable = x[[2]], command = on_group_change),
           anchor = "w")
  }

  sv$players_frame = ttklabelframe(opts, text = "Players", padding = 6)
  tkgrid(sv$players_frame, row = 0, column = 1, sticky = "nsw", padx = c(0, 10))
  sv$players = make_listbox(sv$players_frame, height = 4, width = 26, all_none = TRUE,
                            on_select = refresh_stats_table)
  tkgrid(sv$players$frame, sticky = "ew")

  st = ttklabelframe(opts, text = "Statistics", padding = 6)
  tkgrid(st, row = 0, column = 2, sticky = "nsw")
  sv$stats_type_btns = lapply(c("Per game", "Total"), function(x){
    b = ttkradiobutton(st, text = x, value = x, variable = sv$stats_type,
                       command = refresh_stats_table)
    tkpack(b, anchor = "w")
    b
  })

  sv$tbl = make_table(right, height = 20)
  # don't let a wide table grow the window; it scrolls horizontally instead
  tkconfigure(sv$tbl$frame, width = 700, height = 450)
  tkgrid.propagate(sv$tbl$frame, FALSE)
  tkgrid(sv$tbl$frame, row = 1, column = 0, sticky = "nsew")
  tkgrid(ttklabel(right, textvariable = sv$status, foreground = col_muted),
         row = 2, column = 0, sticky = "w", pady = c(4, 0))

  on_group_change()
}

# Filtering ----------------------------------------------------------------

set_filter <- function(id, x){
  opts = sort(unique(fill_blank(x)))
  listbox_set(sv[[id]], opts)
}

keep <- function(x, id) fill_blank(x) %in% listbox_selected(sv[[id]])

# recompute filter choices at `level` and below, then the table
refresh_stats <- function(level = 1){
  t = rv$teams
  if (level <= 1) set_filter("leagues", t$League)
  t = t[keep(t$League, "leagues"), ]
  if (level <= 2) set_filter("teams", t$Team)
  t = t[keep(t$Team, "teams"), ]
  if (level <= 3) set_filter("seasons", t$Season)
  t = t[keep(t$Season, "seasons"), ]

  g = rv$games[rv$games$TeamID %in% t$TeamID, ]
  g$Margin = g$TeamScore - g$OpponentScore
  if (level <= 4) set_margin_range(g$Margin)
  mn = suppressWarnings(as.numeric(tclvalue(sv$margin_min)))
  mx = suppressWarnings(as.numeric(tclvalue(sv$margin_max)))
  # games without a final score (e.g., in progress) are not filtered by margin
  in_range = is.na(g$Margin) | ((is.na(mn) | g$Margin >= mn) & (is.na(mx) | g$Margin <= mx))
  g = g[in_range, ]
  if (level <= 5) set_filter("opponents", g$Opponent)
  g = g[keep(g$Opponent, "opponents"), ]
  if (level <= 6) set_filter("dates", g$Date)
  g = g[keep(g$Date, "dates"), ]

  sv$teams_sub = t
  sv$games_sub = g
  refresh_players_filter()
  refresh_stats_table()
}

set_margin_range <- function(margin){
  rng = if (any(!is.na(margin))) range(margin, na.rm = TRUE) else NULL
  if (identical(rng, sv$margin_range)) return()
  sv$margin_range = rng
  if (is.null(rng)){
    tclvalue(sv$margin_min) = ""
    tclvalue(sv$margin_max) = ""
  } else {
    for (s in list(sv$min_spin, sv$max_spin)) tkconfigure(s, from = rng[1], to = rng[2])
    tclvalue(sv$margin_min) = rng[1]
    tclvalue(sv$margin_max) = rng[2]
  }
}

game_stats_sub <- function(){
  rv$game_stats |>
    filter(GameID %in% sv$games_sub$GameID) |>
    left_join(sv$games_sub, by = join_by(GameID)) |>
    left_join(sv$teams_sub, by = join_by(TeamID)) |>
    left_join(rv$players, by = join_by(PlayerID))
}

refresh_players_filter <- function(){
  d = game_stats_sub() |>
    filter(is.na(FirstName) | FirstName != "Opponent") |>
    mutate(Name = fill_blank(player_name(FirstName, LastName))) |>
    distinct(PlayerID, Name) |>
    arrange(Name)
  listbox_set(sv$players, d$PlayerID, d$Name)
}

# Summary table ------------------------------------------------------------

group_by_sel <- function(){
  c("TeamID", "GameID", "PlayerID")[c(tclvalue(sv$group_team) == "1",
                                      tclvalue(sv$group_game) == "1",
                                      tclvalue(sv$group_player) == "1")]
}

on_group_change <- function(){
  gb = group_by_sel()
  if ("PlayerID" %in% gb) tkgrid(sv$players_frame) else tkgrid.remove(sv$players_frame)
  for (b in sv$stats_type_btns) set_state(b, !("GameID" %in% gb))
  refresh_stats_table()
}

calc_stats_display <- function(data, gb, per_game){
  by_team = "TeamID" %in% gb
  by_game = "GameID" %in% gb
  by_player = "PlayerID" %in% gb
  out = data
  if (by_player){
    out = out |>
      filter(PlayerID %in% listbox_selected(sv$players)) |>
      mutate(Name = fill_blank(player_name(FirstName, LastName)))
    gb = c(gb, "Name")
  }
  if (by_team){
    # opponent stats are recorded on a roster "player" with first name Opponent;
    # swap Team and Opponent so those stats show up as the opponent's team
    opp = which(out$FirstName == "Opponent")
    out[opp, c("Team", "Opponent")] = out[opp, c("Opponent", "Team")]
    gb = c(gb, "Team")
  }
  if (by_game) gb = c(gb, "Date", "Opponent")
  if (nrow(out) == 0) return(NULL)

  out = out |>
    group_by(across(all_of(gb))) |>
    summarise(across(all_of(c("DNP", events)), ~sum(.x, na.rm = TRUE)),
              Games = length(unique(GameID))) |>
    mutate(GP = Games - DNP) |>
    ungroup()

  if (per_game){
    div = if (by_player) "GP" else "Games"
    out = mutate(out, across(all_of(events), ~round(.x / pmax(.data[[div]], 1), 2)))
  }
  out = suppressWarnings(calc_game_stats(out))

  count_col = if (by_game) {
    if (by_player) "GP"
  } else if (!per_game) {
    if (by_player) "GP" else "Games"
  }
  cols = c(if (by_team) "Team", if (by_game) c("Date", "Opponent"), if (by_player) "Name",
           count_col, stats_display_cols, if (by_player) "EFF")
  select(out, all_of(cols))
}

refresh_stats_table <- function(){
  if (is.null(sv$games_sub)) return()
  gb = group_by_sel()
  per_game = tclvalue(sv$stats_type) == "Per game" && !("GameID" %in% gb)
  data = game_stats_sub()
  sv$data = if (nrow(data) == 0) NULL else calc_stats_display(data, gb, per_game)
  if (!is.null(sv$sort_col) && !(sv$sort_col %in% colnames(sv$data))) sv$sort_col = NULL
  draw_stats_table()
}

draw_stats_table <- function(){
  d = sv$data
  if (is.null(d) || nrow(d) == 0){
    table_set_columns(sv$tbl, c(" " = "empty"), widths = 300)
    table_fill(sv$tbl, NULL)
    tclvalue(sv$status) = "No game stats match the current selections"
    return()
  }
  if (!is.null(sv$sort_col)){
    d = d[order(d[[sv$sort_col]], decreasing = sv$sort_desc, na.last = TRUE), ]
  }
  cols = colnames(d)
  headings = ifelse(cols == sv$sort_col %||% "",
                    paste(cols, if (sv$sort_desc) "▼" else "▲"), cols)
  is_num = vapply(d, is.numeric, logical(1))
  chars = mapply(function(x, nm) max(nchar(c(fmt_cell_vec(x), nm)), na.rm = TRUE), d, cols)
  widths = ifelse(is_num, 62, pmax(chars * 8 + 16, 60))
  table_set_columns(sv$tbl, setNames(cols, headings), widths = widths,
                    anchors = ifelse(is_num, "e", "w"), command = on_sort,
                    stretch = FALSE)
  table_fill(sv$tbl, d)
  tclvalue(sv$status) = paste(nrow(d), if (nrow(d) == 1) "row" else "rows",
                              "- click a column heading to sort")
}

fmt_cell_vec <- function(x) vapply(x, fmt_cell, character(1))

# click a heading to sort descending; click again to toggle
on_sort <- function(col){
  if (identical(sv$sort_col, col)){
    sv$sort_desc = !sv$sort_desc
  } else {
    sv$sort_col = col
    sv$sort_desc = TRUE
  }
  draw_stats_table()
}
