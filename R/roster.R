# Roster tab: create/edit teams and rosters, save to disk, and set the roster
# used by the scorekeeper.

rt = new.env()

teams_cols = c("League" = "League", "Team" = "Team", "Season" = "Season")
roster_cols = c("First Name" = "FirstName", "Last Name" = "LastName", "Number" = "Number")

build_roster_tab <- function(parent){
  tkgrid.columnconfigure(parent, 0, weight = 1, uniform = "rt")
  tkgrid.columnconfigure(parent, 1, weight = 1, uniform = "rt")
  tkgrid.rowconfigure(parent, 0, weight = 1)

  # Teams ----
  left = ttklabelframe(parent, text = "Teams", padding = 8)
  tkgrid(left, row = 0, column = 0, sticky = "nsew", padx = c(0, 6))
  tkgrid.columnconfigure(left, 0, weight = 1)
  tkgrid.rowconfigure(left, 0, weight = 1)

  rt$teams_tbl = make_table(left, height = 14)
  table_set_columns(rt$teams_tbl, teams_cols, widths = 150)
  table_make_editable(rt$teams_tbl, on_edit_team)
  tkbind(rt$teams_tbl$tv, "<<TreeviewSelect>>", on_select_team)
  tkgrid(rt$teams_tbl$frame, sticky = "nsew")

  btns = ttkframe(left)
  rt$add_team_btn = ttkbutton(btns, text = "Add row", command = on_add_team)
  rt$delete_team_btn = ttkbutton(btns, text = "Delete row", command = on_delete_team)
  tkpack(rt$add_team_btn, side = "left", padx = c(0, 6))
  tkpack(rt$delete_team_btn, side = "left")
  tkgrid(btns, sticky = "w", pady = c(8, 0))

  # Roster ----
  right = ttklabelframe(parent, text = "Roster", padding = 8)
  tkgrid(right, row = 0, column = 1, sticky = "nsew", padx = c(6, 0))
  tkgrid.columnconfigure(right, 0, weight = 1)
  tkgrid.rowconfigure(right, 1, weight = 1)

  rt$roster_msg = tclVar("Select a team to view its roster")
  tkgrid(ttklabel(right, textvariable = rt$roster_msg, foreground = col_muted), sticky = "w")

  rt$roster_tbl = make_table(right, height = 14)
  table_set_columns(rt$roster_tbl, roster_cols, widths = 150)
  table_make_editable(rt$roster_tbl, on_edit_roster)
  tkbind(rt$roster_tbl$tv, "<<TreeviewSelect>>", update_roster_buttons)
  tkgrid(rt$roster_tbl$frame, sticky = "nsew")

  btns = ttkframe(right)
  rt$add_player_btn = ttkbutton(btns, text = "Add row", command = on_add_player)
  rt$delete_player_btn = ttkbutton(btns, text = "Delete row", command = on_delete_player)
  tkpack(rt$add_player_btn, side = "left", padx = c(0, 6))
  tkpack(rt$delete_player_btn, side = "left")
  tkgrid(btns, sticky = "w", pady = c(8, 0))

  prev = ttklabelframe(right, text = "Players from other rosters", padding = 6)
  tkgrid(prev, sticky = "ew", pady = c(10, 0))
  tkgrid.columnconfigure(prev, 0, weight = 1)
  rt$prev_lb = make_listbox(prev, height = 5, on_select = update_roster_buttons)
  tkgrid(rt$prev_lb$frame, row = 0, column = 0, sticky = "ew")
  rt$add_prev_btn = ttkbutton(prev, text = "Add selected players", command = on_add_previous)
  tkgrid(rt$add_prev_btn, row = 0, column = 1, sticky = "n", padx = c(8, 0))

  # Save/Set ----
  bottom = ttkframe(parent)
  tkgrid(bottom, row = 1, column = 0, columnspan = 2, sticky = "e", pady = c(10, 0))
  rt$save_btn = ttkbutton(bottom, text = "Save changes", command = on_save_roster)
  rt$set_btn = ttkbutton(bottom, text = "Set roster", style = "Accent.TButton",
                         command = on_set_roster)
  tkpack(rt$save_btn, side = "left", padx = c(0, 6))
  tkpack(rt$set_btn, side = "left")

  table_fill(rt$teams_tbl, rv$teams)
  update_roster_buttons()
}

# Refresh -------------------------------------------------------------------

selected_team_row <- function() table_selected(rt$teams_tbl)

roster_unsaved <- function(){
  !(same_df(saved$teams, rv$teams) && same_df(saved$players, rv$players) &&
      same_df(saved$rosters, rv$rosters))
}

update_roster_buttons <- function(){
  team_sel = !is.null(selected_team_row())
  set_state(rt$delete_team_btn, team_sel)
  set_state(rt$add_player_btn, team_sel)
  set_state(rt$delete_player_btn, team_sel && !is.null(table_selected(rt$roster_tbl)))
  set_state(rt$add_prev_btn, team_sel && length(listbox_selected(rt$prev_lb)) > 0)
  unsaved = roster_unsaved()
  set_state(rt$save_btn, unsaved)
  set_state(rt$set_btn, !unsaved && team_sel && !is.null(rv$roster) && nrow(rv$roster) > 0)
}

refresh_roster <- function(){
  row = selected_team_row()
  if (is.null(row)){
    rv$roster = NULL
    tclvalue(rt$roster_msg) = "Select a team to view its roster"
  } else {
    team = rv$teams[row, ]
    rv$roster = create_roster_view(team$TeamID, rv$players, rv$rosters)
    lab = paste(na.omit(c(team$Team, team$Season)), collapse = " - ")
    tclvalue(rt$roster_msg) = if (lab == "") "Unnamed team" else lab
  }
  table_fill(rt$roster_tbl, rv$roster)
  refresh_previous_players()
  update_roster_buttons()
}

# players entered on other rosters that aren't on the current roster
refresh_previous_players <- function(){
  if (is.null(rv$roster)){
    listbox_set(rt$prev_lb, character(), select = "none")
    return()
  }
  d = rv$players |>
    filter(!(PlayerID %in% rv$roster$PlayerID)) |>
    mutate(PlayerName = player_name(FirstName, LastName)) |>
    filter(!is.na(PlayerName)) |>
    arrange(FirstName)
  listbox_set(rt$prev_lb, d$PlayerID, d$PlayerName, select = "none")
}

# Teams callbacks -----------------------------------------------------------

on_select_team <- function() refresh_roster()

on_add_team <- function(){
  rv$teams = add_teams_row(rv$teams)
  table_fill(rt$teams_tbl, rv$teams)
  n = nrow(rv$teams)
  table_select(rt$teams_tbl, n)
  table_edit_cell(rt$teams_tbl, n, 1, on_edit_team)
  update_roster_buttons()
}

on_delete_team <- function(){
  row = selected_team_row()
  if (is.null(row)) return()
  team = rv$teams[row, ]
  if (!confirm(paste0("Delete team '", fill_blank(team$Team), "' and its roster?"))) return()
  tmp = delete_teams_row(rv$teams, row, rv$players, rv$rosters)
  rv$teams = tmp$teams_table
  rv$players = tmp$players_table
  rv$rosters = tmp$rosters_table
  table_fill(rt$teams_tbl, rv$teams)
  refresh_roster()
}

on_edit_team <- function(row, col, value){
  rv$teams = edit_teams_row(rv$teams, row, which(colnames(rv$teams) == col), value)
  sel = selected_team_row()
  table_fill(rt$teams_tbl, rv$teams)
  table_select(rt$teams_tbl, sel)
  update_roster_buttons()
}

# Roster callbacks ----------------------------------------------------------

update_roster_state <- function(tmp){
  rv$players = tmp$players_table
  rv$rosters = tmp$rosters_table
  rv$roster = tmp$roster_view
}

on_add_player <- function(){
  row = selected_team_row()
  if (is.null(row)) return()
  update_roster_state(add_roster_row(rv$teams$TeamID[row], rv$players, rv$rosters))
  table_fill(rt$roster_tbl, rv$roster)
  n = nrow(rv$roster)
  table_select(rt$roster_tbl, n)
  table_edit_cell(rt$roster_tbl, n, 1, on_edit_roster)
  refresh_previous_players()
  update_roster_buttons()
}

on_delete_player <- function(){
  row = table_selected(rt$roster_tbl)
  if (is.null(row)) return()
  update_roster_state(delete_roster_row(rv$roster, row, rv$players, rv$rosters))
  table_fill(rt$roster_tbl, rv$roster)
  refresh_previous_players()
  update_roster_buttons()
}

on_edit_roster <- function(row, col, value){
  update_roster_state(edit_roster_row(rv$roster, row, which(colnames(rv$roster) == col),
                                      value, rv$players, rv$rosters))
  table_fill(rt$roster_tbl, rv$roster)
  table_select(rt$roster_tbl, row)
  refresh_previous_players()
  update_roster_buttons()
}

on_add_previous <- function(){
  row = selected_team_row()
  ids = listbox_selected(rt$prev_lb)
  if (is.null(row) || length(ids) == 0) return()
  team_id = rv$teams$TeamID[row]
  rv$rosters = bind_rows(rv$rosters,
                         data.frame(TeamID = team_id, PlayerID = ids, Number = NA_character_))
  refresh_roster()
}

# Save/Set callbacks --------------------------------------------------------

on_save_roster <- function(){
  write.csv(rv$teams, file.path(data_dir, "Teams.csv"), row.names = FALSE)
  write.csv(rv$players, file.path(data_dir, "Players.csv"), row.names = FALSE)
  write.csv(rv$rosters, file.path(data_dir, "Rosters.csv"), row.names = FALSE)
  saved$teams = rv$teams
  saved$players = rv$players
  saved$rosters = rv$rosters
  update_roster_buttons()
}

on_set_roster <- function(){
  if (is.null(rv$roster) || nrow(rv$roster) == 0) return()
  if (rv$game_dirty &&
      !confirm("The current game has unsaved changes. Discard them and start a new game?"))
    return()
  start_game(rv$roster)
  tkselect(nb, 1)
}
